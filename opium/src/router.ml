module Route = struct
  open Printf

  type t =
    | Nil
    | Literal of string * t
    | Param of string option * t

  exception E of string

  let rec parse_tokens params tokens =
    match tokens with
    | [] | [ "" ] -> Nil
    | token :: tokens ->
      if token = ""
      then raise (E "Double '/' not allowed")
      else if token = "*"
      then Param (None, parse_tokens params tokens)
      else if token = "**"
      then raise (E (sprintf "double splat allowed only in the end"))
      else if token.[0] = ':'
      then (
        let name =
          let len = String.length token in
          if len > 1
          then String.sub token 1 (len - 1)
          else raise (E "Named paramter is missing a name")
        in
        let params =
          if List.mem name params
          then raise (E (sprintf "duplicate parameter %S" name))
          else name :: params
        in
        Param (Some name, parse_tokens params tokens))
      else Literal (token, parse_tokens params tokens)
  ;;

  let of_string_exn s =
    let tokens = String.split_on_char '/' s in
    match tokens with
    | "" :: tokens -> parse_tokens [] tokens
    | _ -> raise (E "route must start with /")
  ;;

  let of_string s =
    match of_string_exn s with
    | exception E s -> Error s
    | s -> Ok s
  ;;
end

module Params = struct
  type t =
    { named : (string * string) list
    ; unnamed : string list
    }

  let pp fmt { named; unnamed } =
    let pp_sep fmt () = Format.fprintf fmt ";@ " in
    let pp_named fmt named =
      Format.fprintf
        fmt
        "@[[%a]@]"
        (Format.pp_print_list ~pp_sep (fun fmt (name, value) ->
             Format.fprintf fmt "(%s,@ %s)" name value))
        named
    in
    let pp_unnamed fmt unnamed =
      Format.fprintf
        fmt
        "@[[%a]@]"
        (Format.pp_print_list ~pp_sep (fun fmt unnamed -> Format.fprintf fmt "%s" unnamed))
        unnamed
    in
    Format.fprintf
      fmt
      "@[{ named@ = %a @ ;@ unnamed = %a@ }@]"
      pp_named
      named
      pp_unnamed
      unnamed
  ;;

  let named t name = List.assoc name t.named
  let unnamed t = t.unnamed
  let empty = { named = []; unnamed = [] }

  let create route captured =
    let rec loop acc (route : Route.t) captured =
      match route, captured with
      | Nil, [] -> acc
      | Literal (_, route), _ -> loop acc route captured
      | Param (None, route), p :: captured ->
        let acc = { acc with unnamed = p :: acc.unnamed } in
        loop acc route captured
      | Param (Some name, route), p :: captured ->
        let acc = { acc with named = (name, p) :: acc.named } in
        loop acc route captured
      | Param (_, _), [] -> assert false
      | Nil, _ :: _ -> assert false
    in
    let res = loop empty route captured in
    { res with unnamed = List.rev res.unnamed }
  ;;
end

module Smap = Map.Make (String)

type 'a t =
  { data : ('a * Route.t) option
  ; literal : 'a t Smap.t
  ; param : 'a t option
  }

let rec pp f fmt { data; literal; param } =
  ignore (data, param);
  Format.pp_open_box fmt 0;
  Format.fprintf fmt "{@ ";
  Smap.iter (fun lit node -> Format.fprintf fmt "@[(%S,@ %a)@ @]" lit (pp f) node) literal;
  Format.fprintf fmt "@ }";
  Format.pp_close_box fmt ()
;;

let empty = { data = None; literal = Smap.empty; param = None }

let match_url t url =
  let tokens = String.split_on_char '/' url in
  match tokens with
  | "" :: tokens ->
    let rec loop t captured tokens =
      match tokens with
      | [ "" ] | [] ->
        (match t.data with
        | None -> None
        | Some (a, route) ->
          let params = Params.create route captured in
          Some (a, params))
      | s :: tokens ->
        let param =
          match t.param with
          | None -> None
          | Some node -> loop node (s :: captured) tokens
        in
        (match param with
        | Some _ -> param
        | None ->
          (match Smap.find_opt s t.literal with
          | None -> None
          | Some node -> loop node captured tokens))
    in
    loop t [] tokens
  | _ -> None
;;

let match_route t route =
  let rec loop t (route : Route.t) =
    match route with
    | Nil ->
      (match t.data with
      | None -> []
      | Some (_, r) -> [ r ])
    | Literal (lit, route) ->
      let by_param = by_param t route in
      let by_literal =
        match Smap.find_opt lit t.literal with
        | None -> []
        | Some node -> loop node route
      in
      by_param @ by_literal
    | Param (_, route) ->
      let by_param = by_param t route in
      let by_literal =
        Smap.fold (fun _ node acc -> loop node route :: acc) t.literal []
      in
      List.concat (by_param :: by_literal)
  and by_param t route =
    match t.param with
    | None -> []
    | Some node -> loop node route
  in
  match loop t route with
  | [] -> Ok ()
  | routes -> Error routes
;;

let add_no_check t orig_route a =
  let rec loop t (route : Route.t) =
    match route with
    | Nil -> { empty with data = Some (a, orig_route) }
    | Literal (lit, route) ->
      let literal =
        match Smap.find_opt lit t.literal with
        | None -> Smap.add lit (loop empty route) t.literal
        | Some node -> Smap.add lit (loop node route) t.literal
      in
      { t with literal }
    | Param (_, route) ->
      let param =
        match t.param with
        | None -> loop empty route
        | Some node -> loop node route
      in
      { t with param = Some param }
  in
  loop t orig_route
;;

let add t route a =
  match match_route t route with
  | Error _ -> failwith "duplicate routes"
  | Ok () -> add_no_check t route a
;;
