open! Core
include Types
module Vdom = Vdom
module To_html = To_html

let union_of_list = List.fold ~init:Name.Set.empty ~f:Set.union

module Value = struct
  open Value

  type nonrec t = t

  let named name = Named name
  let mapn values = Mapn values

  let rec free_vars = function
    | Redirect name -> Name.Set.singleton name
    | Named name -> Name.Set.singleton name
    | Singleton -> Name.Set.empty
    | Mapn names ->
      names |> List.map ~f:free_vars |> List.fold ~init:Name.Set.empty ~f:Set.union
  ;;

  let singleton () = Singleton
end

module Computation = struct
  open Computation

  type nonrec t = t

  let free_variables { Computation.free_variables; _ } = free_variables

  let return t =
    let kind = Kind.Value t in
    let free_variables = Value.free_vars t in
    { Computation.kind; free_variables }
  ;;

  let sub ~bound ~as_ ~for_ =
    let my_binding = { Binding.bound; as_ } in
    let bindings, last_body =
      match for_.kind with
      | Bindings { bindings; last_body } -> my_binding :: bindings, last_body
      | _ -> [ my_binding ], for_
    in
    let kind = Kind.Bindings { bindings; last_body } in
    let free_var_bound = free_variables bound in
    let free_var_for = Name.Set.remove (free_variables for_) as_ in
    let free_variables = Name.Set.union free_var_bound free_var_for in
    { Computation.kind; free_variables }
  ;;

  let wrap ~name ~introduces ~bodies =
    let kind = Kind.Wrapping { name; introduces; bodies } in
    let introduced_variables = Name.Set.of_list introduces in
    let free_in_bodies = bodies |> List.map ~f:free_variables |> union_of_list in
    let free_variables = Name.Set.diff free_in_bodies introduced_variables in
    { kind; free_variables }
  ;;
end
