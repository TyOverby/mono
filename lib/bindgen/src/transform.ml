open! Core
open Types

let rec replace_v (v : Value.t) ~(from : Name.t) ~(to_ : Name.t) : Value.t =
  match v with
  | Redirect n -> Redirect (if Name.equal n from then to_ else n)
  | Named n -> Named (if Name.equal n from then to_ else n)
  | Singleton -> Singleton
  | Mapn l -> Mapn (List.map l ~f:(replace_v ~from ~to_))

and replace_c
    ({ kind; free_variables } as c : Computation.t)
    ~(from : Name.t)
    ~(to_ : Name.t)
    : Computation.t
  =
  if not (Set.mem free_variables from)
  then c
  else (
    let kind : Kind.t =
      match kind with
      | Bindings { bindings; last_body } ->
        let bindings =
          List.map bindings ~f:(fun binding ->
              { binding with bound = replace_c binding.bound ~from ~to_ })
        in
        let last_body = replace_c last_body ~from ~to_ in
        Bindings { bindings; last_body }
      | Value v -> Value (replace_v v ~from ~to_)
      | Wrapping w ->
        Wrapping { w with bodies = List.map w.bodies ~f:(replace_c ~from ~to_) }
    in
    let free_variables =
      free_variables |> Fn.flip Set.remove from |> Fn.flip Set.add to_
    in
    { kind; free_variables })
;;

let compare_bindings_for_grouping
    { Binding.bound = bound1; as_ = as1 }
    { Binding.bound = bound2; as_ = as2 }
  =
  if Set.mem bound2.free_variables as1
  then -1
  else if Set.mem bound1.free_variables as2
  then 1
  else 0
;;

let compare_bindings_for_sorting
    { Binding.as_ = as1; bound = { free_variables = f1; _ } }
    { Binding.as_ = as2; bound = { free_variables = f2; _ } }
  =
  match Name.Set.compare f1 f2 with
  | 0 -> Name.compare as1 as2
  | other -> other
;;

let organize_bindings (bindings : Binding.t list) =
  bindings
  |> List.sort_and_group ~compare:compare_bindings_for_grouping
  |> List.rev
  |> List.fold
       ~init:([], [], Name.Set.empty)
       ~f:(fun (rows, down_row, missing_one_level_down) row ->
         let provided_here = Name.Set.of_list (List.map row ~f:(fun { as_; _ } -> as_)) in
         let gaps = Set.diff missing_one_level_down provided_here in
         let missing_here =
           Set.union
             gaps
             (List.fold
                row
                ~init:Name.Set.empty
                ~f:(fun acc { Binding.bound = { free_variables; _ }; _ } ->
                  Set.union acc free_variables))
         in
         let rewrite = Map.of_key_set gaps ~f:(fun _ -> Name.create ()) in
         let down_row =
           List.map down_row ~f:(fun binding ->
               let bound =
                 Map.fold rewrite ~init:binding.Binding.bound ~f:(fun ~key ~data acc ->
                     replace_c acc ~from:key ~to_:data)
               in
               { binding with bound })
         in
         let redirections =
           rewrite
           |> Map.to_alist
           |> List.map ~f:(fun (from, to_) ->
                  { Binding.bound =
                      { kind = Kind.Value (Value.Redirect from)
                      ; free_variables = Name.Set.singleton from
                      }
                  ; as_ = to_
                  })
         in
         let this_row_including_redirections =
           redirections @ row |> List.sort ~compare:compare_bindings_for_sorting
         in
         down_row :: rows, this_row_including_redirections, missing_here)
  |> fun (rows, last_row, _) -> last_row :: rows
;;

let organize_bindings
    (bindings : Binding.t list)
    ~(last_body : Computation.t)
    ~(point_to : Name.t)
  =
  organize_bindings ({ Binding.as_ = point_to; bound = last_body } :: bindings)
;;
