open! Core
open! Import

type ('input, 'action, 'model) apply_action =
  inject:('action -> unit Effect.t)
  -> schedule_event:(unit Effect.t -> unit)
  -> 'input
  -> 'model
  -> 'action
  -> 'model

type ('model, 'action, 'result) t =
  | Return : 'result Value.t -> (unit, Nothing.t, 'result) t
  | Leaf1 :
      { input : 'input Value.t
      ; apply_action : ('input, 'action, 'model) apply_action
      ; compute : inject:('action -> unit Effect.t) -> 'input -> 'model -> 'result
      ; name : string
      ; kind : string
      }
      -> ('model, 'action, 'result) t
  | Leaf0 :
      { apply_action : (unit, 'action, 'model) apply_action
      ; compute : inject:('action -> unit Effect.t) -> 'model -> 'result
      ; name : string
      ; kind : string
      }
      -> ('model, 'action, 'result) t
  | Leaf_incr :
      { input : 'input Value.t
      ; apply_action :
          'input Incr.t
          -> inject:('action -> unit Effect.t)
          -> (schedule_event:(unit Effect.t -> unit) -> 'model -> 'action -> 'model)
               Incr.t
      ; compute :
          Incr.Clock.t
          -> 'input Incr.t
          -> 'model Incr.t
          -> inject:('action -> unit Effect.t)
          -> 'result Incr.t
      ; name : string
      }
      -> ('model, 'action, 'result) t
  | Model_cutoff :
      { t : ('m, 'a, 'r) t
      ; model : 'm Meta.Model.t
      }
      -> ('m, 'a, 'r) t
  | Subst :
      { from : ('m1, 'a1, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : ('m2, 'a2, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ('m1 * 'm2, ('a1, 'a2) Either.t, 'r2) t
  | Subst_stateless :
      { from : (unit, Nothing.t, 'r1) t
      ; via : 'r1 Type_equal.Id.t
      ; into : ('m, 'a, 'r2) t
      ; here : Source_code_position.t option
      }
      -> ('m, 'a, 'r2) t
  | Store :
      { id : 'x Type_equal.Id.t
      ; value : 'x Value.t
      ; inner : ('m, 'a, 'r) t
      }
      -> ('m, 'a, 'r) t
  | Fetch :
      { id : 'a Type_equal.Id.t
      ; default : 'r
      ; for_some : 'a -> 'r
      }
      -> (unit, Nothing.t, 'r) t
  | Assoc :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_compare : 'k -> 'k -> int
      ; key_id : 'k Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : ('model, 'action, 'result) t
      ; model_info : 'model Meta.Model.t
      ; action_info : 'action Meta.Action.t
      ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      ; model_by_k : ('model_by_k, ('k, 'model, 'cmp) Map.t) Type_equal.t
      }
      -> ('model_by_k, 'k * 'action, 'result_by_k) t
  | Assoc_simpl :
      { map : ('k, 'v, 'cmp) Map.t Value.t
      ; key_id : 'k Type_equal.Id.t
      ; data_id : 'v Type_equal.Id.t
      ; by : Path.t -> 'k -> 'v -> 'result
      ; result_by_k : ('result_by_k, ('k, 'result, 'cmp) Map.t) Type_equal.t
      }
      -> (unit, Nothing.t, 'result_by_k) t
  | Switch :
      { match_ : int Value.t
      ; arms : 'a packed Int.Map.t
      }
      -> ((int, Int.comparator_witness) Hidden.Multi_model.t, int Hidden.Action.t, 'a) t
  (* Lazy wraps the model in an option because otherwise you could make
     infinitely sized models (by eagerly expanding a recursive model) which
     would stack-overflow during eval.  [None] really means "unchanged from the
     default", and is used to halt the the eager expansion. *)
  | Lazy : 'a packed Lazy.t -> (Hidden.Model.t option, unit Hidden.Action.t, 'a) t
  | Wrap :
      { model_id : 'outer_model Type_equal.Id.t
      ; inject_id : ('outer_action -> unit Effect.t) Type_equal.Id.t
      ; inner : ('inner_model, 'inner_action, 'result) t
      ; apply_action : ('result, 'outer_action, 'outer_model) apply_action
      }
      -> ('outer_model * 'inner_model, ('outer_action, 'inner_action) Either.t, 'result) t
  | With_model_resetter :
      { t : ('m, 'a, 'r) t
      ; default_model : 'm
      }
      -> ('m, (unit, 'a) Either.t, 'r * unit Effect.t) t
  | Path : (unit, Nothing.t, Path.t) t
  | Lifecycle : Lifecycle.t option Value.t -> (unit, Nothing.t, unit) t

and 'a packed =
  | T :
      { t : ('model, 'action, 'a) t
      ; action : 'action Meta.Action.t
      ; model : 'model Meta.Model.t
      }
      -> 'a packed

let rec sexp_of_t : type m a r. (m, a, r) t -> Sexp.t = function
  | Return value -> [%sexp Return (value : Value.t)]
  | Leaf1 { name; _ } -> [%sexp Leaf (name : string)]
  | Leaf0 { name; _ } -> [%sexp Leaf0 (name : string)]
  | Leaf_incr { name; _ } -> [%sexp Leaf_incr (name : string)]
  | Model_cutoff { t; _ } -> [%sexp Model_cutoff (t : t)]
  | Subst { from; via; into; here = _ } ->
    [%sexp Subst { from : t; via : _ Type_equal.Id.t; into : t; here = None }]
  | Subst_stateless { from; via; into; here = _ } ->
    [%sexp Subst_stateless { from : t; via : _ Type_equal.Id.t; into : t; here = None }]
  | Store { id; value; inner } ->
    [%sexp Store { id : _ Type_equal.Id.t; value : Value.t; inner : t }]
  | Fetch { id; _ } -> [%sexp Fetch (id : _ Type_equal.Id.t)]
  | Assoc { map; by; _ } -> [%sexp Assoc { map : Value.t; by : t }]
  | Assoc_simpl { map; _ } -> [%sexp Assoc_simpl { map : Value.t }]
  | Switch { match_; arms; _ } ->
    let arms = arms |> Map.to_alist |> List.map ~f:[%sexp_of: int * packed] in
    [%sexp Switch { match_ : Value.t; arms : Sexp.t list }]
  | Lazy _ -> [%sexp Lazy]
  | With_model_resetter { t; _ } -> [%sexp With_model_resetter (t : t)]
  | Wrap { inner; _ } -> [%sexp Wrap (inner : t)]
  | Path -> [%sexp Path]
  | Lifecycle _ -> [%sexp Lifecycle]

and sexp_of_packed : type r. r packed -> Sexp.t = fun (T { t; _ }) -> sexp_of_t t
