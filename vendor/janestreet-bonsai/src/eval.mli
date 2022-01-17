open! Core
open! Import

val eval
  :  environment:Environment.t
  -> path:Path.t
  -> clock:Incr.Clock.t
  -> model:'model Incr.t
  -> inject:('action -> unit Effect.t)
  -> ('model, 'action, 'result) Computation.t
  -> ('model, 'action, 'result) Snapshot.t
