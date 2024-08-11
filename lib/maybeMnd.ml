[@@@disable_unused_warnings]

open Core

module OptionMonadBasic : Monad.Basic with type 'a t = 'a option = struct
  type 'a t = 'a option

  let bind oa ~f = match oa with None -> None | Some a -> f a
  let return a = Some a
  let map = `Define_using_bind
end

module OptionMonad = Monad.Make (OptionMonadBasic)
(* module Make :
     functor (X : Core.Std.Monad.Basic) ->
       sig
         val ( >>= ) : ’a X.t -> (’a -> ’b X.t) -> ’b X.t
   ...
         val map : ’a X.t -> f:(’a -> ’b) -> ’b X.t
         val join : ’a X.t X.t -> ’a X.t
*)

module Funs (X : Monad.Basic) = struct
  module M = Monad.Make (X)
  open M.Monad_infix

  let rec sequenceM = function
    | [] -> M.return []
    | x :: xs ->
        x >>= fun a ->
        sequenceM xs >>= fun vs -> M.return (a :: vs)
end
(* module Funs :
     functor (X : Core.Std.Monad.Basic) ->
    sig
        val sequenceM : ’a X.t list -> ’a list X.t
*)

module OFuns = Funs (OptionMonadBasic)
