[@@@disable_unused_warnings]

open Core
open Core_unix
open Yojson
module Time = Time_float_unix
open NtyGrid
open Notty
open Notty_unix
open CoreLib
open SearchUI

let pfUI =
  (module Make_PrintfUI ((
    ArrayArrayGrid :
      GridType with type cell_type = string and type t = string array array))
  : UI)

let initial_main_state = create_init_state CoreLib.json

module InitSt : STATE with type t = mainState = struct
  type t = mainState

  let initial_state = initial_main_state
end

module StMonad = StateMonad ((InitSt : STATE with type t = mainState))

let add_debug_msg (msg : string) (part : [ `Left | `Right | `Both ])
    (s : StMonad.state) : StMonad.state =
  let () =
    match part with
    | `Left -> s.leftState.debugMsg <- s.leftState.debugMsg ^ "\n" ^ msg
    | `Right -> s.rightState.debugMsg <- s.rightState.debugMsg ^ "\n" ^ msg
    | `Both ->
        s.leftState.debugMsg <- s.leftState.debugMsg ^ "\n" ^ msg;
        s.rightState.debugMsg <- s.rightState.debugMsg ^ "\n" ^ msg
  in
  s

let set_grid new_grid (part : [ `Left | `Right ]) (s : StMonad.state) :
    StMonad.state =
  let () =
    match part with
    | `Left -> s.leftState.grid <- new_grid
    | `Right -> s.rightState.grid <- new_grid
  in
  s

let set_popup new_popup (s : StMonad.state) : StMonad.state =
  let module Grd =
    (val s.leftState.grid_module
        : GridType with type cell_type = string and type t = string array array)
  in
  let newImg = bldPopImg new_popup 80 10 in
  let () = s.popupImg <- newImg in
  s

let set_recList new_recList (s : StMonad.state) : StMonad.state =
  { s with recList = new_recList }

let applyBoth (f : sAryAry_state -> sAryAry_state) (s : mainState) : mainState =
  let newLeft = f s.leftState in
  let newRight = f s.rightState in
  { s with leftState = newLeft; rightState = newRight }

let display_ui s =
  let module Grd =
    (val s.grid_module
        : GridType with type cell_type = string and type t = string array array)
  in
  let module UI = Make_PrintfUI (Grd) in
  let composite_grid = s.grid in
  UI.showUI ~start:s.top_line ~num_lines:s.num_lines composite_grid

let display_ui3 (s : mainState) =
  let module Grd =
    (val s.leftState.grid_module
        : GridType with type cell_type = string and type t = string array array)
  in
  let module UI = NtyGrid.Make_NtyUI (Grd) in
  s.term <- Term.create ();
  UI.showUIWithState s

let printGrd grd () =
  let program =
    let open StMonad in
    let open Infix in
    let open StMonad in
    let initGrid = grd in
    (* let* a = modify (add_debug_msg "Starting layout") in *)
    (* let* b = return (set_grid initGrid) in *)
    let* a = modify (add_debug_msg "Setting grid" `Left) in
    a |> add_debug_msg "Done" `Both |> fun b ->
    Fmt.pr "Test: %s\n" b.leftState.debugMsg;
    b |> fun c ->
    display_ui c.leftState;
    c |> return
    (*     a |> fun s -> *)
    (* s |> set_grid initGrid |> fun s -> *)
    (* let () = display_ui s in *)
  in
  let calc, _ = StMonad.calculate program in
  Fmt.pr "Final debug message: %s\n" calc.leftState.debugMsg
(* let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(* UI.showUI ~start:calc.top_line ~num_lines:calc.num_lines calc.grid *)

let printGrd2 grd () =
  let program =
    let open StMonad in
    let initGrid = grd in
    let* a = modify (add_debug_msg "First Debug" `Both) in
    a |> fun s ->
    let () = display_ui3 s in
    let* b = get in
    (* let () = Fmt.pr "Start: %d\nNumRows: %d\n" b.top_line b.num_lines in *)
    return s
  in
  let calc, _ = StMonad.calculate program in
  let () = Fmt.pr "Final debug message: %s\n" calc.leftState.debugMsg in
  ()

let printGrd3 grd =
  let open StMonad in
  let program =
    let initGrid = grd in
    let* a = modify (add_debug_msg "" `Both) in
    a |> set_grid grd `Left |> set_grid [| [||] |] `Right |> fun x ->
    display_ui3 x;
    x |> fun s -> return s
    (* let () = Fmt.pr "Start: %d\nNumRows: %d\n" b.top_line b.num_lines in *)
  in
  let calc, _ = calculate program in
  let () = Fmt.pr "Final debug message: %s\n" calc.leftState.debugMsg in
  ()
(* let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(* UI.showUI ~start:calc.top_line ~num_lines:calc.num_lines calc.grid *)

let printTstGrd popupArray () =
  let program =
    let open StMonad in
    (* let initGrid = grd in *)
    let* a = modify (add_debug_msg "" `Both) in
    a |> set_popup popupArray |> fun s ->
    (* a |> set_grid grd `Left |> set_grid [| [||] |] `Right |> fun s -> *)
    (* s.leftState.hili_color <- Notty.A.empty; *)
    (* s |> fun ss -> *)
    let () = display_ui3 s in
    (* let () = Fmt.pr "Start: %d\nNumRows: %d\n" b.top_line b.num_lines in *)
    return s
  in
  let calc, _ = StMonad.calculate program in
  let () = Fmt.pr "Final debug message: %s\n" calc.leftState.debugMsg in
  ()

let termDisp initimg () =
  let term = Term.create () in
  let rec loop img () =
    Term.image term img;
    match Term.event term with `Key (`ASCII 'q', _) -> () | _ -> loop img ()
  in
  loop initimg ()

let testPopDisp () = termDisp initial_main_state.popupImg ()
