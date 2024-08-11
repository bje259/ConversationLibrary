[@@@disable_unused_warnings]

open Core
open Core_unix
open Yojson
module Time = Time_float_unix
open NtyGrid
open NtyGrid.ArrayArrayGrid
open Notty
open Notty_unix

(* type ('a, 'b) state = { *)
(*   grid : 'a; *)
(*   gridE : 'b; *)
(*   debugMsg : string; *)
(*   mutable lastInput : string; *)
(*   mutable top_line : int; *)
(*   mutable num_lines : int; *)
(*   mutable img : I.t; *)
(*   mutable term : Term.t; *)
(*   (* layout : ('a, 'b) state -> 'a * ('a, 'b) state; *) *)
(*   grid_module : (module GridType with type cell_type = 'b and type t = 'a); *)
(*   ui_module : (module MAKE_UI); *)
(*   layout : ('a, 'b) state -> 'a; *)
(* } *)

let pfUI =
  (module Make_PrintfUI ((
    ArrayArrayGrid :
      GridType with type cell_type = string and type t = string array array))
  : UI)

let create_init_state : string -> 'c -> sAryAry_state =
 fun gridE uim : sAryAry_state ->
  let grid = [| [| gridE |] |] in
  let grid_module =
    (module ArrayArrayGrid : GridType
      with type cell_type = string
       and type t = string array array)
  in
  let open ArrayArrayGrid.Infix in
  let layout (s : sAryAry_state) = s.grid in
  let debugMsg = "" in
  let lastInput = "" in
  let top_line = 0 in
  let sel_line = 0 in
  let num_lines = 45 in
  let img = I.empty in
  let term = Term.create () in
  let ui_module = (module Make_PrintfUI : MAKE_UI) in
  (* let ui_module = (module Make_NtyUI : MAKE_UI) in *)
  {
    grid;
    gridE;
    debugMsg;
    lastInput;
    top_line;
    sel_line;
    num_lines;
    img;
    term;
    grid_module;
    ui_module;
    layout;
  }

let initial_state = create_init_state (String.make 50 'a') pfUI

module InitSt : STATE with type t = sAryAry_state = struct
  type t = sAryAry_state

  let initial_state = initial_state
end

module StMonad = StateMonad ((InitSt : STATE with type t = sAryAry_state))

let add_debug_msg (msg : string) (s : StMonad.state) : StMonad.state =
  { s with debugMsg = s.debugMsg ^ "\n" ^ msg }

let set_grid new_grid (s : StMonad.state) : StMonad.state =
  { s with grid = new_grid }

let add_to_layout new_grid s =
  { s with layout = Base.Fn.compose new_grid s.layout }

let display_ui s =
  let module Grd =
    (val s.grid_module
        : GridType with type cell_type = string and type t = string array array)
  in
  let module UI = (val s.ui_module) (Grd) in
  let composite_grid = s.layout s in
  UI.showUI ~start:s.top_line ~num_lines:s.num_lines composite_grid

(* let create_viewport_image (s : InitSt.t) = *)
(*   let module Grid = (val s.grid_module) in *)
(*   let width = Grid.width s.grid in *)
(*   let height = Grid.height s.grid in *)
(*   let start = s.top_line in *)
(*   let viewPortGrid = *)
(*     Grid.subRows s.grid *)
(*       ~start:(min start (height - 1) |> max 0) *)
(*       ~num:(min (max 0 (height - start)) s.num_lines) *)
(*   in *)
(*   let vpDims = (Grid.width viewPortGrid, Grid.height viewPortGrid) in *)
(*   Grid.foldi viewPortGrid *)
(*     (I.void (fst vpDims) (snd vpDims)) *)
(*     ~f:(fun x y img cell -> *)
(*       let cellImg = I.(string A.empty cell |> hpad (x * 1) 0 |> vpad y 0) in *)
(*       I.(img </> cellImg)) *)

(* let updateImg (s : InitSt.t) = *)
(*   let term = Term.create () in *)
(*   let img = create_viewport_image s in *)
(*   let rec handleKey (s : InitSt.t) img term = *)
(*     let module Grid = (val s.grid_module) in *)
(*     let lines = Grid.height s.grid - 1 in *)
(*     Term.image term img; *)
(*     match Term.event term with *)
(*     | `Key (`Arrow `Up, []) -> *)
(*         s.top_line <- max 0 (s.top_line - 1); *)
(*         let newImg = create_viewport_image s in *)
(*         handleKey s newImg term *)
(*     | `Key (`Arrow `Up, [ `Ctrl ]) -> *)
(*         s.top_line <- max 0 (s.top_line - 10); *)
(*         let newImg = create_viewport_image s in *)
(*         handleKey s newImg term *)
(*     | `Key (`Arrow `Down, []) -> *)
(*         s.top_line <- min lines (s.top_line + 1); *)
(*         let newImg = create_viewport_image s in *)
(*         handleKey s newImg term *)
(*     | `Key (`Arrow `Down, [ `Ctrl ]) -> *)
(*         s.top_line <- min lines (s.top_line + 10); *)
(*         let newImg = create_viewport_image s in *)
(*         handleKey s newImg term *)
(*     | `Key (`ASCII 'q', _) -> () *)
(*     | _ -> handleKey s img term *)
(*   in *)
(*   handleKey s img term; *)
(*   Term.release term *)

(* let display_ui2 (s : sAryAry_state) = *)
(*   let module Grd = *)
(*     (val s.grid_module *)
(*         : GridType with type cell_type = string and type t = string array array) *)
(*   in *)
(*   let module UI = (val s.ui_module) (Grd) in *)
(*   let composite_grid = s.layout s in *)
(*   UI.showUI ~f:(fun (_, _, _) _ -> updateImg s) composite_grid *)

let display_ui3 (s : sAryAry_state) =
  let module Grd =
    (val s.grid_module
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
    let* a = modify (add_debug_msg "Setting grid") in
    a |> set_grid initGrid |> add_debug_msg "Done" |> fun b ->
    Fmt.pr "Test: %s\n" b.debugMsg;
    b |> fun c ->
    display_ui c;
    c |> return
    (*     a |> fun s -> *)
    (* s |> set_grid initGrid |> fun s -> *)
    (* let () = display_ui s in *)
  in
  let calc, _ = StMonad.calculate program in
  Fmt.pr "Final debug message: %s\n" calc.debugMsg
(* let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(* UI.showUI ~start:calc.top_line ~num_lines:calc.num_lines calc.grid *)

let printGrd2 grd () =
  let program =
    let open StMonad in
    let initGrid = grd in
    let* a = modify (add_debug_msg "First Debug") in
    a |> fun s ->
    { s with ui_module = (module Make_NtyUI : MAKE_UI) } |> set_grid initGrid
    |> fun s ->
    let () = display_ui3 s in
    let* b = get in
    (* let () = Fmt.pr "Start: %d\nNumRows: %d\n" b.top_line b.num_lines in *)
    return s
  in
  let calc, _ = StMonad.calculate program in
  let () = Fmt.pr "Final debug message: %s\n" calc.debugMsg in
  ()
(* let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(* UI.showUI ~start:calc.top_line ~num_lines:calc.num_lines calc.grid *)
