[@@@disable_unused_warnings]

open Core
open Notty
open Notty_unix
open CoreLib
open CoreLib.Funcs
open CoreLib.Funcs.Print
open SearchUI

(* module MakePfUI = (MPfUI : MAKE_UI) *)
module Make_PrintfUI
    (G : GridType with type cell_type = string and type t = string array array) : 
sig
  module Grid : GridType

  type t = G.t

  val showUI :
    ?start:int ->
    ?num_lines:int ->
    ?f:(int * int * t -> image -> unit) ->
    t ->
    unit
end
with module Grid = G
 and type t = G.t = struct
  (* type t = string array array *)

  module Grid = G

  type t = Grid.t

  let showUI ?(start = 0) ?(num_lines = 50)
      ?(f = fun ((a : int), (b : int), (grid : Grid.t)) img -> ()) grid =
    let width = Grid.width grid in
    let height = Grid.height grid in
    let start2 = min start (height - 1) |> max 0 in
    let num_lines2 = min (max 0 (height - start)) num_lines in
    let viewPortGrid = Grid.subRows grid ~start:start2 ~num:num_lines2 in
    (* let () = *)
    (*   Fmt.pr "Start_Num 1_2: %d %d %d %d" start2 num_lines2 start num_lines *)
    (* in *)
    let vpDims = (viewPortGrid |> Grid.width, viewPortGrid |> Grid.height) in
    Grid.iteri viewPortGrid ~f:(fun x y cell ->
        (* let () = *)
        (*   Fmt.pr "viewPortGrid x y: %d %d ; w h : %d %d" x y (fst vpDims) *)
        (*     (snd vpDims) *)
        (* in *)
        if x = Grid.width viewPortGrid - 1 then
          Fmt.pr "%s\n" cell
        else
          Fmt.pr "%s " cell)
end

let create_init_state json : mainState =
  let recList = read_conversations json in
  let convSumm = getConvSummList () in
  let sel_id, grid2 =
    safeGet 0 recList |> function
    | Some x ->
        ( Some x.id,
          Option.value ~default:[| [| "None found" |] |]
            (recordGetByIary x.id ()) )
    | _ -> (None, [| [| "None found" |] |])
  in
  let grid = convSumm in
  let grid_module =
    (module ArrayArrayGrid : GridType
      with type cell_type = string
       and type t = string array array)
  in
  let open ArrayArrayGrid.Infix in
  let combinedLayoutF s1 s2 pImg pShow () =
    let main = create_viewport_image_s2 s1 s2 in
    let finImg =
      if pShow then
        I.(pImg </> main)
      else
        main
    in
    finImg
  in

  let init_popup = buildPopup "" in
  let debugMsg = "" in
  let top_line = 0 in
  let left_is_active = true in
  let right_is_active = false in
  let sel_line = 0 in
  let num_lines = 45 in
  let left_hili_color =
    Notty.A.(bg (rgb ~r:0 ~g:0 ~b:3) ++ fg (rgb ~r:3 ~g:3 ~b:0) ++ st bold)
  in
  let right_hili_color = Notty.A.empty in
  let left_step_size_sm = 3 in
  let left_step_size_lg = 9 in
  let right_step_size_sm = 1 in
  let right_step_size_lg = 10 in
  let img = I.empty in
  let term = Term.create () in
  let ui_module = (module Make_PrintfUI : MAKE_UI) in
  (* let ui_module = (module Make_NtyUI : MAKE_UI) in *)
  let show_searchUI = false in
  let searchPrompt = "" in
  let leftState =
    {
      grid;
      debugMsg;
      top_line;
      sel_line;
      num_lines;
      is_active = left_is_active;
      hili_color = left_hili_color;
      step_size_sm = left_step_size_sm;
      step_size_lg = left_step_size_lg;
      img;
      sel_id;
      grid_module;
    }
  in
  let rightState =
    {
      grid = grid2;
      debugMsg;
      top_line;
      sel_line;
      num_lines;
      is_active = right_is_active;
      hili_color = right_hili_color;
      step_size_sm = right_step_size_sm;
      step_size_lg = right_step_size_lg;
      img;
      sel_id;
      grid_module;
    }
  in
  {
    leftState;
    rightState;
    popupImg = init_popup;
    show_searchUI;
    combinedLayout =
      (fun ip show_sUI () ->
        combinedLayoutF leftState rightState ip show_sUI ());
    term;
    searchPrompt;
    recList;
  }

module Make_NtyUI
    (G : GridType with type cell_type = string and type t = string array array) :
  ExtendedUI with module Grid = G and type t = G.t = struct
  (* type t = string array array *)

  module Grid = G

  type t = Grid.t
  type state = sAryAry_state

  let termDisp (start, num_lines, grid) img =
    let term = Term.create () in
    let rec loop img () =
      Term.image term img;
      match Term.event term with `Key (`ASCII 'q', _) -> () | _ -> loop img ()
    in
    loop img ()

  let create_viewport_image grid start num_lines =
    let width = Grid.width grid in
    let height = Grid.height grid in
    let viewPortGrid =
      Grid.subRows grid
        ~start:(min start (height - 1) |> max 0)
        ~num:(min (max 0 (height - start)) num_lines)
    in
    let vpDims = (Grid.width viewPortGrid, Grid.height viewPortGrid) in
    Grid.foldi viewPortGrid
      (I.void (fst vpDims) (snd vpDims))
      ~f:(fun x y img cell ->
        let cellImg = I.(string A.empty cell |> hpad (x * 1) 0 |> vpad y 0) in
        I.(img </> cellImg))

  let showUI ?(start = 0) ?(num_lines = 50) ?(f = termDisp) grid =
    let img = create_viewport_image grid start num_lines in
    f (start, num_lines, grid) img

  let mainLoop (s : mainState) sm_step lg_step move_selection ~f () =
    match Term.event s.term with
    | `Key (`Arrow `Up, []) ->
        move_selection (-sm_step);
        f s
    | `Key (`Arrow `Up, [ `Ctrl ]) ->
        move_selection (-lg_step);
        f s
    | `Key (`Arrow `Down, []) ->
        move_selection sm_step;
        f s
    | `Key (`Arrow `Down, [ `Ctrl ]) ->
        move_selection lg_step;
        f s
    | `Key (`Arrow `Left, []) ->
        if s.rightState.is_active then
          toggle_active s;
        f s
    | `Key (`Arrow `Right, []) ->
        if s.leftState.is_active then
          toggle_active s;
        f s
    | `Key (`Enter, []) ->
        get_selectionID ~updGrd:true s ();
        f s
    | `Key (`ASCII 'f', _) ->
        s.show_searchUI <- not s.show_searchUI;
        (* Fmt.pr "Test: %b\n" s.show_searchUI; *)
        f s
    | `Key (`ASCII 'q', _) -> ()
    | _ -> f s

  let searchUILoop (s : mainState) sm_step lg_step move_selection ~f () =
    Fmt.pr "SearchUI Loop";
    match Term.event s.term with
    | `Key (`ASCII x, _) ->
        let charStr = String.make 1 x in
        let newPrompt = s.searchPrompt ^ charStr in
        let () =
          s.searchPrompt <- newPrompt;
          s.leftState.debugMsg <- s.leftState.debugMsg ^ charStr
        in
        let newPImg = buildPopup s.searchPrompt in
        s.popupImg <- newPImg;
        f s
    | `Key (`Backspace, _) ->
        let newPrompt =
          if String.length s.searchPrompt > 0 then
            String.sub s.searchPrompt ~pos:0
              ~len:(String.length s.searchPrompt - 1)
          else
            ""
        in
        let () =
          s.searchPrompt <- newPrompt;
          s.leftState.debugMsg <- s.leftState.debugMsg ^ "Backspace"
        in
        let newPImg = buildPopup s.searchPrompt in
        s.popupImg <- newPImg;
        f s
    | `Key (`Enter, []) ->
        s.show_searchUI <- not s.show_searchUI;
        f s
    | _ -> f s

  let updateImg (s : mainState) =
    let () = get_selectionID ~updGrd:true s () in
    let rec handleKey s =
      let module Grid = (val s.leftState.grid_module) in
      let actSt =
        if s.leftState.is_active then
          s.leftState
        else
          s.rightState
      in
      let lines = Grid.height actSt.grid - 1 in
      let () =
        Term.image s.term (s.combinedLayout s.popupImg s.show_searchUI ())
      in
      let move_selection step =
        if
          actSt.sel_line + step >= 0
          && actSt.sel_line + step < actSt.num_lines
          && not (Core.phys_equal actSt.hili_color Notty.A.empty)
        then
          actSt.sel_line <- actSt.sel_line + step
        else if step < 0 then
          actSt.top_line <- max 0 (actSt.top_line + step)
        else
          actSt.top_line <-
            min (lines - actSt.num_lines + 1) (actSt.top_line + step);
        get_selectionID s ()
      in
      let sm_step = actSt.step_size_sm in
      let lg_step = actSt.step_size_lg in
      let chosenLoopFunc =
        if s.show_searchUI then
          searchUILoop
        else
          mainLoop
      in
      chosenLoopFunc s sm_step lg_step move_selection ~f:handleKey ()
    in
    handleKey s;
    Term.release s.term

  let showUIWithState (s : mainState) = updateImg s
end
