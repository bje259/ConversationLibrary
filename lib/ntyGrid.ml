[@@@disable_unused_warnings]

open Core
open Notty
open Notty_unix

module type MONAD = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type STATE = sig
  type t

  val initial_state : t
end

module type STATE_MONAD = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  type state

  val apply : (state -> 'a) -> 'a t
  val access : 'a t -> 'a
  val put : state -> unit t
  val get : state -> state * state
  val modify : (state -> state) -> state -> state * state
  val calculate : (state -> 'a) -> 'a
end

module StateMonad (State : STATE) :
  STATE_MONAD with type state = State.t and type 'a t = State.t -> 'a * State.t =
struct
  type state = State.t
  type 'a t = State.t -> 'a * state

  let bind (m : State.t -> 'a * state) (f : 'a -> 'b t) : 'b t =
   fun s ->
    let a, transient_state = m s in
    let b, final_state = f a transient_state in
    (b, final_state)

  let return (a : 'a) (s : state) = (a, s)
  let access m = match m State.initial_state with x, _ -> x
  let put (s : state) _ = ((), s)
  let get s = (s, s)
  let ( >>= ) = bind
  let ( let* ) = bind

  let apply f =
    let* s = get in
    return (f s)

  let modify f (s : state) =
    let new_state = f s in
    (new_state, new_state)

  let calculate (f : state -> 'a) = f State.initial_state
end

module type GridType = sig
  type t
  type cell_type

  val width : t -> int
  val height : t -> int
  val get : t -> x:int -> y:int -> cell_type
  val set : t -> x:int -> y:int -> cell_type -> t
  val foldi : t -> 'a -> f:(int -> int -> 'a -> cell_type -> 'a) -> 'a
  val iteri : t -> f:(int -> int -> cell_type -> unit) -> unit
  val map : t -> g:(cell_type -> cell_type) -> t
  val mapRows : t -> f:(int -> cell_type array -> cell_type array) -> t
  val subRows : t -> start:int -> num:int -> t
  val map2_exn : t -> t -> f:(cell_type -> cell_type -> cell_type) -> t
  val hstack : cell_type -> t -> t -> t
  val vstack : t -> t -> t
  val ( <-> ) : 'a array -> 'a array -> 'a array

  val ( <|> ) :
    cell_type array array -> cell_type array array -> cell_type array array

  module Infix : sig
    val ( <-> ) : 'a array -> 'a array -> 'a array

    val ( <|> ) :
      cell_type array array -> cell_type array array -> cell_type array array
  end
end

module ArrayArrayGrid :
  GridType with type cell_type = string and type t = string array array = struct
  (* type t = string array array *)
  type cell_type = string
  type t = string array array

  let width g = Array.length g.(0)
  let height g = Array.length g
  let get g ~x ~y = g.(y).(x)

  let set g ~x ~y cell =
    g.(y).(x) <- cell;
    g

  let foldi grid acc ~f =
    Array.foldi grid ~init:acc ~f:(fun y acc row ->
        Array.foldi row ~init:acc ~f:(fun x acc cell -> f x y acc cell))

  let iteri grid ~f =
    Array.iteri grid ~f:(fun y row ->
        Array.iteri row ~f:(fun x cell -> f x y cell))

  let map grid ~g = Array.map grid ~f:(Array.map ~f:g)
  let mapRows grid ~f = Array.mapi grid ~f
  let subRows grid ~start ~num = Array.sub grid ~pos:start ~len:num

  let map2_exn grid1 grid2 ~f =
    Base.Array.map2_exn grid1 grid2 ~f:(fun row1 row2 ->
        Base.Array.map2_exn row1 row2 ~f)

  let extend_rows_to_match filler g1 g2 =
    let h1 = Array.length g1 in
    let h2 = Array.length g2 in
    match compare h1 h2 with
    | 0 -> (g1, g2)
    | x when x > 0 ->
        let extended_g2 = Array.append g2 (Array.create ~len:(h1 - h2) [||]) in
        let extended_g2 =
          Array.map extended_g2 ~f:(fun row ->
              if Array.length row = 0 then
                Array.create ~len:(Array.length g1.(0)) filler
              else
                row)
        in
        (g1, extended_g2)
    | _ ->
        let extended_g1 = Array.append g1 (Array.create ~len:(h2 - h1) [||]) in
        let extended_g1 =
          Array.map extended_g1 ~f:(fun row ->
              if Array.length row = 0 then
                Array.create ~len:(Array.length g2.(0)) filler
              else
                row)
        in
        (extended_g1, g2)

  let extend_columns_to_match filler g1 g2 =
    let w1 = Array.length g1.(0) in
    let w2 = Array.length g2.(0) in
    let g1_extended =
      if w1 < w2 then
        Array.map g1 ~f:(fun row ->
            Array.append row (Array.create ~len:(w2 - w1) filler))
      else
        g1
    in
    let g2_extended =
      if w2 < w1 then
        Array.map g2 ~f:(fun row ->
            Array.append row (Array.create ~len:(w1 - w2) filler))
      else
        g2
    in
    (g1_extended, g2_extended)

  let extend_to_match filler g1 g2 =
    let g1, g2 = extend_rows_to_match filler g1 g2 in
    extend_columns_to_match filler g1 g2

  let hstack filler g1 g2 =
    let g1, g2 = extend_to_match filler g1 g2 in
    try Array.map2_exn g1 g2 ~f:Array.append
    with _ -> failwith "hstack failed"

  let vstack grid1 grid2 = Array.append grid1 grid2
  let ( <-> ) = vstack
  let ( <|> ) = hstack " "

  module Infix : sig
    val ( <-> ) : 'a array -> 'a array -> 'a array

    val ( <|> ) :
      cell_type array array -> cell_type array array -> cell_type array array
  end = struct
    let ( <-> ) = ( <-> )
    let ( <|> ) = ( <|> )
  end
end

module type UI = sig
  module Grid : GridType

  type t = Grid.t

  val showUI :
    ?start:int ->
    ?num_lines:int ->
    ?f:(int * int * t -> image -> unit) ->
    t ->
    unit
end

module type MAKE_UI = functor
  (G : GridType with type cell_type = string and type t = string array array)
  -> sig
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
 and type t = G.t

type sAryAry_state = {
  grid : string array array;
  gridE : string;
  debugMsg : string;
  mutable lastInput : string;
  mutable sel_line : int;
  mutable top_line : int;
  mutable num_lines : int;
  mutable img : I.t;
  mutable term : Term.t;
  (* layout : ('a, 'b) state -> 'a * ('a, 'b) state; *)
  grid_module :
    (module GridType
       with type cell_type = string
        and type t = string array array);
  ui_module : (module MAKE_UI);
  layout : sAryAry_state -> string array array;
}

module type ExtendedUI = sig
  include UI

  type state = sAryAry_state

  val showUIWithState : sAryAry_state -> unit
end

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

  (* let updateImg (start, num_lines, grid) img = *)
  (*   let term = Term.create () in *)
  (*   let rec handleKey (start, num_lines) img term = *)
  (*     let lines = Grid.height grid - 1 in *)
  (*     Term.image term img; *)
  (*     match Term.event term with *)
  (*     | `Key (`Arrow `Up, []) -> *)
  (*         let newStart = max 0 (start - 1) in *)
  (*         let newImg = create_viewport_image grid newStart num_lines in *)
  (*         handleKey (newStart, num_lines) newImg term *)
  (*     | `Key (`Arrow `Up, [ `Ctrl ]) -> *)
  (*         let newStart = max 0 (start - 10) in *)
  (*         let newImg = create_viewport_image grid newStart num_lines in *)
  (*         handleKey (newStart, num_lines) newImg term *)
  (*     | `Key (`Arrow `Down, []) -> *)
  (*         let newStart = min lines (start + 1) in *)
  (*         let newImg = create_viewport_image grid newStart num_lines in *)
  (*         handleKey (newStart, num_lines) newImg term *)
  (*     | `Key (`Arrow `Down, [ `Ctrl ]) -> *)
  (*         let newStart = min lines (start + 10) in *)
  (*         let newImg = create_viewport_image grid newStart num_lines in *)
  (*         handleKey (newStart, num_lines) newImg term *)
  (*     | `Key (`ASCII 'q', _) -> () *)
  (*     | _ -> handleKey (start, num_lines) img term *)
  (*   in *)
  (*   handleKey (start, num_lines) img term; *)
  (*   Term.release term *)

  let showUI ?(start = 0) ?(num_lines = 50) ?(f = termDisp) grid =
    let img = create_viewport_image grid start num_lines in
    f (start, num_lines, grid) img

  let within_inc_range n ~min ~max = n >= min && n <= max

  let hili i sel_line =
    if within_inc_range (i - sel_line) ~min:0 ~max:1 then
      Notty.A.(bg (rgb ~r:0 ~g:0 ~b:3) ++ fg (rgb ~r:3 ~g:3 ~b:0) ++ st bold)
    else
      Notty.A.empty

  let create_viewport_image_s (s : state) =
    let module Grid = (val s.grid_module) in
    let width = Grid.width s.grid in
    let height = Grid.height s.grid in
    let start = s.top_line in
    let viewPortGrid =
      Grid.subRows s.grid
        ~start:(min start (height - 1) |> max 0)
        ~num:(min (max 0 (height - start)) s.num_lines)
    in
    let vpDims = (Grid.width viewPortGrid, Grid.height viewPortGrid) in
    s.img <-
      Grid.foldi viewPortGrid
        (I.void (fst vpDims) (snd vpDims))
        ~f:(fun x y img cell ->
          let cellImg =
            I.(string (hili y s.sel_line) cell |> hpad (x * 1) 0 |> vpad y 0)
          in
          I.(img </> cellImg))

  let updateImg (s : state) =
    create_viewport_image_s s;
    let rec handleKey () =
      let module Grid = (val s.grid_module) in
      let lines = Grid.height s.grid - 1 in
      Term.image s.term s.img;

      let move_selection step =
        if s.sel_line + step >= 0 && s.sel_line + step < s.num_lines then
          s.sel_line <- s.sel_line + step
        else if step < 0 then
          s.top_line <- max 0 (s.top_line + step)
        else
          s.top_line <- min (lines - s.num_lines + 1) (s.top_line + step);
        create_viewport_image_s s
      in

      match Term.event s.term with
      | `Key (`Arrow `Up, []) ->
          move_selection (-3);
          handleKey ()
      | `Key (`Arrow `Up, [ `Ctrl ]) ->
          move_selection (-9);
          handleKey ()
      | `Key (`Arrow `Down, []) ->
          move_selection 3;
          handleKey ()
      | `Key (`Arrow `Down, [ `Ctrl ]) ->
          move_selection 9;
          handleKey ()
      | `Key (`ASCII 'q', _) -> ()
      | _ -> handleKey ()
    in
    handleKey ();
    Term.release s.term

  let rec event_loop ?(imgf = updateImg) s =
    let module Grid = (val s.grid_module) in
    let lines = Grid.height s.grid - 1 in
    Term.image s.term s.img;
    match Term.event s.term with
    | `Key (`Arrow `Up, []) ->
        s.top_line <- max 0 (s.top_line - 1);
        updateImg s
    | `Key (`Arrow `Down, []) ->
        s.top_line <- min lines (s.top_line + 1);
        updateImg s
    | `Key (`ASCII 'q', _) -> ()
    | _ -> event_loop s ~imgf:updateImg

  let showUIWithState (s : state) =
    let img = create_viewport_image s.grid s.top_line s.num_lines in
    s.img <- img;
    updateImg s
end

(* module Test = struct *)
(*   type ('a, 'b) state = { *)
(*     grid : 'a; *)
(*     gridE : 'b; *)
(*     debugMsg : string; *)
(*     lastInput : string; *)
(*     (* layout : ('a, 'b) state -> 'a * ('a, 'b) state; *) *)
(*     grid_module : (module GridType with type cell_type = 'b and type t = 'a); *)
(*     ui_module : (module MAKE_UI); *)
(*     layout : ('a, 'b) state -> 'a; *)
(*   } *)
(**)
(*   let pfUI = *)
(*     (module Make_PrintfUI (( *)
(*       ArrayArrayGrid : *)
(*         GridType with type cell_type = string and type t = string array array)) *)
(*     : UI) *)
(**)
(*   let create_init_state : 'b -> 'c -> ('a, 'b) state = *)
(*    fun gridE uim : (string array array, string) state -> *)
(*     let grid = [| [| gridE |] |] in *)
(*     let grid_module = *)
(*       (module ArrayArrayGrid : GridType *)
(*         with type cell_type = string *)
(*          and type t = string array array) *)
(*     in *)
(*     let open ArrayArrayGrid.Infix in *)
(*     let layout s = s.grid <-> s.grid <|> (s.grid <-> s.grid) in *)
(*     let debugMsg = "" in *)
(*     let lastInput = "" in *)
(*     let ui_module = (module Make_PrintfUI : MAKE_UI) in *)
(*     { grid; gridE; debugMsg; lastInput; grid_module; ui_module; layout } *)
(**)
(*   let initial_state = create_init_state "0" pfUI *)
(**)
(*   module InitSt : STATE with type t = (string array array, string) state = *)
(*   struct *)
(*     type t = (string array array, string) state *)
(**)
(*     let initial_state = initial_state *)
(*   end *)
(**)
(*   module StMonad = StateMonad (( *)
(*     InitSt : STATE with type t = (string array array, string) state)) *)
(**)
(*   let add_debug_msg (msg : string) (s : StMonad.state) : StMonad.state = *)
(*     { s with debugMsg = msg } *)
(**)
(*   let set_grid new_grid (s : StMonad.state) : StMonad.state = *)
(*     { s with grid = new_grid } *)
(**)
(*   let add_to_layout new_grid s = *)
(*     { s with layout = Base.Fn.compose new_grid s.layout } *)
(**)
(*   let display_ui s = *)
(*     let module Grd = *)
(*       (val s.grid_module *)
(*           : GridType *)
(*           with type cell_type = string *)
(*            and type t = string array array) *)
(*     in *)
(*     let module UI = (val s.ui_module) (Grd) in *)
(*     let composite_grid = s.layout s in *)
(*     UI.showUI composite_grid *)
(**)
(*   (* let test () = *) *)
(*   (*   let program = *) *)
(*   (*     let open StMonad in *) *)
(*   (*     let* a = modify (add_debug_msg "Starting layout") in *) *)
(*   (*     a |> set_grid [| [| "1"; "2" |]; [| "3"; "4" |] |] |> fun s -> *) *)
(*   (*     let () = display_ui s in *) *)
(*   (*     return s *) *)
(*   (*   in *) *)
(*   (*   let calc, _ = StMonad.calculate program in *) *)
(*   (*   let () = Fmt.pr "Final debug message: %s\n" calc.debugMsg in *) *)
(*   (*   let module UI = Make_PrintfUI (ArrayArrayGrid) in *) *)
(*   (*   UI.showUI calc.grid *) *)
(**)
(*   (* let test2 () = *) *)
(*   (*   let program = *) *)
(*   (*     let open StMonad in *) *)
(*   (*     let* a = modify (add_debug_msg "Starting layout") in *) *)
(*   (*     a |> fun s -> *) *)
(*   (*     { s with ui_module = (module Make_NtyUI : MAKE_UI) } *) *)
(*   (*     |> set_grid [| [| "1"; "2" |]; [| "3"; "4" |] |] *) *)
(*   (*     |> fun s -> *) *)
(*   (*     let () = display_ui s in *) *)
(*   (*     return s *) *)
(*   (*   in *) *)
(*   (*   let calc, _ = StMonad.calculate program in *) *)
(*   (*   let () = Fmt.pr "Final debug message: %s\n" calc.debugMsg in *) *)
(*   (*   let module UI = Make_PrintfUI (ArrayArrayGrid) in *) *)
(*   (*   UI.showUI calc.grid *) *)
(* end *)
