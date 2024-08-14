[@@@disable_unused_warnings]

open Core
open Core_unix
open Yojson
module Time = Time_float_unix
open NtyGrid
open Notty
open Notty_unix
open CoreLib
open CoreLib.Funcs

let getJsonFile file = Yojson.Basic.from_file file
let jsonLmtdFull = getJsonFile "./lib/outAllLmtd2.json"
let json = jsonLmtdFull

module JsonUtis = struct
  open Basic.Util
  open MaybeMnd.OptionMonad

  type t = Yojson.Basic.t

  let getConById id jsn = Basic.Util.path [ "mapping"; id ] jsn
  let ps x = Fmt.pr "%s" x
  let run = function Some x -> x | None -> failwith "no value"
  let ( let* ) a f = bind a ~f
  let ( let+ ) a f = map a ~f

  let prep jsn =
    Yojson.Basic.to_string jsn |> Yojson.Basic.prettify |> fun x ->
    let () = ps x in
    x

  (* let rec getConvAndNext id jsn () = *)
  (*   let results = getConById id jsn in *)
  (*   match results with *)
  (*   | None -> (None, fun () -> None) *)
  (*   | Some result -> *)
  (*       let nextChild = getNextChild result jsn in *)
  (*       let nextF = *)
  (*         match nextChild with *)
  (*         | None -> fun () -> None *)
  (*         | Some childId -> *)
  (*             let _, f = getConvAndNext childId jsn () in *)
  (*             let () = ps (prep (run results)) in *)
  (*             f *)
  (*       in *)
  (*       let resultStr = prep result in *)
  (*       (Some result, nextF) *)

  (* and getNextChild (a : t) jsn = *)
  (*   let* parse = Yojson.Basic.Util.path [ "children" ] a in *)
  (*   let childId = *)
  (*     parse |> Yojson.Basic.Util.index 0 |> Yojson.Basic.to_string *)
  (*   in *)
  (*   return childId *)
  (**)
  (* let test () = *)
  (*   let input, nextf = *)
  (*     getConvAndNext "b2a726b8-d338-44e0-8e52-0fd25037645c" json () *)
  (*   in *)
  (*   let+ a = return (prep (input |> run)) in *)
  (*   let () = ps a in *)
  (*   (input, nextf) *)
  (**)
  (* let testa () = *)
  (*   let+ input = getConById "b2a726b8-d338-44e0-8e52-0fd25037645c" json in *)
  (*   let+ child = Yojson.Basic.Util.path [ "children" ] input in *)
  (*   child *)

  let printConByID id jsn =
    let+ input = getConById id jsn in
    let+ a = return (prep input) in
    a

  (* let tempA = *)
  (*   List.map ConverTypes2_j.Yojson_meta_mapping.keys ~f:(fun x -> *)
  (*       [ "mapping"; x ]) *)
  (* [@@deriving refl] *)

  (* let testRefl x = Refl.show [%refl: string list list] [] x *)

  let convert_unix_timestamp (timestamp : float) =
    (* Convert the Unix timestamp to a Time.t value *)
    let time =
      if Float.(timestamp < 1000000000000.) then
        (* Time.of_float_seconds timestamp *)
        Time.of_span_since_epoch (Time.Span.of_sec timestamp)
      else
        Time.of_span_since_epoch (Time.Span.of_ms timestamp)
    in
    (* Format the time as a string *)
    let formatted_date =
      Time.format time "%Y-%m-%d %H:%M:%S" ~zone:Time.Zone.utc
    in
    formatted_date
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
(*     let layout s = s.grid in *)
(*     let debugMsg = "" in *)
(*     let lastInput = "" in *)
(*     (* let ui_module = (module Make_PrintfUI : MAKE_UI) in *) *)
(*     let ui_module = (module Make_NtyUI : MAKE_UI) in *)
(*     { grid; gridE; debugMsg; lastInput; grid_module; ui_module; layout } *)
(**)
(*   let initial_state = create_init_state (String.make 50 'a') pfUI *)
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
(*   let test () = *)
(*     let program = *)
(*       let open StMonad in *)
(*       let* a = modify (add_debug_msg "Starting layout") in *)
(*       a |> set_grid [| [| "1"; "2" |]; [| "3"; "4" |] |] |> fun s -> *)
(*       let () = display_ui s in *)
(*       return s *)
(*     in *)
(*     let calc, _ = StMonad.calculate program in *)
(*     let () = Fmt.pr "Final debug message: %s\n" calc.debugMsg in *)
(*     let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(*     UI.showUI calc.grid *)
(**)
(*   let test2 () = *)
(*     let program = *)
(*       let open StMonad in *)
(*       let* a = modify (add_debug_msg "Starting layout") in *)
(*       a |> fun s -> *)
(*       { s with ui_module = (module Make_NtyUI : MAKE_UI) } *)
(*       |> set_grid [| [| "1"; "2" |]; [| "3"; "4" |] |] *)
(*       |> fun s -> *)
(*       let () = display_ui s in *)
(*       return s *)
(*     in *)
(*     let calc, _ = StMonad.calculate program in *)
(*     let () = Fmt.pr "Final debug message: %s\n" calc.debugMsg in *)
(*     let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(*     UI.showUI calc.grid *)
(**)
(*   let printGrd grd () = *)
(*     let program = *)
(*       let open StMonad in *)
(*       let open Infix in *)
(*       let open StMonad in *)
(*       let initGrid = grd in *)
(*       let* a = modify (add_debug_msg "Starting layout") in *)
(*       fun s -> *)
(*         let () = display_ui s in *)
(*         (a, s) *)
(*     in *)
(*     let calc, _ = StMonad.calculate program in *)
(*     let () = Fmt.pr "Final debug message: %s\n" calc.debugMsg in *)
(*     let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(*     UI.showUI calc.grid *)
(**)
(*   let printGrd2 grd () = *)
(*     let program = *)
(*       let open StMonad in *)
(*       let initGrid = grd in *)
(*       let* a = modify (add_debug_msg "Starting layout") in *)
(*       a |> fun s -> *)
(*       { s with ui_module = (module Make_NtyUI : MAKE_UI) } |> set_grid initGrid *)
(*       |> fun s -> *)
(*       let () = display_ui s in *)
(*       return s *)
(*     in *)
(*     let calc, _ = StMonad.calculate program in *)
(*     let () = Fmt.pr "Final debug message: %s\n" calc.debugMsg in *)
(*     let module UI = Make_PrintfUI (ArrayArrayGrid) in *)
(*     UI.showUI calc.grid *)
(* end *)

(* function getConversationMessages(conversation) { *)
(*     var messages = []; *)
(*     var currentNode = conversation.current_node; *)
(*     while (currentNode != null) { *)
(*         var node = conversation.mapping[currentNode]; *)
(*         if ( *)
(*             node.message && *)
(*             node.message.content && *)
(*             node.message.content.content_type == "text" *)
(*             && node.message.content.parts.length > 0 && *)
(*             node.message.content.parts[0].length > 0 && *)
(*             (node.message.author.role !== "system"  || node.message.metadata.is_user_system_message) *)
(*         ) { *)
(*             author = node.message.author.role; *)
(*             if (author === "assistant") { *)
(*                 author = "ChatGPT"; *)
(*             } else if (author === "system" && node.message.metadata.is_user_system_message) { *)
(*                 author = "Custom user info" *)
(*             } *)
(*             messages.push({ author, text: node.message.content.parts[0] }); *)
(*         } *)
(*         currentNode = node.parent; *)
(*     } *)
(*     return messages.reverse(); *)
(* } *)
