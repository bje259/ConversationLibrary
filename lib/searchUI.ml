[@@@disable_unused_warnings]

open Core
open Yojson
module Time = Time_float_unix
open Notty
open Notty_unix
open CoreLib
open CoreLib.Funcs
open CoreLib.Funcs.Print

let testAryTemp =
  [| [| "╔"; "═"; "╗" |]; [| "║"; " "; "║" |]; [| "╚"; "═"; "╝" |] |]

let rptStrChar (count : int) (str : string) =
  let func acc = function 0 -> acc | _ -> acc ^ str in
  let iterList = List.range ~stop:`inclusive 0 count in
  List.fold iterList ~init:"" ~f:func

let rptStrChar (count : int) (str : string) =
  List.init count ~f:(fun _ -> str) |> String.concat

let ( <-> ) = Array.append

let buildStrings (ary : string array array) (widthFillerCount : int)
    (heightFillerCount : int) =
  let tl, tm, tr = (ary.(0).(0), ary.(0).(1), ary.(0).(2)) in
  let ml, mm, mr = (ary.(1).(0), ary.(1).(1), ary.(1).(2)) in
  let bl, _, br = (ary.(2).(0), ary.(2).(1), ary.(2).(2)) in
  let middleFiller = rptStrChar widthFillerCount mm in
  let widthFiller = rptStrChar widthFillerCount tm in
  let top = [| tl ^ widthFiller ^ tr |] in
  let middle = [| ml ^ middleFiller ^ mr |] in
  let bottom = [| bl ^ widthFiller ^ br |] in
  let fillerRows =
    List.fold (List.range 0 heightFillerCount) ~init:[] ~f:(fun acc _ ->
        middle :: acc)
    |> List.rev
  in
  let fillerRows = List.init heightFillerCount ~f:(fun _ -> middle) in
  top <-> Array.concat fillerRows <-> bottom |> Array.map ~f:(fun x -> [| x |])

let bldPopImg (ary : string array array) (width : int) (height : int) =
  let widthFillerCount = width - 2 in
  let heightFillerCount = height - 2 in
  let strAry = buildStrings ary widthFillerCount heightFillerCount in
  let module Grid :
    GridType with type cell_type = string and type t = string array array =
    ArrayArrayGrid
  in
  let img =
    Grid.foldi strAry (I.void width height) ~f:(fun x y img cell ->
        let cellImg = I.(string A.empty cell |> hpad (x * 1) 0 |> vpad y 0) in
        I.(img </> cellImg))
  in
  img

let showUI (s : mainState) _ _ _ ~(f : mainState -> unit) () =
  let width = 10 in
  let height = 10 in
  let img = bldPopImg testAryTemp width height in
  I.(s.combinedLayout s.popupImg true () </> img) |> fun img -> ()

let buildPrompt str =
  I.string A.(fg lightgreen) (Fmt.str "Search: %s" str)
  |> I.hpad 2 0 |> I.vpad 1 0

let buildPromptWCursor str =
  let cursor = I.string A.(fg lightgreen ++ st blink) "|" in
  let prompt = I.string A.(fg lightgreen) (Fmt.str "Search: %s" str) in
  I.(prompt <|> cursor) |> I.hpad 2 0 |> I.vpad 1 0

let buildPopup str =
  let canvas = bldPopImg testAryTemp 80 10 in
  let prompt =
    (* I.string A.(fg lightgreen) "Search: " |> I.hpad 2 0 |> I.vpad 1 0 *)
    buildPromptWCursor str
  in
  let promptImg = I.(prompt </> canvas) |> I.hpad 50 0 |> I.vpad 15 0 in
  promptImg
