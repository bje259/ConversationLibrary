[@@@disable_unused_warnings]

open Core
open Yojson
module Time = Time_float_unix
open NtyGrid
open CoreLib
open CoreLib.Funcs
open CoreLib.Funcs.Print
open JsonConvGridUI
open SearchUI

module ConversationHist = struct
  type t = Basic.t

  open Yojson.Basic.Util

  (* List.iter recIn.messages ~f:(fun x -> printMessageRecordB x ()) *)
end

let testRecordGet = recordGet

module JsonConvGridUI = JsonConvGridUI
module NtyGrid = NtyGrid
module SearchUI = SearchUI
module CoreLib = CoreLib

let () =
  try
    (* let open Test in *)
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let length = historyLength jsonInput in

    let messageRecords = read_conversations jsonInput in
    let () = printGrd3 (getConvSummList ()) in
    ()
    (* printTstGrd SearchUI.testAryTemp () *)
  with err -> Fmt.pr "Error: %s" (Exn.to_string err)
