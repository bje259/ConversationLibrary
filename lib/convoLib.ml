[@@@disable_unused_warnings]

open Core
open Yojson
module Time = Time_float_unix
open NtyGrid
open NtyGrid.ArrayArrayGrid
open JsConvTest

let getJsonFile file = Yojson.Basic.from_file file
let jsonLmtdFull = getJsonFile "./lib/outAllLmtd2.json"
let json = jsonLmtdFull

module ConversationHist = struct
  type t = Basic.t

  open Yojson.Basic.Util

  let ps x = Fmt.pr "%s" x
  let run = function Some x -> x | None -> failwith "no value"
  let prettyString jsn = Yojson.Basic.to_string jsn |> Yojson.Basic.prettify
  let pr fmt = Fmt.pr "%a\n" fmt

  let pp jsn =
    Yojson.Basic.to_string jsn |> Yojson.Basic.prettify |> fun x ->
    let () = ps x in
    x

  let historyLength json =
    let jsonList = to_list json in
    List.length jsonList

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

  type messageContentB = {
    messageB : string;
    timestampB : string;
    authorB : string;
    idB : string;
  }

  type conversationRec = {
    title : string;
    dateRange : string;
    messages : messageContentB list;
    id : string;
  }

  let defMessageContentB =
    {
      messageB = "empty";
      timestampB = "unknown";
      authorB = "unknown";
      idB = "unknown";
    }

  let defConRec =
    { title = "empty"; dateRange = ""; messages = []; id = "unknown" }

  let extract_string_field field_name default_value json =
    match member field_name json with
    | `String s -> s
    | `Null -> default_value
    | _ -> default_value

  let extract_float_field json field_name default_value =
    match member field_name json with `Float f -> f | _ -> default_value

  let extract_list_field field_name default_value json =
    match member field_name json with `List lst -> lst | _ -> default_value

  let minMaxN n json =
    convert_each
      (fun x ->
        x |> member "messages" |> to_list |> filter_member "timestamp"
        |> fun x -> `List x |> convert_each to_float)
      json
    |> (function x -> List.nth x n)
    |> run
    |> fun lst ->
    let min_max_ref =
      ref (Time_now.nanoseconds_since_unix_epoch () |> Float.of_int63, 0.)
    in
    let result =
      List.iter lst ~f:(fun elm ->
          let min_time, max_time = !min_max_ref in
          min_max_ref := (Float.min min_time elm, Float.max max_time elm))
    in
    result |> fun _ ->
    let min, max = !min_max_ref in
    let () =
      Fmt.pr "min: %s\nmax: %s"
        (convert_unix_timestamp min)
        (convert_unix_timestamp max)
    in
    ()

  let read_messages conversationJson =
    let min_max_ref =
      ref (Time_now.nanoseconds_since_unix_epoch () |> Float.of_int63, 0.)
    in
    let convTupDate (x, y) =
      let resA, resB = (convert_unix_timestamp x, convert_unix_timestamp y) in
      (resA, resB)
    in
    let messages =
      conversationJson
      |> extract_list_field "messages" []
      |> List.map ~f:(fun msg ->
             let timestampraw = extract_float_field msg "timestamp" 0. in
             let timestamp = convert_unix_timestamp timestampraw in
             let min_time, max_time = !min_max_ref in
             min_max_ref :=
               (Float.min min_time timestampraw, Float.max max_time timestampraw);
             let author =
               extract_string_field "author" defMessageContentB.authorB msg
             in
             let content =
               extract_string_field "content" defMessageContentB.messageB msg
             in
             let id = extract_string_field "id" defMessageContentB.idB msg in
             {
               timestampB = timestamp;
               authorB = author;
               messageB = content;
               idB = id;
             })
    in
    (messages, convTupDate !min_max_ref)
  (* Return the min and max timestamps *)

  let read_title json = json |> extract_string_field "title" defConRec.title

  let read_conversations json =
    json |> fun x ->
    match x with
    | `List lst ->
        lst |> fun x ->
        List.map x ~f:(fun conv ->
            let title = read_title conv in
            let messages, (dateMin, dateMax) = read_messages conv in
            let id = extract_string_field "id" defConRec.id conv in
            { title; dateRange = dateMin ^ " - " ^ dateMax; messages; id })
    | _ -> [ defConRec ]

  let wrap_text max_width text =
    let words = String.split ~on:' ' text in
    let rec wrap_text_aux ~currLn:(current_line : cell_type) ~len:current_len
        acc = function
      | [] -> List.rev (current_line :: acc)
      | word :: rest ->
          let word_len = String.length word in
          if current_len + word_len + 1 > max_width then
            wrap_text_aux ~currLn:word ~len:word_len (current_line :: acc) rest
          else
            let new_line =
              match current_line with
              | "" -> word
              | _ -> current_line ^ " " ^ word
            in
            wrap_text_aux ~currLn:new_line
              ~len:(current_len + word_len + 1)
              acc rest
    in
    wrap_text_aux ~currLn:"" ~len:0 [] words

  let split_strings_by_length max_len strings =
    Array.concat_map strings ~f:(fun x -> wrap_text max_len x |> Array.of_list)

  let is_printable c = Stdlib.Char.code c >= 32 && Stdlib.Char.code c <= 126
  let cleanse_string s = String.filter ~f:is_printable s

  let printMessageRecordB (recIn : messageContentB) () =
    Fmt.pr "Timestamp: %s \nAuthor: %s\nMessage: %s \n" recIn.timestampB
      recIn.authorB recIn.messageB

  let printMessageRecordsBS (recordsIn : messageContentB list) =
    (* List.iter recordsIn ~f:(fun x -> printMessageRecordB x ()) *)
    let open ArrayArrayGrid in
    List.fold recordsIn ~init:[| [||] |] ~f:(fun acc x ->
        let msgList = String.split ~on:'\n' x.messageB in
        let msgArray = Array.of_list msgList in
        let conc =
          Array.concat
            [
              [| [| Fmt.str "Timestamp: %s" x.timestampB |] |];
              [| [| Fmt.str "Author: %s" x.authorB |] |];
              [| msgArray |];
              [| [| Fmt.str "Id: %s" x.idB |] |];
            ]
        in
        let concFmt =
          Array.map conc ~f:(fun x -> split_strings_by_length 80 x)
        in
        Array.append acc concFmt)

  let cMap grd =
    List.of_array grd |> Array.concat |> Array.map ~f:(fun x -> [| x |])

  let printConRec (recIn : conversationRec) () =
    Fmt.pr "Title: %s\n" recIn.title;
    Fmt.pr "Date Range: %s\n" recIn.dateRange;
    Fmt.pr "Id: %s\n" recIn.id;
    List.iter recIn.messages ~f:(fun x -> printMessageRecordB x ())

  let printConRecGrd (recIn : conversationRec) =
    let open ArrayArrayGrid in
    let title = [| [| Fmt.str "Title: %s" recIn.title |] |] in
    let dateRange = [| [| Fmt.str "Date Range: %s" recIn.dateRange |] |] in
    let messages = printMessageRecordsBS recIn.messages in
    let id = [| [| Fmt.str "Id: %s" recIn.id |] |] in
    let outputa =
      Array.append title dateRange |> fun x ->
      Array.append x id |> fun y -> Array.append y messages
    in
    let outputb =
      Array.map outputa ~f:(fun y -> Array.map y ~f:(fun x -> cleanse_string x))
    in
    let outputc = cMap outputb in
    (* let () = *)
    (*   Array.iter outputc ~f:(fun y -> *)
    (*       Array.iter y ~f:(fun x -> Fmt.pr "%s\n" x)) *)
    (* in *)
    outputc

  let printConRecSum (recIn : conversationRec) =
    Array.of_list
      [
        Fmt.str "Title: %s" recIn.title;
        Fmt.str "Dates: %s" recIn.dateRange;
        " " (* Fmt.str "Id: %s \n" recIn.id; *);
      ]
  (* |> fun x -> [| x |] *)

  let printConRecSummGrd (recsIn : conversationRec list) =
    let open ArrayArrayGrid in
    let recs = List.map recsIn ~f:(fun x -> printConRecSum x) in
    let recs = Array.concat recs in
    let recs = Array.map recs ~f:(fun x -> [| x |]) in
    let outputa =
      Array.map recs ~f:(fun y -> Array.map y ~f:(fun x -> cleanse_string x))
    in
    outputa

  (* List.iter recIn.messages ~f:(fun x -> printMessageRecordB x ()) *)
  let testPrGr grd =
    let outputb =
      Array.map grd ~f:(fun y -> Array.map y ~f:(fun x -> cleanse_string x))
    in

    let concFmt =
      Array.map outputb ~f:(fun x -> split_strings_by_length 40 x)
    in
    let () =
      Array.iter concFmt ~f:(fun y ->
          Array.iter y ~f:(fun x -> Fmt.pr "%s\n" x))
    in
    concFmt

  let recordGet ?n ?m () =
    let open JsonConvGridUI in
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let limitMessagesF recs idx =
      match idx with
      | None -> recs
      | Some idx ->
          let messages = List.nth recs.messages idx |> run in
          { recs with messages = [ messages ] }
    in
    let messageRecords = read_conversations jsonInput in
    let limitRecords =
      run
        (match n with
        | None -> Some messageRecords
        | Some n -> run (List.nth messageRecords n) |> fun x -> Some [ x ])
    in
    let limitRecords = List.map limitRecords ~f:(fun x -> limitMessagesF x m) in
    (* let () = List.iter limitRecords ~f:(fun x -> printConRec x ()) in *)
    limitRecords

  let recordGetByI i () =
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let messageRecords = read_conversations jsonInput in
    let limitRecords =
      List.filter messageRecords ~f:(fun elm -> String.equal elm.id i)
      |> List.hd
    in
    printConRec (run limitRecords) ()

  let recordGetPGrd ?n ?m () =
    let open JsonConvGridUI in
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let limitMessagesF recs idx =
      match idx with
      | None -> recs
      | Some idx ->
          let messages = List.nth recs.messages idx |> run in
          { recs with messages = [ messages ] }
    in
    let messageRecords = read_conversations jsonInput in
    let limitRecords =
      run
        (match n with
        | None -> Some messageRecords
        | Some n -> run (List.nth messageRecords n) |> fun x -> Some [ x ])
    in
    let limitMessages =
      List.map limitRecords ~f:(fun x -> limitMessagesF x m)
    in
    printGrd2
      (List.map limitMessages ~f:(fun elm -> printConRecGrd elm) |> Array.concat)
      ()

  let recGetGrd ?n ?m () =
    let open JsonConvGridUI in
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let limitMessagesF recs idx =
      match idx with
      | None -> recs
      | Some idx ->
          let messages = List.nth recs.messages idx |> run in
          { recs with messages = [ messages ] }
    in
    let messageRecords = read_conversations jsonInput in
    let limitRecords =
      run
        (match n with
        | None -> Some messageRecords
        | Some n -> run (List.nth messageRecords n) |> fun x -> Some [ x ])
    in
    let limitMessages =
      List.map limitRecords ~f:(fun x -> limitMessagesF x m)
    in
    List.map limitMessages ~f:(fun elm -> printConRecGrd elm) |> Array.concat

  let recordGetByIPGrd i () =
    let open JsonConvGridUI in
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let messageRecords = read_conversations jsonInput in
    let limitRecords =
      List.filter messageRecords ~f:(fun elm -> String.equal elm.id i)
      |> List.hd
    in
    (* let limitMessages = *)
    (*   match m with *)
    (*   | m when m = -1 -> run limitRecords *)
    (*   | m -> *)
    (*       let messages = List.nth (run limitRecords).messages m |> run in *)
    (*       { (run limitRecords) with messages = [ messages ] } *)
    (* in *)
    printGrd2 (printConRecGrd (run limitRecords)) ()

  let getConvSummList ?n ?m () =
    let open JsonConvGridUI in
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let convArray = recordGet ?n ?m () in
    let convSummList = printConRecSummGrd convArray in
    convSummList
end

let testRecordGet = ConversationHist.recordGet

module JsonConvGridUI = JsonConvGridUI
module NtyGrid = NtyGrid

let () =
  try
    let open ConversationHist in
    (* let open Test in *)
    let open JsonConvGridUI in
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let length = historyLength jsonInput in
    (* let sampleMessageList = Yojson.Basic.Util.index 2 jsonInput in *)
    (* let outputA = Fmt.str "Length is %d" length in *)
    (* let outputB = prettyString sampleMessageList in *)
    (* let () = Fmt.pr "%s\n%s\n" outputA outputB in *)
    (* let messageRecords = read_conversations jsonInput in *)
    (* let limitRecords = List.nth messageRecords 2 in *)
    (* printConRec (run limitRecords) () *)
    (* printGrd2 (printConRecGrd (run limitRecords)) () *)
    printGrd2 (getConvSummList ()) ()
  with _ ->
    ConversationHist.ps "Error";
    ()
