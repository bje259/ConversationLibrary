(* Types for the conversation module *)
[@@@disable_unused_warnings]

open Notty
open Notty_unix
open Core

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

let getJsonFile file = Yojson.Basic.from_file file
let jsonLmtdFull = getJsonFile "./lib/outAllLmtd2.json"
let json = jsonLmtdFull

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
  mutable grid : string array array;
  (* mutable grid2 : string array array option; *)
  (* grid2f : string -> unit -> string array array option; *)
  mutable is_active : bool;
  mutable debugMsg : string;
  mutable sel_line : int;
  mutable sel_id : string option;
  mutable top_line : int;
  mutable num_lines : int;
  mutable img : I.t;
  mutable hili_color : A.t;
  step_size_sm : int;
  step_size_lg : int;
  (* layout : ('a, 'b) state -> 'a * ('a, 'b) state; *)
  (* layout : ('a, 'b) state -> 'a * ('a, 'b) state; *)
  grid_module :
    (module GridType
       with type cell_type = string
        and type t = string array array);
      (* layout : sAryAry_state -> string array array; *)
}

type mainState = {
  leftState : sAryAry_state;
  rightState : sAryAry_state;
  combinedLayout : Notty.I.t -> bool -> unit -> I.t;
  mutable term : Term.t;
  recList : conversationRec list; (* gridE : string; *)
  mutable searchPrompt : string;
  mutable popupImg : I.t;
  mutable show_searchUI : bool;
}

module type ExtendedUI = sig
  include UI

  type state = sAryAry_state

  val showUIWithState : mainState -> unit
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

module Funcs = struct
  open Yojson.Basic.Util
  module Time = Time_float_unix

  let wrap_text max_width text =
    let words = String.split ~on:' ' text in
    let rec wrap_text_aux ~currLn:(current_line : string) ~len:current_len acc =
      function
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

  let cMap grd =
    List.of_array grd |> Array.concat |> Array.map ~f:(fun x -> [| x |])

  module Print = struct
    let ps x = Fmt.pr "%s" x
    let pr fmt = Fmt.pr "%a\n" fmt

    let pp jsn =
      Yojson.Basic.to_string jsn |> Yojson.Basic.prettify |> fun x ->
      let () = ps x in
      x

    let messageRecordB (recIn : messageContentB) () =
      Fmt.pr "Timestamp: %s \nAuthor: %s\nMessage: %s \n" recIn.timestampB
        recIn.authorB recIn.messageB

    let messageRecordsBS (recordsIn : messageContentB list) =
      (* List.iter recordsIn ~f:(fun x -> printMessageRecordB x ()) *)
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

    let conRec (recIn : conversationRec) () =
      Fmt.pr "Title: %s\n" recIn.title;
      Fmt.pr "Date Range: %s\n" recIn.dateRange;
      Fmt.pr "Id: %s\n" recIn.id;
      List.iter recIn.messages ~f:(fun x -> messageRecordB x ())

    let conRecGrd (recIn : conversationRec) =
      let title = [| [| Fmt.str "Title: %s" recIn.title |] |] in
      let dateRange = [| [| Fmt.str "Date Range: %s" recIn.dateRange |] |] in
      let messages = messageRecordsBS recIn.messages in
      let id = [| [| Fmt.str "Id: %s" recIn.id |] |] in
      let outputa =
        Array.append title dateRange |> fun x ->
        Array.append x id |> fun y -> Array.append y messages
      in
      let outputb =
        Array.map outputa ~f:(fun y ->
            Array.map y ~f:(fun x -> cleanse_string x))
      in
      let outputc = cMap outputb in
      (* let () = *)
      (*   Array.iter outputc ~f:(fun y -> *)
      (*       Array.iter y ~f:(fun x -> Fmt.pr "%s\n" x)) *)
      (* in *)
      outputc

    let conRecSum (recIn : conversationRec) =
      Array.of_list
        [
          Fmt.str "Title: %s" recIn.title;
          Fmt.str "Dates: %s" recIn.dateRange;
          " " (* Fmt.str "Id: %s \n" recIn.id; *);
        ]
    (* |> fun x -> [| x |] *)

    let conRecSummGrd (recsIn : conversationRec list) =
      let recs = List.map recsIn ~f:(fun x -> conRecSum x) in
      let recs = Array.concat recs in
      let recs = Array.map recs ~f:(fun x -> [| x |]) in
      let outputa =
        Array.map recs ~f:(fun y -> Array.map y ~f:(fun x -> cleanse_string x))
      in
      outputa
  end

  open Print

  let run = function Some x -> x | None -> failwith "no value"
  let prettyString jsn = Yojson.Basic.to_string jsn |> Yojson.Basic.prettify

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

  let within_inc_range n ~min ~max = n >= min && n <= max

  let hili i (s : sAryAry_state) =
    let hili_color =
      if s.is_active then
        s.hili_color
      else
        Notty.A.empty
    in
    if within_inc_range (i - s.sel_line) ~min:0 ~max:1 then
      hili_color
    else
      Notty.A.empty

  let create_viewport_image_s (s : sAryAry_state) =
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
    let () =
      s.img <-
        Grid.foldi viewPortGrid
          (I.void (fst vpDims) (snd vpDims))
          ~f:(fun x y img cell ->
            let cellImg =
              I.(string (hili y s) cell |> hpad (x * 1) 0 |> vpad y 0)
            in
            I.(img </> cellImg))
    in
    (* let img2 = *)
    (*   s.grid2 |> function *)
    (*   | Some g -> *)
    (*       Grid.foldi g I.empty ~f:(fun x y img cell -> *)
    (*           let cellImg = *)
    (*             I.(string A.empty cell |> hpad (x * 1) 0 |> vpad y 0) *)
    (*           in *)
    (*           I.(img </> cellImg)) *)
    (*   | None -> I.empty *)
    (* in *)
    (* s.img <- I.(s.img <|> img2) *)
    s.img <- I.(s.img)

  let create_viewport_image_s1 (s : sAryAry_state) =
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
    Grid.foldi viewPortGrid
      (I.void (fst vpDims) (snd vpDims))
      ~f:(fun x y img cell ->
        let cellImg =
          I.(string (hili y s) cell |> hpad (x * 1) 0 |> vpad y 0)
        in
        I.(img </> cellImg))

  let create_viewport_image_s1upd (s : sAryAry_state) =
    let updatedImg = create_viewport_image_s1 s in
    s.img <- updatedImg

  let create_viewport_image_s2 (s1 : sAryAry_state) (s2 : sAryAry_state) =
    create_viewport_image_s1upd s1;
    create_viewport_image_s1upd s2;
    I.(s1.img |> hpad 0 10 <|> s2.img)

  let crt_vp_img_popup (s1 : sAryAry_state) (s2 : sAryAry_state) =
    create_viewport_image_s1upd s1;
    create_viewport_image_s1upd s2;
    I.(s1.img |> hpad 0 10 <|> s2.img)

  let recordGet ?n ?m () =
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
    conRec (run limitRecords) ()

  let recordGetPGrd ?n ?m ~pFunc () =
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
    pFunc
      (List.map limitMessages ~f:(fun elm -> conRecGrd elm) |> Array.concat)
      ()

  let recGetGrd ?n ?m () =
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
    List.map limitMessages ~f:(fun elm -> conRecGrd elm) |> Array.concat

  let recordGetByIPGrd i ~pFunc () =
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
    pFunc (conRecGrd (run limitRecords)) ()

  let recordGetByIary i () =
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let messageRecords = read_conversations jsonInput in
    let limitRecords =
      List.filter messageRecords ~f:(fun elm -> String.equal elm.id i)
      |> List.hd
    in
    Some (conRecGrd (run limitRecords))

  let get_selectionID ?(updGrd = false) (s : mainState) () =
    let adjSelLine = s.leftState.top_line + s.leftState.sel_line in
    let recIdx = adjSelLine / 3 in
    let selIdx =
      List.nth s.recList recIdx |> function
      | Some x when not updGrd ->
          let () = s.leftState.sel_id <- Some x.id in
          Some x.id
      | Some x when updGrd ->
          let () =
            s.leftState.sel_id <- Some x.id;
            s.rightState.grid <-
              Option.value ~default:[| [||] |] (recordGetByIary x.id ());
            s.rightState.top_line <- 0;
            s.rightState.sel_line <- 0
          in
          Some x.id
      | _ -> None
    in
    (* let () = Fmt.pr "sel_id: %s\n" (Option.value ~default:"None" selIdx) in *)
    ()

  let getConvSummList ?n ?m () =
    let jsonInput = getJsonFile "./lib/outAllLmtd2.json" in
    let convArray = recordGet ?n ?m () in
    let convSummList = conRecSummGrd convArray in
    convSummList

  let applyActive ~f (s : mainState) =
    if s.leftState.is_active then
      f s.leftState
    else if s.rightState.is_active then
      f s.rightState
    else
      ()

  let toggle_active main_state =
    if main_state.leftState.is_active then (
      main_state.leftState.is_active <- false;
      main_state.rightState.is_active <- true)
    else (
      main_state.leftState.is_active <- true;
      main_state.rightState.is_active <- false)
end

(* let testPrGr grd = *)
(*   let outputb = *)
(*     Array.map grd ~f:(fun y -> Array.map y ~f:(fun x -> cleanse_string x)) *)
(*   in *)
(**)
(*   let concFmt = *)
(*     Array.map outputb ~f:(fun x -> split_strings_by_length 40 x) *)
(*   in *)
(*   let () = *)
(*     Array.iter concFmt ~f:(fun y -> *)
(*         Array.iter y ~f:(fun x -> Fmt.pr "%s\n" x)) *)
(*   in *)
(*   concFmt *)
