
(* This file is free software, part of Neperien. See file "LICENSE" for more details. *)

module P = Neperien_Parse
module E = Neperien_Event

(*
   ### Control keys
*)

let pad s l =
  assert (String.length s <= l);
  let b = Bytes.make l ' ' in
  Bytes.blit_string s 0 b 0 (String.length s);
  Bytes.unsafe_to_string b

let align_magic l =
  let max = List.fold_left (fun i (_, s) ->
      max i (String.length s)) 0 l + 1 in
  List.map (fun (x, y) -> (x ^ " ", pad y max)) l

let ctrl_keys = align_magic [
    "Esc/q",      "Quit";
    "Enter",      "Expand";
    "Backspace",  "Retract";
    "t",          "Toggle";
    "a",          "Analyse";
  ]

(*
   ### State
   Current state of the log reader
*)


type cursor =
  | Context of int
  | Current of int

type mode =
  | Causal
  | Hierarchy

type state = {

  parse : P.t;
  filename : string;

  mode : mode;

  mutable cursor : cursor;
  mutable full_display : bool;

  context : E.t CCVector.vector;
  mutable context_start : int;

  current : E.t CCVector.vector;
  mutable current_first : int;
  mutable current_last : int;
}

let mk filename = {
  filename;
  parse = P.open_file_exn filename;
  mode = Hierarchy;
  cursor = Context 0;
  full_display = false;

  context = CCVector.create ();
  context_start = -1;

  current = CCVector.create ();
  current_first = -1;
  current_last = -1;
}

let toggle st =
  st.full_display <- not st.full_display

let set_cursor st c =
  st.cursor <- c;
  match c with
  | Context i ->
    st.context_start <- min i st.context_start
  | Current i ->
    let l = CCVector.length st.current - 1 in
    if i = l - 1 then
      st.current_first <- l
    else
      st.current_first <- max i st.current_first;
    if i = 1 then
      st.current_last <- 0
    else
      st.current_last <- min i st.current_last

let refresh_current st =
  let e = CCVector.get st.context (CCVector.length st.context - 1) in
  CCVector.clear st.current;
  let iter = match st.mode with
    | Hierarchy -> P.iter_children st.parse e
    | Causal -> assert false
  in
  CCVector.append_seq st.current iter;
  st.current_first <- CCVector.length st.current - 1;
  st.current_last <- -1

let add_ctx st e =
  CCVector.push st.context e;
  set_cursor st (Context (CCVector.length st.context - 1));
  refresh_current st

let set_ctx st = function
  | Context i as c ->
    set_cursor st c;
    CCVector.shrink st.context (i + 1);
    refresh_current st
  | Current i ->
    add_ctx st (CCVector.get st.current i)

let reset st e =
  CCVector.clear st.context;
  st.context_start <- 0;
  add_ctx st e

let init st =
  match P.root st.parse with
  | None -> raise Exit
  | Some r -> reset st { r with E.descr = "root"; }

let expand st = set_ctx st st.cursor

let rollback st =
  set_ctx st (Context (
      max 0 (CCVector.length st.context - 2)))

let move_cursor_up st =
  match st.cursor with
  | Context i ->
    set_cursor st (Context (max 0 (i - 1)))
  | Current i when i >= CCVector.length st.current - 1 ->
    set_cursor st (Context (CCVector.length st.context - 1))
  | Current i ->
    set_cursor st (Current (i + 1))

let move_cursor_up_up st =
  match st.cursor with
  | Context _ -> move_cursor_up st
  | Current i ->
    let f = st.current_first in
    let l = st.current_last in
    let j = if i < f then f else i + (f - l) in
    set_cursor st (Current (min j (CCVector.length st.current - 1)))

let move_cursor_down st =
  match st.cursor with
  | Context i when i >= CCVector.length st.context - 1 ->
    set_cursor st (Current (CCVector.length st.current - 1))
  | Context i ->
    set_cursor st (Context (i + 1))
  | Current i ->
    set_cursor st (Current (max 0 (i - 1)))

let move_cursor_down_down st =
  match st.cursor with
  | Context _ -> move_cursor_down st
  | Current i ->
    let f = st.current_first in
    let l = st.current_last in
    let j = if i > l then l else i - (f - l - 1) in
    set_cursor st (Current (max j 0))

(*
   ### Rendering
*)

let bg_blue = Notty.A.(bg blue ++ fg black ++ st bold)
let bg_black = Notty.A.(bg black ++ fg white ++ st bold)
let bg_yellow = Notty.A.(bg yellow ++ fg white ++ st bold)

let render_header st (w, h) =
  let header_s = Printf.sprintf "File: %s (%s) " st.filename (
      match st.mode with Causal -> "causes" | Hierarchy -> "hierarchy"
    ) in
  let header_s_len = String.length header_s in
  let nb_events = Notty.I.(string bg_black (
      Printf.sprintf "Currently counting %d events (at depth %d)"
        (CCVector.length st.current) (CCVector.length st.context - 1))) in
  let nb_displayed =
    if st.current_first <> -1 && st.current_last <> -1 then
      Notty.I.(string bg_black (
          Printf.sprintf ", of which %d are displayed." (st.current_first - st.current_last + 1)))
    else
      Notty.I.empty
  in
  Notty.I.(
    (string bg_black header_s <|> string bg_blue (String.make (w - header_s_len) ' '))
    <->
    (nb_events <|> nb_displayed)
  )


let render_footer (w, h) =
  let img = List.fold_left (fun i (k, s) ->
      Notty.I.(i <|> string bg_black k <|> string bg_blue s))
      Notty.I.empty ctrl_keys in
  let w' = Notty.I.width img in
  if w' < w then
    Notty.I.(img <|> string bg_blue (String.make (w - w') ' '))
  else
    img

let split_string w s =
  assert (w > 0);
  let rec aux w s i acc =
    if i >= String.length s then acc
    else begin
      let len = min w (String.length s - i) in
      aux w s (i + len) (String.sub s i len :: acc)
    end
  in
  aux w s 0 []

let render_event ~highlight ~full_display e w =
  let attr = if highlight then bg_yellow else bg_black in
  let l =
    if full_display then split_string (w - 10) e.E.descr
    else [e.E.descr]
  in
  let img = List.fold_left (fun img s ->
      Notty.I.(string attr s <-> img)) Notty.I.empty l in
  Notty.I.((string attr (Printf.sprintf "[%7d] " e.E.id)) <|> img)

let render_context st (h, w) =
  let cursor = match st.cursor with
    | Context i -> i
    | _ -> -1
  in
  let img = ref Notty.I.empty in
  for index = st.context_start to CCVector.length st.context - 1 do
    let highlight = index = cursor in
    let full_display = highlight && st.full_display in
    let e = CCVector.get st.context index in
    img := Notty.I.(!img <-> (render_event ~highlight ~full_display e w))
  done;
  !img

let render_current st (h, w) =
  if CCVector.is_empty st.current then
    Notty.I.(string bg_black "<empty>")
  else begin
    let highlight, start = match st.cursor with
      | Current i -> true, i
      | _ -> false, st.current_first
    in
    let full_display = highlight && st.full_display in
    let img = ref (
        render_event ~highlight ~full_display
          (CCVector.get st.current start) w)
    in
    let highlight = false in
    let full_display = false in
    (* Compute space left available *)
    let h' = Notty.I.height !img in
    let space = max 0 (h - h') in

    (* The minimum number of lines we want to print after the cursor position *)
    let need_space_after = if start = 0 then 0 else 1 in

    (* what is the first event to print, and do we need to print ellipsis
       before (because it is not the first in the vec) ? *)
    let first, ellipsis_first =
      let f = st.current_first in
      let l = CCVector.length st.current - 1 in
      let tmp = start + space - need_space_after in
      if f < tmp then
        f, (f < l)
      else if tmp = l then
        tmp, false
      else
        tmp - 1, true
    in
    let space_taken = first - start + (if ellipsis_first then 1 else 0) in
    let space = max 0 (space - space_taken) in
    assert (space >= need_space_after);
    (* Same as before, but for the last elemtn to print) *)
    let last, ellipsis_last =
      let tmp = start - space in
      if tmp <= 0 then
        0, false
      else
        tmp + 1, true
    in

    (* Update state information *)
    st.current_first <- first;
    st.current_last <- last;

    for index = start + 1 to first do
      let e = CCVector.get st.current index in
      img := Notty.I.((render_event ~highlight ~full_display e w) <-> !img)
    done;
    if ellipsis_first then
      img := Notty.I.(string bg_black "..." <-> !img);
    for index = start - 1 downto last do
      let e = CCVector.get st.current index in
      img := Notty.I.(!img <-> (render_event ~highlight ~full_display e w))
    done;
    if ellipsis_last then
      img := Notty.I.(!img <-> string bg_black "...");
    !img
  end

let render_st st (h, w) =
  let ctx = render_context st (h, w) in
  let ctx_h = Notty.I.height ctx in
  let cur = render_current st (h - ctx_h, w - 1) in
  Notty.I.(ctx <-> (pad ~l:1 cur))

let img st (w, h) =
  let header = render_header st (w, h) in
  let header_h = Notty.I.height header in
  let footer = render_footer (w, h) in
  let footer_h = Notty.I.height footer in
  let body = render_st st (h - header_h - footer_h - 1, w - 1) in
  Notty.I.(
    (vpad (max 0 @@ h - footer_h) 0 footer)
    </>
    (header <-> pad ~l:1 ~t:1 body)
  )

let render st term =
  Notty_lwt.Term.image term (img st (Notty_lwt.Term.size term))

(*
   ### Interface even handling
*)

let update st = function

  (* Up and Down arrows *)
  | `Key (`Arrow `Up, _) ->
    move_cursor_up st;
    Lwt.return_unit
  | `Key (`Arrow `Down, _) ->
    move_cursor_down st;
    Lwt.return_unit

  (* Page Up and Down *)
  | `Key (`Page `Up, _) ->
    move_cursor_up_up st;
    Lwt.return_unit
  | `Key (`Page `Down, _) ->
    move_cursor_down_down st;
    Lwt.return_unit

  (* Enter & Backspace (expand & retract) *)
  | `Key (`Enter, _) ->
    expand st;
    Lwt.return_unit
  | `Key (`Backspace, _) ->
    rollback st;
    Lwt.return_unit

  (* Toggle full display *)
  | `Key (`Uchar 116, _) (* 't' *) ->
    toggle st;
    Lwt.return_unit
  | _ -> Lwt.return_unit

let handle st term = function
  | `Key (`Escape, _) | `Key (`Uchar 113, _) (* 'q' *) ->
    Notty_lwt.Term.release term
  | event ->
    let%lwt () = update st event in
    render st term

(*
   ### Main function
*)

let main filename =
  let st = mk filename in
  let () = init st in
  let term = Notty_lwt.Term.create () in
  let _ = Lwt_unix.on_signal Sys.sigint (fun _ ->
      Lwt_main.run @@ Notty_lwt.Term.release term) in
  Lwt_main.run @@
  let%lwt () = render st term in
  Lwt_stream.iter_s (handle st term) (Notty_lwt.Term.events term)

let argspec = Arg.align []

let usage = "./logn file"

let () =
  let file = ref "" in
  Arg.parse argspec (fun s -> file := s) usage;
  if !file = "" then begin
    Arg.usage argspec usage;
    exit 2
  end;
  main !file

