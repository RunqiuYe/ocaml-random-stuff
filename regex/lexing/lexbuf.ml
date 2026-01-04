type t = {
  refill_buff : t -> unit;
  mutable buffer : bytes;
  mutable buffer_len : int; (* number of remaining characters in the buffer *)
  mutable abs_pos : int; (* absolute position in the input, for debugging *)
  mutable start_pos : int; (* start of token position in buffer *)
  mutable curr_pos : int; (* currently read character position in buffer *)
  mutable last_pos : int; (* last accepting position for longest prefix match *)
  mutable eof_reached : bool;
}

let from_string s =
  let len = String.length s in
  let buffer = Bytes.of_string s in
  let refill t = t.eof_reached <- true in
  {
    refill_buff = refill;
    buffer;
    buffer_len = len;
    abs_pos = 0;
    start_pos = 0;
    curr_pos = 0;
    last_pos = 0;
    eof_reached = false;
  }

let from_channel inx =
  let buf_size = 1024 in
  let buffer = Bytes.create buf_size in
  let refill t =
    if t.start_pos <> 0 then (
      (* move active bytes to the front and discard inactive bytes *)
      let n_active = t.curr_pos - t.start_pos in
      Bytes.blit t.buffer t.start_pos t.buffer 0 n_active;
      t.curr_pos <- n_active;
      t.start_pos <- 0;
      t.last_pos <- t.last_pos - n_active);
    if t.buffer_len = Bytes.length t.buffer then (
      (* grow the buffer by a factor of 1.25 if buffer is full *)
      let new_buf_size = (Bytes.length t.buffer / 4) + Bytes.length t.buffer in
      let new_buffer = Bytes.create new_buf_size in
      Bytes.blit t.buffer 0 new_buffer 0 t.buffer_len;
      t.buffer <- new_buffer);
    let n_empty = Bytes.length t.buffer - t.buffer_len in
    let n_read = input inx buffer t.curr_pos n_empty in
    t.buffer_len <- t.buffer_len + n_read
  in
  {
    refill_buff = (fun t -> if t.eof_reached then () else refill t);
    buffer;
    buffer_len = 0;
    abs_pos = 0;
    start_pos = 0;
    curr_pos = 0;
    last_pos = 0;
    eof_reached = false;
  }

let next_char t =
  if t.curr_pos >= t.buffer_len then t.refill_buff t;
  if t.curr_pos >= t.buffer_len then None
  else
    let c = Bytes.get t.buffer t.curr_pos in
    t.curr_pos <- t.curr_pos + 1;
    t.abs_pos <- t.abs_pos + 1;
    Some c

let mark_start t =
  t.start_pos <- t.curr_pos;
  t.last_pos <- t.curr_pos

let mark_accept t = t.last_pos <- t.curr_pos

let commit t =
  let len = t.last_pos - t.start_pos in
  let lexeme = Bytes.sub_string t.buffer t.start_pos len in
  t.curr_pos <- t.last_pos;
  t.start_pos <- t.last_pos;
  lexeme

let rollback t =
  let delta = t.curr_pos - t.last_pos in
  t.curr_pos <- t.last_pos;
  t.abs_pos <- t.abs_pos - delta

let position t = t.abs_pos
