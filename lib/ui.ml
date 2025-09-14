type bounding_box = { width : int; height : int; x : int; y : int }
type direction = Horizontal | Vertical | Both
type box_sides = { left : int; right : int; top : int; bottom : int }

type box = {
  mutable name : string option;
  mutable content : box_content option;
  mutable bbox : bounding_box;
  mutable text_wrap : bool;
  mutable background_color : float * float * float * float;
  mutable border : bool;
  mutable flow : direction option;
  mutable take_remaining_space : direction option;
}

and box_content = Box of box | Boxes of box list | Text of string

let get_box_sides ~(box : box) : box_sides =
  let right = box.bbox.x + box.bbox.width
  and bottom = box.bbox.y + box.bbox.height in
  { left = box.bbox.x; top = box.bbox.y; right; bottom }

let smol_box: box = {
    name = Some "test";
    background_color = (1., 1., 0., 1.);
    content = None;
    bbox = { width = 20; height = 20; x = 0; y = 0 };
    text_wrap = false;
    border = false;
    flow = None;
    take_remaining_space = None;
}

let inner_box : box =
  {
    name = None;
    background_color = (0., 1., 0., 1.);
    content = Some (Boxes [smol_box; smol_box]);
    bbox = { width = 500; height = 50; x = 0; y = 0 };
    text_wrap = false;
    border = false;
    flow = Some Horizontal;
    take_remaining_space = None;
  }

let box : box =
  {
    name = None;
    background_color = (1., 0., 0., 1.);
    content = Some (Box inner_box);
    bbox = { width = 100; height = 100; x = 0; y = 0 };
    text_wrap = false;
    border = false;
    flow = None;
    take_remaining_space = None;
  }

let smol_box_event_handler ~(e : Sdl.Sdl.event) =
  match e with
  | Sdl.Sdl.MouseMotionEvt { x; y; _ } -> (
    let _, window_height = Sdl.Sdl.sdl_get_window_size Sdl.Sdl.w in
    let _, gl_window_height = Sdl.Sdl.sdl_gl_getdrawablesize () in
    let height_ratio = gl_window_height / window_height in
    let { left; top; right; bottom } = get_box_sides ~box:smol_box in
    if x >= left && x <= right && y >= top / height_ratio && y <= bottom / height_ratio then smol_box.background_color <- (1.,0.,1.,1.)
    else smol_box.background_color <- (1., 1., 0., 1.)
  )
  | _ -> ()

let () = Ui_events.event_handlers := smol_box_event_handler :: !Ui_events.event_handlers
