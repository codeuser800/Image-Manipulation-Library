open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let calc_horizontal_grad ~img ~x ~y =
  let grad = [| [| -1; 0; 1 |]; [| -2; 0; 2 |]; [| -1; 0; 1 |] |] in
  let top_row = Array.get grad 0 in
  let curr_row = Array.get grad 1 in
  let bottom_row = Array.get grad 2 in
  let left_top =
    if x - 1 >= 0 && y - 1 >= 0 then Array.get top_row 0 * else 0
  in
  let left_mid = if x - 1 >= 0 then Array.get curr_row 0 else 0 in
  let left_down =
    if x - 1 >= 0 && y + 1 < Image.height img
    then Array.get bottom_row 0
    else 0
  in
  let right_top =
    if x + 1 < Image.width img && y - 1 >= 0 then Array.get top_row 2 else 0
  in
  let right_mid =
    if x + 1 < Image.width img then Array.get curr_row 2 else 0
  in
  let right_down =
    if x + 1 < Image.width img && y + 1 < Image.height img
    then Array.get bottom_row 2
    else 0
  in
  left_top + left_mid + left_down + right_top + right_mid + right_down
;;

let calc_vertical_grad ~img ~x ~y =
  let grad = [| [| -1; 0; 1 |]; [| -2; 0; 2 |]; [| -1; 0; 1 |] |] in
  let first_col = Array.get grad 0 in
  let second_col = Array.get grad 1 in
  let third_col = Array.get grad 2 in
  let left_top =
    if x - 1 >= 0 && y - 1 >= 0 then Array.get first_col 0 else 0
  in
  let mid_top = if y - 1 >= 0 then Array.get second_col 0 else 0 in
  let left_down =
    if x - 1 >= 0 && y + 1 < Image.height img
    then Array.get first_col 2
    else 0
  in
  let right_top =
    if x + 1 < Image.width img && y - 1 >= 0
    then Array.get third_col 0
    else 0
  in
  let mid_down =
    if y + 1 < Image.height img then Array.get second_col 2 else 0
  in
  let right_down =
    if x + 1 < Image.width img && y + 1 < Image.height img
    then Array.get third_col 2
    else 0
  in
  left_top + mid_top + left_down + right_top + mid_down + right_down
;;

let transform image =
  let image = Grayscale.transform image in
  let image = Blur.transform image ~radius:2 in
  let threshold = 0.4 *. float_of_int (Image.max_val image) in
  Image.mapi image ~f:(fun ~x ~y (_r, _g, _b) ->
    let gx = calc_horizontal_grad ~img:image ~x ~y in
    let gy = calc_vertical_grad ~img:image ~x ~y in
    let g = sqrt (float_of_int (gx * gx) +. float_of_int (gy * gy)) in
    if Float.( > ) g threshold then 0, 0, 0 else 1, 1, 1)
;;

let command =
  Command.basic
    ~summary:"Edge Detection of an Image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edge.ppm")]
;;
