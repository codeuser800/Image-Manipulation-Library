open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let calc_horizontal_grad ~img_slice =
  let grad = [| [| -1; 0; 1 |]; [| -2; 0; 2 |]; [| -1; 0; 1 |] |] in
  Image.foldi img_slice ~init:0 ~f:(fun ~x ~y sum (r, _g, _b) ->
    sum + (Array.get (Array.get grad x) y * r))
;;

let calc_vertical_grad ~img_slice =
  let grad = [| [| -1; -2; 1 |]; [| 0; 0; 0 |]; [| 1; 2; 1 |] |] in
  Image.foldi img_slice ~init:0 ~f:(fun ~x ~y sum (r, _g, _b) ->
    sum + (Array.get (Array.get grad x) y * r))
;;

let transform image =
  let image = Grayscale.transform image in
  let image = Blur.transform image ~radius:2 in
  let threshold = 0.4 *. float_of_int (Image.max_val image) in
  Image.mapi image ~f:(fun ~x ~y (_r, _g, _b) ->
    if x - 1 >= 0
       && y - 1 >= 0
       && x + 1 < Image.width image
       && y + 1 < Image.height image
    then (
      let slice =
        Image.slice
          image
          ~x_start:(x - 1)
          ~x_end:(x + 1)
          ~y_start:(y - 1)
          ~y_end:(y + 1)
      in
      let gx = calc_horizontal_grad ~img_slice:slice in
      let gy = calc_vertical_grad ~img_slice:slice in
      let g = sqrt (float_of_int (gx * gx) +. float_of_int (gy * gy)) in
      if Float.( > ) g threshold then 0, 0, 0 else 1, 1, 1)
    else 0, 0, 0)
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
