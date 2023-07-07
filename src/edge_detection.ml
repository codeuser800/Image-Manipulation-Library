open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)

let calc_horizontal_grad ~img_slice =
  let grad = [| [| -1; 0; 1 |]; [| -2; 0; 2 |]; [| -1; 0; 1 |] |] in
  let x =
    Image.foldi img_slice ~init:0 ~f:(fun ~x ~y sum (r, _g, _b) ->
      (* Core.print_s [%message "x" (r : int) (img_slice : Image.t)]; *)
      sum + (Array.get (Array.get grad y) x * r))
  in
  (* Core.print_s [%message (x : int)]; *)
  x
;;

let calc_vertical_grad ~img_slice =
  let grad = [| [| -1; -2; -1 |]; [| 0; 0; 0 |]; [| 1; 2; 1 |] |] in
  let y =
    Image.foldi img_slice ~init:0 ~f:(fun ~x ~y sum (r, _g, _b) ->
      (* Core.print_s [%message "y" (r : int) (img_slice : Image.t)]; *)
      sum + (Array.get (Array.get grad y) x * r))
  in
  (* Core.print_s [%message (y : int)]; *)
  y
;;

let transform image =
  let image = Grayscale.transform image in
  let image = Blur.transform image ~radius:2 in
  let threshold = 0.4 *. float_of_int (Image.max_val image) in
  let max_value = Image.max_val image in
  Image.mapi image ~f:(fun ~x ~y (_r, _g, _b) ->
    match
      x > 0
      && y > 0
      && x + 1 < Image.width image
      && y + 1 < Image.height image
    with
    | false -> 0, 0, 0
    | true ->
      let img_slice =
        Image.slice
          image
          ~x_start:(x - 1)
          ~x_end:(x + 2)
          ~y_start:(y - 1)
          ~y_end:(y + 2)
      in
      let gx = calc_horizontal_grad ~img_slice in
      let gy = calc_vertical_grad ~img_slice in
      let g = sqrt (float_of_int (gx * gx) +. float_of_int (gy * gy)) in
      if Float.( < ) g threshold
      then 0, 0, 0
      else max_value, max_value, max_value)
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
