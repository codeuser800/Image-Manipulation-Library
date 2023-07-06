open Core

let dist_error image x y error =
  let error = float_of_int error in
  if x + 1 < Image.width image
  then (
    let right_pixel = Image.get image ~x:(x + 1) ~y in
    let pix_value = int_of_float (7. /. 16. *. error) in
    Image.set
      image
      ~x:(x + 1)
      ~y
      (Pixel.( + ) right_pixel (pix_value, pix_value, pix_value)));
  if x - 1 > 0 && y + 1 < Image.height image
  then (
    let sw_pixel = Image.get image ~x:(x - 1) ~y:(y + 1) in
    let pix_value = int_of_float (3. /. 16. *. error) in
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + ) sw_pixel (pix_value, pix_value, pix_value)));
  if y + 1 < Image.height image
  then (
    let s = Image.get image ~x ~y:(y + 1) in
    let pix_value = int_of_float (3. /. 16. *. error) in
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + ) s (pix_value, pix_value, pix_value)));
  if y + 1 < Image.height image && x + 1 < Image.width image
  then (
    let se_pixel = Image.get image ~x:(x + 1) ~y:(y + 1) in
    let pix_value = int_of_float (1. /. 16. *. error) in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + ) se_pixel (pix_value, pix_value, pix_value)))
;;

(* let _ = dist_error *)

let set_error ~image x y r =
  print_s [%message (Image.get image ~x ~y : Pixel.t)];
  let max_val = Image.max_val image in
  if r > max_val / 2
  then (
    Image.set image ~x ~y (max_val, max_val, max_val);
    let error = r - Image.max_val image in
    error)
  else (
    Image.set image ~x ~y (0, 0, 0);
    let error = r in
    error)
;;

(* This should look familiar by now! *)
let transform image =
  Image.mapi image ~f:(fun ~x ~y (r, _g, _b) ->
    let error = set_error ~image x y r in
    dist_error image x y error;
    (* let _error = set_error ~image x y r in *)
    Image.get image ~x ~y)
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
