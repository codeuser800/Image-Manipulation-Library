open Core

let dist_error image x y error =
  if x + 1 < Image.width image
  then (
    let right_pixel = Image.get image ~x:(x + 1) ~y:(y + 1) in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + ) right_pixel (7 * 255 / 16 * error, 7 * 255 / 16 * error, 7 * 255 / 16 * error)));
  if x - 1 > 0 && y + 1 < Image.height image
  then (
    let sw_pixel = Image.get image ~x:(x - 1) ~y:(y - 1) in
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + ) sw_pixel (3 * 255 / 16 * error, 3 * 255 / 16 * error, 3 * 255 / 16 * error)));
  if y + 1 < Image.height image
  then (
    let s = Image.get image ~x ~y:(y - 1) in
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + ) s (3 * 255 / 16 * error, 3 * 255 / 16 * error, 3 * 255 / 16 * error)));
  if y + 1 < Image.height image && x + 1 < Image.width image
  then (
    let se_pixel = Image.get image ~x:(x + 1) ~y:(y + 1) in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + ) se_pixel (255 / 16, 255 / 16, 255 / 16)))
;;

(* This should look familiar by now! *)
let transform image =
  Image.mapi image ~f:(fun ~x ~y (r, g, b) ->
    if r >  255 / 2 then dist_error (image x y); (0, 0, 0) else 1, 1, 1)
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
