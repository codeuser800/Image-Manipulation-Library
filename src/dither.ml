open Core

(* This should look familiar by now! *)
let transform image =
  (* Image.mapi image ~f:(fun ~x ~y (r, g, b) -> ) *)
  ignore image
;;

let add_error image x y =
  if x + 1 < Image.width image
  then (
    let right_pixel = Image.get image ~x:(x + 1) ~y:(y + 1) in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + ) right_pixel (7 * 255 / 16, 7 * 255 / 16, 7 * 255 / 16)));
  if x - 1 > 0 && y + 1 < Image.height image
  then (
    let left_pixel = Image.get image ~x:(x - 1) ~y:(y - 1) in
    Image.set
      image
      ~x:(x - 1)
      ~y:(y + 1)
      (Pixel.( + ) left_pixel (3 * 255 / 16, 3 * 255 / 16, 3 * 255 / 16)));
  if y + 1 < Image.height image
  then (
    let left_pixel = Image.get image ~x ~y:(y - 1) in
    Image.set
      image
      ~x
      ~y:(y + 1)
      (Pixel.( + ) left_pixel (3 * 255 / 16, 3 * 255 / 16, 3 * 255 / 16)));
  if y + 1 < Image.height image && x + 1 < Image.width image
  then (
    let se_pixel = Image.get image ~x:(x + 1) ~y:(y + 1) in
    Image.set
      image
      ~x:(x + 1)
      ~y:(y + 1)
      (Pixel.( + ) se_pixel (255 / 16, 255 / 16, 255 / 16)))
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
