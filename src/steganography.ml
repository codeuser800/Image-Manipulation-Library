open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let change_r = r % 4 in
    let change_g = g % 4 in
    let change_b = b % 4 in
    change_r lsl 6, change_g lsl 6, change_b lsl 6)
;;

let command =
  Command.basic
    ~summary:"Convert an image to steganography"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_stego.ppm")]
;;
