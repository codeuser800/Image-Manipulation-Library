open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius =
  Image.mapi image ~f:(fun ~x ~y (_r, _g, _b) ->
    let slice =
      Image.slice
        image
        ~x_start:(if x - radius < 0 then 0 else x - radius)
        ~x_end:
          (if x + radius > Image.width image
           then Image.width image
           else x + radius)
        ~y_start:(if y - radius < 0 then 0 else y - radius)
        ~y_end:
          (if y + radius > Image.height image
           then Image.height image
           else y + radius)
    in
    Image.mean_pixel slice)
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
