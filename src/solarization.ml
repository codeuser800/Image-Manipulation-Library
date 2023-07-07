open Core

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image =
  let threshold = 0.3 *. float_of_int (Image.max_val image) in
  let max_val = float_of_int (Image.max_val image) in
  Image.map image ~f:(fun (r, g, b) ->
    let r = float_of_int r in
    let g = float_of_int g in
    let b = float_of_int b in
    if Float.( > ) r threshold
       && Float.( > ) g threshold
       && Float.( > ) b threshold
    then
      ( int_of_float (max_val -. r)
      , int_of_float (max_val -. g)
      , int_of_float (max_val -. b) )
    else int_of_float r, int_of_float g, int_of_float b)
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
