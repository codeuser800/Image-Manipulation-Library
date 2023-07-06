open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let avg = (r + g + b) / 3 in
    avg, avg, avg)
;;

let mse ~reg_a ~reg_b ~w ~h =
  let w = float_of_int w in
  let h = float_of_int h in
  let pre_mse = 
  Image.foldi reg_a ~init:0.0 ~f:(fun ~x ~y acc (r, g, b) ->
    let (rd, gd, bd) =
      Pixel.( - ) Image.get reg_a ~x ~y Image.get reg_b ~x ~y
    in
    let squared = (rd *. rd, gd *. gd, bd *. bd) in 
    Pixel.( +. ) acc squared) in 
  let
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
