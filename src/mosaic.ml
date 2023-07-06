open Core

(* Gives one float MSE value for the comparison of the two regions *)

let mse ~reg_a ~reg_b ~w ~h =
  let w = float_of_int w in
  let h = float_of_int h in
  let rm, gm, bm =
    Image.foldi reg_a ~init:(0, 0, 0) ~f:(fun ~x ~y acc (r, g, b) ->
      let rd, gd, bd = Pixel.( - ) (r, g, b) (Image.get reg_b ~x ~y) in
      let squared = rd * rd, gd * gd, bd * bd in
      Pixel.( + ) acc squared)
  in
  let factor = 1. /. (w *. h) in
  let rm, gm, bm = float_of_int rm, float_of_int gm, float_of_int bm in
  (factor *. rm) +. (factor *. gm) +. (factor *. bm)
;;

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. -> need to check if provided width 
   and height are greater than the image and do error checkig *)
let transform image width height moves = 
  let max_start_x = Image.width image - width in
  let max_start_y = Image.height image - height in 
  let rand_x = Random.int max_start_x in 
  let rand_y = Random.int max_start_y in 
  let region_1 = Image.slice image 


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
