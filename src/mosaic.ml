open Core

(* Gives one float MSE value for the comparison of the two regions *)

let mse_calc ~reg_a ~reg_b ~w ~h =
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
  (factor *. rm) +. (factor *. gm) +. (factor *. bm /. 3.)
;;

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. -> need to
   check if provided width and height are greater than the image and do error
   checking --> implement a for loop to go through the image *)
let transform image ~width ~height ~moves =
  let max_start_x = Image.width image - width in
  let max_start_y = Image.height image - height in
  for _i = 1 to moves do
    let rand_x = Random.int max_start_x in
    let rand_y = Random.int max_start_y in
    let region_1 =
      Image.slice
        image
        ~x_start:rand_x
        ~y_start:rand_y
        ~x_end:(rand_x + width)
        ~y_end:(rand_y + height)
    in
    let to_swap, swap_x, swap_y, _ =
      Image.foldi
        image
        ~init:(region_1, 0, 0, Float.infinity)
        ~f:(fun ~x ~y (best_img, sx, sy, best_mse) (_r, _g, _b) ->
        if (x <> 0 && x % width = 0)
           && (y <> 0 && y % height = 0)
           && x + width < Image.width image
           && y + height < Image.height image
        then (
          let region_b =
            Image.slice
              image
              ~x_start:x
              ~y_start:y
              ~x_end:(x + width)
              ~y_end:(y + height)
          in
          let mse_curr =
            mse_calc ~reg_a:region_1 ~reg_b:region_b ~w:width ~h:height
          in
          if Float.( < ) mse_curr best_mse
          then region_b, x, y, mse_curr
          else best_img, sx, sy, best_mse)
        else best_img, sx, sy, best_mse)
    in
    let _new_img =
      Image.mapi to_swap ~f:(fun ~x ~y (r, g, b) ->
        Image.set
          image
          ~x:(swap_x + x)
          ~y:(swap_y + y)
          (Image.get image ~x:(rand_x + x) ~y:(rand_y + y));
        Image.set image ~x:(rand_x + x) ~y:(rand_y + y) (r, g, b);
        r, g, b)
    in
    ()
  done;
  Image.copy image
;;

let command =
  Command.basic
    ~summary:"Convert an image to a mosaic"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and moves =
        flag
          "moves"
          (required Command.Param.int)
          ~doc:"number of moves for the transform"
      and width =
        flag
          "width"
          (required Command.Param.int)
          ~doc:"width of image regions"
      and height =
        flag
          "height"
          (required Command.Param.int)
          ~doc:"height of image regions"
      in
      fun () ->
        let image =
          Image.load_ppm ~filename |> transform ~width ~height ~moves
        in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
