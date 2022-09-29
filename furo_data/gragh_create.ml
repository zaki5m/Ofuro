let make_gragh lst = 
  let m = List.length lst in 
  let rec loop i a2 b2 tmp =
    let (a,b,c) = List.nth lst i in
    let tmp = 
      if ((a2 *. b *. b *. b) > c && b < 0.) || (b2 *. b *.b > c) && b > 0. then 
        tmp + 1
      else
        tmp
    in
    let _ = 
      if i = 0 then 
        if tmp >= 142 then 
          (Printf.printf "%f %f %d\n" a2 b2 tmp; flush stdout;)
        else if tmp > 0 then 
          (*Printf.printf "%f,%f,%d\n" a2 b2 tmp; flush stdout;*)
          ()
        else
          ()
      else
        ()
    in
    if i = 0 then 
      if a2 >= 10000000.0 then
        if b2 >= 1000.0 then
          ()
        else
          loop (m-1) a2 (b2 +. 1.0) 0 
      else
        if b2 >= 1000.0 then
          loop (m-1) (a2 +. 10000.) (0.0) 0
        else
          loop (m-1) a2 (b2 +. 1.0) 0 
    else
      loop (i-1) a2 b2 tmp
  in
  loop (m-1) 20000. (0.0) 0
           



    


let _ = 
  let file = open_in "correct_data_total_10to12.dat" in 
  let rec loop tmp = 
    let a, b, c = Scanf.sscanf (input_line file) "%d %f %f " (fun a b c -> (a,b,c)) in 
    let tmp = (a,b,c)::tmp in
    try loop tmp 
    with End_of_file -> close_in file; tmp
  in
  let lst = loop [] in
  make_gragh lst