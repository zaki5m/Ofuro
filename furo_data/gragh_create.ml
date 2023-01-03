let make_gragh lst = 
  let m = List.length lst in 
  let rec loop i a2 b2 c2 tmp =
    let (a,b,c) = List.nth lst i in
    let tmp = 
      if (*((a2 *. b *. b *. b) > c && b < 0.) ||*) (((-1.) *. b2 *. b *.b +. c2 < c) && b > 0.) || c > 0. then 
        tmp + 1
      else
        tmp
    in
    let _ = 
      if i = 0 then 
        if tmp >= 700 && tmp <= 800 then 
          (Printf.printf "%f %f %f %d\n" a2 b2 c2 tmp; flush stdout;)
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
        if b2 >= 10000000.0 then
          if c2 >= 0. then
            ()
          else
            loop (m-1) a2 b2 (c2 +. 10.) 0 
        else
          if c2 >= 0. then
            loop (m-1) a2 (b2 +. 1000.) 0. 0 
          else
          loop (m-1) a2 b2 (c2 +. 10.) 0 
      else
        if b2 >= 10.0 then
          if c2 >= 10000. then
            loop (m-1) (a2 +. 100000.) (0.0) 0. 0
          else
          loop (m-1) a2 b2 (c2 +. 100.) 0 
        else
          if c2 >= 10000. then
            loop (m-1) a2 (b2 +. 1.) 0. 0 
          else
          loop (m-1) a2 b2 (c2 +. 100.) 0 
    else
      loop (i-1) a2 b2 c2 tmp
  in
  loop (m-1) 10000000. (1000000.0) (0.) 0
           



    


let _ = 
  let file = open_in "fix_data10to12.dat" in 
  let rec loop tmp = 
    let a, b, c = Scanf.sscanf (input_line file) "%d %f %f " (fun a b c -> (a,b,c)) in 
    let tmp = (a,b,c)::tmp in
    try loop tmp 
    with End_of_file -> close_in file; tmp
  in
  let lst = loop [] in
  make_gragh lst