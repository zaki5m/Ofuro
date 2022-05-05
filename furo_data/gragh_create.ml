let make_gragh lst = 
  let m = List.length lst in 
  let rec loop i a2 b2 tmp =
    let (a,b,c,d,e) = List.nth lst i in
    let tmp = 
      if (a2 *. b *. b +. b2) > c then 
        tmp + 1
      else
        tmp
    in
    let _ = 
      if i = 0 then 
        if tmp = 49 then 
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
      if a2 = 10000.0 then
        if b2 = -2000.0 then
          ()
        else
          loop (m-1) a2 (b2 +. 100.0) 0 
      else
        if b2 = -2000.0 then
          loop (m-1) (a2 +. 100.0) (-10000.0) 0
        else
          loop (m-1) a2 (b2 +. 100.0) 0 
    else
      loop (i-1) a2 b2 tmp
  in
  loop (m-1) 100.0 (-10000.0) 0
           



    


let _ = 
  let rec loop tmp = 
    let a, b, c, d, e = Scanf.sscanf (read_line()) "%d %f %f %f %f" (fun a b c d e -> (a,b,c,d,e)) in 
    let tmp = (a,b,c,d,e)::tmp in
    try loop tmp 
    with End_of_file -> tmp
  in
  let lst = loop [] in
  make_gragh lst