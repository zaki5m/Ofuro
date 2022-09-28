

let _ = 
  let file = open_in "correct_data_total.dat" in 
  let rec loop tmp = 
    let a, b, c, d, e = Scanf.sscanf (input_line file) "%d %f %f %f %f" (fun a b c d e -> (a,b,c,d,e)) in 
    let tmp = (a,b,c,d,e)::tmp in
    try loop tmp 
    with End_of_file -> close_in file; tmp
  in
  let lst = loop [] in
  let rec loop2 tmp1 tmp2 = match tmp1 with
    | [] -> tmp2
    | h::t -> let (a,b,c,d,e) = h in 
              let tmp2 = 
                if  a <= 15 then 
                  tmp2
                else
                  (a,b,c,d,e)::tmp2
              in
              loop2 t tmp2
  in
  let lst2 = loop2 lst [] in 
  let m = List.length lst2 in 
  Printf.printf "%d\n" m;
  let lst2 = List.sort (fun (a,b,c,d,e) (a',b',c',d',e') -> if a < a' then 1 else -1) lst2 in 
  let rec loop3 i = 
    let (a,b,c,d,e) = List.nth lst2 i in 
    Printf.printf "%d %f %f\n" a d e; flush stdout;
    if i = 0 then 
      ()
    else
      loop3 (i-1)
  in
  loop3 (m-1)