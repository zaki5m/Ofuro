
(*
let _ = 
  let file = open_in "totaldata.txt" in 
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
                if a <= 9 || a > 12 then 
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
*)
(*
 let lst2 = loop2 lst [[];[];[];[]] in
 let lsta = List.nth lst2 0 in 
 let lstb = List.nth lst2 1 in 
 let lstc = List.nth lst2 2 in 
 let lstd = List.nth lst2 3 in
 let a = List.length (List.filter (fun x -> x = 1.0) lsta) in 
 let b = List.length (List.filter (fun x -> x = 2.0) lsta) in 
 let c = List.length (List.filter (fun x -> x = 3.0) lsta) in 
 let d = List.length (List.filter (fun x -> x = 4.0) lsta) in 
 Printf.printf "a:%d %d %d %d\n"a b c d; flush stdout;
 let a = List.length (List.filter (fun x -> x = 1.0) lstb) in 
 let b = List.length (List.filter (fun x -> x = 2.0) lstb) in 
 let c = List.length (List.filter (fun x -> x = 3.0) lstb) in 
 let d = List.length (List.filter (fun x -> x = 4.0) lstb) in 
 Printf.printf "b:%d %d %d %d\n"a b c d; flush stdout;
 let a = List.length (List.filter (fun x -> x = 1.0) lstc) in 
 let b = List.length (List.filter (fun x -> x = 2.0) lstc) in 
 let c = List.length (List.filter (fun x -> x = 3.0) lstc) in 
 let d = List.length (List.filter (fun x -> x = 4.0) lstc) in 
 Printf.printf "c:%d %d %d %d\n"a b c d; flush stdout;
 let a = List.length (List.filter (fun x -> x = 1.0) lstd) in 
 let b = List.length (List.filter (fun x -> x = 2.0) lstd) in 
 let c = List.length (List.filter (fun x -> x = 3.0) lstd) in 
 let d = List.length (List.filter (fun x -> x = 4.0) lstd) in 
 Printf.printf "d:%d %d %d %d\n"a b c d; flush stdout;
*)
(*
let _ = 
  let file = open_in "same_count.txt" in 
  let rec loop tmp tmp2 tmp3 now = 
    let ip_s = input_line file in 
    if ip_s = "finish" then 
      let tmp2 = tmp2 + 1 in 
      try loop tmp tmp2 tmp3 now 
      with End_of_file -> close_in file; (tmp,tmp2,tmp3)
    else
      let tmp = tmp+1 in
      let tmp3 = if now = tmp2 then tmp3 else tmp3 + 1 in 
      let now = tmp2 in 
      try loop tmp tmp2 tmp3 now 
      with End_of_file -> close_in file; (tmp,tmp2,tmp3)
  in
  let (lst1,lst2,lst3) = loop 0 0 0 (-1) in
  Printf.printf "%d %d %d\n" lst1 lst2 lst3;
*)

let _ = 
  let file = open_in "total.txt" in 
  let rec loop tmp = 
    let _, _, a, b, c, d, e, f, g, h = Scanf.sscanf (input_line file) "%s %s %s %s %s %s %s" (fun a b c d e f g h i j -> (a,b,c,d,e,f,g,h,i,j)) in 
    let tmp = (a,b,c,d,e,f,g,h)::tmp in
    try loop tmp 
    with End_of_file -> close_in file; tmp
  in
  let lst = loop [] in
  let rec loop2 tmp1 = match tmp1 with
    | [] -> ()
    | h::t -> let (a,b,c,d,e,f,g,h) = h in 
              let a = String.sub a 1 (String.len a - 1) in 
              let b1 = String.sub b 5 12 in
              let b2 = String.sub b 21 28 in 
              let b3 = String.sub b 31 (String.len b - 1) in
              let c = String.sub c 1 (String.len c - 1) in 
              let d1 = String.sub d 5 12 in
              let d2 = String.sub d 21 28 in 
              let d3 = String.sub d 31 (String.len d - 1) in
              let e = String.sub e 1 (String.len e - 1) in 
              let f1 = String.sub f 5 12 in
              let f2 = String.sub f 21 28 in 
              let f3 = String.sub f 31 (String.len f - 1) in
              let g = String.sub g 1 (String.len g - 1) in 
              let h1 = String.sub h 5 12 in
              let h2 = String.sub h 21 28 in 
              let h3 = String.sub h 31 (String.len h - 1) in
              let total = a ^ "," ^ b1 ^ "," ^ b2 ^ "," ^ b3 ^ "," ^ a ^ "," ^ d1 ^ "," ^ d2 ^ "," ^ d3 ^ "," ^ a ^ "," ^ f1 ^ "," ^ f2 ^ "," ^ f3 ^ "," ^ a ^ "," ^ h1 ^ "," ^ h2 ^ "," ^ h3 ^ "\n" in
              Printf.printf total;
              loop2 t
  in
  loop2 lst
  