

let _ = 
  let rec loop tmp = 
    let a, b, c, d, e, f, g, h, i = Scanf.sscanf (read_line()) "%d %s %s %s %s %f %f %f %f" (fun a b c d e f g h i -> (a,b,c,d,e,f,g,h,i)) in 
    let tmp = (f,g,h,i)::tmp in
    try loop tmp 
    with End_of_file -> tmp
  in
  let lst = loop [] in
  let rec loop2 tmp1 tmp2 = match tmp1 with
    | [] -> tmp2
    | h::t -> let (f,g,h',i) = h in 
    let tmp2 = [f::(List.nth tmp2 0);g::(List.nth tmp2 1);h'::(List.nth tmp2 2);i::(List.nth tmp2 3)] in 
              loop2 t tmp2
  in
  (*
  let lst2 = loop2 lst [] in 
  let m = List.length lst2 in 
  Printf.printf "%d\n" m;
  let lst2 = List.sort (fun (a,b,c,d,e) (a',b',c',d',e') -> if a < a' then 1 else -1) lst2 in 
  let rec loop3 i = 
    let (a,b,c,d,e) = List.nth lst2 i in 
    Printf.printf "%d %f %f %f %f\n" a b c d e; flush stdout;
    if i = 0 then 
      ()
    else
      loop3 (i-1)
  in
  loop3 (m-1)
*)
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
