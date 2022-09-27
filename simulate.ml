open M_gragh
open Loop
open Mahjong_base
open Mahjong_haieff
open Tenpai_prob
open Mahjong_admin

let zyuniten a b c d (uma1,uma2) = 
  let lst = [a;b;c;d] in
  let lst = List.sort (fun x y -> if x > y then -1 else 1) lst in
  let rec loop_samescore i tmp = 
    let z = List.nth lst i in
    let z' = 
      if i = 3 then 
        0
      else
        List.nth lst (i+1) in
    let e = if z = z' && i <> 3 then i + 1 else i in
    let (a',b',c',d') = tmp in
    let tmp = 
      if z = z' then
        if i = 0 then
          if z = a then
            if z' = b then
              (a+(uma1+uma2)/2,b+(uma1+uma2)/2,c',d')
            else if z' = c then
              (a+(uma1+uma2)/2,b',c+(uma1+uma2)/2,d')
            else
              (a+(uma1+uma2)/2,b',c',d+(uma1+uma2)/2)
          else if z = b then
            if z' = c then
              (a',b+(uma1+uma2)/2,c+(uma1+uma2)/2,d')
            else
              (a',b+(uma1+uma2)/2,c',d+(uma1+uma2)/2)
          else
            (a',b',c+(uma1+uma2)/2,d+(uma1+uma2)/2)
        else if i = 1 then
          if z = a then
            if z' = b then
              (a,b,c',d')
            else if z' = c then
              (a,b',c,d')
            else
              (a,b',c',d)
          else if z = b then
            if z' = c then
              (a',b,c,d')
            else
              (a',b,c',d)
          else
            (a',b',c,d)
        else
          if z = a then
            if z' = b then
              (a-(uma1+uma2)/2,b-(uma1+uma2)/2,c',d')
            else if z' = c then
              (a-(uma1+uma2)/2,b',c-(uma1+uma2)/2,d')
            else
              (a-(uma1+uma2)/2,b',c',d-(uma1+uma2)/2)
          else if z = b then
            if z' = c then
              (a',b-(uma1+uma2)/2,c-(uma1+uma2)/2,d')
            else
              (a',b-(uma1+uma2)/2,c',d-(uma1+uma2)/2)
          else
            (a',b',c-(uma1+uma2)/2,d-(uma1+uma2)/2)
      else
        if i = 0 then
          if z = a then
            (a+uma2,b',c',d')
          else if z = b then
            (a',b+uma2,c',d')
          else if z = c then
            (a',b',c+uma2,d')
          else
            (a',b',c',d+uma2)
        else if i = 1 then
          if z = a then
            (a+uma1,b',c',d')
          else if z = b then
            (a',b+uma1,c',d')
          else if z = c then
            (a',b',c+uma1,d')
          else
            (a',b',c',d+uma1)
        else if i = 2 then
          if z = a then
            (a-uma1,b',c',d')
          else if z = b then
            (a',b-uma1,c',d')
          else if z = c then
            (a',b',c-uma1,d')
          else
            (a',b',c',d-uma1)
        else
          if z = a then
            (a-uma2,b',c',d')
          else if z = b then
            (a',b-uma2,c',d')
          else if z = c then
            (a',b',c-uma2,d')
          else
            (a',b',c',d-uma2)
    in
    if e = 3 then
      tmp
    else
      loop_samescore (e+1) tmp
  in

  let rec loop_score i tmp = 
    let z = List.nth lst i in
    let (a',b',c',d') = tmp in
    let tmp = 
      if i = 0 then
        if z = a then
          (a+uma2,b',c',d')
        else if z = b then
          (a',b+uma2,c',d')
        else if z = c then
          (a',b',c+uma2,d')
        else
          (a',b',c',d+uma2)
      else if i = 1 then
        if z = a then
          (a+uma1,b',c',d')
        else if z = b then
          (a',b+uma1,c',d')
        else if z = c then
          (a',b',c+uma1,d')
        else
          (a',b',c',d+uma1)
      else if i = 2 then
        if z = a then
          (a-uma1,b',c',d')
        else if z = b then
          (a',b-uma1,c',d')
        else if z = c then
          (a',b',c-uma1,d')
        else
          (a',b',c',d-uma1)
      else
        if z = a then
          (a-uma2,b',c',d')
        else if z = b then
          (a',b-uma2,c',d')
        else if z = c then
          (a',b',c-uma2,d')
        else
          (a',b',c',d-uma2)
    in
    if i = 3 then
      tmp
    else
      loop_score (i+1) tmp
    
  in

  if a = b && b = c && c = d then
    (a,b,c,d)
  else if a = b && b = c || b = c && c = d || a = b && b = d || a = c && c = d then
    if List.hd lst = List.nth lst 1 then
      if a = List.nth lst 3 then
        (a-uma2,b+(uma2/3),c+(uma2/3),d+(uma2/3))
      else if b = List.nth lst 3 then
        (a+(uma2/3),b-uma2,c+(uma2/3),d+(uma2/3))
      else if c = List.nth lst 3 then
        (a+(uma2/3),b+(uma2/3),c-uma2,d+(uma2/3))
      else
        (a+(uma2/3),b+(uma2/3),c+(uma2/3),d-uma2)
    else
      if a = List.nth lst 0 then
        (a+uma2,b-(uma2/3),c-(uma2/3),d-(uma2/3))
      else if b = List.nth lst 3 then
        (a-(uma2/3),b+uma2,c-(uma2/3),d-(uma2/3))
      else if c = List.nth lst 3 then
        (a-(uma2/3),b-(uma2/3),c+uma2,d-(uma2/3))
      else
        (a-(uma2/3),b-(uma2/3),c-(uma2/3),d+uma2)
  else if a = b || b = c || c = d || a = c || b = d || a = d then
    loop_samescore 0 (0,0,0,0)
  else
    loop_score 0 (0,0,0,0)

let zyuni a b c d = 
  let lst = List.sort (fun x y -> if x > y then -1 else 1) [a;b;c;d] in
  let rec loop i a' b' c' d' = 
    let x = List.nth lst i in 
    let a' = if x = a && a' = 0 then (i+1) else a' in
    let b' = if x = b && b' = 0 then (i+1) else b' in 
    let c' = if x = c && c' = 0 then (i+1) else c' in 
    let d' = if x = d && d' = 0 then (i+1) else d' in 
    if i = 3 then 
      [a';b';c';d']
    else
      loop (i+1) a' b' c' d'
  in
  let n_lst = loop 0 0 0 0 0 in 
  let n_lst = List.map (fun a -> float_of_int a) n_lst in 
  let n = List.fold_left (fun a b -> a +. b) 0.0 n_lst in
  if n = 10.0 then 
    ((List.nth n_lst 0),(List.nth n_lst 1),(List.nth n_lst 2),(List.nth n_lst 3))
  else if n = 9.0 then 
    if List.exists (fun a -> if a = 4.0 then true else false) n_lst then 
      let n_lst = List.map (fun a -> if a = 3.0 then 3.5 else a) n_lst in 
      ((List.nth n_lst 0),(List.nth n_lst 1),(List.nth n_lst 2),(List.nth n_lst 3))
    else if List.exists (fun a -> if a = 3.0 then true else false) n_lst then 
      let n_lst = List.map (fun a -> if a = 2.0 then 2.5 else a) n_lst in 
      ((List.nth n_lst 0),(List.nth n_lst 1),(List.nth n_lst 2),(List.nth n_lst 3)) 
    else
      let n_lst = List.map (fun a -> if a = 1.0 then 1.5 else a) n_lst in 
      ((List.nth n_lst 0),(List.nth n_lst 1),(List.nth n_lst 2),(List.nth n_lst 3))
  else if n = 8.0 then 
    let n_lst = List.map (fun a -> if a = 1.0 then 1.5 else 3.5) n_lst in 
    ((List.nth n_lst 0),(List.nth n_lst 1),(List.nth n_lst 2),(List.nth n_lst 3))
  else if n = 7.0 then 
    let n_lst = List.map (fun a -> if a = 1.0 then 2.0 else a) n_lst in 
    ((List.nth n_lst 0),(List.nth n_lst 1),(List.nth n_lst 2),(List.nth n_lst 3))
  else if n = 4.0 then 
    (2.5,2.5,2.5,2.5)
  else
    let n_lst = List.map (fun a -> if a = 4.0 then 3.0 else a) n_lst in 
    ((List.nth n_lst 0),(List.nth n_lst 1),(List.nth n_lst 2),(List.nth n_lst 3))





(*not automatic*)
(*
let simulate count (uma1,uma2) =
  let rec loop i (tmp1,tmp2,tmp3,tmp4) total  = 
    let (total_kyoku,a,b,c,d) = hantyan () in
    let a' = a - 25000 in
    let b' = b - 25000 in
    let c' = c - 25000 in
    let d' = d - 25000 in
    let (a,b,c,d) = zyuniten a' b' c' d' (uma1,uma2) in
    let tmp1 = a + tmp1 in
    let tmp2 = b + tmp2 in
    let tmp3 = c + tmp3 in
    let tmp4 = d + tmp4 in
    (*Printf.printf "%d\n" (total+total_kyoku);*)
    Printf.printf "%d A:%d B:%d c:%d d:%d\n"i a b c d; flush stdout;
    if i = 0 then
      ((*Printf.printf "%d\n" (total+total_kyoku);*)
      Printf.printf "result: %dtimes A:%d B:%d c:%d d:%d\n" count tmp1 tmp2 tmp3 tmp4;)
    else
      loop (i-1) (tmp1,tmp2,tmp3,tmp4) (total+ total_kyoku) 
  in
  loop (count-1) (0,0,0,0) 0
*)

let zyuni_count lst (a,b,c,d) =
  let (a1,a2,a3,a4) = List.nth lst 0 in
  let (b1,b2,b3,b4) = List.nth lst 1 in
  let (c1,c2,c3,c4) = List.nth lst 2 in
  let (d1,d2,d3,d4) = List.nth lst 3 in
  let (a1,a2,a3,a4) = 
    if a = 1 then 
      (a1+1,a2,a3,a4)
    else if a = 2 then 
      (a1,a2+1,a3,a4)
    else if a = 3 then 
      (a1,a2,a3+1,a4)
    else
      (a1,a2,a3,a4+1)
  in
  let (b1,b2,b3,b4) = 
    if b = 1 then 
      (b1+1,b2,b3,b4)
    else if b = 2 then 
      (b1,b2+1,b3,b4)
    else if b = 3 then 
      (b1,b2,b3+1,b4)
    else
      (b1,b2,b3,b4+1)
  in
  let (c1,c2,c3,c4) = 
    if c = 1 then 
      (c1+1,c2,c3,c4)
    else if d = 2 then 
      (c1,c2+1,c3,c4)
    else if d = 3 then 
      (c1,c2,c3+1,c4)
    else
      (c1,c2,c3,c4+1)
  in
  let (d1,d2,d3,d4) = 
    if d = 1 then 
      (d1+1,d2,d3,d4)
    else if d = 2 then 
      (d1,d2+1,d3,d4)
    else if d = 3 then 
      (d1,d2,d3+1,d4)
    else
      (d1,d2,d3,d4+1)
  in
  [(a1,a2,a3,a4);(b1,b2,b3,b4);(c1,c2,c3,c4);(d1,d2,d3,d4)]




(*automatic*)
let simulate count (uma1,uma2) furoritu_lst =
  let rec loop i (tmp1,tmp2,tmp3,tmp4) total zyuni_lst = 
    let (total_kyoku,a,b,c,d) = hantyan furoritu_lst in
    let a' = a - 25000 in
    let b' = b - 25000 in
    let c' = c - 25000 in
    let d' = d - 25000 in
    let (a,b,c,d) = zyuniten a' b' c' d' (uma1,uma2) in
    let tmp1 = a + tmp1 in
    let tmp2 = b + tmp2 in
    let tmp3 = c + tmp3 in
    let tmp4 = d + tmp4 in
    (*Printf.printf "%d\n" (total+total_kyoku);*)
    let (a',b',c',d') = zyuni a b c d in 
    let zyuni_lst = zyuni_count zyuni_lst (int_of_float a',int_of_float b',int_of_float c',int_of_float d') in 
    flush stdout;
    (*Printf.printf "%d A:%d B:%d c:%d d:%d %f %f %f %f\n"i a b c d a' b' c' d'; flush stdout;*)
    if i = 0 then
      ((*Printf.printf "%d\n" (total+total_kyoku);*)
      (*Printf.printf "result: %dtimes A%f:%d B%f:%d c%f:%d d%f:%d\n" count (List.nth furoritu_lst 0) tmp1 (List.nth furoritu_lst 1) tmp2 (List.nth furoritu_lst 2) tmp3 (List.nth furoritu_lst 3) tmp4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 0 in  
      Printf.printf "A: %d %d %d %d \n" s1 s2 s3 s4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 1 in  
      Printf.printf "B: %d %d %d %d \n" s1 s2 s3 s4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 2 in  
      Printf.printf "C: %d %d %d %d \n" s1 s2 s3 s4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 3 in  
      Printf.printf "D: %d %d %d %d \n" s1 s2 s3 s4;*))
    else
      loop (i-1) (tmp1,tmp2,tmp3,tmp4) (total+ total_kyoku) zyuni_lst
  in
  loop (count-1) (0,0,0,0) 0 [(0,0,0,0);(0,0,0,0);(0,0,0,0);(0,0,0,0)]


let simulate_kyoku count furoritu_lst =
  let player_score = (25000,25000,25000,25000) in
  let rec loop i (tmp1,tmp2,tmp3,tmp4) zyuni_lst = 
    let (_,(a,b,c,d),player_score) = kyoku_s 0 1 0 0 player_score furoritu_lst 0 in
    flush stdout;
    let a = a - 25000 in
    let b = b - 25000 in
    let c = c - 25000 in
    let d = d - 25000 in
    let tmp1 = a + tmp1 in
    let tmp2 = b + tmp2 in
    let tmp3 = c + tmp3 in
    let tmp4 = d + tmp4 in
    (*Printf.printf "%d\n" (total+total_kyoku);*)
    let (a',b',c',d') = zyuni a b c d in 
    let zyuni_lst = zyuni_count zyuni_lst (int_of_float a',int_of_float b',int_of_float c',int_of_float d') in 
    flush stdout;
    (*Printf.printf "%d A:%d B:%d c:%d d:%d %f %f %f %f\n"i a b c d a' b' c' d'; flush stdout;*)
    if i = 0 then
      ((*Printf.printf "%d\n" (total+total_kyoku);*)
      (*Printf.printf "result: %dtimes A%f:%d B%f:%d c%f:%d d%f:%d\n" count (List.nth furoritu_lst 0) tmp1 (List.nth furoritu_lst 1) tmp2 (List.nth furoritu_lst 2) tmp3 (List.nth furoritu_lst 3) tmp4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 0 in  
      Printf.printf "A: %d %d %d %d \n" s1 s2 s3 s4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 1 in  
      Printf.printf "B: %d %d %d %d \n" s1 s2 s3 s4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 2 in  
      Printf.printf "C: %d %d %d %d \n" s1 s2 s3 s4;
      let (s1,s2,s3,s4) = List.nth zyuni_lst 3 in  
      Printf.printf "D: %d %d %d %d \n" s1 s2 s3 s4;*)
      Printf.printf "finish\n";)
    else
      loop (i-1) (tmp1,tmp2,tmp3,tmp4) zyuni_lst
  in
  loop (count-1) (0,0,0,0) [(0,0,0,0);(0,0,0,0);(0,0,0,0);(0,0,0,0)]

(*let _ = simulate 250 (10000,30000) [10.0;35.0;35.0;10.0] *)
let _ = simulate_kyoku 5 [25.;25.;25.;25.]

