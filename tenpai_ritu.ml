(*open Domainslib

module C = Domainslib.Chan

let num_domains = int_of_string Sys.argv.(1) 

type 'a message = Task of 'a | Quit

let c = C.make_unbounded ()

let create_work tasks =
  Array.iter (fun t -> C.send c (Task t)) tasks;
  for _ = 1 to num_domains do
    C.send c Quit
  done

let create_work_lst tasks =
  List.iter (fun t -> C.send c (Task t)) tasks

let rec worker f () =
  match C.recv c with
  | Task a ->
      f a;
      worker f ()
  | Quit -> ()
*)
let syanten_4 tumo_len yama_len = 
  Printf.printf "%d %d "tumo_len yama_len;
  let rec loop a b c d = 
    Printf.printf "%d %d %d %d\n" a b c d;
    if a = 1 then 
      if b = 1 then 
        if c = 1 then
          if d = 1 then 
            ()
          else
            (Printf.printf "%d %d "tumo_len yama_len;
            loop a b c (d-1))
        else
          if d = 1 then 
            (Printf.printf "%d %d "tumo_len yama_len;
             loop a b (c-1) 4)
          else
            (Printf.printf "%d %d "tumo_len yama_len;
            loop a b c (d-1))
      else
        if c = 1 then
          if d = 1 then 
            (Printf.printf "%d %d "tumo_len yama_len;
            loop a (b-1) 4 4)
          else
            (Printf.printf "%d %d "tumo_len yama_len;
            loop a b c (d-1))
        else
          if d = 1 then 
            (Printf.printf "%d %d "tumo_len yama_len;
             loop a b (c-1) 4)
          else
            (Printf.printf "%d %d "tumo_len yama_len;
            loop a b c (d-1))
      else
        if b = 1 then 
          if c = 1 then
            if d = 1 then 
              (Printf.printf "%d %d "tumo_len yama_len;
               loop (a-1) 4 4 4)
            else
              (Printf.printf "%d %d "tumo_len yama_len;
              loop a b c (d-1))
          else
            if d = 1 then 
              (Printf.printf "%d %d "tumo_len yama_len;
               loop a b (c-1) 4)
            else
              (Printf.printf "%d %d "tumo_len yama_len;
              loop a b c (d-1))
        else
          if c = 1 then
            if d = 1 then 
              (Printf.printf "%d %d "tumo_len yama_len;
              loop a (b-1) 4 4)
            else
              (Printf.printf "%d %d "tumo_len yama_len;
              loop a b c (d-1))
          else
            if d = 1 then 
              (Printf.printf "%d %d "tumo_len yama_len;
               loop a b (c-1) 4)
            else
              (Printf.printf "%d %d "tumo_len yama_len;
              loop a b c (d-1))
  in
  loop 4 4 4 4

let syanten_3 tumo_len yama_len = 
  Printf.printf "%d %d "tumo_len yama_len;
  let rec loop b c d= 
    Printf.printf "%d %d %d\n" b c d;
    if b = 1 then 
      if c = 1 then
        if d = 1 then 
          ()
        else
          (Printf.printf "%d %d "tumo_len yama_len;
          loop b c (d-1))
      else
        if d = 1 then 
          (Printf.printf "%d %d "tumo_len yama_len;
            loop b (c-1) 4)
        else
          (Printf.printf "%d %d "tumo_len yama_len;
          loop b c (d-1))
    else
      if c = 1 then
        if d = 1 then 
          (Printf.printf "%d %d "tumo_len yama_len;
          loop (b-1) 4 4)
        else
          (Printf.printf "%d %d "tumo_len yama_len;
          loop b c (d-1))
      else
        if d = 1 then 
          (Printf.printf "%d %d "tumo_len yama_len;
            loop b (c-1) 4)
        else
          (Printf.printf "%d %d "tumo_len yama_len;
          loop b c (d-1))
      
  in
  loop 4 4 4

let syanten_2 tumo_len yama_len = 
  Printf.printf "%d %d "tumo_len yama_len;
  let rec loop c d= 
    Printf.printf "%d %d\n" c d;
    if c = 1 then
      if d = 1 then 
        ()
      else
        (Printf.printf "%d %d "tumo_len yama_len;
        loop c (d-1))
    else
      if d = 1 then 
        (Printf.printf "%d %d "tumo_len yama_len;
          loop (c-1) 4)
      else
        (Printf.printf "%d %d "tumo_len yama_len;
        loop c (d-1))
      
  in
  loop 4 4


let syanten_1 tumo_len yama_len = 
  Printf.printf "%d %d "tumo_len yama_len;
  let rec loop d= 
    Printf.printf "%d\n" d;
    if d = 1 then 
      ()
    else
      (Printf.printf "%d %d "tumo_len yama_len;
      loop (d-1))     
  in
  loop 4 
            


let make_patern_4 yama_len = 
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 4 then 
        ()
      else
        (syanten_4 yama_len tumo_len;)
    in
    if yama_len = 4 then 
      ()
    else
      loop (yama_len - 1)
  in
  loop yama_len

let make_patern_3 yama_len = 
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 4 then 
        if tumo_len < 3 then 
            ()
        else
          (syanten_3 yama_len tumo_len;)
      else
        (syanten_3 yama_len tumo_len;)
    in
    if yama_len = 4 then 
      ()
    else
      loop (yama_len - 1)
  in
  loop yama_len

let make_patern_2 yama_len = 
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 4 then 
        if tumo_len < 3 then 
          if tumo_len < 2 then 
            ()
          else
            (syanten_2 yama_len tumo_len;)
        else
          (syanten_2 yama_len tumo_len;)
      else
        (syanten_2 yama_len tumo_len;)
    in
    if yama_len = 4 then 
      ()
    else
      loop (yama_len - 1)
  in
  loop yama_len

let make_patern_1 yama_len = 
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 4 then 
        if tumo_len < 3 then 
          if tumo_len < 2 then 
            (syanten_1 yama_len tumo_len;)
          else
            (
            syanten_1 yama_len tumo_len;)
        else
          (
          syanten_1 yama_len tumo_len;)
      else
        (syanten_1 yama_len tumo_len;)
    in
    if yama_len = 4 then 
      ()
    else
      loop (yama_len - 1)
  in
  loop yama_len
  


let combination n r = 
  let rec fib i tmp = match i with  
    | 0 -> tmp
    | _ -> fib (i-1) (i::tmp)
  in
  let rec loop i tmp = match i with 
    | 0 -> tmp
    | _ -> let rec loop2 t_lst tmp = match t_lst with 
              | [] -> []
              | h::t -> (h+tmp)::(loop2 t (h+tmp))
           in
           let tmp = loop2 tmp 0 in 
           loop (i-1) tmp 
  in
  if r = 0 then 
    1.0
  else if r = 1 then 
    float_of_int n
  else
    let lst = fib n [] in 
    let lst = loop (r-2) lst in 
    float_of_int (List.fold_left (fun a b -> a + b) 0 lst)

let pure_combination n r = 
  let rec loop i tmp = 
    if i = (n-r+1) then 
      tmp * i
    else
      loop (i-1) (tmp*i)
  in
  let rec loop2 i tmp =
    if i = 1 then 
      tmp
    else
      loop2 (i-1) (tmp * i) 
  in
  if r = 0 then 
    1
  else
    (loop n 1)/(loop2 r 1)

let factorial n = 
  let rec loop i tmp =
    if i = 1 then 
      tmp
    else
      loop (i-1) (tmp * i) 
  in
  if n = 0 then 
    1
  else
    (loop n 1)
(*
(*iは最後に積もる時に残っているtargetの牌の枚数*)
let target1 yama_len tumo_len a i yukouhai = 
  let rec under_loop j tmp = 
    let tmp = tmp *. (float_of_int (yama_len-j)) in 
    if j = 0 then 
      tmp
    else
      under_loop (j-1) tmp
  in 
  let rec over j target tmp = 
    if j = 0 then 
      tmp
    else
      over (j-1) target (tmp*((yama_len+1)-(target*4)-j))
  in
  (*x:a,y:i*)
  let rec over2 x y tmp = 
    let tmp = y * tmp in 
    if x = y then 
      tmp
    else
      over2 x (y+1) tmp
  in
  let under = under_loop (a-1) 1. in
  let rec loop j tmp = 
    let tmp_c = float_of_int (pure_combination (j*3) (a-i)) in 
    let tmp_o = float_of_int (over (i-1) j 1) in 
    let tmp_o2 = float_of_int (over2 a i 1) in 
    let tmp = ((tmp_o *. tmp_o2 *. tmp_c) /. under) +. tmp in 
    if j = tumo_len then 
      tmp
    else
      loop (j+1) tmp
  in
  loop 1 0.0
*)

let yukouhai_tumo_lst_diff yukou_lst = 
  let rec loop2 tmp lst t_lst = match t_lst with 
    | [] -> tmp
    | (h,n)::t -> let tmp = 
                    if List.exists (fun (a,_) -> a = h) lst then 
                      n::tmp
                    else
                      tmp
                  in
                  loop2 tmp lst t
  in
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | h::[] -> tmp
    | h::x::t -> let tmp = (loop2 [] h x)::tmp in
                 loop tmp t
  in
  loop [] yukou_lst
let target1 yama_len tumo_len a i = 
  let rec under_loop j tmp = 
    let tmp = tmp *. (float_of_int (yama_len-j)) in 
    if j = 0 then 
      tmp
    else
      under_loop (j-1) tmp
  in 
  let rec over j target tmp = 
    if j = 0 then 
      tmp
    else
      over (j-1) target (tmp*((yama_len+1)-(target*4)-j))
  in
  (*x:a,y:i*)
  let rec over2 x y tmp = 
    let tmp = y * tmp in 
    if x = y then 
      tmp
    else
      over2 x (y+1) tmp
  in
  let under = under_loop (a-1) 1. in
  let loop j k tmp = 
    let tmp_c = float_of_int (pure_combination (j*3) (a-k)) in 
    let tmp_o = float_of_int (over (k-1) j 1) in 
    let tmp_o2 = float_of_int (over2 a k 1) in 
    let tmp = ((tmp_o *. tmp_o2 *. tmp_c) /. under) +. tmp in 
    tmp
  in
  if tumo_len * 3 < (a-i) then 
    loop tumo_len (tumo_len*3) 0.
  else
    loop tumo_len i 0.0

let ave_calc ave ai ai2 new_yukou_sum = 
  let rec loop i tmp = match i with 
    | 0 -> tmp
    | _ -> let x = Float.pow (float_of_int new_yukou_sum /. float_of_int ai) (float_of_int i) in
           let x = x *. (combination ave i) in
           let tmp = if x > 1.0 then tmp +. 1. else tmp +. x in 
           loop (i-1) tmp
  in
  ai2 - (int_of_float (loop ave 0.))

let tenpai_ritu_1 yama_len tumo_len tumo_lst yukou_lst = 
  let yama_len = yama_len + 14 in 
  let n = List.length tumo_lst in
  let new_yukou_lst = yukouhai_tumo_lst_diff yukou_lst in 
  let first_ai = List.fold_right (fun (_,a) b -> a+b) (List.nth yukou_lst 0) 0 in
  let rec loop2 len a ai tumo_hai th (ave,tmp) = match ai with 
    | 0 -> (ave,tmp) 
    | _ ->  if th > ai then 
              loop2 len a (ai-1) tumo_hai th (ave,tmp)
            else
              let tmp2 = target1 yama_len tumo_len a ai in 
              let tmp2 = (float_of_int ai *.  (float_of_int tumo_hai /. float_of_int a)) *. tmp2 in 
              let ave2 = if len * 3 < (a-ai) then len*3 else (a-ai) in 
              loop2 len a (ai-1) tumo_hai th ((ave+ave2),(tmp+.tmp2))
  in
  let rec loop len i next n_tmp tmp = match len with 
    | 0 -> tmp 
    | _ ->  if n - i > len then 
              loop (len-1) i next n_tmp 1.
            else
              let tumo_hai = List.nth tumo_lst i in 
              let ai2 = if (i+1) = List.length yukou_lst then 0 else List.fold_right (fun (_,a) b -> a+b) (List.nth yukou_lst (i+1)) 0 in
              let new_yukou_sum = if (i+1) >= List.length new_yukou_lst then 0 else List.fold_right (fun a b -> a+b) (List.nth new_yukou_lst i) 0 in  
              let (ave,x) = loop2 len next next tumo_hai (n-i) (0,0.) in
              let ave = ave / (next-(n-i-1)) in 
              let ave = ave_calc ave next ai2 new_yukou_sum in (*次の有効牌の枚数*)
              let tmp2 = if i = (n-1) then 
                            (tmp*.x)
                        else
                          loop (len-1) (i+1) ave 0. (tmp*.x) 
              in
              loop (len-1) i next (n_tmp +. tmp2) 1.
          in
  loop tumo_len 0 first_ai 0. 1.






let yukou_to_ritu yama_len tumo_len yukou_lst tumo_lst = 
  let yama_len = yama_len + 14 in 
  let tumo_len_len = List.length tumo_lst in
  let rec make_lst tmp i =
    if i <= 14 then 
      tmp
    else
      make_lst (i::tmp) (i-4)
  in
  let lst = make_lst [] (yama_len+1) in
  let lst_len =  List.length lst in 
  let rec make_x n tmp yama i = match i with 
    | 0 -> tmp
    | _ ->  let tmp = tmp +. (n /. float_of_int (yama-i)) in
            make_x n tmp yama (i-1)
  in
  let rec loop len max_i tmp tmp2 x i j = match len with 
    | 0 ->  tmp
    | _ ->  let n = List.nth yukou_lst j in 
            let yama = List.nth lst (lst_len-len) in 
            let yama2 = List.nth lst (lst_len-len-1) in 
            let x = x -. ((i /. max_i) *. (make_x (float_of_int n) 0. yama (int_of_float max_i))) in 
            let tmp = ((i-.1.)/.max_i)*.(x /. (float_of_int yama2)) *.tmp in
            let tmp2 = 
              if  (lst_len-len-1) < 0 then 
                tmp+.tmp2
              else if j = (List.length yukou_lst) - 1 then 
                tmp+.tmp2
              else
                let y = (List.nth yukou_lst (j+1)) in 
                let y = (float_of_int y) -. (((float_of_int n) -. x) *. (float_of_int y) /.(float_of_int n)) in
                tmp2 +. (loop (len-1) i tmp 0. y i (j+1))
            in
            if (i-.2.) <= 0. then 
              loop 0 max_i tmp tmp2 x (i-.1.) j
            else
              loop (len-1) max_i tmp tmp2 x (i-.1.) j
  in
  loop tumo_len (float_of_int (tumo_len - tumo_len_len + 1)) 1. 0. (float_of_int (List.nth yukou_lst 0)) (float_of_int (tumo_len - tumo_len_len + 1)) 0











   








let target2 yama_len tumo_len a b ai bi = 
  let rec under_loop j tmp = 
    let tmp = tmp *. (float_of_int (yama_len-j)) in 
    if j = 0 then 
      tmp
    else
      under_loop (j-1) tmp
  in 
  let rec over j target n_t tmp = 
    if j = 0 then 
      tmp
    else
      let tmp = tmp *. float_of_int (((yama_len+1)-(target*4)-(j+n_t))) in 
      over (j-1) target n_t tmp
  in
  (*x:a,y:i*)
  let rec over2 x y tmp = 
    let tmp = y * tmp in 
    if x = y then 
      tmp
    else
      over2 x (y+1) tmp
  in
  let under = under_loop (a+b-1) 1. in
  let rec loop j k l tmp = 
    let tmp2 = 
      if l = 0 then 
        let tmp_ac = float_of_int (pure_combination (k*3) (a-ai)) in
        let tmp_bc = float_of_int (pure_combination ((j-k)*3) (b-bi)) in
        let tmp_oa = over (ai-1) k bi 1. in
        let tmp_ob = over (bi-1) j 0 1. in 
        let tmp_o2a = float_of_int (over2 a ai 1) in 
        let tmp_o2b = float_of_int (over2 b bi 1) in 
        ((tmp_oa *. tmp_ob *. tmp_o2a *. tmp_ac *. tmp_o2b *. tmp_bc) /. under) 
      else
        let tmp_ac = float_of_int (pure_combination (k*3) ((a-ai)+l)) in
        let tmp_ac = tmp_ac *. (float_of_int ((a-ai)+l) /. float_of_int ((factorial l) * (factorial (a-ai)))) in
        let tmp_bc = float_of_int (pure_combination ((j-k)*3) ((b-bi)-l)) in
        let tmp_oa = over (ai-1) k bi 1. in
        let tmp_ob = over (bi-1) j 0 1. in 
        let tmp_o2a = float_of_int (over2 a ai 1) in 
        let tmp_o2b = float_of_int (over2 b bi 1) in 
        ((tmp_oa *. tmp_ob *. tmp_o2a *. tmp_ac *. tmp_o2b *. tmp_bc) /. under) 
    in 
    if k = 1 then 
      if l = 3 - (a-ai) || l = (b-bi) then
        if k = j - 1 then 
          if j = tumo_len then 
            tmp +.tmp2
          else
            loop (j+1) 1 0 (tmp+.tmp2)
        else
          loop j (k+1) 0 (tmp+.tmp2)
      else
        loop j k (l+1) (tmp+.tmp2)
    else if l = (b-bi) then 
      if k = j - 1 then 
        if j = tumo_len then 
          tmp +.tmp2
        else
          loop (j+1) 1 0 (tmp+.tmp2)
      else
        loop j (k+1) 0 (tmp+.tmp2)
    else
      loop j k (l+1) (tmp+.tmp2)
  in
  loop 2 1 0 0.0
  
  
let tenpai_ritu_2 yama_len tumo_len a b = 
  let yama_len = yama_len + 14 in
  let rec loop2 i j tmp = 
    let tmp2 = target2 yama_len tumo_len a b i j in 
    let tmp = tmp +. tmp2 in
    if i = 1 then 
      if j = 1 then 
        tmp
      else
        loop2 i (j-1) tmp
    else
      if j = 1 then 
        loop2 (i-1) b tmp
      else
        loop2 i (j-1) tmp
  in
  loop2 a b 0.0
           

let target3 yama_len tumo_len a b c ai bi ci= 
  let rec under_loop j tmp = 
    let tmp = tmp *. (float_of_int (yama_len-j)) in 
    if j = 0 then 
      tmp
    else
      under_loop (j-1) tmp
  in 
  let rec over j target n_t tmp = 
    if j = 0 then 
      tmp
    else
      let tmp = tmp *. float_of_int ((yama_len+1)-(target*4)-(j+n_t)) in 
      over (j-1) target n_t tmp
  in
  (*x:a,y:i*)
  let rec over2 x y tmp = 
    let tmp = y * tmp in 
    if x = y then 
      tmp
    else
      over2 x (y+1) tmp
  in
  let under = under_loop (a+b+c-1) 1. in
  let loop j k1 k2 l m1 m2 = 
    let tmp2 = 
        let tmp_ac = float_of_int (pure_combination (k1*3) ((a-ai)+l+m1)) in
        let tmp_ac = tmp_ac *. (float_of_int (factorial ((a-ai)+l+m1)) /.  float_of_int (((factorial l) * (factorial (a-ai)) * (factorial m1)))) in
        let tmp_bc = float_of_int (pure_combination ((k2-k1)*3) ((b-bi-l)+m2)) in
        let tmp_bc = tmp_bc *. (float_of_int (factorial ((b-bi-l)+m2)) /. float_of_int (((factorial m2) * (factorial (b-bi-l))))) in
        let tmp_cc = float_of_int (pure_combination ((j-k2)*3) (c-ci-(m1+m2))) in
        let tmp_oa = over (ai-1) k1 (bi+ci) 1. in
        let tmp_ob = over (bi-1) k2 ci 1. in
        let tmp_oc = over (ci-1) j 0 1. in 
        let tmp_o2a = float_of_int (over2 a ai 1) in 
        let tmp_o2b = float_of_int (over2 b bi 1) in 
        let tmp_o2c = float_of_int (over2 c ci 1) in 
        ((tmp_oa *. tmp_ob *. tmp_oc *. tmp_o2a *. tmp_ac *. tmp_o2b *. tmp_bc *. tmp_o2c *. tmp_cc) /. under) 
    in 
    tmp2
  in
  let rec loop3 j k1 k2 l m1 m2 tmp = 
    let tmp2 = loop j k1 k2 l m1 m2 in
    if m2 < (3-((b-bi)-l)) then 
      if m2 < (c-ci) - m1 then
        loop3 j k1 k2 l m1 (m2+1) (tmp+.tmp2)
      else
        tmp+.tmp2
    else
      tmp +. tmp2
  in
  let rec loop3_normal j k1 k2 l m1 m2 tmp = 
    let tmp2 = loop j k1 k2 l m1 m2 in
    if m2 < (c-ci) - m1 then
      loop3_normal j k1 k2 l m1 (m2+1) (tmp+.tmp2)
    else
      tmp+.tmp2
  in
  let rec loop2 j k1 k2 l m1 tmp =
    let tmp2 = loop3 j k1 k2 l m1 0 tmp in 
    if (l+m1) < (3-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2 j k1 k2 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_k1 j k1 k2 l m1 tmp =
    let tmp2 = loop3_normal j k1 k2 l m1 0 tmp in 
    if (l+m1) < (3-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2_k1 j k1 k2 l (m1+1) tmp2
      else
        tmp2
    else 
      tmp2
  in
  let rec loop2_k1_2 j k1 k2 l m1 tmp =
    let tmp2 = 
      if k2 = 3 then 
        loop3 j k1 k2 l m1 0 tmp 
      else
        loop3_normal j k1 k2 l m1 0 tmp 
    in 
    if (l+m1) < (6-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2_k1_2 j k1 k2 l (m1+1) tmp2
      else
        tmp2
    else 
      tmp2
  in
  let rec loop2_k2 j k1 k2 l m1 tmp =
    let tmp2 = loop3 j k1 k2 l m1 0 tmp in 
    if m1 < (c-ci) then 
      loop2_k2 j k1 k2 l (m1+1) tmp2
    else
      tmp2
  in
  let rec loop2_normal j k1 k2 l m1 tmp =
    let tmp2 = loop3_normal j k1 k2 l m1 0 tmp in 
    if m1 < (c-ci) then 
      loop2_normal j k1 k2 l (m1+1) tmp2
    else
      tmp2
  in
  let rec loop1 j k1 k2 l tmp = 
    if l <= (3-(a-ai)) then 
      let tmp = loop2 j k1 k2 l 0 tmp in
      if l < (b-bi) then
        loop1 j k1 k2 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_k1 j k1 k2 l tmp = 
    if l <= (3-(a-ai)) then 
      let tmp = loop2_k1 j k1 k2 l 0 tmp in
      if l < (b-bi) then
        loop1_k1 j k1 k2 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_k1_2 j k1 k2 l tmp = 
    if l <= (6-(a-ai)) then 
      let tmp = loop2_k1_2 j k1 k2 l 0 tmp in
      if l < (b-bi) then
        loop1_k1_2 j k1 k2 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_k2 j k1 k2 l tmp = 
    let tmp = loop2_k2 j k1 k2 l 0 tmp in
    if l < (b-bi) then
      loop1_k2 j k1 k2 (l+1) tmp
    else
      tmp
  in
  let rec loop1_normal j k1 k2 l tmp = 
    let tmp = loop2_normal j k1 k2 l 0 tmp in
    if l < (b-bi) then
      loop1_normal j k1 k2 (l+1) tmp
    else
      tmp
  in
  let rec out_loop j k1 k2 tmp = 
    if k1 = 1 && k2 = 2 then 
      let tmp2 = loop1 j k1 k2 0 tmp in
      if k2 = j - 1 then 
        if j = tumo_len then 
          tmp2
        else
          out_loop (j+1) 1 2 tmp2
      else
        out_loop j 1 (k2+1) tmp2
    else if k1 = 2 then 
      let tmp2 = loop1_k1_2 j k1 k2 0 tmp in 
      if k1 = k2 -1 then  
        if k2 = j - 1 then
          if j = tumo_len then 
            tmp2
          else
            out_loop (j+1) 1 2 tmp2
        else
          out_loop j 1 (k2+1) tmp2
      else
        out_loop j (k1+1) k2 tmp2
    else if k1 = 1 then 
      let tmp2 = loop1_k1 j k1 k2 0 tmp in
      if k1 = k2 -1 then  
        if k2 = j - 1 then
          if j = tumo_len then 
            tmp2
          else
            out_loop (j+1) 1 2 tmp2
        else
          out_loop j 1 (k2+1) tmp2
      else
        out_loop j (k1+1) k2 tmp2
    else if k2 = k1 + 1 then 
      let tmp2 = loop1_k2 j k1 k2 0 tmp in
      if k2 = j - 1 then
        if j = tumo_len then 
          tmp2
        else
          out_loop (j+1) 1 2 tmp2
      else
        out_loop j 1 (k2+1) tmp2
    else
      let tmp2 = loop1_normal j k1 k2 0 tmp in
      if k1 = k2 -1 then  
        if k2 = j - 1 then
          if j = tumo_len then 
            tmp2
          else
            out_loop (j+1) 1 2 tmp2
        else
          out_loop j 1 (k2+1) tmp2
      else
        out_loop j (k1+1) k2 tmp2
      in   
  out_loop 3 1 2 0.0
  
let tenpai_ritu_3 yama_len tumo_len a b c = 
  let yama_len = yama_len + 14 in
  let rec loop2 i j k tmp = 
    let tmp2 = target3 yama_len tumo_len a b c i j k in 
    let tmp = tmp +. tmp2 in
    if i = 1 then 
      if j = 1 then 
        if k = 1 then 
          tmp
        else
          loop2 i j (k-1) tmp
      else
        if k = 1 then 
          loop2 i (j-1) c tmp
        else
          loop2 i j (k-1) tmp
    else
      if j = 1 then 
        if k = 1 then 
          loop2 (i-1) b c tmp
        else
          loop2 i j (k-1) tmp
      else
        if k = 1 then 
          loop2 i (j-1) c tmp
        else
          loop2 i j (k-1) tmp
  in
  loop2 a b c 0.0


let target4 yama_len tumo_len a b c d ai bi ci di = 
  let rec under_loop j tmp = 
    let tmp = tmp *. (float_of_int (yama_len-j)) in 
    if j = 0 then 
      tmp
    else
      under_loop (j-1) tmp
  in 
  let rec over j target n_t tmp = 
    if j = 0 then 
      tmp
    else
      let tmp = tmp *. float_of_int ((yama_len+1)-(target*4)-(j+n_t)) in 
      over (j-1) target n_t tmp
  in
  (*x:a,y:i*)
  let rec over2 x y tmp = 
    let tmp = y * tmp in 
    if x = y then 
      tmp
    else
      over2 x (y+1) tmp
  in
  let under = under_loop (a+b+c+d-1) 1. in
  let loop j k1 k2 k3 l m1 m2 n1 n2 n3 = 
    let tmp2 = 
        let tmp_ac = float_of_int (pure_combination (k1*3) ((a-ai)+l+m1+n1)) in
        let tmp_ac = tmp_ac *. (float_of_int (factorial ((a-ai)+l+m1+n1)) /. float_of_int ((factorial l) * (factorial (a-ai)) * (factorial m1) * (factorial n1))) in
        let _ = if float_of_int ((factorial l) * (factorial (a-ai)) * (factorial m1) * (factorial n1)) = 0.0 then (Printf.printf "%d %d %d\n" l m1 n1;flush stdout;) else () in 
        let tmp_bc = float_of_int (pure_combination ((k2-k1)*3) (((b-bi)-l)+m2+n2)) in
        let tmp_bc = tmp_bc *.  (float_of_int (factorial ((b-bi-l)+m2+n2)) /. float_of_int ((factorial m2) * (factorial (b-bi-l)) * (factorial n2))) in
        let _ = if float_of_int ((factorial m2) * (factorial (b-bi-l)) * (factorial n2)) = 0.0 then (Printf.printf "%d %d\n" m2 n2;flush stdout;) else () in
        let tmp_cc = float_of_int (pure_combination ((k3-k2)*3) ((c-ci-m1-m2)+n3)) in
        let tmp_cc = tmp_cc *. (float_of_int (factorial ((c-ci-m1-m2)+n3)) /. float_of_int ((factorial n3) * (factorial ((c-ci-m1-m2))))) in
        let _ = if float_of_int ((factorial n3) * (factorial ((c-ci-m1-m2)))) = 0.0 then (Printf.printf "%d\n" n3;flush stdout;) else () in
        let tmp_dc = float_of_int (pure_combination ((j-k3)*3) (d-di-n1-n2-n3)) in
        let tmp_oa = over (ai-1) k1 (bi+ci+di) 1. in
        let tmp_ob = over (bi-1) k2 (ci+di) 1. in
        let tmp_oc = over (ci-1) k3 di 1. in
        let tmp_od = over (di-1) j 0 1. in 
        let tmp_o2a = float_of_int (over2 a ai 1) in 
        let tmp_o2b = float_of_int (over2 b bi 1) in 
        let tmp_o2c = float_of_int (over2 c ci 1) in 
        let tmp_o2d = float_of_int (over2 d di 1) in 
        ((tmp_oa *. tmp_ob *. tmp_oc *. tmp_od *. tmp_o2a *. tmp_ac *. tmp_o2b *. tmp_bc *. tmp_o2c *. tmp_cc *. tmp_o2d *. tmp_dc) /. under) 
    in
    tmp2
  in 
  let rec loop6 j k1 k2 k3 l m1 n1 m2 n2 n3 tmp = 
    let tmp2 = loop j k1 k2 k3 l m1 m2 n1 n2 n3 in
    if n3 < (3-((c-ci)-(m1+m2))) then 
      if n3 < (d-di) - (n1+n2)  then
        loop6 j k1 k2 k3 l m1 n1 m2 n2 (n3+1) (tmp+.tmp2)
      else
        tmp +. tmp2
    else
      tmp +. tmp2
  in
  let rec loop6_normal j k1 k2 k3 l m1 n1 m2 n2 n3 tmp = 
    let tmp2 = loop j k1 k2 k3 l m1 m2 n1 n2 n3 in
    if n3 < (d-di) - (n1+n2)  then
      loop6_normal j k1 k2 k3 l m1 n1 m2 n2 (n3+1) (tmp+.tmp2)
    else
      tmp +. tmp2
  in
  let rec loop5 j k1 k2 k3 l m1 n1 m2 n2 tmp = 
    let tmp2 = 
      if k3 = k2 + 1 then 
        loop6 j k1 k2 k3 l m1 n1 m2 n2 0 tmp 
      else
        loop6_normal j k1 k2 k3 l m1 n1 m2 n2 0 tmp 
    in
    if (m2+n2) < (3-((b-bi)-l)) then 
      if n2 < (d-di) - n1  then
        loop5 j k1 k2 k3 l m1 n1 m2 (n2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop5_2 j k1 k2 k3 l m1 n1 m2 n2 tmp = 
    let tmp2 = 
      if k3 = k2 + 1 then 
        loop6 j k1 k2 k3 l m1 n1 m2 n2 0 tmp 
      else
        loop6_normal j k1 k2 k3 l m1 n1 m2 n2 0 tmp 
    in
    if (m2+n2) < (6-((b-bi)-l)) then 
      if n2 < (d-di) - n1  then
        loop5_2 j k1 k2 k3 l m1 n1 m2 (n2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop5_normal j k1 k2 k3 l m1 n1 m2 n2 tmp = 
    let tmp2 = 
      if k3 = k2 + 1 then 
        loop6 j k1 k2 k3 l m1 n1 m2 n2 0 tmp 
      else
        loop6_normal j k1 k2 k3 l m1 n1 m2 n2 0 tmp 
    in
    if n2 < (d-di) - n1  then
      loop5_normal j k1 k2 k3 l m1 n1 m2 (n2+1) tmp2
    else
      tmp2
  in
  let rec loop4 j k1 k2 k3 l m1 n1 m2 tmp = 
    let tmp2 = loop5 j k1 k2 k3 l m1 n1 m2 0 tmp in
    if m2 < (3-((b-bi)-l)) then 
      if m2 < (c-ci) - m1  then
        loop4 j k1 k2 k3 l m1 n1 (m2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop4_2 j k1 k2 k3 l m1 n1 m2 tmp = 
    let tmp2 = loop5_2 j k1 k2 k3 l m1 n1 m2 0 tmp in
    if m2 < (6-((b-bi)-l)) then 
      if m2 < (c-ci) - m1  then
        loop4_2 j k1 k2 k3 l m1 n1 (m2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop4_normal j k1 k2 k3 l m1 n1 m2 tmp = 
    let tmp2 = loop5_normal j k1 k2 k3 l m1 n1 m2 0 tmp in
    if m2 < (c-ci) - m1  then
      loop4_normal j k1 k2 k3 l m1 n1 (m2+1) tmp2
    else
      tmp2
  in
  let rec loop3 j k1 k2 k3 l m1 n1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop4 j k1 k2 k3 l m1 n1 0 tmp
      else if k2 = k1 + 2 then 
        loop4_2 j k1 k2 k3 l m1 n1 0 tmp
      else
        loop4_normal j k1 k2 k3 l m1 n1 0 tmp 
    in
    if (l+m1+n1) < (3-(a-ai)) then 
      if n1 < (d-di) then
        loop3 j k1 k2 k3 l m1 (n1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop3_2 j k1 k2 k3 l m1 n1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop4 j k1 k2 k3 l m1 n1 0 tmp
      else if k2 = k1 + 2 then 
        loop4_2 j k1 k2 k3 l m1 n1 0 tmp
      else
        loop4_normal j k1 k2 k3 l m1 n1 0 tmp 
    in
    if (l+m1+n1) < (6-(a-ai)) then 
      if n1 < (d-di) then
        loop3 j k1 k2 k3 l m1 (n1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop3_3 j k1 k2 k3 l m1 n1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop4 j k1 k2 k3 l m1 n1 0 tmp
      else if k2 = k1 + 2 then 
        loop4_2 j k1 k2 k3 l m1 n1 0 tmp 
      else
        loop4_normal j k1 k2 k3 l m1 n1 0 tmp 
    in
    if (l+m1+n1) < (9-(a-ai)) then 
      if n1 < (d-di) then
        loop3 j k1 k2 k3 l m1 (n1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop3_normal j k1 k2 k3 l m1 n1 tmp = 
    let tmp2 =
      if k2 = k1 + 1 then 
        loop4 j k1 k2 k3 l m1 n1 0 tmp
      else if k2 = k1 + 2 then 
        loop4_2 j k1 k2 k3 l m1 n1 0 tmp
      else
        loop4_normal j k1 k2 k3 l m1 n1 0 tmp 
    in
    if n1 < (d-di) then
      loop3_normal j k1 k2 k3 l m1 (n1+1) tmp2
    else
      tmp2
  in
  let rec loop2 j k1 k2 k3 l m1 tmp =
    let tmp2 = loop3 j k1 k2 k3 l m1 0 tmp in 
    if (l+m1) < (3-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2 j k1 k2 k3 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_2 j k1 k2 k3 l m1 tmp =
    let tmp2 = loop3_2 j k1 k2 k3 l m1 0 tmp in 
    if (l+m1) < (6-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2_2 j k1 k2 k3 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_3 j k1 k2 k3 l m1 tmp =
    let tmp2 = loop3_3 j k1 k2 k3 l m1 0 tmp in 
    if (l+m1) < (9-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2_3 j k1 k2 k3 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_normal j k1 k2 k3 l m1 tmp =
    let tmp2 = loop3_normal j k1 k2 k3 l m1 0 tmp in 
    if m1 < (c-ci) then 
      loop2_normal j k1 k2 k3 l (m1+1) tmp2
    else
      tmp2
  in
  let rec loop1 j k1 k2 k3 l tmp = 
    if l <= (3-(a-ai)) then 
      let tmp = loop2 j k1 k2 k3 l 0 tmp in
      if l < (b-bi) then
        loop1 j k1 k2 k3 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_2 j k1 k2 k3 l tmp = 
    if l <= (6-(a-ai)) then 
      let tmp = loop2_2 j k1 k2 k3 l 0 tmp in
      if l < (b-bi) then
        loop1_2 j k1 k2 k3 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_3 j k1 k2 k3 l tmp = 
    if l <= (9-(a-ai)) then 
      let tmp = loop2_3 j k1 k2 k3 l 0 tmp in
      if l < (b-bi) then
        loop1_3 j k1 k2 k3 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_normal j k1 k2 k3 l tmp = 
    let tmp = loop2_normal j k1 k2 k3 l 0 tmp in
    if l < (b-bi) then
      loop1_normal j k1 k2 k3 (l+1) tmp
    else
      tmp
  in
  let rec out_loop j k1 k2 k3 tmp = 
    if k1 = 1 then 
      let tmp2 = loop1 j k1 k2 k3 0 tmp in 
      if k1 = k2 - 1 then 
        if k2 = k3 - 1 then
          if k3 = j - 1 then 
            if j = tumo_len then 
              tmp2
            else
              out_loop (j+1) 1 2 3 tmp2
          else
            out_loop j 1 2 (k3+1) tmp2
        else
          out_loop j 1 (k2+1) k3 tmp2
      else
        out_loop j (k1+1) k2 k3 tmp2
    else if k2 = 2 then 
      let tmp2 = loop1_2 j k1 k2 k3 0 tmp in
      if k1 = k2 - 1 then 
        if k2 = k3 - 1 then
          if k3 = j - 1 then 
            if j = tumo_len then 
              tmp2
            else
              out_loop (j+1) 1 2 3 tmp2
          else
            out_loop j 1 2 (k3+1) tmp2
        else
          out_loop j 1 (k2+1) k3 tmp2
      else
        out_loop j (k1+1) k2 k3 tmp2
    else if k3 = 3 then 
      let tmp2 = loop1_3 j k1 k2 k3 0 tmp in 
      if k1 = k2 - 1 then 
        if k2 = k3 - 1 then
          if k3 = j - 1 then 
            if j = tumo_len then 
              tmp2
            else
              out_loop (j+1) 1 2 3 tmp2
          else
            out_loop j 1 2 (k3+1) tmp2
        else
          out_loop j 1 (k2+1) k3 tmp2
      else
        out_loop j (k1+1) k2 k3 tmp2
    else
      let tmp2 = loop1_normal j k1 k2 k3 0 tmp in 
      if k1 = k2 - 1 then 
        if k2 = k3 - 1 then
          if k3 = j - 1 then 
            if j = tumo_len then 
              tmp2
            else
              out_loop (j+1) 1 2 3 tmp2
          else
            out_loop j 1 2 (k3+1) tmp2
        else
          out_loop j 1 (k2+1) k3 tmp2
      else
        out_loop j (k1+1) k2 k3 tmp2
  in
  out_loop 4 1 2 3 0.0

  
  
let tenpai_ritu_4 yama_len tumo_len a b c d =
  let yama_len = yama_len + 14 in
  let rec loop2 i j k l tmp = 
    let tmp2 = target4 yama_len tumo_len a b c d i j k l in 
    let tmp = tmp +. tmp2 in
    if i = 1 then 
      if j = 1 then 
        if k = 1 then 
          if l = 1 then 
            tmp
          else
            loop2 i j k (l-1) tmp
        else
          if l = 1 then 
            loop2 i j (k-1) d tmp
          else
            loop2 i j k (l-1) tmp
      else
        if k = 1 then
          if l = 1 then 
            loop2 i (j-1) c d tmp
          else
            loop2 i j k (l-1) tmp 
        else
          if l = 1 then 
            loop2 i j (k-1) d tmp
          else
            loop2 i j k (l-1) tmp
    else
      if j = 1 then 
        if k = 1 then 
          if l = 1 then 
            loop2 (i-1) b c d tmp
          else
            loop2 i j k (l-1) tmp
        else
          if l = 1 then 
            loop2 i j (k-1) d tmp
          else
            loop2 i j k (l-1) tmp
      else
        if k = 1 then
          if l = 1 then 
            loop2 i (j-1) c d tmp
          else
            loop2 i j k (l-1) tmp 
        else
          if l = 1 then 
            loop2 i j (k-1) d tmp
          else
            loop2 i j k (l-1) tmp
  in
  loop2 a b c d 0.0

(*
let target5 yama_len tumo_len a b c d e ai bi ci di ei = 
  let rec under_loop j tmp = 
    let tmp = tmp *. (float_of_int (yama_len-j)) in 
    if j = 0 then 
      tmp
    else
      under_loop (j-1) tmp
  in 
  let rec over j target n_t tmp = 
    if j = 0 then 
      tmp
    else
      let tmp = tmp *. float_of_int ((yama_len+1)-(target*4)-(j+n_t)) in 
      over (j-1) target n_t tmp
  in
  (*x:a,y:i*)
  let rec over2 x y tmp = 
    let tmp = y * tmp in 
    if x = y then 
      tmp
    else
      over2 x (y+1) tmp
  in
  let under = under_loop (a+b+c+d+e-1) 1. in
  let loop j k1 k2 k3 k4 l m1 m2 n1 n2 n3 o1 o2 o3 o4 = 
    let tmp2 = 
        let tmp_ac = float_of_int (pure_combination (k1*3) ((a-ai)+l+m1+n1+o1)) in
        let tmp_ac = tmp_ac *. (float_of_int (factorial ((a-ai)+l+m1+n1+o1)) /. float_of_int ((factorial l) * (factorial (a-ai)) * (factorial m1) * (factorial n1) * (factorial o1))) in
        let tmp_bc = float_of_int (pure_combination ((k2-k1)*3) (((b-bi)-l)+m2+n2+o2)) in
        let tmp_bc = tmp_bc *.  (float_of_int (factorial ((b-bi-l)+m2+n2+o2)) /. float_of_int ((factorial m2) * (factorial (b-bi-l)) * (factorial n2) * (factorial o2))) in
        let tmp_cc = float_of_int (pure_combination ((k3-k2)*3) ((c-ci-m1-m2)+n3+o3)) in
        let tmp_cc = tmp_cc *. (float_of_int (factorial ((c-ci-m1-m2)+n3+o3)) /. float_of_int ((factorial n3) * (factorial ((c-ci-m1-m2))) * (factorial o3))) in
        let tmp_dc = float_of_int (pure_combination ((k4-k3)*3) ((d-di-n1-n2-n3)+o4)) in
        let tmp_dc = tmp_dc *. (float_of_int (factorial ((d-di-n1-n2-n3)+o4)) /. float_of_int ((factorial (d-di-n1-n2-n3)) * (factorial o4))) in
        let tmp_ec = float_of_int (pure_combination ((j-k4)*3) ((e-ei-o1-o2-o3-o4))) in
        let tmp_oa = over (ai-1) k1 (bi+ci+di+ei) 1. in
        let tmp_ob = over (bi-1) k2 (ci+di+ei) 1. in
        let tmp_oc = over (ci-1) k3 (di+ei) 1. in
        let tmp_od = over (di-1) k4 ei 1. in
        let tmp_oe = over (ei-1) j 0 1. in 
        let tmp_o2a = float_of_int (over2 a ai 1) in 
        let tmp_o2b = float_of_int (over2 b bi 1) in 
        let tmp_o2c = float_of_int (over2 c ci 1) in 
        let tmp_o2d = float_of_int (over2 d di 1) in 
        let tmp_o2e = float_of_int (over2 e ei 1) in 
        ((tmp_oa *. tmp_ob *. tmp_oc *. tmp_od *. tmp_oe *. tmp_o2a *. tmp_ac *. tmp_o2b *. tmp_bc *. tmp_o2c *. tmp_cc *. tmp_o2d *. tmp_dc *. tmp_o2e *. tmp_ec) /. under) 
    in
    tmp2
  in 
  let rec loop10 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 o4 tmp = 
    let tmp2 = loop j k1 k2 k3 k4 l m1 m2 n1 n2 n3 o1 o2 o3 o4 in
    if o4 < (3-((d-di)-(n1+n2+n3))) then 
      if o4 < (e-ei) - (o1+o2+o3)  then
        loop10 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 (o4+1) (tmp+.tmp2)
      else
        tmp +. tmp2
    else
      tmp +. tmp2
  in
  let rec loop10_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 o4 tmp = 
    let tmp2 = loop j k1 k2 k3 k4 l m1 m2 n1 n2 n3 o1 o2 o3 o4 in
    if o4 < (e-ei) - (o1+o2+o3)  then
      loop10_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 (o4+1) (tmp+.tmp2)
    else
      tmp +. tmp2
  in
  let rec loop9 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 tmp = 
    let tmp2 = 
      if k4 = k3 + 1 then 
        loop10 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 0 tmp
      else
        loop10_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 0 tmp
    in
    if o3 < (3-((c-ci)-(m1+m2))) then 
      if o3 < (e-ei) - (o1+o2)  then
        loop9 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 (o3+1) tmp2
      else
        tmp 
    else
      tmp
  in
  let rec loop9_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 tmp = 
    let tmp2 = 
      if k4 = k3 + 1 then 
        loop10 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 0 tmp
      else
        loop10_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 0 tmp
    in
    if o3 < (6-((c-ci)-(m1+m2))) then 
      if o3 < (e-ei) - (o1+o2)  then
        loop9_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 (o3+1) tmp2
      else
        tmp 
    else
      tmp
  in
  let rec loop9_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 tmp = 
    let tmp2 = 
      if k4 = k3 + 1 then 
        loop10 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 0 tmp
      else
        loop10_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 o3 0 tmp
    in
    if o3 < (e-ei) - (o1+o2)  then
      loop9_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 (o3+1) tmp2
    else
      tmp 
  in
  let rec loop8 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 tmp = 
    let tmp2 = loop9 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 0 tmp in
    if n3 < (3-((c-ci)-(m1+m2))) then 
      if n3 < (d-di) - (n1+n2)  then
        loop8 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 (n3+1) tmp2
      else
        tmp 
    else
      tmp
  in
  let rec loop8_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 tmp = 
    let tmp2 = loop9_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 0 tmp in
    if n3 < (6-((c-ci)-(m1+m2))) then 
      if n3 < (d-di) - (n1+n2)  then
        loop8_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 (n3+1) tmp2
      else
        tmp 
    else
      tmp
  in
  let rec loop8_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 tmp = 
    let tmp2 = loop9_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 n3 0 tmp in
    if n3 < (d-di) - (n1+n2)  then
      loop8_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 (n3+1) tmp2
    else
      tmp 
  in
  let rec loop7 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 tmp = 
    let tmp2 = 
      if k3 = k2 + 1 then 
        loop8 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
      else if k3 = k2 + 2 then 
        loop8_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp
      else
        loop8_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
    in
    if (m2+n2+o2) < (3-((b-bi)-l)) then 
      if o2 < (e-ei) - o1  then
        loop7 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 (o2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop7_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 tmp = 
    let tmp2 = 
      if k3 = k2 + 1 then 
        loop8 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
      else if k3 = k2 + 2 then 
        loop8_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp
      else
        loop8_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
    in
    if (m2+n2+o2) < (6-((b-bi)-l)) then 
      if o2 < (e-ei) - o1  then
        loop7_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 (o2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop7_3 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 tmp = 
    let tmp2 = 
      if k3 = k2 + 1 then 
        loop8 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
      else if k3 = k2 + 2 then 
        loop8_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp
      else
        loop8_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
    in
    if (m2+n2+o2) < (9-((b-bi)-l)) then 
      if o2 < (e-ei) - o1  then
        loop7_3 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 (o2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop7_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 tmp = 
    let tmp2 = 
      if k3 = k2 + 1 then 
        loop8 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
      else if k3 = k2 + 2 then 
        loop8_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp
      else
        loop8_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 o2 0 tmp 
    in
    if o2 < (e-ei) - o1  then
      loop7_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 (o2+1) tmp2
    else
      tmp2
  in
  let rec loop6 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 tmp = 
    let tmp2 = loop7 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 0 tmp 
    in
    if (m2+n2) < (3-((b-bi)-l)) then 
      if n2 < (d-di) - n1  then
        loop6 j k1 k2 k3 k4 l m1 n1 m2 o1 (n2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop6_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 tmp = 
    let tmp2 = loop7_2 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 0 tmp 
    in
    if (m2+n2) < (6-((b-bi)-l)) then 
      if n2 < (d-di) - n1  then
        loop6_2 j k1 k2 k3 k4 l m1 n1 m2 o1 (n2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop6_3 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 tmp = 
    let tmp2 = loop7_3 j k1 k2 k3 k4 l m1 n1 o1 m2 n2 0 tmp 
    in
    if (m2+n2) < (9-((b-bi)-l)) then 
      if n2 < (d-di) - n1  then
        loop6_3 j k1 k2 k3 k4 l m1 n1 m2 o1 (n2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop6_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 tmp = 
    let tmp2 = loop7_normal j k1 k2 k3 k4 l m1 n1 o1 m2 n2 0 tmp 
    in
    if n2 < (d-di) - n1  then
      loop6_normal j k1 k2 k3 k4 l m1 n1 m2 o1 (n2+1) tmp2
    else
      tmp2
  in
  let rec loop5 j k1 k2 k3 k4 l m1 n1 o1 m2 tmp = 
    let tmp2 = loop6 j k1 k2 k3 k4 l m1 n1 o1 m2 0 tmp in
    if m2 < (3-((b-bi)-l)) then 
      if m2 < (c-ci) - m1  then
        loop5 j k1 k2 k3 k4 l m1 n1 o1 (m2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop5_2 j k1 k2 k3 k4 l m1 n1 o1 m2 tmp = 
    let tmp2 = loop6_2 j k1 k2 k3 k4 l m1 n1 o1 m2 0 tmp in
    if m2 < (6-((b-bi)-l)) then 
      if m2 < (c-ci) - m1  then
        loop5_2 j k1 k2 k3 k4 l m1 n1 o1 (m2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop5_3 j k1 k2 k3 k4 l m1 n1 o1 m2 tmp = 
    let tmp2 = loop6_3 j k1 k2 k3 k4 l m1 n1 o1 m2 0 tmp in
    if m2 < (9-((b-bi)-l)) then 
      if m2 < (c-ci) - m1  then
        loop5_3 j k1 k2 k3 k4 l m1 n1 o1 (m2+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop5_normal j k1 k2 k3 k4 l m1 n1 o1 m2 tmp = 
    let tmp2 = loop6_normal j k1 k2 k3 k4 l m1 n1 o1 m2 0 tmp in
    if m2 < (c-ci) - m1  then
      loop5_normal j k1 k2 k3 k4 l m1 n1 o1 (m2+1) tmp2
    else
      tmp2
  in
  let rec loop4 j k1 k2 k3 k4 l m1 n1 o1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop5 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 2 then 
        loop5_2 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 3 then 
        loop5_3 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else
        loop5_normal j k1 k2 k3 k4 l m1 n1 o1 0 tmp 
    in
    if (l+m1+n1+o1) < (3-(a-ai)) then 
      if o1 < (e-ei) then
        loop4 j k1 k2 k3 k4 l m1 n1 (o1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop4_2 j k1 k2 k3 k4 l m1 n1 o1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop5 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 2 then 
        loop5_2 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 3 then 
        loop5_3 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else
        loop5_normal j k1 k2 k3 k4 l m1 n1 o1 0 tmp 
    in
    if (l+m1+n1+o1) < (6-(a-ai)) then 
      if o1 < (e-ei) then
        loop4_2 j k1 k2 k3 k4 l m1 n1 (o1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop4_3 j k1 k2 k3 k4 l m1 n1 o1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop5 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 2 then 
        loop5_2 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 3 then 
        loop5_3 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else
        loop5_normal j k1 k2 k3 k4 l m1 n1 o1 0 tmp 
    in
    if (l+m1+n1+o1) < (9-(a-ai)) then 
      if o1 < (e-ei) then
        loop4_3 j k1 k2 k3 k4 l m1 n1 (o1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop4_4 j k1 k2 k3 k4 l m1 n1 o1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop5 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 2 then 
        loop5_2 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 3 then 
        loop5_3 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else
        loop5_normal j k1 k2 k3 k4 l m1 n1 o1 0 tmp 
    in
    if (l+m1+n1+o1) < (12-(a-ai)) then 
      if o1 < (e-ei) then
        loop4_4 j k1 k2 k3 k4 l m1 n1 (o1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop4_normal j k1 k2 k3 k4 l m1 n1 o1 tmp = 
    let tmp2 = 
      if k2 = k1 + 1 then 
        loop5 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 2 then 
        loop5_2 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else if k2 = k1 + 3 then 
        loop5_3 j k1 k2 k3 k4 l m1 n1 o1 0 tmp
      else
        loop5_normal j k1 k2 k3 k4 l m1 n1 o1 0 tmp 
    in
    if o1 < (e-ei) then
      loop4_normal j k1 k2 k3 k4 l m1 n1 (o1+1) tmp2
    else
      tmp2
  in
  let rec loop3 j k1 k2 k3 k4 l m1 n1 tmp =
    let tmp2 = loop4 j k1 k2 k3 k4 l m1 n1 0 tmp in 
    if (l+m1+n1) < (3-(a-ai)) then 
      if n1 < (d-di) then
        loop3 j k1 k2 k3 k4 l m1 (n1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop3_2 j k1 k2 k3 k4 l m1 n1 tmp =
    let tmp2 = loop4_2 j k1 k2 k3 k4 l m1 n1 0 tmp in 
    if (l+m1+n1) < (6-(a-ai)) then 
      if n1 < (d-di) then
        loop3_2 j k1 k2 k3 k4 l m1 (n1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop3_3 j k1 k2 k3 k4 l m1 n1 tmp =
    let tmp2 = loop4_3 j k1 k2 k3 k4 l m1 n1 0 tmp in 
    if (l+m1+n1) < (9-(a-ai)) then 
      if n1 < (d-di) then
        loop3_3 j k1 k2 k3 k4 l m1 (n1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop3_4 j k1 k2 k3 k4 l m1 n1 tmp =
    let tmp2 = loop4_4 j k1 k2 k3 k4 l m1 n1 0 tmp in 
    if (l+m1+n1) < (12-(a-ai)) then 
      if n1 < (d-di) then
        loop3_4 j k1 k2 k3 k4 l m1 (n1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop3_normal j k1 k2 k3 k4 l m1 n1 tmp =
    let tmp2 = loop4_normal j k1 k2 k3 k4 l m1 n1 0 tmp in 
    if n1 < (d-di) then
      loop3_normal j k1 k2 k3 k4 l m1 (n1+1) tmp2
    else
      tmp2
  in
  let rec loop2 j k1 k2 k3 k4 l m1 tmp =
    let tmp2 = loop3 j k1 k2 k3 k4 l m1 0 tmp in 
    if (l+m1) < (3-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2 j k1 k2 k3 k4 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_2 j k1 k2 k3 k4 l m1 tmp =
    let tmp2 = loop3_2 j k1 k2 k3 k4 l m1 0 tmp in 
    if (l+m1) < (6-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2_2 j k1 k2 k3 k4 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_3 j k1 k2 k3 k4 l m1 tmp =
    let tmp2 = loop3_3 j k1 k2 k3 k4 l m1 0 tmp in 
    if (l+m1) < (9-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2_3 j k1 k2 k3 k4 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_4 j k1 k2 k3 k4 l m1 tmp =
    let tmp2 = loop3_4 j k1 k2 k3 k4 l m1 0 tmp in 
    if (l+m1) < (12-(a-ai)) then 
      if m1 < (c-ci) then 
        loop2_4 j k1 k2 k3 k4 l (m1+1) tmp2
      else
        tmp2
    else
      tmp2
  in
  let rec loop2_normal j k1 k2 k3 k4 l m1 tmp =
    let tmp2 = loop3_normal j k1 k2 k3 k4 l m1 0 tmp in 
    if m1 < (c-ci) then 
      loop2_normal j k1 k2 k3 k4 l (m1+1) tmp2
    else
      tmp2
  in
  let rec loop1 j k1 k2 k3 k4 l tmp = 
    if l <= (3-(a-ai)) then 
      let tmp = loop2 j k1 k2 k3 k4 l 0 tmp in
      if l < (b-bi) then
        loop1 j k1 k2 k3 k4 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_2 j k1 k2 k3 k4 l tmp = 
    if l <= (6-(a-ai)) then 
      let tmp = loop2_2 j k1 k2 k3 k4 l 0 tmp in
      if l < (b-bi) then
        loop1_2 j k1 k2 k3 k4 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_3 j k1 k2 k3 k4 l tmp = 
    if l <= (9-(a-ai)) then 
      let tmp = loop2_3 j k1 k2 k3 k4 l 0 tmp in
      if l < (b-bi) then
        loop1_3 j k1 k2 k3 k4 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_4 j k1 k2 k3 k4 l tmp = 
    if l <= (12-(a-ai)) then 
      let tmp = loop2_4 j k1 k2 k3 k4 l 0 tmp in
      if l < (b-bi) then
        loop1_4 j k1 k2 k3 k4 (l+1) tmp
      else
        tmp
    else
      tmp
  in
  let rec loop1_normal j k1 k2 k3 k4 l tmp = 
    let tmp = loop2_normal j k1 k2 k3 k4 l 0 tmp in
    if l < (b-bi) then
      loop1_normal j k1 k2 k3 k4 (l+1) tmp
    else
      tmp
  in
  let rec out_loop j k1 k2 k3 k4 tmp = 
    if k1 = 1 then 
      let tmp2 = loop1 j k1 k2 k3 k4 0 tmp in 
      if k2 = k1 + 1 then 
        if k3 = k2 + 1 then 
          if k4 = k3 + 1 then 
            if k4 = j - 1 then 
              if j = tumo_len then 
                tmp2
              else
                out_loop (j+1) 1 2 3 4 tmp2
            else
              out_loop j 1 2 3 (k4+1) tmp2
          else
            out_loop j 1 2 (k3+1) k4 tmp2
        else
          out_loop j 1 (k2+1) k3 k4 tmp2
      else
        out_loop j (k1+1) k2 k3 k4 tmp2
    else if k1 = 2 then 
      let tmp2 = loop1_2 j k1 k2 k3 k4 0 tmp in
      if k2 = k1 + 1 then 
        if k3 = k2 + 1 then 
          if k4 = k3 + 1 then 
            if k4 = j - 1 then 
              if j = tumo_len then 
                tmp2
              else
                out_loop (j+1) 1 2 3 4 tmp2
            else
              out_loop j 1 2 3 (k4+1) tmp2
          else
            out_loop j 1 2 (k3+1) k4 tmp2
        else
          out_loop j 1 (k2+1) k3 k4 tmp2
      else
        out_loop j (k1+1) k2 k3 k4 tmp2
    else if k1 = 3 then 
      let tmp2 = loop1_3 j k1 k2 k3 k4 0 tmp in
      if k2 = k1 + 1 then 
        if k3 = k2 + 1 then 
          if k4 = k3 + 1 then 
            if k4 = j - 1 then 
              if j = tumo_len then 
                tmp2
              else
                out_loop (j+1) 1 2 3 4 tmp2
            else
              out_loop j 1 2 3 (k4+1) tmp2
          else
            out_loop j 1 2 (k3+1) k4 tmp2
        else
          out_loop j 1 (k2+1) k3 k4 tmp2
      else
        out_loop j (k1+1) k2 k3 k4 tmp2
    else if k1 = 4 then 
      let tmp2 = loop1_4 j k1 k2 k3 k4 0 tmp in
      if k2 = k1 + 1 then 
        if k3 = k2 + 1 then 
          if k4 = k3 + 1 then 
            if k4 = j - 1 then 
              if j = tumo_len then 
                tmp2
              else
                out_loop (j+1) 1 2 3 4 tmp2
            else
              out_loop j 1 2 3 (k4+1) tmp2
          else
            out_loop j 1 2 (k3+1) k4 tmp2
        else
          out_loop j 1 (k2+1) k3 k4 tmp2
      else
        out_loop j (k1+1) k2 k3 k4 tmp2
    else
      let tmp2 = loop1_normal j k1 k2 k3 k4 0 tmp in
      if k2 = k1 + 1 then 
        if k3 = k2 + 1 then 
          if k4 = k3 + 1 then 
            if k4 = j - 1 then 
              if j = tumo_len then 
                tmp2
              else
                out_loop (j+1) 1 2 3 4 tmp2
            else
              out_loop j 1 2 3 (k4+1) tmp2
          else
            out_loop j 1 2 (k3+1) k4 tmp2
        else
          out_loop j 1 (k2+1) k3 k4 tmp2
      else
        out_loop j (k1+1) k2 k3 k4 tmp2
  in
  let rec out_loop_p () = 
    let rec make_lst j k1 k2 k3 k4 t_lst = 
      let t_lst = (j,k1,k2,k3,k4)::t_lst in 
      if k2 = k1 + 1 then 
        if k3 = k2 + 1 then 
          if k4 = k3 + 1 then 
            if k4 = j - 1 then 
              if j = tumo_len then 
                t_lst
              else
                make_lst (j+1) 1 2 3 4 t_lst
            else
              make_lst j 1 2 3 (k4+1) t_lst
          else
            make_lst j 1 2 (k3+1) k4 t_lst
        else
          make_lst j 1 (k2+1) k3 k4 t_lst
      else
        make_lst j (k1+1) k2 k3 k4 t_lst
    in
    let lst = make_lst 5 1 2 3 4 [] in 
    let ary = Array.of_list lst in 
    let n = List.length lst in 
    let tasks = Array.init n (fun i -> i) in
    create_work tasks;
    let loop_p x = 
      let (j,k1,k2,k3,k4) = x in 
      if k1 = 1 then
        loop1 j k1 k2 k3 k4 0 0.0
      else if k1 = 2 then 
        loop1_2 j k1 k2 k3 k4 0 0.0 
      else if k1 = 3 then 
        loop1_3 j k1 k2 k3 k4 0 0.0
      else if k1 = 4 then 
        loop1_4 j k1 k2 k3 k4 0 0.0
      else
        loop1_normal j k1 k2 k3 k4 0 0.0
    in
    let update p r i = p.(i) <- loop_p r.(i) in 
    let pre =  Array.make n 0.0 in 
    let domains = Array.init (num_domains - 1)
                (fun _ -> Domain.spawn(worker (update pre ary))) in
    worker (update pre ary) ();
    Array.iter Domain.join domains;
    let x = Array.fold_right (fun a b -> a +. b) pre 0.0  in
    x
  in
  out_loop_p ()
    (*out_loop 5 1 2 3 4 0.0*)

let tenpai_ritu_5 yama_len tumo_len a b c d e =
  let yama_len = yama_len + 14 in
  let rec loop2 i j k l m tmp = 
    let tmp2 = target5 yama_len tumo_len a b c d e i j k l m in 
    let tmp = tmp +. tmp2 in
    if i = 1 then 
      if j = 1 then 
        if k = 1 then 
          if l = 1 then 
            if m = 1 then 
              tmp
            else
              loop2 i j k l (m-1) tmp
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp         
        else
          if l = 1 then 
            if m = 1 then 
              loop2 i j (k-1) d e tmp
            else
              loop2 i j k l (m-1) tmp  
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp  
      else
        if k = 1 then
          if l = 1 then 
            if m = 1 then 
              loop2 i (j-1) c d e tmp
            else
              loop2 i j k l (m-1) tmp  
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp 
        else
          if l = 1 then 
            if m = 1 then 
              loop2 i j (k-1) d e tmp
            else
              loop2 i j k l (m-1) tmp  
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp
    else
      if j = 1 then 
        if k = 1 then 
          if l = 1 then 
            if m = 1 then 
              loop2 (i-1) d c d e tmp
            else
              loop2 i j k l (m-1) tmp  
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp
        else
          if l = 1 then 
            if m = 1 then 
              loop2 i j (k-1) d e tmp
            else
              loop2 i j k l (m-1) tmp  
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp
      else
        if k = 1 then
          if l = 1 then 
            if m = 1 then 
              loop2 i (j-1) c d e tmp
            else
              loop2 i j k l (m-1) tmp  
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp
        else
          if l = 1 then 
            if m = 1 then 
              loop2 i j (k-1) d e tmp
            else
              loop2 i j k l (m-1) tmp  
          else
            if m = 1 then 
              loop2 i j k (l-1) e tmp
            else
              loop2 i j k l (m-1) tmp
  in
  loop2 a b c d e 0.0

let syanten_5_write tumo_len yama_len = 
  Printf.printf "[|[|[|[|[|"; 
  let rec loop a b c d e = 
    flush stdout;
    if a = 1 then 
      if b = 1 then 
        if c = 1 then
          if d = 1 then 
            if e = 1 then 
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|]|]|]|]|]" n; 
              ()
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
          else
            if e = 1 then 
              (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|];[|" n; 
              loop a b c (d-1) 4)
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
        else
          if d = 1 then 
            if e = 1 then 
              (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|]|];[|[|" n; 
              loop a b (c-1) 4 4)
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
          else
            if e = 1 then 
              (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|];[|" n; 
              loop a b c (d-1) 4)
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
      else
        if c = 1 then
          if d = 1 then 
            if e = 1 then 
              (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|]|]|];[|[|[|" n; 
              loop a (b-1) 4 4 4)
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
          else
            if e = 1 then 
              (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|];[|" n; 
              loop a b c (d-1) 4)
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
        else
          if d = 1 then 
            if e = 1 then 
              (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|]|];[|[|" n; 
              loop a b (c-1) 4 4)
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
          else
            if e = 1 then 
              (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F|];[|" n; 
              loop a b c (d-1) 4)
            else
              let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
      else
        if b = 1 then 
          if c = 1 then
            if d = 1 then 
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|]|]|]|];[|[|[|[|" n; 
                loop (a-1) 4 4 4 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
              Printf.printf "%F;" n; 
              loop a b c d (e-1)
            else
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|];[|" n; 
                loop a b c (d-1) 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F;" n; 
                loop a b c d (e-1)
          else
            if d = 1 then 
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|]|];[|[|" n; 
                loop a b (c-1) 4 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F;" n; 
                loop a b c d (e-1)
            else
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|];[|" n; 
                loop a b c (d-1) 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F;" n; 
                loop a b c d (e-1)
        else
          if c = 1 then
            if d = 1 then 
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|]|]|];[|[|[|" n; 
                loop a (b-1) 4 4 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F;" n; 
                loop a b c d (e-1)
            else
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|];[|" n; 
                loop a b c (d-1) 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F;" n; 
                loop a b c d (e-1)
          else
            if d = 1 then 
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|]|];[|[|" n; 
                loop a b (c-1) 4 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F;" n; 
                loop a b c d (e-1)
            else
              if e = 1 then 
                (let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F|];[|" n; 
                loop a b c (d-1) 4)
              else
                let n = tenpai_ritu_5 yama_len tumo_len a b c d e in 
                Printf.printf "%F;" n; 
                loop a b c d (e-1)
  in
  loop 4 4 4 4 4

let syanten5_write_a () = 
  let yama_len = 69 in 
  Printf.printf "[|"; flush stdout;
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 4 then 
        ()
      else
        (syanten_5_write tumo_len yama_len;
        Printf.printf ";")
    in
    if yama_len = 4 then 
      Printf.printf "|]"
    else
      if yama_len mod 5 = 0 then 
        (Printf.printf "\n";
        loop (yama_len - 1))
      else
        loop (yama_len - 1)
  in
  loop yama_len
*)
let syanten_4_write tumo_len yama_len = 
  Printf.printf "[|[|[|[|"; 
  let rec loop a b c d = 
    flush stdout;
    if a = 1 then 
      if b = 1 then 
        if c = 1 then
          if d = 1 then 
            let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
            Printf.printf "%F|]|]|]|]" n; 
            ()
          else
            (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
            Printf.printf "%F;" n; 
            loop a b c (d-1))
        else
          if d = 1 then 
            (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
              Printf.printf "%F|];[|" n; 
             loop a b (c-1) 4)
          else
            (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
            Printf.printf "%F;" n; 
            loop a b c (d-1))
      else
        if c = 1 then
          if d = 1 then 
            (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
            Printf.printf "%F|]|];[|[|" n; 
            loop a (b-1) 4 4)
          else
            (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
            Printf.printf "%F;" n; 
            loop a b c (d-1))
        else
          if d = 1 then 
            (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
             Printf.printf "%F|];[|" n; 
             loop a b (c-1) 4)
          else
            (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
            Printf.printf "%F;" n; 
            loop a b c (d-1))
      else
        if b = 1 then 
          if c = 1 then
            if d = 1 then 
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
               Printf.printf "%F|]|]|];[|[|[|" n; 
               loop (a-1) 4 4 4)
            else
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
              Printf.printf "%F;" n; 
              loop a b c (d-1))
          else
            if d = 1 then 
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
               Printf.printf "%F|];[|" n; 
               loop a b (c-1) 4)
            else
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
              Printf.printf "%f;" n; 
              loop a b c (d-1))
        else
          if c = 1 then
            if d = 1 then 
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
              Printf.printf "%F|]|];[|[|" n; 
              loop a (b-1) 4 4)
            else
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
              Printf.printf "%F;" n; 
              loop a b c (d-1))
          else
            if d = 1 then 
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
               Printf.printf "%F|];[|" n; 
               loop a b (c-1) 4)
            else
              (let n = tenpai_ritu_4 yama_len tumo_len a b c d in 
              Printf.printf "%F;" n; 
              loop a b c (d-1))
  in
  loop 4 4 4 4

let syanten4_write_a () = 
  let yama_len = 69 in 
  Printf.printf "[|"; flush stdout;
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 4 then 
        ()
      else
        (syanten_4_write tumo_len yama_len;
        Printf.printf ";")
    in
    if yama_len = 4 then 
      Printf.printf "|]"
    else
      if yama_len mod 5 = 0 then 
        (Printf.printf "\n";
        loop (yama_len - 1))
      else
        loop (yama_len - 1)
  in
  loop yama_len


let syanten_3_write tumo_len yama_len = 
  Printf.printf "[|[|[|";
  let rec loop b c d= 
    if b = 1 then 
      if c = 1 then
        if d = 1 then 
          let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F|]|]|]"n;
        else
          (let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F;"n;
          loop b c (d-1))
      else
        if d = 1 then 
          (let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F|];[|"n;
            loop b (c-1) 4)
        else
          (let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F;"n;
          loop b c (d-1))
    else
      if c = 1 then
        if d = 1 then 
          (let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F|]|];[|[|"n;
          loop (b-1) 4 4)
        else
          (let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F;"n;
          loop b c (d-1))
      else
        if d = 1 then 
          (let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F|];[|"n;
          loop b (c-1) 4)
        else
          (let n = tenpai_ritu_3 yama_len tumo_len b c d in 
          Printf.printf "%F;"n;
          loop b c (d-1))
      
  in
  loop 4 4 4

let syanten3_write_a () = 
  let yama_len = 69 in 
  Printf.printf "[|"; flush stdout;
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 3 then 
        ()
      else
        (syanten_3_write tumo_len yama_len;
        Printf.printf ";")
    in
    if yama_len = 4 then 
      Printf.printf "|]"
    else
      if yama_len mod 5 = 0 then 
        (Printf.printf "\n";
        loop (yama_len - 1))
      else
        loop (yama_len - 1)
      
  in
  loop yama_len



let syanten_2_write tumo_len yama_len = 
  Printf.printf "[|[|";
  let rec loop c d= 
    if c = 1 then
      if d = 1 then 
        let n = tenpai_ritu_2 yama_len tumo_len c d in 
        Printf.printf "%F|]|]" n;
      else
        (let n = tenpai_ritu_2 yama_len tumo_len c d in 
        Printf.printf "%F;" n;
        loop c (d-1))
    else
      if d = 1 then 
        (let n = tenpai_ritu_2 yama_len tumo_len c d in 
        Printf.printf "%F|];[|" n;
          loop (c-1) 4)
      else
        (let n = tenpai_ritu_2 yama_len tumo_len c d in 
        Printf.printf "%F;" n;
        loop c (d-1))
      
  in
  loop 4 4


let syanten2_write_a () = 
  let yama_len = 69 in 
  Printf.printf "[|"; flush stdout;
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 2 then 
        ()
      else
        (syanten_2_write tumo_len yama_len;
        Printf.printf ";")
    in
    if yama_len = 4 then 
      Printf.printf "|]"
    else

      loop (yama_len - 1)
  in
  loop yama_len

(*
let syanten_1_write tumo_len yama_len = 
  Printf.printf "[|";
  let rec loop d= 
    if d = 1 then 
      (Printf.printf "|]";)
    else
      (let n = tenpai_ritu_1 yama_len tumo_len d in
      Printf.printf "%F;" n;
      loop (d-1))     
  in
  loop 4 
*)
(*
let syanten1_write_a () = 
  let yama_len = 69 in 
  Printf.printf "[|"; flush stdout;
  let rec loop yama_len = 
    let tumo_len = yama_len / 4 in
    let _ = 
      if tumo_len < 1 then 
        ()
      else
        (syanten_1_write tumo_len yama_len;
        Printf.printf ";";)
    in
    if yama_len = 4 then 
      Printf.printf "|]"
    else
      loop (yama_len - 1)
  in
  loop yama_len
*)

let _ =  tenpai_ritu_1 16 4 [1] [[((2,1),4)]]