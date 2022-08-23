open Mahjong_base
open Loop
open Mahjong_safty

let lst_to_ary_type lst = 
  let ary = Array.make 9 0 in
  let rec loop lst = match lst with 
    | [] -> ()
    | (x,_)::t -> let n = ary.(x-1) in
                  ary.(x-1) <- n + 1;
                  loop t
  in
  let _ = loop lst in 
  ary

let lst_to_ary_type_zi lst = 
  let ary = Array.make 7 0 in
  let rec loop lst = match lst with 
    | [] -> ()
    | (x,y)::t -> let (x,y) = hai_to_ary (x,y) in
                  let n = ary.(y) in
                  ary.(y) <- n + 1;
                  loop t
  in
  let _ = loop lst in 
  ary

let yukouhai (x,y) = match y with
  | Manzu -> if x = 1 then
              [(1,Manzu);(2,Manzu);(3,Manzu)]
             else if x = 2 then
              [(1,Manzu);(2,Manzu);(3,Manzu);(4,Manzu)]
            else if x = 3 then
              [(1,Manzu);(2,Manzu);(3,Manzu);(4,Manzu);(5,Manzu)]
            else if x = 4 then
              [(2,Manzu);(3,Manzu);(4,Manzu);(5,Manzu);(6,Manzu)]
            else if x = 5 then
              [(3,Manzu);(4,Manzu);(5,Manzu);(6,Manzu);(7,Manzu)]
            else if x = 6 then
              [(4,Manzu);(5,Manzu);(6,Manzu);(7,Manzu);(8,Manzu)]
            else if x = 7 then
              [(5,Manzu);(6,Manzu);(7,Manzu);(8,Manzu);(9,Manzu)]
            else if x = 8 then
              [(6,Manzu);(7,Manzu);(8,Manzu);(9,Manzu)]
            else
              [(7,Manzu);(8,Manzu);(9,Manzu)]
  | Pinzu -> if x = 1 then
              [(1,Pinzu);(2,Pinzu);(3,Pinzu)]
            else if x = 2 then
              [(1,Pinzu);(2,Pinzu);(3,Pinzu);(4,Pinzu)]
            else if x = 3 then
              [(1,Pinzu);(2,Pinzu);(3,Pinzu);(4,Pinzu);(5,Pinzu)]
            else if x = 4 then
              [(2,Pinzu);(3,Pinzu);(4,Pinzu);(5,Pinzu);(6,Pinzu)]
            else if x = 5 then
              [(3,Pinzu);(4,Pinzu);(5,Pinzu);(6,Pinzu);(7,Pinzu)]
            else if x = 6 then
              [(4,Pinzu);(5,Pinzu);(6,Pinzu);(7,Pinzu);(8,Pinzu)]
            else if x = 7 then
              [(5,Pinzu);(6,Pinzu);(7,Pinzu);(8,Pinzu);(9,Pinzu)]
            else if x = 8 then
              [(6,Pinzu);(7,Pinzu);(8,Pinzu);(9,Pinzu)]
            else
              [(7,Pinzu);(8,Pinzu);(9,Pinzu)]
  | Souzu -> if x = 1 then
              [(1,Souzu);(2,Souzu);(3,Souzu)]
            else if x = 2 then
              [(1,Souzu);(2,Souzu);(3,Souzu);(4,Souzu)]
            else if x = 3 then
              [(1,Souzu);(2,Souzu);(3,Souzu);(4,Souzu);(5,Souzu)]
            else if x = 4 then
              [(2,Souzu);(3,Souzu);(4,Souzu);(5,Souzu);(6,Souzu)]
            else if x = 5 then
              [(3,Souzu);(4,Souzu);(5,Souzu);(6,Souzu);(7,Souzu)]
            else if x = 6 then
              [(4,Souzu);(5,Souzu);(6,Souzu);(7,Souzu);(8,Souzu)]
            else if x = 7 then
              [(5,Souzu);(6,Souzu);(7,Souzu);(8,Souzu);(9,Souzu)]
            else if x = 8 then
              [(6,Souzu);(7,Souzu);(8,Souzu);(9,Souzu)]
            else
              [(7,Souzu);(8,Souzu);(9,Souzu)]
  | _ -> [(0,y)]

let kokushi_syanten lst = 
  let ary = Array.make 14 0 in 
  let rec loop lst = match lst with 
    | [] -> ()
    | (1,Manzu)::t -> if ary.(0) = 0 then ary.(0) <- 1 else ary.(13) <- 1; loop t 
    | (9,Manzu)::t -> if ary.(1) = 0 then ary.(1) <- 1 else ary.(13) <- 1; loop t 
    | (1,Pinzu)::t -> if ary.(2) = 0 then ary.(2) <- 1 else ary.(13) <- 1; loop t 
    | (9,Pinzu)::t -> if ary.(3) = 0 then ary.(3) <- 1 else ary.(13) <- 1; loop t 
    | (1,Souzu)::t -> if ary.(4) = 0 then ary.(4) <- 1 else ary.(13) <- 1; loop t 
    | (9,Souzu)::t -> if ary.(5) = 0 then ary.(5) <- 1 else ary.(13) <- 1; loop t 
    | (0,Ton)::t -> if ary.(6) = 0 then ary.(6) <- 1 else ary.(13) <- 1; loop t 
    | (0,Nan)::t -> if ary.(7) = 0 then ary.(7) <- 1 else ary.(13) <- 1; loop t 
    | (0,Sya)::t -> if ary.(8) = 0 then ary.(8) <- 1 else ary.(13) <- 1; loop t 
    | (0,Pei)::t -> if ary.(9) = 0 then ary.(9) <- 1 else ary.(13) <- 1; loop t 
    | (0,Haku)::t -> if ary.(10) = 0 then ary.(10) <- 1 else ary.(13) <- 1; loop t 
    | (0,Hatsu)::t -> if ary.(11) = 0 then ary.(11) <- 1 else ary.(13) <- 1; loop t 
    | (0,Tyun)::t -> if ary.(12) = 0 then ary.(12) <- 1 else ary.(13) <- 1; loop t 
    | (_,_)::t -> loop t 
  in
  let _ = loop lst in
  let n = Array.fold_left (fun a b -> a + b) 0 ary in 
  13 - n 
      

(*let kokushi_syanten lst = 
  let a1 = if (List.exists (fun x -> x = (1,Manzu)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (1,Manzu)) lst in
  let a2 = if (List.exists (fun x -> x = (9,Manzu)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (9,Manzu)) tmp in
  let a3 = if (List.exists (fun x -> x = (1,Pinzu)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (1,Pinzu)) tmp in
  let a4 = if (List.exists (fun x -> x = (9,Pinzu)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (9,Pinzu)) tmp in
  let a5 = if (List.exists (fun x -> x = (1,Souzu)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (1,Souzu)) tmp in
  let a6 = if (List.exists (fun x -> x = (9,Souzu)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (9,Souzu)) tmp in
  let a7 = if (List.exists (fun x -> x = (0,Ton)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (0,Ton)) tmp in
  let a8 = if (List.exists (fun x -> x = (0,Nan)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (0,Nan)) tmp in
  let a9 = if (List.exists (fun x -> x = (0,Sya)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (0,Sya)) tmp in
  let a10 = if (List.exists (fun x -> x = (0,Pei)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (0,Pei)) tmp in
  let a11 = if (List.exists (fun x -> x = (0,Haku)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (0,Haku)) tmp in
  let a12 = if (List.exists (fun x -> x = (0,Hatsu)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (0,Hatsu)) tmp in
  let a13 = if (List.exists (fun x -> x = (0,Tyun)) lst) = true then 1 else 0 in
  let tmp = List.filter (fun x -> x <> (0,Tyun)) tmp in
  let b = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13 in
  let n = List.length tmp in
  if n = b then
    13 - b
  else
    12 - b
*)

let titoi_syanten (lst:(int*hai)list) = 
  let rec loop' (lst:(int*hai)list) tmp i = 
    let x = List.hd lst in 
    let tmp = 
      if List.exists (fun a -> a = x) (List.tl lst) then
        tmp + 1
      else
        tmp
    in
    let lst = List.filter (fun a -> a <> x) lst in
    if lst = [] then
      (tmp,i)
    else
      loop' lst tmp (i+1) 
  in
  let (m,n) = loop' lst 0 1 in
  if n < 7 then
    6 - m + (7-n)
  else
    6 - m

(*man,pin,sou,zi*)
let sprit_lst lst = 
  let rec loop m_lst p_lst s_lst zi_lst lst = match lst with
    | [] -> (m_lst,p_lst,s_lst,zi_lst)
    | (x,y)::t -> let m_lst = 
                    if y = Manzu then
                      (x,y)::m_lst
                    else
                      m_lst
                  in
                  let p_lst = 
                    if y = Pinzu then
                      (x,y)::p_lst
                    else
                      p_lst
                  in
                  let s_lst = 
                    if y = Souzu then
                      (x,y)::s_lst
                    else
                      s_lst
                  in
                  let zi_lst = 
                    if x = 0 then
                      (x,y)::zi_lst
                    else
                      zi_lst
                  in
                  loop m_lst p_lst s_lst zi_lst t
  in
  loop [] [] [] [] lst

let opt_kouho lst f_lst_len = 
  let m = List.length lst in 
  let rec loop' i tmp = 
    let (x,y) = List.nth lst i in
    let (x,y) = 
      if x + y > (4-f_lst_len) then
        let y = if x + (y-1) > (4-f_lst_len) then y-2 else y-1 in
        (x,y) 
      else
        (x,y)
    in
    let n = x*2 + y in
    let (x',y') = tmp in
    let n' = x'*2 + y' in
    let tmp =
      if n > n' then
        (x,y)
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 then
    (0,0)
  else
    loop' (m-1) (0,0)



let reverse_mentsu ary = 
  let rec loop i mentsu =
    let mentsu' = 
      if ary.(i) > 0 && ary.(i-1) > 0 && ary.(i-2) > 0 then
        (let n1 = ary.(i) in
        let n2 = ary.(i-1) in
        let n3 = ary.(i-2) in
        ary.(i) <- n1 - 1;
        ary.(i-1) <- n2 - 1;
        ary.(i-2) <- n3 - 1;
        mentsu+1
        )
      else
        mentsu
    in
    if mentsu = mentsu' then
      if i = 2 then
        (ary,mentsu)
      else
        loop (i-1) mentsu' 
    else
      loop i mentsu'
  in
  loop 8 0

let forward_mentsu ary = 
  let rec loop' i mentsu = 
    let mentsu' = 
      if ary.(i) > 0 && ary.(i+1) > 0 && ary.(i+2) > 0 then
        (let n1 = ary.(i) in
        let n2 = ary.(i+1) in
        let n3 = ary.(i+2) in
        ary.(i) <- n1 - 1;
        ary.(i+1) <- n2 - 1;
        ary.(i+2) <- n3 - 1;
        mentsu+1
        )
      else
        mentsu
    in
    if mentsu = mentsu' then
      if i = 6 then
        (ary,mentsu')
      else
        loop' (i+1) mentsu'
    else
      loop' i mentsu' 
  in
  loop' 0 0

let midium_mentsu ary = 
  let rec loop' i mentsu = 
    let mentsu' = 
      if ary.(i) > 0 && ary.(i+1) > 0 && ary.(i+2) > 0 then
        (let n1 = ary.(i) in
        let n2 = ary.(i+1) in
        let n3 = ary.(i+2) in
        ary.(i) <- n1 - 1;
        ary.(i+1) <- n2 - 1;
        ary.(i+2) <- n3 - 1;
        mentsu+1
        )
      else
        mentsu
    in
    if mentsu = mentsu' then
      if i = 3 then
        mentsu'
      else
        loop' (i+1) mentsu'
    else
      loop' i mentsu' 
  in
  let rec loop i mentsu =
    let mentsu' = 
      if ary.(i) > 0 && ary.(i-1) > 0 && ary.(i-2) > 0 then
        (let n1 = ary.(i) in
        let n2 = ary.(i-1) in
        let n3 = ary.(i-2) in
        ary.(i) <- n1 - 1;
        ary.(i-1) <- n2 - 1;
        ary.(i-2) <- n3 - 1;
        mentsu+1
        )
      else
        mentsu
    in
    if mentsu = mentsu' then
      if i = 5 then
        (ary,mentsu)
      else
        loop (i-1) mentsu' 
    else
      loop i mentsu'
  in
  let n = loop' 0 0 in
  loop 8 n


let mentsu_kouho_syuntsu ary f_lst_len = 
  let aryf = Array.copy ary in
  let aryr = Array.copy ary in
  let arym = Array.copy ary in
  let (aryf,f) = forward_mentsu aryf in
  let (aryr,r) = reverse_mentsu aryr in
  let (arym,m) = midium_mentsu arym in
  let rec loop2' ary i kouho = 
    let kouho' = 
      if ary.(i) > 0 then 
        if ary.(i) = 2 then
          let n = ary.(i) in
          ary.(i) <- n - 2;
          kouho + 1
        else
          if i < 7  then
            if ary.(i+1) > 0 then
              let n1 = ary.(i) in
              let n2 = ary.(i+1) in
              ary.(i) <- n1 - 1;
              ary.(i+1) <- n2 - 1;
              kouho+1
            else if ary.(i+2) > 0 then
              let n1 = ary.(i) in
              let n2 = ary.(i+2) in
              ary.(i) <- n1 - 1;
              ary.(i+2) <- n2 - 1;
              kouho+1
            else
              kouho
          else if i < 8 then
            if ary.(i+1) > 0 then
              let n1 = ary.(i) in
              let n2 = ary.(i+1) in
              ary.(i) <- n1 - 1;
              ary.(i+1) <- n2 - 1;
              kouho+1
            else
              kouho
          else
              kouho
      else
        kouho
    in
    if kouho = kouho' then
      if i = 8 then
        (ary,kouho')
      else
        loop2' ary (i+1) kouho'
    else
      loop2' ary i kouho'
    in
  let (aryf,fk) = loop2' aryf 0 0 in
  let (aryk,rk) = loop2' aryr 0 0 in
  let (arym,mk) = loop2' arym 0 0 in
  let n = opt_kouho [(f,fk);(r,rk);(m,mk)] f_lst_len in
  if n = (f,fk) then
    (aryf,n)
  else if n = (r,rk) then
    (aryk,n)
  else
    (arym,n)




let serch_koritsu (lst:(int array*(int*int))list) m = 
  let fil_lst = List.filter (fun (_,(a,b)) -> if (a,b) = m then true else false) lst in 
  let rec loop i tmp t_lst = match t_lst with 
    | [] -> tmp
    | (ary,_)::t -> loop i (tmp + ary.(i)) t
  in
  let rec loop2 i tmp = match i with 
  | 0 -> tmp
  | _ -> let x = loop i 0 fil_lst in 
          loop2 (i-1) (x::tmp)
  in
  if fil_lst = [] then 
    []
  else
    loop2 8 []





let mentsu_kouho_kotsu ary f_lst_len = 
  let rec loop' ary i mentsu = 
    let mentsu = 
      if ary.(i) >= 3 then
        let ary2 = Array.copy ary in
        let n = ary2.(i) in
        ary2.(i) <- n - 3;
        let tmp = loop' ary2 (i) [] in
        let tmp = List.map (fun (z,(x,y)) -> (z,(x+1,y))) tmp in  
        tmp@mentsu
      else
        mentsu
    in
    if i = 8 then
      (mentsu_kouho_syuntsu ary f_lst_len)::mentsu
    else
      loop' ary (i+1) mentsu
    in
  let n = loop' ary 0 [] in 
  let lst = List.map (fun (_,(b,c)) -> (b,c)) n in
  let m = opt_kouho lst f_lst_len in 
  let x = serch_koritsu n m in
  (x,m) 
  

let mentsu_kouho_zi ary = 
  let rec loop' i (x,y) = 
    let (x,y) = 
      if ary.(i) >= 3 then
        let n = ary.(i) in
        ary.(i) <-  n-3;
        (x+1,y)
      else if ary.(i) = 2 then
        (ary.(i) <- 0;
        (x,y+1))
      else
        (x,y)
    in
    if i = 0 then
      (x,y)
    else
      loop' (i-1) (x,y)
  in
  let (x,y) = loop' 6 (0,0) in
  let lst = Array.to_list ary in
  (lst,(x,y))

let koritsu_zi lst = 
  let n = List.length lst in
  let rec loop i tmp = 
    let tmp = 
      if List.nth lst i <> 0 then
        ((ary_to_hai (3,i)),[ary_to_hai (3,i)])::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if n = 0 then
    []
  else
    loop (n-1) []


let koritsu_suhai lst x = 
  let n = List.length lst in
  let rec loop i tmp = 
    let tmp = 
      if List.nth lst i <> 0 then
        ((i+1,x),(yukouhai ((i+1),x)))::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if n = 0 then
    []
  else
    loop (n-1) []



let koritsu m p s zi = 
  let m = koritsu_suhai m Manzu in
  let p = koritsu_suhai p Pinzu in
  let s = koritsu_suhai s Souzu in
  let zi = koritsu_zi zi in
  m@p@s@zi



(*
let compile_yukouhai lst = 
  let m = List.length lst in
  let rec loop i tmp = 
    let (_,a) = List.nth lst i in
    let a = List.filter (fun b -> (List.exists (fun c -> c = b) tmp) = false) a in
    let tmp = a@tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop (m-1) []
 
let loss_hai b_lst a_lst ary zi_ary = 
  let lst = List.filter (fun b -> (List.exists (fun c -> c = b) a_lst) = false) b_lst in
  let m = List.length lst in
  let rec loop i tmp = 
    let n = List.nth lst i in
    let (x,y) = hai_to_ary n in
    let a = 
      if x = 3 then
        zi_ary.(y)
      else
        ary.(x).(y)
    in
    let tmp = tmp + a in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    0
  else
    loop (m-1) 0
*)

(*  
let opt_yukouhai lst ary zi_ary= 
  let m = List.length lst in
  let base_lst = compile_yukouhai lst in
  let rec loop i tmp =
    let (a,b) = List.nth lst i in
    let new_lst = d_tehai lst (a,b) in
    let new_lst = compile_yukouhai new_lst in
    let n = loss_hai base_lst new_lst ary zi_ary in
    let (a',_) = tmp in
    let tmp = 
      if a' > n then
        (n,a)
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if  m = 0 then
    (-1,(1,Not_hai))
  else
    loop (m-1) (30,(1,Not_hai))
*)
(*mentsu,mentsukouho*)
let mentsu_kouho m_lst p_lst s_lst zi_lst f_lst_len = 
  let m_ary = lst_to_ary_type m_lst in
  let p_ary = lst_to_ary_type p_lst in
  let s_ary = lst_to_ary_type s_lst in
  let zi_ary = lst_to_ary_type_zi zi_lst in
  let (m,(xm,ym)) = mentsu_kouho_kotsu m_ary f_lst_len in
  let (p,(xp,yp)) = mentsu_kouho_kotsu p_ary f_lst_len in
  let (s,(xs,ys)) = mentsu_kouho_kotsu s_ary f_lst_len in
  let (zi,(xzi,yzi)) = mentsu_kouho_zi zi_ary in
  let n = (xm+xp+xs+xzi,ym+yp+ys+yzi) in
  let (x,y) = opt_kouho [n] f_lst_len in
  let lst = koritsu m p s zi in 
  (lst,8-((x+f_lst_len)*2+y))



let opt_syanten lst = 
  let m = List.length lst in
  let rec loop' i tmp = 
    let n = List.nth lst i in 
    let tmp = 
      if tmp > n then
        n
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 then
    8
  else
    loop' (m-1) 8


let common_syanten lst = 
  let f_lst_len = (14 - (List.length lst))/3 in
  let (ary,zi_ary) = list_to_ary lst in
  let h_lst = (possible_head ary)@(find_head_zi zi_ary) in
  let m = List.length h_lst in
  let (m_lst,p_lst,s_lst,zi_lst) = sprit_lst lst in
  let rec loop' i tmp = 
    let (x,y) = List.nth h_lst i in
    let (x,y) = ary_to_hai (x,y) in
    let tmp = 
      if y = Manzu then
        let lst = d_tehai (d_tehai m_lst (x,y)) (x,y) in
        let (a,b) = mentsu_kouho lst p_lst s_lst zi_lst f_lst_len in
        (a,b-1)::tmp
      else if y = Pinzu then
        let lst = d_tehai (d_tehai p_lst (x,y)) (x,y) in
        let (a,b) = mentsu_kouho m_lst lst s_lst zi_lst f_lst_len in
        (a,b-1)::tmp
      else if y = Souzu then
        let lst = d_tehai (d_tehai s_lst (x,y)) (x,y) in
        let (a,b) = mentsu_kouho m_lst p_lst lst zi_lst f_lst_len in
        (a,b-1)::tmp
      else 
        let lst = d_tehai (d_tehai zi_lst (x,y)) (x,y) in
        let (a,b) = mentsu_kouho m_lst p_lst s_lst lst f_lst_len in
        (a,b-1)::tmp
      in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
    in
  if m = 0 then
    mentsu_kouho m_lst p_lst s_lst zi_lst f_lst_len
  else
    let sy_lst = loop' (m-1) [] in
    let n = mentsu_kouho m_lst p_lst s_lst zi_lst f_lst_len in
    let sy_lst = n::sy_lst in
    let sy_lst' = List.map (fun (_,b) -> b) sy_lst in
    let n = opt_syanten sy_lst' in
    let lst = List.filter (fun (a,b) -> b = n) sy_lst in
    let lst = List.map (fun (a,b) -> a) lst in
    let lst = List.fold_left (fun a b -> a@b) [] lst in
    (lst,n)

    
let syanten lst = 
  let n1 = kokushi_syanten lst in
  let n2 = titoi_syanten lst in
  let (lst',n3) = common_syanten lst in
  let n = [n1;n2;n3] in
  (lst',opt_syanten n)


let create_table sutehai_lst tehai = 
  let ary = Array.make_matrix 3 9 4 in
  let zi_ary = Array.make 7 4 in
  let rec loop2 t_lst = match t_lst with 
    | [] -> ()
    | (x,y,_)::t -> let (x,y) = hai_to_ary (x,y) in 
                 let _ = if x = 3 then 
                            let n = zi_ary.(y) in 
                            zi_ary.(y) <- n-1
                         else
                            let n = ary.(x).(y) in
                            ary.(x).(y) <- n - 1
                  in
                  loop2 t
  in
  let rec loop3 t_lst = match t_lst with 
    | [] -> ()
    | (x,y)::t -> let (x,y) = hai_to_ary (x,y) in 
                 let _ = if x = 3 then 
                            let n = zi_ary.(y) in 
                            zi_ary.(y) <- n-1
                         else
                            let n = ary.(x).(y) in
                            ary.(x).(y) <- n - 1
                  in
                  loop3 t
  in
  let rec loop i = 
    let _ = loop2 (List.nth sutehai_lst i) in
    if i = 0 then
      ()
    else
      loop (i-1)
  in
  let _ = loop 3 in
  let _ = loop3 tehai in
  (ary,zi_ary)


(*
let hai_eff_select sutehai_lst tehai furo_lst yaku_lst player furo_double_lst = 
  let yaku = List.nth yaku_lst player in
  let reach_q = List.exists (fun a -> List.exists (fun b -> b = Reach || b = Doublereach) a) yaku_lst in
  let (lst,n) = syanten tehai in
  let (_,n') = common_syanten tehai in
  let (ary,zi_ary) = create_table sutehai_lst tehai in
  let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in
  let (d,trush) = opt_yukouhai lst ary zi_ary in
  let rec loop i k_hai = 
    let a = List.nth tehai i in
    let tmp = d_tehai tehai a in
    if k_hai = (1,Not_hai) then
      let (_,m) = syanten tmp in
      if m = n then
        i
      else
        if i = 0 then
          0
        else
          loop (i-1) k_hai
    else
      if k_hai = a then
        i
      else if i = 0 then
        0
      else
        loop (i-1) k_hai

  in
  if List.exists (fun a -> a = Reach || a = Doublereach) yaku = true then
    tumogiri tehai
  else if reach_q = true then
    reach_defence ary zi_ary yaku_lst sutehai_lst tehai 
  else if n = n' && d <> -1 && trush <> (1,Not_hai) then
    loop ((List.length tehai) - 1) trush
  else
    loop ((List.length tehai) - 1) (1,Not_hai)
*)

(*
let _ = 
  (*let (_,n)  = common_syanten [(1,Manzu);(9,Manzu);(1,Pinzu);(4,Pinzu);(7,Pinzu);(5,Souzu);(8,Souzu);(9,Souzu);(9,Souzu);(9,Souzu);(9,Souzu);(0,Sya);(0,Sya)] in
*)
  let (x,(y,z)) = mentsu_kouho_syuntsu [|2;2;2;0;0;0;2;2;0|] 0 in
  Printf.printf "%d %d\n" y z;*)