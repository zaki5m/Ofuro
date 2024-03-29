open Mahjong_base
open Loop
open Mahjong_safty
open Effect
open Effect.Deep
open Domainslib

let syanten_hash = Hashtbl.create 12345
(*let tenpai_hash = Hashtbl.create 12345
let open_hash = Hashtbl.create 123456
let draw_hash = Hashtbl.create 123456*)

module C = Domainslib.Chan

module T = Domainslib.Task

type 'a message = Task of 'a | Quit

let lk1 = Mutex.create ()

let lk2 = Mutex.create ()

let lk = Mutex.create ()

let core_count = int_of_string Sys.argv.(1)

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
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (x,y)::t -> let (x,y) = 
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
                    loop tmp t
                in
  loop (0,0) lst 



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

let mentsu_kouho_syuntsu_2 ary = 
  let rec loop2' i kouho = 
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
        kouho'
      else
        loop2' (i+1) kouho'
    else
      loop2' i kouho'
    in
    let kouho = loop2' 0 0 in 
    (ary,kouho)


let mentsu_kouho_syuntsu ary f_lst_len = 
  let aryf = Array.copy ary in
  let aryr = Array.copy ary in
  let arym = Array.copy ary in
  let (aryf,f) = forward_mentsu aryf in
  let (aryr,r) = reverse_mentsu aryr in
  let (arym,m) = midium_mentsu arym in
  let (aryf,fk) = mentsu_kouho_syuntsu_2 aryf in
  let (aryk,rk) = mentsu_kouho_syuntsu_2 aryr in
  let (arym,mk) = mentsu_kouho_syuntsu_2 arym in
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
  let rec loop_mk ary i mentsu = 
    let mentsu = 
      if ary.(i) >= 3 then
        let ary2 = Array.copy ary in
        let n = ary2.(i) in
        ary2.(i) <- n - 3;
        let tmp = loop_mk ary2 (i) [] in
        let tmp = List.map (fun (z,(x,y)) -> (z,(x+1,y))) tmp in  
        tmp@mentsu
      else
        mentsu
    in
    if i = 8 then
      (mentsu_kouho_syuntsu ary f_lst_len)::mentsu
    else
      loop_mk ary (i+1) mentsu
    in
  let n = loop_mk ary 0 [] in 
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
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | h::t -> let tmp = 
                if tmp > h then
                  h
                else
                  tmp
              in
                loop tmp t
  in
  loop 8 lst

let make_h_lst ary zi_ary =
  let rec loop i j lst =
    let lst =
      if ary.(i).(j) >= 2 then
        (i,j)::lst
      else
        lst
    in

    if i = 2 then
      if j = 8 then
        lst
      else
        loop i (j+1) lst
    else
      if j = 8 then
        loop (i+1) 0 lst
      else
        loop i (j+1) lst
      
    in
    let lst = find_head_zi zi_ary in 
    loop 0 0 lst 


let common_syanten lst = 
  let f_lst_len = (14 - (List.length lst))/3 in
  let (ary,zi_ary) = list_to_ary lst in
  let h_lst = make_h_lst ary zi_ary in
  let m = List.length h_lst in
  let (m_lst,p_lst,s_lst,zi_lst) = sprit_lst lst in
  let rec loop' tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (x,y)::t -> let (x,y) = ary_to_hai (x,y) in
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
                  loop' tmp t
                  in
  if m = 0 then
    mentsu_kouho m_lst p_lst s_lst zi_lst f_lst_len
  else
    let sy_lst = loop' [] h_lst in
    let n = mentsu_kouho m_lst p_lst s_lst zi_lst f_lst_len in
    let sy_lst = n::sy_lst in
    let sy_lst' = List.map (fun (_,b) -> b) sy_lst in
    let n = opt_syanten sy_lst' in
    let lst = List.filter (fun (a,b) -> b = n) sy_lst in
    let lst = List.map (fun (a,b) -> a) lst in
    let lst = List.fold_left (fun a b -> a@b) [] lst in
    (lst,n)

    
let syanten lst = 
  let lst = rhai_to_hai lst in
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
  let rec loop i = match i with 
    | -1 -> ()
    | _ -> let _ = loop2 (tapl_player sutehai_lst i) in
           loop (i-1)
  in
  let _ = loop 3 in
  let _ = loop3 tehai in
  (ary,zi_ary)

let count_yukouhai ary zi_ary (x,y) = 
  if x = 3 then 
    zi_ary.(y)
  else
    let n1 = ary.(x).(y) in 
    if y = 0 then 
      let n2 = ary.(x).(y+1) in 
      let n3 = ary.(x).(y+2) in 
      n1+n2+n3
    else if y = 1 then 
      let n2 = ary.(x).(y+1) in 
      let n3 = ary.(x).(y+2) in 
      let n4 = ary.(x).(y-1) in 
      n1+n2+n3+n4
    else if y = 7 then 
      let n2 = ary.(x).(y-1) in 
      let n3 = ary.(x).(y-2) in 
      let n4 = ary.(x).(y+1) in 
      n1+n2+n3+n4
    else if y = 8 then 
      let n2 = ary.(x).(y-1) in 
      let n3 = ary.(x).(y-2) in 
      n1+n2+n3
    else
      let n2 = ary.(x).(y-1) in 
      let n3 = ary.(x).(y-2) in 
      let n4 = ary.(x).(y+1) in 
      let n5 = ary.(x).(y+2) in 
      n1+n2+n3+n4+n5


let tehai_to_yukouhai tehai = 
  let (_,n) = syanten tehai in 
  let rec loop i j tmp = 
    let (x,y) = ary_to_hai (i,j) in 
    let n_tehai = add_tehai tehai (x,y) in 
    let (_,n1) = syanten n_tehai in 
    let tmp = 
      if n - n1 = 1 then 
        (x,y)::tmp
      else
        tmp
    in
    if i = 3 then 
      if j = 6 then 
        tmp
      else
        loop i (j+1) tmp
    else
      if j = 8 then 
        loop (i+1) 0 tmp
      else
        loop i (j+1) tmp
  in
  loop 0 0 []

let convert_int_to_hai i = match i with 
  | 0 -> (1,Manzu)
  | 1 -> (2,Manzu)
  | 2 -> (3,Manzu)
  | 3 -> (4,Manzu)
  | 4 -> (5,Manzu)
  | 5 -> (6,Manzu)
  | 6 -> (7,Manzu)
  | 7 -> (8,Manzu)
  | 8 -> (9,Manzu)
  | 9 -> (1,Pinzu)
  | 10 -> (2,Pinzu)
  | 11 -> (3,Pinzu)
  | 12 -> (4,Pinzu)
  | 13 -> (5,Pinzu)
  | 14 -> (6,Pinzu)
  | 15 -> (7,Pinzu)
  | 16 -> (8,Pinzu)
  | 17 -> (9,Pinzu)
  | 18 -> (1,Souzu)
  | 19 -> (2,Souzu)
  | 20 -> (3,Souzu)
  | 21 -> (4,Souzu)
  | 22 -> (5,Souzu)
  | 23 -> (6,Souzu)
  | 24 -> (7,Souzu)
  | 25 -> (8,Souzu)
  | 26 -> (9,Souzu)
  | 27 -> (0,Ton)
  | 28 -> (0,Nan)
  | 29 -> (0,Sya)
  | 30 -> (0,Pei)
  | 31 -> (0,Haku)
  | 32 -> (0,Hatsu)
  | 33 -> (0,Tyun)
  | 34 -> (5,Manzu_red)
  | 35 -> (5,Pinzu_red)
  | 36 -> (5,Souzu_red)
  | _ -> (1,Not_hai)




let calc_max_tumo_len max_tumo_len lst1 lst2 = 
  let m = List.length lst1 in 
  let ary = Array.of_list lst1 in 
  let ary = Array.map (fun a -> Array.of_list a) ary in 
  let ary2 = Array.of_list lst2 in
  let rec loop2 len end_len tmp sum_kitaiti = 
    let sum = tmp*. ary2.(len-end_len) in 
    if end_len > 1 then 
      loop2 len (end_len-1) tmp (sum_kitaiti+.sum)
    else
      (sum_kitaiti+.sum)
  in
  let rec loop_cal len end_len i tmp sum sum_t_ritu sum_kitaiti = 
    let next = tmp *. ary.(i-1).(len-end_len) in
    let (t_ritu_tmp,tmp2,sum_k_tmp) = if i = 1 then (0.,next,0.) else loop_cal (end_len-1) (end_len-1) (i-1) next 0. 0. 0. in 
    let tmp3 = if i = 2 then loop2 (end_len-1) (end_len-1) next 0. else 0. in  
    let tmp4 = if i = 2 then next else 0. in  
    if end_len > i then 
      loop_cal len (end_len-1) i tmp (tmp2+.sum) (sum_t_ritu+.t_ritu_tmp+.tmp4) (sum_kitaiti+.sum_k_tmp+.tmp3)
    else
      ((sum_t_ritu+.t_ritu_tmp+.tmp4),(tmp2+.sum),(sum_kitaiti+.sum_k_tmp+.tmp3))   
  in
  if max_tumo_len < m then 
    (0.,0.,0.) 
  else if m = 1 then 
    let (_,b,_) = loop_cal max_tumo_len max_tumo_len m 1. 0. 0. 0. in 
    let c = loop2 max_tumo_len max_tumo_len 1. 0. in 
    (1.0,b,c)
  else
    let (a,b,c) = loop_cal max_tumo_len max_tumo_len m 1. 0. 0. 0. in 
    (a,b,c)
(*return (牌，残り枚数，向聴数の変化)*)
let make_draw_ary tehai syanten_count ary zi_ary =
  let rec loop tmp i = match i with
    | -1 -> tmp
    | _ -> let (x,y) = convert_int_to_hai i in 
          let (x1,y1) = hai_to_ary (x,y) in
          let m =  
            if x1 = 3 then
              zi_ary.(y1)
            else
              ary.(x1).(y1)
          in
          if m = 0 then 
            loop tmp (i-1)
          else
            let n_tehai = add_tehai tehai (x,y) in
            let (_,n) = common_syanten n_tehai in 
            loop (((x,y),m,(syanten_count - n))::tmp) (i-1)
  in
  loop [] 33

let make_discard_ary tehai syanten_count = 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | h::t -> let n_tehai = d_tehai tehai h in 
              let (_,n) = common_syanten n_tehai in 
              loop ((h,(n-syanten_count))::tmp) t 
  in
  loop [] tehai 
(*List.nth lst i は　16から0*)
let lst_create under top = 
  let rec loop t_lst tmp i = match i with 
    | 17 -> t_lst
    | _ -> let n_top = under - top - i in 
           let n_under =  under - i in 
           let tmp = tmp *. (float_of_int n_top /. float_of_int n_under) in 
           loop ((tmp/.float_of_int (n_under-1))::t_lst) tmp (i+1)
  in
  loop [] 1. 0
           

let rest_yama_hai ary zi_ary = 
  let a1 = Array.fold_left (fun a b -> a + b) 0 ary.(0) in 
  let a2 = Array.fold_left (fun a b -> a + b) 0 ary.(1) in 
  let a3 = Array.fold_left (fun a b -> a + b) 0 ary.(2) in 
  let a4 = Array.fold_left (fun a b -> a + b) 0 zi_ary in
  a1+a2+a3+a4 

let create_agari lst ary zi_ary = 
  let rec agari_kitaiti under sum kitaiti (lst1,lst2) t_lst = match t_lst with 
    | [] -> (((float_of_int sum /. under)::lst1),(kitaiti /. under)::lst2)
    | h::t -> agari_kitaiti under sum kitaiti (((h*.float_of_int sum)::lst1),((h*.kitaiti)::lst2)) t
  in
  let rec loop (tmp,x) t_lst = match t_lst with
    | [] -> (tmp,x)
    | ((a,b),(c,_))::t -> let n = 
                            if a = 3 then 
                              zi_ary.(b)
                            else
                              ary.(a).(b)
                          in
                          let tmp = (float_of_int n) *. (float_of_int c) +. tmp in
                          loop (tmp,x+n) t 
  in
  let (tmp,total) = loop (0.,0) lst in 
  let under = rest_yama_hai ary zi_ary in 
  let lst = lst_create under total in 
  let under = float_of_int under in 
  agari_kitaiti under total tmp ([],[]) lst


let rec t_ritu_create under sum lst1 t_lst = match t_lst with 
    | [] ->((float_of_int sum /. under)::lst1)
    | h::t -> t_ritu_create under sum ((h*.float_of_int sum)::lst1) t
  

let draw_with_tegawari n_extra_tumo syanten tehai ary zi_ary table_lst = 
  let flags = make_draw_ary tehai syanten ary zi_ary in 
  let under = rest_yama_hai ary zi_ary in 
  let top = List.fold_left (fun a (_,b,c) -> a+b) 0 flags in 
  let lst = lst_create under top in 
  let under = float_of_int under in 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp
    | ((a,b),count,syanten_diff)::t -> if count <> 0 then
                                        if syanten_diff = 1 then
                                          let n_tehai = add_tehai tehai (a,b) in 
                                            let ary2 = Array.map (fun x -> Array.copy x) ary in 
                                            let zi_ary2 = Array.copy zi_ary in 
                                            let (x1,y1) = hai_to_ary (a,b) in 
                                            let ary2 = if x1 = 3 then ary2 else let n = ary2.(x1).(y1) in ary2.(x1).(y1) <- n - 1; ary2 in
                                            let zi_ary2 = if x1 = 3 then let n = zi_ary2.(y1) in zi_ary2.(y1) <- n - 1; zi_ary2 else zi_ary2 in
                                            let hash_val = Hashtbl.hash (under,top,count) in
                                            let hash_return = Hashtbl.find_opt syanten_hash hash_val in
                                            let lst0 = if hash_return = None then t_ritu_create under count [] lst else Option.get hash_return in
                                            let _ = if hash_return = None then 
                                                        (Mutex.lock lk; 
                                                        Hashtbl.add syanten_hash hash_val lst0; 
                                                        Mutex.unlock lk)
                                                    else 
                                                      () 
                                            in 
                                            let tmp = (n_extra_tumo,(syanten - 1),n_tehai,(ary2,zi_ary2),(lst0::table_lst,[]))::tmp in 
                                            loop tmp t
                                        else if n_extra_tumo = 0 then 
                                          let n_tehai = add_tehai tehai (a,b) in 
                                          let ary2 = Array.map (fun x -> Array.copy x) ary in 
                                          let zi_ary2 = Array.copy zi_ary in 
                                          let (x1,y1) = hai_to_ary (a,b) in 
                                          let ary2 = if x1 = 3 then ary2 else let n = ary2.(x1).(y1) in ary2.(x1).(y1) <- n - 1; ary2 in
                                          let zi_ary2 = if x1 = 3 then let n = zi_ary2.(y1) in zi_ary2.(y1) <- n - 1; zi_ary2 else zi_ary2 in
                                          let hash_val = Hashtbl.hash (under,top,count) in
                                          let hash_return = Hashtbl.find_opt syanten_hash hash_val in
                                          let lst0 = if hash_return = None then t_ritu_create under count [] lst else Option.get hash_return in
                                          let _ = if hash_return = None then 
                                                      (Mutex.lock lk; 
                                                      Hashtbl.add syanten_hash hash_val lst0; 
                                                      Mutex.unlock lk)
                                                  else 
                                                    () 
                                          in 
                                          let tmp = (n_extra_tumo+1,syanten,n_tehai,(ary2,zi_ary2),(lst0::table_lst,[]))::tmp in
                                          loop tmp t 
                                        else
                                          loop tmp t
                                      else
                                        loop tmp t
  in
  loop [] flags


let draw_without_tegawari n_extra_tumo syanten tehai ary zi_ary table_lst = 
  let flags = make_draw_ary tehai syanten ary zi_ary in 
  let under = rest_yama_hai ary zi_ary in 
  let top = List.fold_left (fun a (_,b,c) -> if c = 1 then a + b else a) 0 flags in
  let lst = lst_create under top in 
  let under = float_of_int under in 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp
    | ((a,b),count,syanten_diff)::t -> if count <> 0 then 
                                        if syanten_diff = 1 then
                                          let n_tehai = add_tehai tehai (a,b) in 
                                            let ary2 = Array.map (fun x -> Array.copy x) ary in 
                                            let zi_ary2 = Array.copy zi_ary in 
                                            let (x1,y1) = hai_to_ary (a,b) in 
                                            let ary2 = if x1 = 3 then ary2 else let n = ary2.(x1).(y1) in ary2.(x1).(y1) <- n - 1; ary2 in
                                            let zi_ary2 = if x1 = 3 then let n = zi_ary2.(y1) in zi_ary2.(y1) <- n - 1; zi_ary2 else zi_ary2 in
                                            let hash_val = Hashtbl.hash (under,top,count) in
                                            let hash_return = Hashtbl.find_opt syanten_hash hash_val in
                                            let lst0 = if hash_return = None then t_ritu_create under count [] lst else Option.get hash_return in
                                            let _ = if hash_return = None then 
                                                        (Mutex.lock lk; 
                                                        Hashtbl.add syanten_hash hash_val lst0; 
                                                        Mutex.unlock lk)
                                                    else 
                                                      () 
                                            in 
                                            let tmp = (n_extra_tumo,(syanten - 1),n_tehai,(ary2,zi_ary2),(lst0::table_lst,[]))::tmp in 
                                            loop tmp t
                                        else
                                          loop tmp t
                                      else
                                        loop tmp t 
  in
  loop [] flags



(*
let draw n_extra_tumo syanten tehai ary zi_ary table_lst = 
  let tehai = ripai tehai in 
  let hash_val = Hashtbl.hash (n_extra_tumo,tehai,ary,zi_ary,table_lst) in
  let hash_return = Hashtbl.find_opt draw_hash hash_val in
  if hash_return = None then
    let tmp = 
      if n_extra_tumo = 0 then 
        draw_with_tegawari n_extra_tumo syanten tehai ary zi_ary table_lst
      else
        draw_without_tegawari n_extra_tumo syanten tehai ary zi_ary table_lst
    in
    let _ = 
      Mutex.lock lk2; 
      Hashtbl.add draw_hash hash_val tmp; 
      Mutex.unlock lk2 in 
    tmp
  else
    Option.get hash_return
*)

let draw n_extra_tumo syanten tehai ary zi_ary table_lst = 
  let tehai = ripai tehai in 
  if n_extra_tumo = 0 then 
    draw_with_tegawari n_extra_tumo syanten tehai ary zi_ary table_lst
  else
    draw_without_tegawari n_extra_tumo syanten tehai ary zi_ary table_lst

let discard n_extra_tumo tehai syanten_count ary zi_ary table_lst dora_lst =
  let flags = make_discard_ary tehai syanten_count in 
  let rec loop tmp t_lst = match t_lst with
    | [] -> tmp 
    | (hai,syan)::t -> let n_tehai = d_tehai tehai hai in 
                        let tmp = 
                          if syan = 1 && n_extra_tumo = 0 && syanten_count < 3 then (*三向聴以上は向聴戻ししない*) 
                              (hai,(draw (n_extra_tumo+1) (syanten_count+1) n_tehai ary zi_ary table_lst))::tmp
                          else if syan = 0 then 
                            if syanten_count = 0 then 
                              let (n_ary,n_zi_ary) = list_to_ary n_tehai in 
                              let red = tehai_in_red n_tehai in 
                              let tenpai_lst = tehai_to_ten n_ary n_zi_ary 0 0 false [] [Reach] dora_lst red in
                              let (lst1,lst2) = create_agari tenpai_lst ary zi_ary in
                              (hai,[(n_extra_tumo,(syanten_count-1),n_tehai,(ary,zi_ary),(lst1::table_lst,lst2))])::tmp
                            else
                              (hai,(draw (n_extra_tumo) (syanten_count) n_tehai ary zi_ary table_lst))::tmp
                          else
                            tmp
                        in
                        loop tmp t 
  in
  loop [] flags
                       
let dis_add_main tehai ary zi_ary max_tumo_len dora_lst =
  let tehai = rhai_to_hai tehai in  
  let (_,n) = syanten tehai in 
  let first_lst = discard 0 tehai n ary zi_ary [] dora_lst in 
  let rec loop (k_hai,t_ritu,agariritu,kitaiti) t_lst = match t_lst with 
    | [] -> (k_hai,t_ritu,agariritu,kitaiti)
    | (hai,lst)::t -> let rec loop2 (tmp_a,tmp_b,tmp_c) t_lst2 = match t_lst2 with 
                        | [] -> (tmp_a,tmp_b,tmp_c) 
                        | (n_extra_tumo,syanten_count,n_tehai,(ary2,zi_ary2),(lst1,lst2))::t2 -> let (t_max,a_max,k_max) = 
                                                                                            if syanten_count = -1 then 
                                                                                              calc_max_tumo_len max_tumo_len lst1 lst2
                                                                                            else
                                                                                              let n_lst = discard n_extra_tumo n_tehai syanten_count ary2 zi_ary2 lst1 dora_lst in 
                                                                                              let (k_hai,a,b,c) = loop ((1,Not_hai),-1.,-1.,-1.) n_lst in 
                                                                                              if k_hai = (1,Not_hai) then 
                                                                                                (0.,0.,0.)
                                                                                              else
                                                                                                (a,b,c)
                                                                                          in
                                                                                          
                                                                                          loop2 (tmp_a+.t_max,tmp_b+.a_max,tmp_c+.k_max) t2
                      in
                      let (t_max,a_max,k_max) = loop2 (0.,0.,0.) lst in 
                      if k_max > kitaiti then
                        loop (hai,t_max,a_max,k_max) t
                      else
                        loop (k_hai,t_ritu,agariritu,kitaiti) t
  in
  let (a,b,c,d) = loop ((1,Not_hai),-1.,-1.,-1.) first_lst in 
  (a,(b,c,d))

(*
let dis_add_main tehai ary zi_ary max_tumo_len dora_lst = 
  let tehai = rhai_to_hai tehai in  
  let (_,n) = common_syanten tehai in 
  let first_lst = discard 0 tehai n ary zi_ary [] dora_lst in 
  let rec f_loop t_lst pool = 
    let new_lst = List.map (fun (hai,lst) -> (hai,(T.async pool (fun _ -> loop2 (0.,0.,0.) lst pool)))) t_lst in 
    let new_lst = List.map (fun (a,b) -> (a,(T.await pool b))) new_lst in
    List.iter (fun ((a,_),(b,c,d)) -> Printf.printf "%d %f %f %f \n"a b c d) new_lst;
    Printf.printf "\n";
    List.fold_left (fun (a,(b,c,d)) (a1,(b1,c1,d1)) -> if d > d1 then (a,(b,c,d)) else (a1,(b1,c1,d1))) ((1,Not_hai),(0.,0.,0.)) new_lst 
  and loop2 (tmp_a,tmp_b,tmp_c) t_lst2 pool = match t_lst2 with 
        | [] -> (tmp_a,tmp_b,tmp_c) 
        | (n_extra_tumo,syanten_count,n_tehai,(ary2,zi_ary2),(lst1,lst2))::t2 -> let n_tehai = ripai n_tehai in 
                                                                                let hash_val = Hashtbl.hash (n_extra_tumo,n_tehai,(ary2,zi_ary2),(lst1,lst2)) in
                                                                                let hash_return = Hashtbl.find_all open_hash hash_val in
                                                                                (*let hash_return = if hash_return = None then None else let (tmp_lst,_) = Option.get hash_return in if tmp_lst = n_tehai then hash_return else None in  *)
                                                                                let (t_max,a_max,k_max) = 
                                                                                if hash_return = [] then 
                                                                                  let (t_max,a_max,k_max) =
                                                                                    if syanten_count = -1 then 
                                                                                      calc_max_tumo_len max_tumo_len lst1 lst2
                                                                                    else
                                                                                      let n_lst = discard n_extra_tumo n_tehai syanten_count ary2 zi_ary2 lst1 dora_lst in 
                                                                                      Printf.printf "%d\n" syanten_count;
                                                                                      let (k_hai,(a,b,c)) = f_loop n_lst pool in 
                                                                                      if k_hai = (1,Not_hai) then 
                                                                                        (0.,0.,0.)
                                                                                      else
                                                                                        (a,b,c)
                                                                                  in
                                                                                  let _ = 
                                                                                    Mutex.lock lk2; 
                                                                                    Hashtbl.add open_hash hash_val ((n_extra_tumo,(ary2,zi_ary2),n_tehai),(t_max,a_max,k_max)); 
                                                                                    Mutex.unlock lk2 in 
                                                                                    (t_max,a_max,k_max)
                                                                                else
                                                                                  (*let (tmp_lst,(t_max,a_max,k_max)) = Option.get hash_return in*)
                                                                                  if List.exists (fun (tmp_lst,_) -> tmp_lst = (n_extra_tumo,(ary2,zi_ary2),n_tehai)) hash_return then 
                                                                                    let (_,(t_max,a_max,k_max)) = List.find (fun ((_,_,tmp_lst),_) -> tmp_lst = n_tehai) hash_return in
                                                                                    (t_max,a_max,k_max)
                                                                                  else
                                                                                    let (t_max,a_max,k_max) = 
                                                                                      if syanten_count = -1 then 
                                                                                        calc_max_tumo_len max_tumo_len lst1 lst2
                                                                                      else
                                                                                        let n_lst = discard n_extra_tumo n_tehai syanten_count ary2 zi_ary2 lst1 dora_lst in 
                                                                                        Printf.printf "%d\n" syanten_count;
                                                                                        let (k_hai,(a,b,c)) = f_loop n_lst pool in 
                                                                                        if k_hai = (1,Not_hai) then 
                                                                                          (0.,0.,0.)
                                                                                        else
                                                                                          (a,b,c)
                                                                                    in
                                                                                    let _ = 
                                                                                      Mutex.lock lk2; 
                                                                                      Hashtbl.add open_hash hash_val ((n_extra_tumo,(ary2,zi_ary2),n_tehai),(t_max,a_max,k_max)); 
                                                                                      Mutex.unlock lk2 in 
                                                                                      (t_max,a_max,k_max)
                                                                              in
                                                                              
                                                                              loop2 (tmp_a+.t_max,tmp_b+.a_max,tmp_c+.k_max) t2 pool
      in
    let pool = T.setup_pool ~num_additional_domains:(core_count - 1) () in
    let res = T.run pool (fun _ -> f_loop first_lst pool) in
    T.teardown_pool pool;
    res

*)
(*

let create_work tasks c num =
    Array.iter (fun t -> C.send c (Task t)) tasks;
    for _ = 1 to num do
      C.send c Quit
    done


let rec worker f c () =
    match C.recv c with
    | Task a ->
        f a;
        worker f c ()
    | Quit -> ()


let dis_add_main_p tehai ary zi_ary max_tumo_len =
  let (_,n) = syanten tehai in syanten_count
  let first_lst = discard 0 tehai n ary zi_ary [] in 
  let loop t_lst = 
    if t_lst = [] then 
      ((1,Not_hai),-1.,-1.,-1.)
    else
      perform (Xchg t_lst)
  in
  let rec loop2 (tmp_a,tmp_b,tmp_c) (hai,t_lst2) = match t_lst2 with 
    | [] -> (tmp_a,tmp_b,tmp_c) 
    | (n_extra_tumo,syanten_count,n_tehai,(ary2,zi_ary2),(lst1,lst2))::t2 -> let (t_max,a_max,k_max) = 
                                                              syanten_count            if k_hai = (1,Not_hai) then 
                                                                            (0.,0.,0.)
                                                                          else
                                                                            (a,b,c)
                                                                      in
                                                                      loop2 (tmp_a+.t_max,tmp_b+.a_max,tmp_c+.k_max) ((1,Not_hai),t2)
  in
  loop first_lst 

  let dis_add_main_p1 tehai ary zi_ary max_tumo_len = 
    try_with dis_add_main_p tehai ary zi_ary max_tumo_len 
      { effc = fun (type a) (eff: a t) ->
          match eff with
          | Xchg t_lst -> Some (fun (k: (a, _) continuation) ->
            let lst_len = List.length t_lst in 
          let lst_len = List.length t_lst in 
            let lst_len = List.length t_lst in 
          let lst_len = List.length t_lst in 
            let lst_len = List.length t_lst in 
            let c = C.make_unbounded () in 
          let c = C.make_unbounded () in 
            let c = C.make_unbounded () in 
          let c = C.make_unbounded () in 
            let c = C.make_unbounded () in 
            let t_ary = Array.of_list t_lst in 
          let t_ary = Array.of_list t_lst in 
            let t_ary = Array.of_list t_lst in 
          let t_ary = Array.of_list t_lst in 
            let t_ary = Array.of_list t_lst in 
            let tasks = Array.init lst_len (fun i -> i) in
            let results = Array.make lst_len ((1,Not_hai),-1.,-1.,-1.) in
            let update r i = r.(i) <- loop2 ((1,Not_hai),-1.,-1.,-1.) t_ary.(i) in 
          let update r i = r.(i) <- loop2 ((1,Not_hai),-1.,-1.,-1.) t_ary.(i) in 
            let update r i = r.(i) <- loop2 ((1,Not_hai),-1.,-1.,-1.) t_ary.(i) in 
          let update r i = r.(i) <- loop2 ((1,Not_hai),-1.,-1.,-1.) t_ary.(i) in 
            let update r i = r.(i) <- loop2 ((1,Not_hai),-1.,-1.,-1.) t_ary.(i) in 
            Mutex.lock lk1;
            let use_count = if !core_count < (lst_len-1) then 0 else (lst_len-1) in
            let count = !core_count in 
          let count = !core_count in 
            let count = !core_count in 
          let count = !core_count in 
            let count = !core_count in 
            core_count := count - use_count;
            Mutex.unlock lk1;
            create_work tasks c (use_count+1);
            let _ =
              if use_count = 0 then 
            if use_count = 0 then 
              if use_count = 0 then 
            if use_count = 0 then 
              if use_count = 0 then 
                worker (update results) ()
              else 
            else 
              else 
            else 
              else 
                let domains = Array.init use_count
                        (fun _ -> Domain.spawn(worker (update results))) in
                worker (update results) ();
                Array.iter Domain.join domains(*ここは終わり次第戻す処理に変更*)
            in
            Mutex.lock lk1;
            let count = !core_count in 
          let count = !core_count in 
            let count = !core_count in 
          let count = !core_count in 
            let count = !core_count in 
            core_count := count + use_count;
            Mutex.unlock lk1;
            let fun_result = Array.fold_right (fun (a1,a2,a3,a4) (b1,b2,b3,b4) -> if a4 > b4 then (a1,a2,a3,a4) else (b1,b2,b3,b4)) results ((1,Not_hai),-1.,-1.,-1.) in 
          let fun_result = Array.fold_right (fun (a1,a2,a3,a4) (b1,b2,b3,b4) -> if a4 > b4 then (a1,a2,a3,a4) else (b1,b2,b3,b4)) results ((1,Not_hai),-1.,-1.,-1.) in 
            let fun_result = Array.fold_right (fun (a1,a2,a3,a4) (b1,b2,b3,b4) -> if a4 > b4 then (a1,a2,a3,a4) else (b1,b2,b3,b4)) results ((1,Not_hai),-1.,-1.,-1.) in 
          let fun_result = Array.fold_right (fun (a1,a2,a3,a4) (b1,b2,b3,b4) -> if a4 > b4 then (a1,a2,a3,a4) else (b1,b2,b3,b4)) results ((1,Not_hai),-1.,-1.,-1.) in 
            let fun_result = Array.fold_right (fun (a1,a2,a3,a4) (b1,b2,b3,b4) -> if a4 > b4 then (a1,a2,a3,a4) else (b1,b2,b3,b4)) results ((1,Not_hai),-1.,-1.,-1.) in 
            continue k fun_result)
          | _ -> None }

  (*let lst_len = List.length first_lst in 
  let tasks = Array.init lst_len (fun i -> i) in
  create_work tasks ;
  let first_ary = Array.of_list first_lst in 
  let results = Array.make lst_len ((1,Not_hai),-1.,-1.,-1.) in
  let update r i = r.(i) <- loop ((1,Not_hai),-1.,-1.,-1.) [first_ary.(i)] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update results))) in
  worker (update results) ();
  Array.iter Domain.join domains;
  Array.fold_left (fun (a1,b1,c1,d1) (a2,b2,c2,d2) ->if d1 > d2 then (a1,b1,c1,d1) else (a2,b2,c2,d2)) ((1,Not_hai),-1.,-1.,-1.) results*)
  loop ((1,Not_hai),-1.,-1.,-1.) first_lst
*)

(*
let _ = let tehai = [(4,Manzu);(4,Manzu);(5,Manzu);(6,Manzu);(7,Manzu);(4,Pinzu);(5,Pinzu);(7,Pinzu);(8,Pinzu);(9,Pinzu);(6,Souzu);(7,Souzu);(8,Souzu);(0,Ton)] in 
        let tehai = [(4,Manzu);(4,Manzu);(6,Manzu);(7,Manzu);(5,Pinzu);(7,Pinzu);(8,Pinzu);(8,Pinzu);(9,Pinzu);(6,Souzu);(7,Souzu);(8,Souzu);(0,Ton);(0,Nan)] in
        (*let tehai = [(4,Manzu);(4,Manzu);(6,Manzu);(5,Pinzu);(7,Pinzu);(8,Pinzu);(8,Pinzu);(9,Pinzu);(6,Souzu);(7,Souzu);(8,Souzu);(0,Ton);(0,Nan);(0,Sya)] in *)
let (ary,zi_ary) = create_table ([],[],[],[]) tehai in
let ((a,b),(c,d,e)) = dis_add_main tehai ary zi_ary 15 [(0,2)] in 
let (a,b) = hai_to_ary (a,b) in 
Printf.printf "(%d,%d)%f %f %f\n"a b c d e;
*)
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

  let (x,(y,z)) = mentsu_kouho_syuntsu [|2;2;2;0;0;0;2;2;0|] 0 in
  Printf.printf "%d %d\n" y z;*)
*)
