open Mahjong_base
open Loop
open Domainslib
open Mahjong_haieff
open Mahjong_safty

module C = Domainslib.Chan

let num_domains = int_of_string Sys.argv.(1) 

type 'a message = Task of 'a | Quit

let c = C.make_unbounded ()

let create_work tasks =
  Array.iter (fun t -> C.send c (Task t)) tasks;
  for _ = 1 to num_domains do
    C.send c Quit
  done

let rec worker f () =
  match C.recv c with
  | Task a ->
      f a;
      worker f ()
  | Quit -> ()

(*ary,zi_aryは残り枚数のテーブル。　返り値(kitaiti,agariritu) 0th: reach無し 1th:reachあり*)
let tenpai_kitaiti lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary tumo_l rm_wan =
  let (t_ary,t_zi_ary) = list_to_ary lst in
  let ten_lst = tehai_to_ten t_ary t_zi_ary zi_kaze ba_kaze naki f_lst yaku_lst dora_lst in
  let m = List.length ten_lst in
  let tumo_times = Int.to_float tumo_l in
  let rec loop i t_lst tmp_kitaiti tmp_agari = 
    let ((a,b),(c,d)) = List.nth t_lst i in
    let n = 
      if a = 3 then
        zi_ary.(b)
      else
        ary.(a).(b)
    in
    let n = Int.to_float n in
    let c = Int.to_float c in
    let tmp_kitaiti = (n /. rm_wan) *. c *. tumo_times +. tmp_kitaiti in
    let tmp_agari = (n /. rm_wan) *. tumo_times +. tmp_agari in
    if i = 0 then 
      (tmp_kitaiti,tmp_agari)
    else
      loop (i-1) t_lst tmp_kitaiti tmp_agari
  in
  if naki = false then
    let ten_lst2 = tehai_to_ten t_ary t_zi_ary zi_kaze ba_kaze naki f_lst (Reach::yaku_lst) dora_lst in
    if m = 0 then
      []
    else
      [(loop (m-1) ten_lst 0.0 0.0);(loop (m-1) ten_lst2 0.0 0.0)]
  else
    if m = 0 then
      []
    else
      [(loop (m-1) ten_lst 0.0 0.0)]
    






      





let tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary = 
  let m = List.length tehai in
  let rec loop i tmp = 
    let x = List.nth tehai i in
    let lst = d_tehai tehai x in
    let (_,n) = syanten lst in
    let tmp = 
      if n = 0 then
        (i,(tenpai_kitaiti lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary tumo_l rm_wan))::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  (*reach判断を加える*)
  let rec loop2 i a_lst tmp = 
    let (a,lst) = List.nth a_lst i in
    let (b,c) = 
      if lst = [] then
        (0.0,0.0)
      else if (List.length lst) = 2 then 
        List.nth lst 1
      else
        List.hd lst 
    in
    let (x,(y,z)) = tmp in
    let tmp =
      if y > b then
        tmp
      else
        (a,(b,c))
    in
    if i = 0 then
      tmp
    else
      loop2 (i-1) a_lst tmp
  in
  if m = 0 then 
    (13,(0.0,0.0))
  else
    let t_lst = loop (m-1) [] in
    if t_lst = [] then
      (13,(0.0,0.0))
    else
      let m = List.length t_lst in
      loop2 (m-1) t_lst (13,(0.0,0.0))


let tenpai_to_opt_f tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary kuikae_lst = 
  let m = List.length tehai in
  let rec loop i tmp = 
    let x = List.nth tehai i in
    let lst = d_tehai tehai x in
    let (_,n) = syanten lst in
    let tmp = 
      if List.exists (fun a -> a = x) kuikae_lst then 
        tmp 
      else if n = 0 then
        (x,(tenpai_kitaiti lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary tumo_l rm_wan))::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  let rec loop2 i a_lst tmp = 
    let (a,lst) = List.nth a_lst i in
    let (b,c) = 
      if lst = [] then
        (0.0,0.0)
      else
        List.hd lst 
    in
    let (x,(y,z)) = tmp in
    let tmp =
      if y > b then
        tmp
      else
        (a,(b,c))
    in
    if i = 0 then
      tmp
    else
      loop2 (i-1) a_lst tmp
  in
  if m = 0 then 
    ((1,Not_hai),(0.0,0.0))
  else
    let t_lst = loop (m-1) [] in
    if t_lst = [] then
      ((1,Not_hai),(0.0,0.0))
    else
      let m = List.length t_lst in
      loop2 (m-1) t_lst ((1,Not_hai),(0.0,0.0))
      

let ary_opt ary zi_ary lst = 
  let lst = List.map (fun (a,b) -> hai_to_ary (a,b)) lst in
  let m = List.length lst in
  let ary2 = Array.map (fun a -> Array.copy a) ary in
  let zi_ary2 = Array.copy zi_ary in
  let rec loop i =
    let (x,y) = List.nth lst i in
    let _ =
      if x = 3 then
        let n = zi_ary2.(y) in
        zi_ary2.(y) <- n-1;
      else
        let n = ary2.(x).(y) in
        ary2.(x).(y) <- n-1;
    in
    if i = 0 then
      (ary2,zi_ary2)
    else
      loop (i-1)
  in
  if m = 0 then
    (ary2,zi_ary2)
  else
    loop (m-1)


(*返り値(k_lst,tumo_lst,rest_tumo_lst,current_tehai)list*)
let all_tumo ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = 
  let (_,n) = syanten current_tehai in
  let (ary2,zi_ary2) = ary_opt ary zi_ary tumo_lst in
  let rec loop i j tmp = 
    let tmp = 
      if ary2.(i).(j) > 0 then
        let (x,y) = ary_to_hai (i,j) in
        let tmp_tehai = add_tehai current_tehai (x,y) in
        let (_,new_n) = syanten tmp_tehai in 
        if new_n = (n - 1) then
          let rest = ary2.(i).(j) in
          (k_lst,(x,y)::tumo_lst,rest::rest_tumo_lst,tmp_tehai)::tmp
        else
          tmp
      else
        tmp
    in
    if i = 2 then
      if j = 8 then
        tmp
      else
        loop i (j+1) tmp
    else
      if j = 8 then
        loop (i+1) 0 tmp
      else
        loop i (j+1) tmp
  in
  let rec loop2 i tmp = 
    let tmp = 
      if zi_ary2.(i) > 0 then
        let (x,y) = ary_to_hai (3,i) in
        let tmp_tehai = add_tehai current_tehai (x,y) in
        let (_,new_n) = syanten tmp_tehai in 
        if new_n = (n - 1) then
          let rest = zi_ary2.(i) in
          (k_lst,(x,y)::tumo_lst,rest::rest_tumo_lst,tmp_tehai)::tmp
        else
          tmp
      else
        tmp
    in
    if i = 6 then
      tmp
    else
      loop2 (i+1) tmp
  in
  let t_lst = loop 0 0 [] in
  loop2 0 t_lst




  

let opt_tumohai ary zi_ary tenpai_lst = 
  let tenpai_lst_len = List.length tenpai_lst in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tenpai_lst i in
    let tmp = (all_tumo ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai))@tmp in
    if i = 0 then
      tmp
    else 
      loop (i-1) tmp
  in
  if tenpai_lst_len = 0 then 
    []
  else
    loop (tenpai_lst_len-1) []









let k_fase ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = 
  let (lst,n) = syanten current_tehai in
  let tehai_len = List.length current_tehai in
  let rec loop i tmp = 
    let ihai = List.nth current_tehai i in
    let new_tehai = d_tehai current_tehai ihai in
    let (lst,new_n) = syanten new_tehai in
    let tmp = 
      if new_n = n then
        all_tumo ary zi_ary (ihai::k_lst,tumo_lst,rest_tumo_lst,new_tehai)@tmp
      else
        tmp 
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  loop (tehai_len-1) []

let all_k_fase ary zi_ary tenpai_lst = 
  let m = List.length tenpai_lst in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tenpai_lst i in
    let n = List.length current_tehai in
    let rec loop2 j tmp2 = 
      let ihai = List.nth current_tehai j in
      let new_tehai = d_tehai current_tehai ihai in
      let (lst,new_n) = syanten new_tehai in
      let tmp2 = 
        if new_n = n then
          all_tumo ary zi_ary (ihai::k_lst,tumo_lst,rest_tumo_lst,new_tehai)@tmp 
        else
          tmp2
    in
      if j = 0 then
        tmp2
      else
        loop2 (j-1) tmp2
    in
    let tmp = (loop2 (n-1) [])@tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop (m-1) [] 

let syanten_to_tenpai ary zi_ary tenpai_lst = 
  let m = List.length tenpai_lst in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tmp i in
      let tmp = (k_fase ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai))@tmp in
      if i = 0 then
        tmp
      else
        loop (i-1) tmp
    in
  if m = 0 then
    []
  else
    loop (m-1) []

let operate_tenapai_ritu ary zi_ary tehai = 
  let tenpai_lst = [([],[],[],tehai)] in
  let (_,n) = syanten tehai in
  let rec loop i tmp =
    let m = List.length tmp in
    let rec loop2 j tmp2 = 
      let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tmp j in
      let tmp2 = (k_fase ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai))@tmp2 in
      if j = 0 then
        tmp2
      else
        loop2 (j-1) tmp2
    in
    if m = 0 then 
      ([],[])
    else
      let tmp' = loop2 (m-1) [] in
      if i = 0 then
        (tmp,tmp')
      else
        loop (i-1) tmp'
  in
  let (tmp,tmp') = loop (n-1) tenpai_lst in
  if tmp = [] then
    tmp'
  else
    let all_t = all_k_fase ary zi_ary tmp in
    let tmp = syanten_to_tenpai ary zi_ary all_t in
    tmp@tmp'

let operate_tenpai_ritu_parallel ary zi_ary tenpai_lst = 
  let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = tenpai_lst in 
  let (_,n) = syanten current_tehai in 
  let tenpai_lst = [tenpai_lst] in 
  let rec loop i tmp =
    let m = List.length tmp in
    let rec loop2 j tmp2 = 
      let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tmp j in
      let tmp2 = (k_fase ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai))@tmp2 in
      if j = 0 then
        tmp2
      else
        loop2 (j-1) tmp2
    in
    if m = 0 then 
      ([],[])
    else
      let tmp' = loop2 (m-1) [] in
      if i = 0 then
        (tmp,tmp')
      else
        loop (i-1) tmp'
  in
  let (tmp,tmp') = loop (n-1) tenpai_lst in
  if tmp = [] then 
    tmp'
  else
    let all_t = all_k_fase ary zi_ary tmp in
    let tmp = syanten_to_tenpai ary zi_ary all_t in
    tmp@tmp'
(*
let operate_tenpai_ritu_parallel ary zi_ary tenpai_lst_lst = 
  let m' = List.length tenpai_lst_lst in 
  let rec loop i tmp =
    let m = List.length tmp in
    let rec loop2 j tmp2 = 
      let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tmp j in
      let tmp2 = (k_fase ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai))@tmp2 in
      if j = 0 then
        tmp2
      else
        loop2 (j-1) tmp2
    in
    if m = 0 then 
      ([],[])
    else
      let tmp' = loop2 (m-1) [] in
      if i = 0 then
        (tmp,tmp')
      else
        loop (i-1) tmp'
  in
  let rec loop' k return = 
    let tenpai_lst = [List.nth tenpai_lst_lst k] in 
    let (tmp,tmp') = loop 2 tenpai_lst in
    let return2 = 
      if tmp = [] then 
        tmp'
      else
        let all_t = all_k_fase ary zi_ary tmp in
        let tmp = syanten_to_tenpai ary zi_ary all_t in
        tmp@tmp'
    in
    if k = 0 then 
      return2@return 
    else
      loop' (k-1) (return2@return) 
    in
  if m' = 0 then 
    []
  else
    loop' (m'-1) [] 
    *)
(*
let rec parallel ary zi_ary pool tenpai_lst tmp3 = 
  let m = List.length tenpai_lst in 
  if m = 0 then
    []
  else
    let (k_lst0,tumo_lst0,rest_tumo_lst0,current_tehai0) = List.hd tenpai_lst in 
    let tenpai_lst = List.tl tenpai_lst in 
    let a = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (k_lst0,tumo_lst0,rest_tumo_lst0,current_tehai0)) in
    let (tmp2,new_tenpai_lst) = 
      if m = 1 then 
        let a' = Task.await pool a in 
        (a',tenpai_lst)
      else
        let (k_lst1,tumo_lst1,rest_tumo_lst1,current_tehai1) = List.hd tenpai_lst in
        let tenpai_lst = List.tl tenpai_lst in 
        let b = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst1,tumo_lst1,rest_tumo_lst1,current_tehai1)) in 
        if m = 2 then
          let a' = Task.await pool a in
          let b' = Task.await pool b in 
          (a'@b',tenpai_lst)
        else
          let (k_lst2,tumo_lst2,rest_tumo_lst2,current_tehai2) = List.hd tenpai_lst in
          let tenpai_lst = List.tl tenpai_lst in 
          let c = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst2,tumo_lst2,rest_tumo_lst2,current_tehai2)) in 
          if m = 3 then
            let a' = Task.await pool a in
            let b' = Task.await pool b in
            let c' = Task.await pool c in  
            (a'@b'@c',tenpai_lst)
          else
            let (k_lst3,tumo_lst3,rest_tumo_lst3,current_tehai3) = List.hd tenpai_lst in
            let tenpai_lst = List.tl tenpai_lst in 
            let d = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst3,tumo_lst3,rest_tumo_lst3,current_tehai3)) in 
            if m = 4 then
              let a' = Task.await pool a in
              let b' = Task.await pool b in
              let c' = Task.await pool c in
              let d' = Task.await pool d in 
              (a'@b'@c'@d',tenpai_lst)
            else
              let (k_lst4,tumo_lst4,rest_tumo_lst4,current_tehai4) = List.hd tenpai_lst in
              let tenpai_lst = List.tl tenpai_lst in 
              let e = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst4,tumo_lst4,rest_tumo_lst4,current_tehai4)) in 
              if m = 5 then
                let a' = Task.await pool a in
                let b' = Task.await pool b in
                let c' = Task.await pool c in
                let d' = Task.await pool d in
                let e' = Task.await pool e in  
                (a'@b'@c'@d'@e',tenpai_lst)
              else
                let (k_lst5,tumo_lst5,rest_tumo_lst5,current_tehai5) = List.hd tenpai_lst in
                let tenpai_lst = List.tl tenpai_lst in
                let f = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst5,tumo_lst5,rest_tumo_lst5,current_tehai5)) in 
                if m = 6 then
                  let a' = Task.await pool a in
                  let b' = Task.await pool b in
                  let c' = Task.await pool c in
                  let d' = Task.await pool d in
                  let e' = Task.await pool e in
                  let f' = Task.await pool f in  
                  (a'@b'@c'@d'@e'@f',tenpai_lst)
                else
                  let (k_lst6,tumo_lst6,rest_tumo_lst6,current_tehai6) = List.hd tenpai_lst in
                  let tenpai_lst = List.tl tenpai_lst in 
                  let g = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst6,tumo_lst6,rest_tumo_lst6,current_tehai6)) in 
                  if m = 7 then
                    let a' = Task.await pool a in
                    let b' = Task.await pool b in
                    let c' = Task.await pool c in
                    let d' = Task.await pool d in
                    let e' = Task.await pool e in
                    let f' = Task.await pool f in
                    let g' = Task.await pool g in 
                    (a'@b'@c'@d'@e'@f'@g',tenpai_lst)
                  else
                    let (k_lst7,tumo_lst7,rest_tumo_lst7,current_tehai7) = List.hd tenpai_lst in
                    let tenpai_lst = List.tl tenpai_lst in 
                    let h = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst7,tumo_lst7,rest_tumo_lst7,current_tehai7)) in 
                    if m = 8 then
                      let a' = Task.await pool a in
                      let b' = Task.await pool b in
                      let c' = Task.await pool c in
                      let d' = Task.await pool d in
                      let e' = Task.await pool e in
                      let f' = Task.await pool f in
                      let g' = Task.await pool g in
                      let h' = Task.await pool h in 
                      (a'@b'@c'@d'@e'@f'@g'@h',tenpai_lst)
                    else
                      let (k_lst8,tumo_lst8,rest_tumo_lst8,current_tehai8) = List.hd tenpai_lst in
                      let tenpai_lst = List.tl tenpai_lst in 
                      let i = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary (k_lst8,tumo_lst8,rest_tumo_lst8,current_tehai8)) in 
                      if m = 9 then
                        let a' = Task.await pool a in
                        let b' = Task.await pool b in
                        let c' = Task.await pool c in
                        let d' = Task.await pool d in
                        let e' = Task.await pool e in
                        let f' = Task.await pool f in
                        let g' = Task.await pool g in
                        let h' = Task.await pool h in
                        let i' = Task.await pool i in

                        (a'@b'@c'@d'@e'@f'@g'@h'@i',tenpai_lst)
                      else
                        let (k_lst9,tumo_lst9,rest_tumo_lst9,current_tehai9) = List.hd tenpai_lst in
                        let tenpai_lst = List.tl tenpai_lst in 
                        let j = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst9,tumo_lst9,rest_tumo_lst9,current_tehai9)) in 
                        if m = 10 then
                          let a' = Task.await pool a in
                          let b' = Task.await pool b in
                          let c' = Task.await pool c in
                          let d' = Task.await pool d in
                          let e' = Task.await pool e in
                          let f' = Task.await pool f in
                          let g' = Task.await pool g in
                          let h' = Task.await pool h in
                          let i' = Task.await pool i in
                          let j' = Task.await pool j in
                          (a'@b'@c'@d'@e'@f'@g'@h'@i'@j',tenpai_lst)
                        else
                          let (k_lst10,tumo_lst10,rest_tumo_lst10,current_tehai10) = List.hd tenpai_lst in
                          let tenpai_lst = List.tl tenpai_lst in 
                          let k = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst10,tumo_lst10,rest_tumo_lst10,current_tehai10)) in 
                          if m = 11 then
                            let a' = Task.await pool a in
                            let b' = Task.await pool b in
                            let c' = Task.await pool c in
                            let d' = Task.await pool d in
                            let e' = Task.await pool e in
                            let f' = Task.await pool f in
                            let g' = Task.await pool g in
                            let h' = Task.await pool h in
                            let i' = Task.await pool i in
                            let j' = Task.await pool j in
                            let k' = Task.await pool k in
                            (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k',tenpai_lst)
                          else
                            let (k_lst11,tumo_lst11,rest_tumo_lst11,current_tehai11) = List.hd tenpai_lst in
                            let tenpai_lst = List.tl tenpai_lst in 
                            let l = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst11,tumo_lst11,rest_tumo_lst11,current_tehai11)) in 
                            if m = 12 then
                              let a' = Task.await pool a in
                              let b' = Task.await pool b in
                              let c' = Task.await pool c in
                              let d' = Task.await pool d in
                              let e' = Task.await pool e in
                              let f' = Task.await pool f in
                              let g' = Task.await pool g in
                              let h' = Task.await pool h in
                              let i' = Task.await pool i in
                              let j' = Task.await pool j in
                              let k' = Task.await pool k in
                              let l' = Task.await pool l in
                              (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l',tenpai_lst)
                            else
                              let (k_lst12,tumo_lst12,rest_tumo_lst12,current_tehai12) = List.hd tenpai_lst in
                              let tenpai_lst = List.tl tenpai_lst in 
                              let n = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst12,tumo_lst12,rest_tumo_lst12,current_tehai12)) in 
                              if m = 13 then
                                let a' = Task.await pool a in
                                let b' = Task.await pool b in
                                let c' = Task.await pool c in
                                let d' = Task.await pool d in
                                let e' = Task.await pool e in
                                let f' = Task.await pool f in
                                let g' = Task.await pool g in
                                let h' = Task.await pool h in
                                let i' = Task.await pool i in
                                let j' = Task.await pool j in
                                let k' = Task.await pool k in
                                let l' = Task.await pool l in
                                let n' = Task.await pool n in
                                (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n',tenpai_lst)
                              else
                                let (k_lst13,tumo_lst13,rest_tumo_lst13,current_tehai13) = List.hd tenpai_lst in
                                let tenpai_lst = List.tl tenpai_lst in 
                                let o = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst13,tumo_lst13,rest_tumo_lst13,current_tehai13)) in 
                                if m = 14 then
                                  let a' = Task.await pool a in
                                  let b' = Task.await pool b in
                                  let c' = Task.await pool c in
                                  let d' = Task.await pool d in
                                  let e' = Task.await pool e in
                                  let f' = Task.await pool f in
                                  let g' = Task.await pool g in
                                  let h' = Task.await pool h in
                                  let i' = Task.await pool i in
                                  let j' = Task.await pool j in
                                  let k' = Task.await pool k in
                                  let l' = Task.await pool l in
                                  let n' = Task.await pool n in
                                  let o' = Task.await pool o in
                                  (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n'@o',tenpai_lst)
                                else
                                  let (k_lst14,tumo_lst14,rest_tumo_lst14,current_tehai14) = List.hd tenpai_lst in
                                  let tenpai_lst = List.tl tenpai_lst in 
                                  let p = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst14,tumo_lst14,rest_tumo_lst14,current_tehai14)) in 
                                  if m = 15 then
                                    let a' = Task.await pool a in
                                    let b' = Task.await pool b in
                                    let c' = Task.await pool c in
                                    let d' = Task.await pool d in
                                    let e' = Task.await pool e in
                                    let f' = Task.await pool f in
                                    let g' = Task.await pool g in
                                    let h' = Task.await pool h in
                                    let i' = Task.await pool i in
                                    let j' = Task.await pool j in
                                    let k' = Task.await pool k in
                                    let l' = Task.await pool l in
                                    let n' = Task.await pool n in
                                    let o' = Task.await pool o in
                                    let p' = Task.await pool p in
                                    (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n'@o'@p',tenpai_lst)
                                  else
                                    let (k_lst15,tumo_lst15,rest_tumo_lst15,current_tehai15) = List.hd tenpai_lst in
                                    let tenpai_lst = List.tl tenpai_lst in 
                                    let q = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst15,tumo_lst15,rest_tumo_lst15,current_tehai15)) in 
                                    if m = 16 then
                                      let a' = Task.await pool a in
                                      let b' = Task.await pool b in
                                      let c' = Task.await pool c in
                                      let d' = Task.await pool d in
                                      let e' = Task.await pool e in
                                      let f' = Task.await pool f in
                                      let g' = Task.await pool g in
                                      let h' = Task.await pool h in
                                      let i' = Task.await pool i in
                                      let j' = Task.await pool j in
                                      let k' = Task.await pool k in
                                      let l' = Task.await pool l in
                                      let n' = Task.await pool n in
                                      let o' = Task.await pool o in
                                      let p' = Task.await pool p in
                                      let q' = Task.await pool q in
                                      (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n'@o'@p'@q',tenpai_lst)
                                    else
                                      let (k_lst16,tumo_lst16,rest_tumo_lst16,current_tehai16) = List.hd tenpai_lst in
                                      let tenpai_lst = List.tl tenpai_lst in 
                                      let r = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst16,tumo_lst16,rest_tumo_lst16,current_tehai16)) in 
                                      if m = 17 then
                                        let a' = Task.await pool a in
                                        let b' = Task.await pool b in
                                        let c' = Task.await pool c in
                                        let d' = Task.await pool d in
                                        let e' = Task.await pool e in
                                        let f' = Task.await pool f in
                                        let g' = Task.await pool g in
                                        let h' = Task.await pool h in
                                        let i' = Task.await pool i in
                                        let j' = Task.await pool j in
                                        let k' = Task.await pool k in
                                        let l' = Task.await pool l in
                                        let n' = Task.await pool n in
                                        let o' = Task.await pool o in
                                        let p' = Task.await pool p in
                                        let q' = Task.await pool q in
                                        let r' = Task.await pool r in
                                        (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n'@o'@p'@q'@r',tenpai_lst)
                                      else
                                        let (k_lst17,tumo_lst17,rest_tumo_lst17,current_tehai17) = List.hd tenpai_lst in
                                        let tenpai_lst = List.tl tenpai_lst in 
                                        let s = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst17,tumo_lst17,rest_tumo_lst17,current_tehai17)) in 
                                        if m = 18 then
                                          let a' = Task.await pool a in
                                          let b' = Task.await pool b in
                                          let c' = Task.await pool c in
                                          let d' = Task.await pool d in
                                          let e' = Task.await pool e in
                                          let f' = Task.await pool f in
                                          let g' = Task.await pool g in
                                          let h' = Task.await pool h in
                                          let i' = Task.await pool i in
                                          let j' = Task.await pool j in
                                          let k' = Task.await pool k in
                                          let l' = Task.await pool l in
                                          let n' = Task.await pool n in
                                          let o' = Task.await pool o in
                                          let p' = Task.await pool p in
                                          let q' = Task.await pool q in
                                          let r' = Task.await pool r in
                                          let s' = Task.await pool s in
                                          (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n'@o'@p'@q'@r'@s',tenpai_lst)
                                        else
                                          let (k_lst18,tumo_lst18,rest_tumo_lst18,current_tehai18) = List.hd tenpai_lst in
                                          let tenpai_lst = List.tl tenpai_lst in 
                                          let t = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst18,tumo_lst18,rest_tumo_lst18,current_tehai18)) in 
                                          if m = 19 then
                                            let a' = Task.await pool a in
                                            let b' = Task.await pool b in
                                            let c' = Task.await pool c in
                                            let d' = Task.await pool d in
                                            let e' = Task.await pool e in
                                            let f' = Task.await pool f in
                                            let g' = Task.await pool g in
                                            let h' = Task.await pool h in
                                            let i' = Task.await pool i in
                                            let j' = Task.await pool j in
                                            let k' = Task.await pool k in
                                            let l' = Task.await pool l in
                                            let n' = Task.await pool n in
                                            let o' = Task.await pool o in
                                            let p' = Task.await pool p in
                                            let q' = Task.await pool q in
                                            let r' = Task.await pool r in
                                            let s' = Task.await pool s in
                                            let t' = Task.await pool t in
                                            (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n'@o'@p'@q'@r'@s'@t',tenpai_lst)
                                          else
                                            let (k_lst19,tumo_lst19,rest_tumo_lst19,current_tehai19) = List.hd tenpai_lst in
                                            let tenpai_lst = List.tl tenpai_lst in 
                                            let u = Task.async pool (fun _ ->  operate_tenpai_ritu_parallel ary zi_ary  (k_lst19,tumo_lst19,rest_tumo_lst19,current_tehai19)) in 
                                            let a' = Task.await pool a in
                                            let b' = Task.await pool b in
                                            let c' = Task.await pool c in
                                            let d' = Task.await pool d in
                                            let e' = Task.await pool e in
                                            let f' = Task.await pool f in
                                            let g' = Task.await pool g in
                                            let h' = Task.await pool h in
                                            let i' = Task.await pool i in
                                            let j' = Task.await pool j in
                                            let k' = Task.await pool k in
                                            let l' = Task.await pool l in
                                            let n' = Task.await pool n in
                                            let o' = Task.await pool o in
                                            let p' = Task.await pool p in
                                            let q' = Task.await pool q in
                                            let r' = Task.await pool r in
                                            let s' = Task.await pool s in
                                            let t' = Task.await pool t in
                                            let u' = Task.await pool u in
                                            (a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@n'@o'@p'@q'@r'@s'@t'@u',tenpai_lst)
  in
  if new_tenpai_lst = [] then 
    tmp2@tmp3
  else
    parallel ary zi_ary pool new_tenpai_lst (tmp2@tmp3)
*)
(*spilit thread block*)
(*
let split_nth_i ls n =
  let rec iter ls n a =
    if n <= 0 || ls = [] then (a,ls)
    else iter (List.tl ls) (n - 1) (List.hd ls :: a)
  in
    iter ls n []
      

let parallel ary zi_ary pool lst = 
  let m = List.length lst in 
  let div_20 = m/20 in 
  let min_20 = m - (div_20*20) in 
  let rec loop i tmp rem_lst = 
    let (a,b) = 
      if i < min_20 then 
        split_nth_i rem_lst (div_20+1) 
      else 
        split_nth_i rem_lst (div_20)
    in
    if i = 18 then
      a::b::tmp 
    else
      loop (i+1) (a::tmp) b 
  in 
  let new_lst = loop 0 [] lst in  
  let a = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 0)) in
  let b = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 1)) in
  let c = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 2)) in
  let d = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 3)) in
  let e = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 4)) in
  let f = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 5)) in
  let g = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 6)) in
  let h = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 7)) in
  let i = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 8)) in
  let j = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 9)) in
  let k = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 10)) in
  let l = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 11)) in
  let m = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 12)) in
  let n = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 13)) in
  let o = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 14)) in
  let p = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 15)) in
  let q = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 16)) in
  let r = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 17)) in
  let s = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 18)) in
  let t = Task.async pool (fun _ -> operate_tenpai_ritu_parallel ary zi_ary (List.nth new_lst 19)) in
  let a' = Task.await pool a in
  let b' = Task.await pool b in
  let c' = Task.await pool c in
  let d' = Task.await pool d in
  let e' = Task.await pool e in
  let f' = Task.await pool f in
  let g' = Task.await pool g in
  let h' = Task.await pool h in
  let i' = Task.await pool i in
  let j' = Task.await pool j in
  let k' = Task.await pool k in
  let l' = Task.await pool l in
  let m' = Task.await pool m in
  let n' = Task.await pool n in
  let o' = Task.await pool o in
  let p' = Task.await pool p in
  let q' = Task.await pool q in
  let r' = Task.await pool r in
  let s' = Task.await pool s in
  let t' = Task.await pool t in
  a'@b'@c'@d'@e'@f'@g'@h'@i'@j'@k'@l'@m'@n'@o'@p'@q'@r'@s'@t'
  
*)



let parallel ary zi_ary tmp =
  let n = List.length tmp in  
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let update p r i = p.(i) <- operate_tenpai_ritu_parallel ary zi_ary r.(i) in 
  let results = Array.of_list tmp in
  let pre =  Array.make n [] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre results))) in
  worker (update pre results) ();
  Array.iter Domain.join domains;
  Array.to_list results 

  

      




let judge_parallel ary zi_ary tehai = 
  let (_,n) = syanten tehai in 
  if n  =  3 then 
    let tenpai_lst = [([],[],[],tehai)] in 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.hd tenpai_lst in
    let tmp = k_fase ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai) in
    (*let pool = Task.setup_pool ~num_additional_domains:20 () in
    (*let res = Task.run pool (fun () -> parallel ary zi_ary pool tmp [])  in*)
    let res = Task.run pool (fun () -> parallel ary zi_ary pool tmp)  in
    Task.teardown_pool pool;*)
    let res = parallel ary zi_ary tmp in 
    res
  else
    operate_tenapai_ritu ary zi_ary tehai

let tenpai_ritu rest_tumo_lst tumo_l rm_wan = 
  let m = List.length rest_tumo_lst in
  let rec loop i tmp = 
    let h = List.nth rest_tumo_lst i in
    let h = Int.to_float h in
    let tumo_c = Int.to_float (tumo_l - i) in
    let tmp = h /. rm_wan *. tumo_c *. tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    1.0
  else
    loop (m-1) 1.0

let tenpai_to_kitaiti ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan = 
  let m = List.length tenpai_lst in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu) = List.nth tenpai_lst i in
    let (ary2,zi_ary2) = ary_opt ary zi_ary tumo_lst in
    let current_tehai = ripai current_tehai in
    let tumo_l' = tumo_l - (List.length tumo_lst) in
    let a_k_lst = tenpai_to_opt current_tehai tumo_l' rm_wan f_lst zi_kaze ba_kaze naki [] dora_lst ary2 zi_ary2 in
    let (_,(kitaiti,agariritu)) = a_k_lst in
    let kitaiti = t_ritu *. kitaiti in
    let agariritu = agariritu *. t_ritu in
    let tmp = (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti)::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then 
    []
  else
    loop (m-1) []

let compile_kitaiti p_lst current_tehai = 
  let m = List.length p_lst in
  let rec loop i tmp = 
    let (_,_,_,_,_,agariritu,kitaiti) = List.nth p_lst i in
    let (x,y) = tmp in
    let tmp = ((x +. agariritu),(y +. kitaiti)) in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    ([],0.0,0.0)
  else
    let (agariritu,kitaiti) = loop (m-1) (0.0,0.0) in
    (current_tehai,agariritu,kitaiti)
  

let opt_tenpai_form tenpai_lst = 
  let rec loop tmp t_lst = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti) = List.hd t_lst in
    let p_lst = List.filter (fun (a,b,c,d,e,f,g) -> d = current_tehai) t_lst in
    let tmp = (compile_kitaiti p_lst current_tehai)::tmp in
    let t_lst = List.filter (fun (a,b,c,d,e,f,g) -> d <> current_tehai) t_lst in
    if t_lst = [] then
      tmp
    else
      loop tmp t_lst
  in
  let kitaiti_lst = loop [] tenpai_lst in
  let kitaiti_lst = List.sort (fun (x,y,z) (x',y',z') -> if z > z' then -1 else 1) kitaiti_lst in
  List.hd kitaiti_lst

let minus_kitaiti t_lst p_lst = 
  let m = List.length p_lst in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo) = List.nth p_lst i in
    let k_n = List.length k_lst in
    let k_hai = List.nth k_lst (k_n-1) in
    let minus_lst = List.filter (fun (a,b,c,d,e,f,g) -> 
                                      let a_n = List.length a in
                                      let a_hai = List.nth a (a_n-1) in
                                      a_hai = k_hai) t_lst in
    let minus_kitaiti = List.fold_left (fun g (a',b',c',d',e',f',g') -> g +. g') 0.0 minus_lst in
    let tmp = (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti)::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop (m-1) []


let minus_kitaiti_f t_lst p_lst = 
  let m = List.length p_lst in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo) = List.nth p_lst i in
    let k_n = List.length k_lst in
    let k_hai = List.nth k_lst (k_n-1) in
    let minus_lst = List.filter (fun (a,b,c,d,e,f,g) -> 
                                      let a_n = List.length a in
                                      let a_hai = List.nth a (a_n-1) in
                                      a_hai = k_hai) t_lst in
    let minus_kitaiti = List.fold_left (fun g (a',b',c',d',e',f',g') -> g +. g') 0.0 minus_lst in
    let tmp = (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti)::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop (m-1) []

let opt_kitaiti p_lst = 
  let m = List.length p_lst in
  let rec loop i tmp = 
    let (a,b,c,d,e,f,g,h,i') = List.nth p_lst i in
    let h = Int.to_float h in
    let j = (i' -. (h *. 10000.0)) in
    let tmp = (a,b,c,d,e,f,g,h,i',j)::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop (m-1) []

let max_kitaiti tenpai_lst = 
  let rec loop lst tmp = match lst with
    | [] -> tmp
    | h::t -> let (_,_,_,_,_,_,_,_,_,j) = h in
              let (_,_,_,_,_,_,_,_,_,j') = tmp in
              if j > j' then
                loop t h
              else
                loop t tmp
  in
  loop tenpai_lst ([],[],[],[],0.0,0.0,0.0,0.0,0.0,0.0)

let max_tenpairitu tenpai_lst = 
  let rec loop lst tmp = match lst with
    | [] -> tmp
    | h::t -> let (_,_,_,_,e,_,_,_,_,_) = h in
              let (_,_,_,_,e',_,_,_,_,_) = tmp in
              if e > e' then
                loop t h
              else
                loop t tmp
  in
  loop tenpai_lst ([],[],[],[],0.0,0.0,0.0,0.0,0.0,0.0)

let max_agariritu tenpai_lst = 
  let rec loop lst tmp = match lst with
    | [] -> tmp
    | h::t -> let (_,_,_,_,_,f,_,_) = h in
              let (_,_,_,_,_,f',_,_) = tmp in
              if f > f' then
                loop t h
              else
                loop t tmp
  in
  loop tenpai_lst ([],[],[],[],0.0,-100.0,0.0,0)


(*(k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti,total_kitaiti)*)    
let col_tenpai ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst = 
  let tenpai_lst = judge_parallel ary zi_ary tehai in
  let m = List.length tenpai_lst in
  let rm_wan = yama_len-14 in
  let tumo_l =  rm_wan / 4 in
  let rm_wan = Int.to_float rm_wan in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tenpai_lst i in
    let t_ritu = tenpai_ritu rest_tumo_lst tumo_l rm_wan in 
    let tmp = (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu)::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    ([],[],[],[],0.0,0.0,0.0,0.0,0.0,0.0)
  else
    let tenpai_lst = loop (m-1) [] in
    let tenpai_lst = tenpai_to_kitaiti ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
    let (o_current_tehai,o_agariritu,o_kitaiti) = opt_tenpai_form tenpai_lst in
    let p_lst = List.filter (fun (a,b,c,d,e,f,g) -> d = o_current_tehai) tenpai_lst in
    let t_lst = List.filter (fun (a,b,c,d,e,f,g) -> d <> o_current_tehai) tenpai_lst in
    let p_lst = List.map (fun (a,b,c,d,e,f,g) -> (a,b,c,d,e,f,g,(anzen ary zi_ary a))) p_lst in
    let p_lst = minus_kitaiti t_lst p_lst in
    let p_lst = opt_kitaiti p_lst in
    let max = max_kitaiti p_lst in 
    if max = ([],[],[],[],0.0,0.0,0.0,0.0,0.0,0.0) then 
      max_tenpairitu p_lst
    else
      max


let col_tenpai_f_kuikae ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst kuikae_lst = 
  let tenpai_lst = judge_parallel ary zi_ary tehai in
  let m = List.length tenpai_lst in
  let rm_wan = yama_len-14 in
  let tumo_l =  rm_wan / 4 in
  let rm_wan = Int.to_float rm_wan in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tenpai_lst i in
    let n = List.length k_lst in
    let tmp = 
      if n = 0 then
        tmp 
      else
        if List.exists (fun a -> a = (List.nth k_lst (n-1))) kuikae_lst then
          tmp
        else
          let t_ritu = tenpai_ritu rest_tumo_lst tumo_l rm_wan in 
          (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu)::tmp 
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    ([],[],[],[],0.0,0.0,0.0,0.0,0.0,0.0)
  else
    let tenpai_lst = loop (m-1) [] in
    if tenpai_lst = [] then 
      ([],[],[],[],0.0,0.0,0.0,0.0,0.0,0.0)
    else
      let tenpai_lst = tenpai_to_kitaiti ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
      let (o_current_tehai,o_agariritu,o_kitaiti) = opt_tenpai_form tenpai_lst in
      let p_lst = List.filter (fun (a,b,c,d,e,f,g) -> d = o_current_tehai) tenpai_lst in
      let t_lst = List.filter (fun (a,b,c,d,e,f,g) -> d <> o_current_tehai) tenpai_lst in
      let p_lst = List.map (fun (a,b,c,d,e,f,g) -> (a,b,c,d,e,f,g,(anzen ary zi_ary a))) p_lst in
      let p_lst = minus_kitaiti t_lst p_lst in
      let p_lst = opt_kitaiti p_lst in
      let max = max_kitaiti p_lst in 
      if max = ([],[],[],[],0.0,0.0,0.0,0.0,0.0,0.0) then 
        max_tenpairitu p_lst
      else
        max


let mode_kokushi ary zi_ary k_lst = 
  let k_lst = tehai_to_anzen ary zi_ary k_lst in
  let m = List.length k_lst in
  let rec loop i tmp = 
    let ((x,y),z) = List.nth k_lst i in
    let ((x',y'),z') = tmp in
    let tmp = 
      if z' < z then
        ((x,y),z)
      else
        tmp
    in
    if i = 0 then
      let (x,_) = tmp in
      x
    else
      loop (i-1) tmp
  in
  if m = 0 then
    (1,Not_hai)
  else
    loop (m-1) (List.hd k_lst)

let mode_somete ary zi_ary tehai s_hai =
  let n_tehai = List.filter (fun (a,b) -> b <> s_hai) tehai in
  let n_tehai = List.filter (fun (a,b) -> a <> 0 ) n_tehai in
  let n_tehai = tehai_to_anzen ary zi_ary n_tehai in
  let m = List.length n_tehai in
  let rec loop i tmp = 
    let ((x,y),z) = List.nth n_tehai i in
    let ((x',y'),z') = tmp in
    let tmp = 
      if z' < z then
        ((x,y),z)
      else
        tmp
    in
    if i = 0 then
      let (x,_) = tmp in
      x
    else
      loop (i-1) tmp
  in
  if m = 0 then
    (1,Not_hai)
  else
    loop (m-1) (List.hd n_tehai)

(*anzen_baseの処理未実装*)
let mode_titoi ary zi_ary t_lst tehai = 
  let t_lst = tehai_to_anzen ary zi_ary t_lst in
  let m = List.length t_lst in
  let rec loop i tmp = 
    let ((x,y),z) = List.nth t_lst i in
    let ((x',y'),z') = tmp in
    let tmp = 
      if z' < z then
        ((x,y),z)
      else
        tmp
    in
    if i = 0 then
      let (x,_) = tmp in
      x
    else
      loop (i-1) tmp
  in
  let k_hai = 
    if m = 0 then
      (1,Not_hai)
    else
      loop (m-1) (List.hd t_lst)
  in
  k_hai


let mode_common_b ary zi_ary sutehai_lst tehai player = 
  let (_,n) = syanten tehai in
  let m = List.length tehai in
  let rec loop i tmp = 
    let x = List.nth tehai i in
    let n_tehai = d_tehai tehai x in 
    let (_,n') = syanten n_tehai in 
    let tmp = 
      if n = n' then
        if anzen_base ary zi_ary n_tehai sutehai_lst player then
          x::tmp
        else
          tmp
      else
        tmp
    in 
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  let k_lst = 
    if m = 0 then
      [] 
    else
      loop (m-1) []
  in
  if k_lst = [] then 
    let n_tehai = tehai_to_anzen ary zi_ary tehai in
    let ((x,y),z) = max_anzen n_tehai in
    (x,y)
  else
    let n_tehai = tehai_to_anzen ary zi_ary k_lst in
    let ((x,y),z) = max_anzen n_tehai in
    (x,y)


let common_b ary zi_ary tehai sutehai_lst tumo_len f_lst dora_lst player = 
  let (_,n) = syanten tehai in
  if tumo_len >= n then
    let (k_lst,kind_k) = kind_kokushi tehai in
    let (s_hai,s_count) = somete tehai f_lst in
    let (t_lst,t_count) = titoi_allow tehai f_lst in
    let mode = 
      if kind_k >= 9 then
        Kokushi
      else if s_count >= 9 then
        Some
      else if t_count >= 3 then
        Titoi
      else
        CommonB
    in
    let k_hai = 
      if mode = Kokushi then
        mode_kokushi ary zi_ary k_lst
      else if mode = Some then 
        mode_somete ary zi_ary tehai s_hai
      else if mode = Titoi then
        mode_titoi ary zi_ary t_lst tehai
      else
        mode_common_b ary zi_ary sutehai_lst tehai player
    in
    hai_to_int tehai k_hai
  else
    let yaku_lst = 
      if player = 0 then 
        [[];[Reach];[Reach];[Reach]]
      else if player = 1 then
        [[Reach];[];[Reach];[Reach]]
      else if player = 2 then
        [[Reach];[Reach];[];[Reach]]
      else
        [[Reach];[Reach];[Reach];[]]
    in
    reach_defence ary zi_ary yaku_lst sutehai_lst tehai 


(*true:攻撃的な戦術,false:守備的な戦術*)
let mode_choice count tumo_len = 
  if tumo_len > 15 then
    if count <= 3 then
      true
    else
      false 
  else if tumo_len > 12 then
    if count <= 2 then
      true
    else
      false 
  else if tumo_len > 9 then
    if count <= 1 then
      true
    else
      false  
  else 
    if count = 0 then 
      true
    else
      false

let judge_suzi k_hai ary zi_ary g_lst = 
  let (x',y') = k_hai in 
  let env_suzi = 
    if x' = 0 then 
      true
    else
      let suzi_lst = suzi x' in 
      let suzi_lst = List.map (fun a -> (a,y')) suzi_lst in
      if List.for_all (fun a -> List.exists (fun b -> b = a) g_lst) suzi_lst then 
        true
      else
        false
  in
  if env_suzi = true then 
    true
  else
    let (x,y) = hai_to_ary k_hai in
    let saf_env = 
      if x = 3 then
        same_hai zi_ary.(y)
      else
        anzen_s_ary ary (x,y)
      in
    if saf_env < 10 then 
      true
    else
      false



    
  
  


let judge_reach ary zi_ary tehai sutehai_lst yaku_lst yama_len f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan yaku = 
  let (_,n) = syanten tehai in 
  if n = 0 then 
    let (x,(agariritu,kitaiti)) = tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku dora_lst ary zi_ary in
    if kitaiti > 0.0 then
      x
    else
      reach_defence ary zi_ary yaku_lst sutehai_lst tehai
  else
    let (a,b,c,d,e,f,g,h,i,j) = col_tenpai ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst in
    let a_len = List.length a in 
    if a_len = 0 then      
      reach_defence ary zi_ary yaku_lst sutehai_lst tehai
    else
      let k_hai = List.nth a (a_len - 1) in
      let g_lst = reach_genbutu yaku_lst sutehai_lst tehai in
      if List.exists (fun x -> x = k_hai) g_lst then
        hai_to_int tehai k_hai
      else
        if judge_suzi k_hai ary zi_ary g_lst then 
          hai_to_int tehai k_hai
        else
          reach_defence ary zi_ary yaku_lst sutehai_lst tehai

    




       


  





  
let prob_select sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst = 
  let yaku = List.nth yaku_lst player in
  let reach_q = List.exists (fun a -> List.exists (fun b -> b = Reach || b = Doublereach) a) yaku_lst in
  let (_,n) = syanten tehai in
  let n' = titoi_syanten tehai in 
  let (ary,zi_ary) = create_table sutehai_lst tehai in
  let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in   
  let f_lst = List.nth furo_lst player in
  let rm_wan = (yama_len-14) in
  let tumo_l = (rm_wan)/4 in
  let rm_wan = Int.to_float rm_wan in
  if mode_choice n tumo_l || n = n' && n' < 3 then 
    if List.exists (fun a -> a = Reach || a = Doublereach) yaku = true then
      tumogiri tehai 
    else if reach_q = true then 
      judge_reach ary zi_ary tehai sutehai_lst yaku_lst yama_len f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan yaku 
    else if n = 0 then
      let (x,_) = tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku dora_lst ary zi_ary in
      x
    else 
      let (a,b,c,d,e,f,g,h,i,j) = col_tenpai ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst in
      let a_len = List.length a in
      if a_len = 0 then 
        let a_tehai = tehai_to_anzen ary zi_ary tehai in
        let ((a,b),c) = minimum_anzen a_tehai in
        hai_to_int tehai (a,b)
      else
      let x = List.nth a (a_len-1) in
      hai_to_int tehai x
  else
    if reach_q = true then
      reach_defence ary zi_ary yaku_lst sutehai_lst tehai 
    else
      common_b ary zi_ary tehai sutehai_lst tumo_l f_lst dora_lst player

(*(agariritu,kitaiti),furohai*)
let f_kitaiti p_f_lst tehai f_lst (x,y) ary zi_ary yama_len zi_kaze ba_kaze dora_lst =
  let m = List.length p_f_lst in
  let n = List.length tehai in
  let rm_wan = (yama_len-14) in
  let tumo_l = rm_wan/4 in
  let rm_wan = Int.to_float rm_wan in
  let rec loop i tmp = 
    let (s,(a,(b,c,d))) =List.nth p_f_lst i in
    let n_f_lst = (s,(a,(b,c,d)))::f_lst in
    let (xa,ya) = hai_to_ary (x,y) in
    let n_tehai =
      if s = Minko then
        let n_tehai = List.filter (fun z -> z <> (x,y)) tehai in
        let n_tehai = 
          if (n-2) = List.length n_tehai then
            n_tehai
          else
            add_tehai n_tehai (x,y)
        in
        n_tehai
      else
        if ya = b then
          let n_tehai = d_tehai tehai (c+1,y) in
          let n_tehai = d_tehai n_tehai (d+1,y) in
          n_tehai
        else if ya = c then
          let n_tehai = d_tehai tehai (b+1,y) in
          let n_tehai = d_tehai n_tehai (d+1,y) in
          n_tehai
        else
          let n_tehai = d_tehai tehai (b+1,y) in
          let n_tehai = d_tehai n_tehai (c+1,y) in
          n_tehai
    in
    let (_,n) = syanten n_tehai in
    let kuikae_lst = kuikae (x,y) (s,(a,(b,c,d))) in 
    let (t_ritu,agariritu,total_kitaiti,k_hai) = 
      if n <= 0 then
        let (k_hai,x) = tenpai_to_opt_f n_tehai tumo_l rm_wan n_f_lst zi_kaze ba_kaze true [] dora_lst ary zi_ary kuikae_lst in
        let (kitaiti,agariritu) = x in
        (1.0,agariritu,kitaiti,k_hai)
      else 
        let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti,total_kitaiti) = col_tenpai_f_kuikae ary zi_ary n_tehai yama_len n_f_lst zi_kaze ba_kaze true dora_lst kuikae_lst in
        let k_lst_len = List.length k_lst in 
        if k_lst_len = 0 then 
          (t_ritu,agariritu,total_kitaiti,(1,Not_hai))
        else
          (t_ritu,agariritu,total_kitaiti,(List.nth k_lst (k_lst_len-1)))
    in
    let tmp = ((t_ritu,agariritu,total_kitaiti,k_hai),(s,(a,(b,c,d))))::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop (m-1) []

let min_f_safty lst = 
  let m = List.length lst in 
  let rec loop i tmp = 
    let ((an_pai,anzen),f_hai) = List.nth lst i in
    let ((_,anzen'),_) = tmp in 
    let tmp = 
      if anzen' > anzen then
        ((an_pai,anzen),f_hai)
      else
        tmp 
    in
    if i = 0 then 
      tmp 
    else
      loop (i-1) tmp
  in
  if m = 0 then
    (((1,Not_hai),100),(Minko,(0,(0,0,0))))
  else
    loop (m-1) (List.hd lst) 



let f_safty p_f_lst tehai f_lst (x,y) ary zi_ary  =
  let m = List.length p_f_lst in
  let n = List.length tehai in
  let rec loop i tmp = 
    let (s,(a,(b,c,d))) =List.nth p_f_lst i in
    let (xa,ya) = hai_to_ary (x,y) in
    let n_tehai =
      if s = Minko then
        let n_tehai = List.filter (fun z -> z <> (x,y)) tehai in
        let n_tehai = 
          if (n-2) = List.length n_tehai then
            n_tehai
          else
            add_tehai n_tehai (x,y)
        in
        n_tehai
      else
        if ya = b then
          let n_tehai = d_tehai tehai (c+1,y) in
          let n_tehai = d_tehai n_tehai (d+1,y) in
          n_tehai
        else if ya = c then
          let n_tehai = d_tehai tehai (b+1,y) in
          let n_tehai = d_tehai n_tehai (d+1,y) in
          n_tehai
        else
          let n_tehai = d_tehai tehai (b+1,y) in
          let n_tehai = d_tehai n_tehai (c+1,y) in
          n_tehai
    in
    let (an_pai,anzen) = minimum_anzen (tehai_to_anzen ary zi_ary n_tehai) in 
    let tmp = ((an_pai,anzen),(s,(a,(b,c,d))))::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop (m-1) []





let operate_tenapai_ritu_f ary zi_ary tehai = 
  let tenpai_lst = [([],[],[],tehai)] in
  let (_,n) = syanten tehai in
  let rec loop i tmp =
    let m = List.length tmp in
    let rec loop2 j tmp2 = 
      let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tmp j in
      let tmp2 = 
        if i = (n-1) then
          all_tumo ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai)@tmp2
        else
          (k_fase ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai))@tmp2 
      in
      if j = 0 then
        tmp2
      else
        loop2 (j-1) tmp2
    in
    let tmp' = 
      if m = 0 then 
        []
      else
        loop2 (m-1) [] 
      in
    if i <= 0 then
      (tmp,tmp')
    else
      loop (i-1) tmp'
  in
  let (tmp,tmp') = loop (n-1) tenpai_lst in
  let all_t = all_k_fase ary zi_ary tmp in
  let tmp = syanten_to_tenpai ary zi_ary all_t in
  tmp@tmp'

let judge_parallel_f ary zi_ary tehai = 
  let (_,n) = syanten tehai in 
  if n  =  3 then 
    let tenpai_lst = [([],[],[],tehai)] in 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.hd tenpai_lst in
    let tmp = all_tumo ary zi_ary (k_lst,tumo_lst,rest_tumo_lst,current_tehai) in
    (*
    let pool = Task.setup_pool ~num_additional_domains:20 () in
    (*let res = Task.run pool (fun () -> parallel ary zi_ary pool tmp [])  in*)
    let res = Task.run pool (fun () -> parallel ary zi_ary pool tmp)  in
    Task.teardown_pool pool;
    *)
    let res = parallel ary zi_ary tmp in
    res
  else
    operate_tenapai_ritu_f ary zi_ary tehai

let col_tenpai_f ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst tenpai_lst  = 
  let m = List.length tenpai_lst in
  let rm_wan = yama_len-14 in
  let tumo_l =  rm_wan / 4 in
  let rm_wan = Int.to_float rm_wan in
  let rec loop i tmp = 
    let (k_lst,tumo_lst,rest_tumo_lst,current_tehai) = List.nth tenpai_lst i in
    let t_ritu = tenpai_ritu rest_tumo_lst tumo_l rm_wan in 
    let tmp = (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu)::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    ([],[],[],[],0.0,0.0,0.0,0)
  else
    let tenpai_lst = loop (m-1) [] in
    let tenpai_lst = tenpai_to_kitaiti ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
    let (o_current_tehai,o_agariritu,o_kitaiti) = opt_tenpai_form tenpai_lst in
    let p_lst = List.filter (fun (a,b,c,d,e,f,g) -> d = o_current_tehai) tenpai_lst in
    let p_lst = List.map (fun (a,b,c,d,e,f,g) -> (a,b,c,d,e,f,g,(anzen_f ary zi_ary a))) p_lst in
    max_agariritu p_lst
        
let max_f_agariritu lst = 
  let m = List.length lst in 
  let rec loop i tmp = 
    let ((a,x,y),z) = List.nth lst i in
    let ((a',x',y'),_) = tmp in 
    let tmp = 
      if x < x' then 
        tmp
      else
        ((a,x,y),z) 
    in
    if i = 0 then 
      tmp 
    else
      loop (i-1) tmp 
  in
  if m = 0 then 
    ((0.0,0.0,0.0),(Minko,(0,(0,0,0))))
  else
    loop (m-1) ((0.0,0.0,0.0),(Minko,(0,(0,0,0))))

let max_f_agariritu_a lst = 
  let m = List.length lst in 
  let rec loop i tmp = 
    let ((a,x,y,z),b) = List.nth lst i in
    let ((a',x',y',z'),_) = tmp in 
    let tmp = 
      if x < x' then 
        tmp
      else
        ((a,x,y,z),b) 
    in
    if i = 0 then 
      tmp 
    else
      loop (i-1) tmp 
  in
  if m = 0 then 
    ((0.0,0.0,0.0,(1,Not_hai)),(Minko,(0,(0,0,0))))
  else
    loop (m-1) ((0.0,0.0,0.0,(1,Not_hai)),(Minko,(0,(0,0,0))))

let tenpai_hai_lst n_tehai = 
  let m = List.length n_tehai in 
  let rec loop i tmp = 
    let (x,y) = List.nth n_tehai i in 
    let tehai = d_tehai n_tehai (x,y) in 
    let (_,n) = syanten tehai in
    let tmp = 
      if n = 0 then
        (x,y)::tmp 
      else
        tmp
    in
    if i = 0 then 
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then 
    []
  else
    loop (m-1) []




let keiten tehai sutehai_lst f_lst p_f_lst yama_len (x,y) yaku_lst ary zi_ary = 
  let m = List.length p_f_lst in 
  let n = List.length tehai in
  let rm_wan = (yama_len-14) in
  let tumo_l = rm_wan/4 in
  let rec loop i tmp = 
    let (s,(a,(b,c,d))) =List.nth p_f_lst i in
    let (xa,ya) = hai_to_ary (x,y) in
    let n_tehai =
      if s = Minko then
        let n_tehai = List.filter (fun z -> z <> (x,y)) tehai in
        let n_tehai = 
          if (n-2) = List.length n_tehai then
            n_tehai
          else
            add_tehai n_tehai (x,y)
        in
        n_tehai
      else
        if ya = b then
          let n_tehai = d_tehai tehai (c+1,y) in
          let n_tehai = d_tehai n_tehai (d+1,y) in
          n_tehai
        else if ya = c then
          let n_tehai = d_tehai tehai (b+1,y) in
          let n_tehai = d_tehai n_tehai (d+1,y) in
          n_tehai
        else
          let n_tehai = d_tehai tehai (b+1,y) in
          let n_tehai = d_tehai n_tehai (c+1,y) in
          n_tehai
    in
    let (_,n) = syanten n_tehai in
    let tenpai_lst = 
      if n = 0 then
        tenpai_hai_lst n_tehai
      else 
        []
    in
    let tmp = 
      if tenpai_lst = [] then 
          tmp
      else
        let genbutu_lst = reach_genbutu yaku_lst sutehai_lst tenpai_lst in 
        let tenpai_lst = tehai_to_anzen ary zi_ary tenpai_lst in (*((int*hai)*kikendo)*)
        let kikendo = reach_genbutu_anzen_zero tenpai_lst genbutu_lst in 
        let min_den = List.hd (List.sort (fun (_,a) (_,b) -> if a < b then -1 else 1) kikendo) in
        let ((a1,b1),c1) = tmp in 
        let (a2,b2) = min_den in 
        if b1 > b2 then 
                (min_den,(s,(a,(b,c,d))))
        else
                tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 || tumo_l > 6 then
    (((1,Not_hai),100),(Minko,(0,(0,0,0))))
  else
    loop (m-1) (((1,Not_hai),100),(List.hd p_f_lst))


(*deebug*)
(*
let purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst =
  let (_,n) = syanten tehai in
  let reach_q = List.exists (fun a -> List.exists (fun b -> b = Reach || b = Doublereach) a) yaku_lst in
  let n' = kokushi_syanten tehai in
  let tn = titoi_syanten tehai in
  let tumo_len = (yama_len-14)/4 in
  if n = 0 || tumo_len = 0 || n = n' then
    nofuro()
  else if n > 3 || n = tn && tn >= 3 then 
    nofuro ()
  else 
    let (x,y) = ary_to_hai (x,y) in
    let (ary,zi_ary) = create_table sutehai_lst tehai in
    let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in
    let p_f_lst = possible_furo_patern tehai (x,y) in
    let f_kitaiti_lst = f_kitaiti p_f_lst tehai (List.nth furo_lst player) (x,y) ary zi_ary yama_len zi_kaze ba_kaze dora_lst in
    let tenpai_lst = judge_parallel_f ary zi_ary tehai in
    if tenpai_lst = [] then 
      nofuro()
    else if reach_q = false then
      let not_naki = col_tenpai_f ary zi_ary tehai yama_len (List.nth furo_lst player) zi_kaze ba_kaze naki dora_lst tenpai_lst in
      if f_kitaiti_lst = [] then 
        nofuro()
      else
        let ((f_t_ritu,f_agariritu,f_kitaiti,k_hai),f_hai) = max_f_agariritu_a f_kitaiti_lst in  
        let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo) = not_naki in
        let _ = 
          if (f_agariritu -. agariritu) > 0.0 && f_kitaiti > 0.0 && agariritu > 0.0 then
            ((*let (x',y') = hai_to_ary (x,y) in
                            Printf.printf "%d,%d " x' y'; flush stdout; print_list tehai;*)
              (*Printf.printf "%d %f %f %f %f\n" tumo_len (f_agariritu -. agariritu) (kitaiti -. f_kitaiti) agariritu f_kitaiti; flush stdout;*))
          else
            let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in 
            if k_hai = (1,Not_hai) then 
                   (*let (x',y') = hai_to_ary (x,y) in
                            Printf.printf "%d,%d " x' y'; flush stdout; print_list tehai;*)
                     (Printf.printf "%d %f %f\n" tumo_len f_t_ritu (f_t_ritu -. t_ritu); flush stdout;)
            else
                    ()
              (*Printf.printf "%d %d\n" tumo_len den; flush stdout;*)
        in
        nofuro()
    else 
      let haitei_s = haitei_slide (yama_len - 14) yaku_lst player in
      if haitei_s = true && (yama_len-14) < 4 then
        let ((an_pai,anzen),_) = min_f_safty (f_safty p_f_lst tehai (List.nth furo_lst player) (x,y) ary zi_ary) in 
        (*Printf.printf "a:%d\n" anzen; flush stdout;*)
        nofuro ()
      else
        let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in
        let _ = 
          if k_hai = (1,Not_hai) then 
            ()          
          else
                  ()
            (*Printf.printf "%d %d\n" tumo_len den; flush stdout;*)
        in
        nofuro ()
*)

let threthhold_furo_10 agariritu kitaiti tumo_len = 
  if tumo_len > 12 then 
    if (2100.0 *. agariritu *. agariritu -. 7000.0) > kitaiti then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (3700.0 *. agariritu *. agariritu -. 3800.0) > kitaiti then 
      true
    else
      false
  else
    if (5300.0 *. agariritu *. agariritu -. 2400.0) > kitaiti then 
      true
    else
      false

let threthhold_furo_15 agariritu kitaiti tumo_len = 
  if tumo_len > 12 then 
    if (4500.0 *. agariritu *. agariritu -. 1600.0) > kitaiti then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (8800.0 *. agariritu *. agariritu -. 1400.0) > kitaiti then 
      true
    else
      false
  else
    if (3300.0 *. agariritu *. agariritu -. 300.0) > kitaiti then 
      true
    else
      false

let threthhold_furo_20 agariritu kitaiti tumo_len = 
  if tumo_len > 12 then 
    if (14000.0 *. agariritu *. agariritu -. 800.0) > kitaiti then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (9600.0 *. agariritu *. agariritu -. 200.0) > kitaiti then 
      true
    else
      false
  else
    if (10000.0 *. agariritu *. agariritu -. 200.0) > kitaiti then 
      true
    else
      false

let threthhold_furo_25 agariritu kitaiti tumo_len = 
  if tumo_len > 12 then 
    if (16000.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (16000.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else
      false
  else
    if (44000.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else
      false

let threthhold_furo_30 agariritu kitaiti tumo_len = 
  if tumo_len > 12 then 
    if (34000.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (44400.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else
      false
  else
    true


let threthhold_furo_35 agariritu kitaiti tumo_len = 
  if tumo_len > 12 then 
    if (48000.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (67000.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else
      false
  else
    true


let furoritu_to_furo furoritu agariritu kitaiti tumo_len = 
  if furoritu >= 32.5 then 
    threthhold_furo_35 agariritu kitaiti tumo_len
  else if furoritu >= 27.5 then 
    threthhold_furo_30 agariritu kitaiti tumo_len
  else if furoritu >= 22.5 then 
    threthhold_furo_25 agariritu kitaiti tumo_len
  else if furoritu >= 17.5 then 
    threthhold_furo_20 agariritu kitaiti tumo_len
  else if furoritu >= 12.5 then 
    threthhold_furo_15 agariritu kitaiti tumo_len
  else if furoritu >= 7.5 then 
    threthhold_furo_10 agariritu kitaiti tumo_len
  else
    false

let threthhold_keiten_15 f_t_ritu tumo_len = 
  if tumo_len < 6 then 
    if f_t_ritu >= 0.75 then 
      true
    else
      false
  else
    false

let threthhold_keiten_20 f_t_ritu tumo_len = 
  if tumo_len < 6 then 
    if f_t_ritu >= 0.50 then 
      true
    else
      false
  else
    false

let threthhold_keiten_25 f_t_ritu tumo_len = 
  if tumo_len < 7 then 
    if f_t_ritu >= 0.40 then 
      true
    else
      false
  else
    false

let threthhold_keiten_30 f_t_ritu tumo_len = 
  if tumo_len < 7 then 
    if f_t_ritu >= 0.20 then 
      true
    else
      false
  else
    false

let threthhold_keiten_35 f_t_ritu tumo_len = 
  if tumo_len < 7 then 
    if f_t_ritu >= 0.20 then 
      true
    else
      false
  else if tumo_len < 10 then 
    if f_t_ritu <= 0.5 then 
      true 
    else
      false
  else
    false


let furoritu_to_keiten furoritu f_t_ritu tumo_len = 
  if furoritu >= 32.5 then 
    threthhold_keiten_35 f_t_ritu tumo_len
  else if furoritu >= 27.5 then 
    threthhold_keiten_30 f_t_ritu tumo_len
  else if furoritu >= 22.5 then 
    threthhold_keiten_25 f_t_ritu tumo_len
  else if furoritu >= 17.5 then 
    threthhold_keiten_20 f_t_ritu tumo_len
  else if furoritu >= 12.5 then 
    threthhold_keiten_15 f_t_ritu tumo_len
  else
    false


(*automatic*)
let purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst furoritu_lst =
  let (_,n) = syanten tehai in
  let reach_q = List.exists (fun a -> List.exists (fun b -> b = Reach || b = Doublereach) a) yaku_lst in
  let n' = kokushi_syanten tehai in
  let tn = titoi_syanten tehai in
  let tumo_len = (yama_len-14)/4 in
  if n = 0 || tumo_len = 0 || n = n' then
    []
  else if n > 3 || n = tn && tn >= 3 then 
    []
  else 
    let (x,y) = ary_to_hai (x,y) in
    let (ary,zi_ary) = create_table sutehai_lst tehai in
    let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in
    let p_f_lst = possible_furo_patern tehai (x,y) in
    let f_kitaiti_lst = f_kitaiti p_f_lst tehai (List.nth furo_lst player) (x,y) ary zi_ary yama_len zi_kaze ba_kaze dora_lst in
    let tenpai_lst = judge_parallel_f ary zi_ary tehai in
    if tenpai_lst = [] then 
      []
    else if reach_q = false then
      let not_naki = col_tenpai_f ary zi_ary tehai yama_len (List.nth furo_lst player) zi_kaze ba_kaze naki dora_lst tenpai_lst in
      if f_kitaiti_lst = [] then 
        []
      else
        let ((f_t_ritu,f_agariritu,f_kitaiti,k_hai),f_hai) = max_f_agariritu_a f_kitaiti_lst in  
        let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo) = not_naki in
        if naki = false then 
          if (f_agariritu -. agariritu) > 0.0 && furoritu_to_furo (List.nth furoritu_lst player) (f_agariritu -. agariritu) (kitaiti -.f_kitaiti) tumo_len && k_hai <> (1,Not_hai) then
            [(k_hai,f_hai)]
          else
            let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in 
            if k_hai = (1,Not_hai) || den > 5 then 
              if furoritu_to_keiten (List.nth furoritu_lst player) f_t_ritu tumo_len && (f_t_ritu -. t_ritu) >= 0.0 && k_hai <> (1,Not_hai) then
                [(k_hai,f_hai)]
              else
                []
            else
              [(k_hai,f_hai)]
        else if (f_agariritu -. agariritu) > 0.0 && k_hai <> (1,Not_hai) then 
          [(k_hai,f_hai)]
        else
          let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in 
          if k_hai = (1,Not_hai) || den > 5 then 
            if furoritu_to_keiten (List.nth furoritu_lst player) f_t_ritu tumo_len && (f_t_ritu -. t_ritu) >= 0.0 && k_hai <> (1,Not_hai) then
              [(k_hai,f_hai)]
            else
              []
          else
            [(k_hai,f_hai)]

    else 
      let haitei_s = haitei_slide (yama_len - 14) yaku_lst player in
      if haitei_s = true && (yama_len-14) < 4 then
        []
      else
        let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in
        if k_hai = (1,Not_hai) || den > 5 then 
          []
        else
          [(k_hai,f_hai)]
        










