open Mahjong_base
open Loop
open Mahjong_haieff
open Mahjong_safty

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

  let t_lst = loop (m-1) [] in
  if t_lst = [] then
    (13,(0.0,0.0))
  else
    let m = List.length t_lst in
    loop2 (m-1) t_lst (14,(0.0,0.0))
    

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
    let tmp' = loop2 (m-1) [] in
    if i = 0 then
      (tmp,tmp')
    else
      loop (i-1) tmp'
  in
  let (tmp,tmp') = loop (n-1) tenpai_lst in
  let all_t = all_k_fase ary zi_ary tmp in
  let tmp = syanten_to_tenpai ary zi_ary all_t in
  tmp@tmp'



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
  loop tenpai_lst (List.hd tenpai_lst)

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
  let tenpai_lst = operate_tenapai_ritu ary zi_ary tehai in
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
  let tenpai_lst = loop (m-1) [] in
  let tenpai_lst = tenpai_to_kitaiti ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
  let (o_current_tehai,o_agariritu,o_kitaiti) = opt_tenpai_form tenpai_lst in
  let p_lst = List.filter (fun (a,b,c,d,e,f,g) -> d = o_current_tehai) tenpai_lst in
  let t_lst = List.filter (fun (a,b,c,d,e,f,g) -> d <> o_current_tehai) tenpai_lst in
  let p_lst = List.map (fun (a,b,c,d,e,f,g) -> (a,b,c,d,e,f,g,(anzen ary zi_ary a))) p_lst in
  let p_lst = minus_kitaiti t_lst p_lst in
  let p_lst = opt_kitaiti p_lst in
  max_kitaiti p_lst

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
  if mode_choice n tumo_l then 
    if List.exists (fun a -> a = Reach || a = Doublereach) yaku = true then
      tumogiri tehai 
    else if n = 0 then
      let (x,_) = tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku dora_lst ary zi_ary in
      x
    else if n > 3 || (n = n' && n' = 3) then
      if reach_q = true then
        reach_defence ary zi_ary yaku_lst sutehai_lst tehai 
      else
      hai_eff_select sutehai_lst tehai furo_lst yaku_lst player furo_double_lst
    else
      if reach_q = true then
        reach_defence ary zi_ary yaku_lst sutehai_lst tehai 
      else
        let (a,b,c,d,e,f,g,h,i,j) = col_tenpai ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst in
        let a_len = List.length a in
        let x = List.nth a (a_len-1) in
        hai_to_int tehai x
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
    let (agariritu,kitaiti) = 
      if n <= 0 then
        let (_,x) = tenpai_to_opt n_tehai tumo_l rm_wan n_f_lst zi_kaze ba_kaze true [] dora_lst ary zi_ary in
        x
      else 
        let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti,total_kitaiti) = col_tenpai ary zi_ary n_tehai yama_len n_f_lst zi_kaze ba_kaze true dora_lst in
        (agariritu,kitaiti)
    in
    let tmp = ((agariritu,kitaiti),(s,(a,(b,c,d))))::tmp in
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
    let tmp' = loop2 (m-1) [] in
    if i = 0 then
      (tmp,tmp')
    else
      loop (i-1) tmp'
  in
  let (tmp,tmp') = loop (n-1) tenpai_lst in
  let all_t = all_k_fase ary zi_ary tmp in
  let tmp = syanten_to_tenpai ary zi_ary all_t in
  tmp@tmp'



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
  let tenpai_lst = loop (m-1) [] in
  let tenpai_lst = tenpai_to_kitaiti ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
  let (o_current_tehai,o_agariritu,o_kitaiti) = opt_tenpai_form tenpai_lst in
  let p_lst = List.filter (fun (a,b,c,d,e,f,g) -> d = o_current_tehai) tenpai_lst in
  let p_lst = List.map (fun (a,b,c,d,e,f,g) -> (a,b,c,d,e,f,g,(anzen_f ary zi_ary a))) p_lst in
  max_agariritu p_lst
        
let max_f_agariritu lst = 
  let m = List.length lst in 
  let rec loop i tmp = 
    let ((x,y),z) = List.nth lst i in
    let ((x',y'),_) = tmp in 
    let tmp = 
      if x < x' then 
        tmp
      else
        ((x,y),z) 
    in
    if i = 0 then 
      tmp 
    else
      loop (i-1) tmp 
  in
  if m = 0 then 
    ((0.0,0.0),(Minko,(0,(0,0,0))))
  else
    loop (m-1) ((0.0,0.0),(Minko,(0,(0,0,0))))


    
let purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst = 
  let (_,n) = syanten tehai in
  let n' = kokushi_syanten tehai in
  let tumo_len = yama_len/4 in
  if n = 0 || tumo_len = 0 || n = n' then
    nofuro()
  else if n < 3 then
    let (x,y) = ary_to_hai (x,y) in
    let (ary,zi_ary) = create_table sutehai_lst tehai in
    let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in
    let p_f_lst = possible_furo_patern tehai (x,y) in
    let f_kitaiti_lst = f_kitaiti p_f_lst tehai (List.nth furo_lst player) (x,y) ary zi_ary yama_len zi_kaze ba_kaze dora_lst in
    let tenpai_lst = operate_tenapai_ritu_f ary zi_ary tehai in
    if tenpai_lst = [] then 
      nofuro()
    else
      let not_naki = col_tenpai_f ary zi_ary tehai yama_len (List.nth furo_lst player) zi_kaze ba_kaze naki dora_lst tenpai_lst in
      let ((f_agariritu,f_kitaiti),f_hai) = max_f_agariritu f_kitaiti_lst in  
      let (k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo) = not_naki in
      let _ = 
        if (f_agariritu -. agariritu) > 0.0 then
          (Printf.printf "%d %f %f %f %f\n" tumo_len (f_agariritu -. agariritu) (kitaiti -. f_kitaiti) agariritu f_kitaiti;)
        else
          ()
      in
      nofuro()
  else
    nofuro()











