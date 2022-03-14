open Mahjong_base


let serch_kanzen_anpai_zi zi_ary = 
  let rec loop i tmp = 
    let n = zi_ary.(i) in
    let tmp = 
      if n = 0 then
        (ary_to_hai (3,i))::tmp
      else
        tmp  
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  loop 6 []

let d_suzi lst d_lst = 
  let m = List.length lst in
  let rec loop i tmp = 
    let x = List.nth lst i in
    let tmp = 
      if List.exists (fun a -> a = x) d_lst then
        tmp
      else
        x::tmp
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


let serch_kabe_to_suzi ary = 
  let m_ary = ary.(0) in
  let p_ary = ary.(1) in
  let s_ary = ary.(2) in
  let rec loop a i tmp = 
    let n = a.(i) in
    let tmp = 
      if n = 0 then
        (i+1)::tmp
      else
        tmp  
    in
    if i = 0 then
      tmp
    else
      loop a (i-1) tmp
  in
  let m = loop m_ary 8 [] in
  let p = loop p_ary 8 [] in 
  let s = loop s_ary 8 [] in 
  let m_safty = List.fold_left (fun x y -> x@(kabe_to_deleat_suzi y)) [] m in
  let p_safty = List.fold_left (fun x y -> x@(kabe_to_deleat_suzi y)) [] p in
  let s_safty = List.fold_left (fun x y -> x@(kabe_to_deleat_suzi y)) [] s in
  let m_lst = d_suzi suzi_lst m_safty in
  let p_lst = d_suzi suzi_lst p_safty in
  let s_lst = d_suzi suzi_lst s_safty in
  (m_lst,p_lst,s_lst)

let serch_gukei ary lst = 
  let n = List.length lst in
  let rec loop i g_lst tmp = 
    let (x,y) = List.nth g_lst i in
    let tmp = 
      if ary.(x-1) = 0 || ary.(y-1) = 0 then
        tmp
      else
        false
    in
    if i = 0 then
      tmp
    else
      loop (i-1) g_lst tmp
    in


  let rec loop2 i tmp = 
    let x = List.nth lst i in
    let tmp = 
      if x <> 1 && x <> 9 then
        let g_lst = List.find (fun (a,b) -> a = x) gukei_lst in
        let (_,g_lst) = g_lst in
        let jud = loop ((List.length g_lst) - 1) g_lst true in
        if jud = true then
          x::tmp
        else
          tmp
      else
        x::tmp
    in
    if i = 0 then
      tmp
    else
      loop2 (i-1) tmp
  in
  if n = 0 then
    []
  else
    loop2 (n-1) []





(*rest of suzi list -> array -> anpai lst*)
let serch_kanzen_anpai_2 lst ary = 
  let rec loop i tmp = 
    let n = ary.(i) in
    let tmp = 
      if n = 0 then
        (i+1)::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  let new_lst = loop 8 [] in
  let n = List.length new_lst in
  let rec loop2 i tmp = 
    let a = List.nth new_lst i in
    let tmp = 
      if (List.exists (fun (x,y) -> x = a || y = a) lst) = false then
        a::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop2 (i-1) tmp
  in
  let t_lst = 
    if n = 0 then
      []
    else
      loop2 (n-1) []
  in
  serch_gukei ary t_lst






let serch_kanzen_anpai_su ary =
  let (m_lst,p_lst,s_lst) = serch_kabe_to_suzi ary in
  let m_lst = serch_kanzen_anpai_2 m_lst ary.(0) in
  let p_lst = serch_kanzen_anpai_2 p_lst ary.(1) in
  let s_lst = serch_kanzen_anpai_2 s_lst ary.(2) in
  let m_lst = List.map (fun a -> (a,Manzu)) m_lst in
  let p_lst = List.map (fun a -> (a,Pinzu)) p_lst in
  let s_lst = List.map (fun a -> (a,Souzu)) s_lst in
  m_lst@p_lst@s_lst

(*捨て牌から全員が捨てているものをlistで返す*)
let sutehai_common_hai ary zi_ary sutehai_lst player = 
  let rec loop i j tmp = 
    let n = ary.(i).(j) in
    let tmp = 
      if n <= 1 then
        ary_to_hai (i,j)::tmp
      else
        tmp
    in
    if j = 0 then
      if i = 0 then
        tmp
      else
        loop (i-1) 8 tmp
    else
      loop i (j-1) tmp
  in
  let rec loop_zi i tmp = 
    let n = zi_ary.(i) in
    let tmp = 
      if n <= 1 then
        ary_to_hai (3,i)::tmp
      else
        tmp
      in
    if i = 0 then
      tmp
    else
      loop_zi (i-1) tmp
  in

  let lst = (loop 2 8 [])@(loop_zi 6 []) in
  let rec loop2 i lst_tmp = 
    let lst_tmp =
      if player = i then
        lst_tmp
      else
        (List.nth sutehai_lst i)::lst_tmp
    in
    if i = 0 then
      lst_tmp
    else
      loop2 (i-1) lst_tmp
  in
  let n_stehai_lst = loop2 3 [] in
  let sutehai_lst_1 = List.nth n_stehai_lst 0 in
  let sutehai_lst_2 = List.nth n_stehai_lst 1 in
  let sutehai_lst_3 = List.nth n_stehai_lst 2 in

  let rec loop3 i tmp = 
    let x = List.nth lst i in
    let tmp =
      if (List.exists (fun (a,b,c) -> (a,b) = x)sutehai_lst_1) && (List.exists (fun (a,b,c) -> (a,b) = x)sutehai_lst_2) && (List.exists (fun (a,b,c) -> (a,b) = x)sutehai_lst_3) then
        x::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop3 (i-1) tmp
  in
  let n = List.length lst in
  if n = 0 then
    []
  else
    loop3 (n-1) []


let serch_kanzen_anpai ary zi_ary sutehai_lst player = 
  let lst1 = serch_kanzen_anpai_zi zi_ary in
  let lst2 = serch_kanzen_anpai_su ary in
  let lst3 = sutehai_common_hai ary zi_ary sutehai_lst player in
  lst1@lst2@lst3

let same_hai count = match count with
  | 0 -> 0
  | 1 -> 1
  | 2 -> 3
  | _ -> 0

let anzen_s_ary ary (x,y) = 
  if y >= 2 && y <= 6 then
    let n1 = ary.(x).(y-2) in
    let n2 = ary.(x).(y-1) in
    let n3 = ary.(x).(y+1) in
    let n4 = ary.(x).(y+2) in
    let o_n1 = n1*n2 in
    let o_n2 = n2*n3 in
    let o_n3 = n3*n4 in
    let o_n4 = same_hai ary.(x).(y) in
    o_n1 + o_n2 + o_n3 + o_n4
  else if y = 1 then
    let n2 = ary.(x).(y-1) in
    let n3 = ary.(x).(y+1) in
    let n4 = ary.(x).(y+2) in
    let o_n2 = n2*n3 in
    let o_n3 = n3*n4 in
    let o_n4 = same_hai ary.(x).(y) in
    o_n2 + o_n3 + o_n4
  else if y = 0 then
    let n3 = ary.(x).(y+1) in
    let n4 = ary.(x).(y+2) in
    let o_n3 = n3*n4 in
    let o_n4 = same_hai ary.(x).(y) in
    o_n3 + o_n4
  else if y = 7 then
    let n1 = ary.(x).(y-2) in
    let n2 = ary.(x).(y-1) in
    let n3 = ary.(x).(y+1) in
    let o_n1 = n1*n2 in
    let o_n2 = n2*n3 in
    let o_n4 = same_hai ary.(x).(y) in
    o_n1 + o_n2 + o_n4
  else
    let n1 = ary.(x).(y-2) in
    let n2 = ary.(x).(y-1) in
    let o_n1 = n1*n2 in
    let o_n4 = same_hai ary.(x).(y) in
    o_n1 + o_n4




let anzen ary zi_ary k_lst = 
  let m = List.length k_lst in
  let rec loop i tmp = 
    let hai = List.nth k_lst i in
    let (x,y) = hai_to_ary hai in
    let tmp =
      if x = 3 then
        (same_hai zi_ary.(y)) + tmp
      else
        (anzen_s_ary ary (x,y)) + tmp
    in
    if i = 1 then
      tmp
    else
      loop (i-1) tmp
  in
  if m <= 1 then
    0
  else
    loop (m-1) 0

let kyoutu_anpai sutehai_lst tehai player =
  let tehai_len = List.length tehai in
  let rec loop i tmp = 
     let x = List.nth tehai i in
     let p = 
      if player = 0 then
        0
      else
      if List.exists (fun (a,b,c) -> if (a,b) = x then true else false) (List.nth sutehai_lst 0) then
        1
      else
        0
    in
    let p = 
      if player = 1 then
        p
      else
      if List.exists (fun (a,b,c) -> if (a,b) = x then true else false) (List.nth sutehai_lst 1) then
        p + 1
      else
        p
    in
    let p = 
      if player = 0 then
        p
      else
      if List.exists (fun (a,b,c) -> if (a,b) = x then true else false) (List.nth sutehai_lst 0) then
        p+1
      else
        p
    in
    let p = 
      if player = 0 then
        p
      else
      if List.exists (fun (a,b,c) -> if (a,b) = x then true else false) (List.nth sutehai_lst 0) then
        p+1
      else
        p
    in
    let tmp = (x,p)::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
    in
    loop (tehai_len -1) []

let player_anpai sutehai tehai =
  let m = List.length tehai in
  let rec loop i tmp = 
    let x = List.nth tehai i in
    let tmp =
      if List.exists (fun (a,b,c) -> if (a,b) = x then true else false) sutehai then
        x::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if  m = 0 then
    []
  else
    loop (m-1) []


let other_reach yaku_lst sutehai_lst tehai player =  
  let rec loop i tmp = 
    let reach = List.nth yaku_lst i in 
    let tmp = 
      if reach <> [] then
        player_anpai (List.nth sutehai_lst i)
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  loop 3 tehai



