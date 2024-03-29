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

let d_suzi (lst:(int*int) list) d_lst = 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | x::t -> let tmp = if List.exists (fun a -> a = x) d_lst then
                          tmp
                        else
                          x::tmp
                        in
              loop tmp t 
  in
  loop [] lst


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
(*
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


*)

(*
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
*)



(*

let serch_kanzen_anpai_su ary =
  let (m_lst,p_lst,s_lst) = serch_kabe_to_suzi ary in
  let m_lst = serch_kanzen_anpai_2 m_lst ary.(0) in
  let p_lst = serch_kanzen_anpai_2 p_lst ary.(1) in
  let s_lst = serch_kanzen_anpai_2 s_lst ary.(2) in
  let m_lst = List.map (fun a -> (a,Manzu)) m_lst in
  let p_lst = List.map (fun a -> (a,Pinzu)) p_lst in
  let s_lst = List.map (fun a -> (a,Souzu)) s_lst in
  m_lst@p_lst@s_lst
*)
(*
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
        (tapl_player sutehai_lst i)::lst_tmp
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
*)
(*
let serch_kanzen_anpai ary zi_ary sutehai_lst player = 
  let lst1 = serch_kanzen_anpai_zi zi_ary in
  let lst2 = serch_kanzen_anpai_su ary in
  let lst3 = sutehai_common_hai ary zi_ary sutehai_lst player in
  lst1@lst2@lst3
*)
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
    let o_n1 = if y = 2 then 0 else n1*n2 in
    let o_n2 = n2*n3 in
    let o_n3 = if y = 6 then 0 else n3*n4 in
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

let anzen_s_ary_furiten ary (x,y) furiten_lst = 
  if y >= 2 && y <= 6 then
    let n1 = ary.(x).(y-2) in
    let n2 = ary.(x).(y-1) in
    let n3 = ary.(x).(y+1) in
    let n4 = ary.(x).(y+2) in
    let o_n1 = n1*n2 in
    let o_n1 = if y = 2 then o_n1 else if List.exists (fun a -> a = (ary_to_hai (x,y-3))) furiten_lst then 0 else o_n1 in  
    let o_n2 = n2*n3 in
    let o_n3 = n3*n4 in
    let o_n3 = if y = 6 then o_n3 else if List.exists (fun a -> a = (ary_to_hai (x,y+3))) furiten_lst then 0 else o_n1 in 
    let o_n4 = same_hai ary.(x).(y) in
    o_n1 + o_n2 + o_n3 + o_n4
  else if y = 1 then
    let n2 = ary.(x).(y-1) in
    let n3 = ary.(x).(y+1) in
    let n4 = ary.(x).(y+2) in
    let o_n2 = n2*n3 in
    let o_n3 = n3*n4 in
    let o_n3 = if List.exists (fun a -> a = (ary_to_hai (x,y+3))) furiten_lst then 0 else o_n3 in 
    let o_n4 = same_hai ary.(x).(y) in
    o_n2 + o_n3 + o_n4
  else if y = 0 then
    let n3 = ary.(x).(y+1) in
    let n4 = ary.(x).(y+2) in
    let o_n3 = n3*n4 in
    let o_n3 = if List.exists (fun a -> a = (ary_to_hai (x,y+3))) furiten_lst then 0 else o_n3 in 
    let o_n4 = same_hai ary.(x).(y) in
    o_n3 + o_n4
  else if y = 7 then
    let n1 = ary.(x).(y-2) in
    let n2 = ary.(x).(y-1) in
    let n3 = ary.(x).(y+1) in
    let o_n1 = n1*n2 in
    let o_n1 = if List.exists (fun a -> a = (ary_to_hai (x,y-3))) furiten_lst then 0 else o_n1 in  
    let o_n2 = n2*n3 in
    let o_n4 = same_hai ary.(x).(y) in
    o_n1 + o_n2 + o_n4
  else
    let n1 = ary.(x).(y-2) in
    let n2 = ary.(x).(y-1) in
    let o_n1 = n1*n2 in
    let o_n1 = if List.exists (fun a -> a = (ary_to_hai (x,y-3))) furiten_lst then 0 else o_n1 in  
    let o_n4 = same_hai ary.(x).(y) in
    o_n1 + o_n4

let max_anzen lst = 
  let m = List.length lst in
  let rec loop i tmp = 
    let ((a,b),c) = List.nth lst i in
    let ((a',b'),c') = tmp in
    let tmp = 
      if c' < c then
        ((a,b),c)
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    ((0,Not_hai),-1)
  else
    loop (m-1) ((0,Not_hai),-1)


let minimum_anzen lst = 
  let m = List.length lst in
  let rec loop i tmp = 
    let ((a,b),c) = List.nth lst i in
    let (_,c') = tmp in
    let tmp = 
      if c' > c then
        ((a,b),c)
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    ((0,Not_hai),100)
  else
    loop (m-1) ((0,Not_hai),100)

   


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

let anzen_f ary zi_ary k_lst = 
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
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    0
  else
    loop (m-1) 0

let tehai_to_anzen ary zi_ary tehai = 
  let tehai = rhai_to_hai tehai in
  let m = List.length tehai in
  let rec loop i tmp = 
    let (x,y) = List.nth tehai i in
    let (x',y') = hai_to_ary (x,y) in
    let tmp = 
      if x' = 3 then
        ((x,y),same_hai zi_ary.(y'))::tmp
      else
        ((x,y),(anzen_s_ary ary (x',y')))::tmp
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

let furiten_lst_to_comp new_furiten_lst = 
  let new_furiten_lst = List.map rhai_to_hai new_furiten_lst in 
  let new_furiten_lst_len = List.length new_furiten_lst in 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | h::t -> if List.for_all (fun lst -> List.exists (fun a -> a = h) lst) new_furiten_lst then 
                loop (h::tmp) t 
              else
                loop tmp t 
  in
  if new_furiten_lst_len = 1 then 
    List.hd new_furiten_lst
  else
    loop [] (List.hd new_furiten_lst)


let tehai_to_anzen_in_reach ary zi_ary tehai yaku_lst furiten_lst = 
  let tehai = rhai_to_hai tehai in  
  let rec furiten_lst_loop tmp i = match i with
    | -1 -> tmp 
    | _ -> let yaku = tapl_player yaku_lst i in 
           if yaku = [] then 
              furiten_lst_loop tmp (i-1)
           else
              let furiten = tapl_player furiten_lst i in 
              furiten_lst_loop (furiten::tmp) (i-1) 
  in
  let new_furiten_lst = furiten_lst_loop [] 3 in 
  let new_furiten_lst = furiten_lst_to_comp new_furiten_lst in 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (x,y)::t -> let (x',y') = hai_to_ary (x,y) in
                  let tmp = if x' = 3 then 
                             if List.exists (fun a -> a = (x,y)) new_furiten_lst then 
                              ((x,y),0)::tmp
                             else
                              ((x,y),same_hai zi_ary.(y'))::tmp
                          else
                            if List.exists (fun a -> a = (x,y)) new_furiten_lst then 
                              ((x,y),0)::tmp
                             else
                              ((x,y),(anzen_s_ary_furiten ary (x',y') new_furiten_lst))::tmp
                  in
                  loop tmp t
  in
  loop [] tehai


(*((int*hai)*int) List -> ((int*hai)*int*bool List) List*)    
let kyoutu_anpai sutehai_lst (tehai:((int*hai)*int)list) player =
  let tehai_len = List.length tehai in
  let rec loop i tmp_s = 
    let ((x,y),z) = List.nth tehai i in
    let kyoutu_lst = 
      if player = 0 then
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 3) then
            [true]
          else
            [false]
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 2) then
            true::tmp
          else
            false::tmp
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 1) then
            true::tmp
          else
            false::tmp
        in
        tmp
      else if player = 1 then
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 3) then
            [true]
          else
            [false]
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 2) then
            true::tmp
          else
            false::tmp
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 0) then
            true::tmp
          else
            false::tmp
        in
        tmp
      else if player = 2 then
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 3) then
            [true]
          else
            [false]
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 1) then
            true::tmp
          else
            false::tmp
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 0) then
            true::tmp
          else
            false::tmp
        in
        tmp
      else 
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 2) then
            [true]
          else
            [false]
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 1) then
            true::tmp
          else
            false::tmp
        in
        let tmp = 
          if List.exists (fun (a,b,c) -> (a,b) = (x,y)) (tapl_player sutehai_lst 0) then
            true::tmp
          else
            false::tmp
        in
        tmp
    in
    let tmp_s = ((x,y),z,kyoutu_lst)::tmp_s in
    if i = 0 then
      tmp_s
    else
      loop (i-1) tmp_s
    in
    loop (tehai_len -1) []

let bool_kyoutu_anpai n_tehai = 
  let m = List.length n_tehai in
  let rec loop i tmp = 
    let ((_,_),_,a) = List.nth n_tehai i in
    let n1 = List.nth a 2 in
    let n2 = List.nth a 1 in
    let n3 = List.nth a 0 in
    let (s3,s2,s1) = tmp in
    let s1 = if n1 = true then true else s1 in
    let s2 = if n2 = true then true else s2 in
    let s3 = if n3 = true then true else s3 in
    let tmp = (s3,s2,s1) in
    if i = 0 then 
      tmp
    else
      loop (i-1) tmp
  in
  if m = 0 then
    false
  else
    let kyoutu_a = loop (m-1) (false,false,false) in
    if kyoutu_a = (true,true,true) then
      true
    else
      false

let second_anzen n_tehai = 
  let m = List.length n_tehai in
  let rec loop i m4 m10 = 
    let ((_,_),a,_) = List.nth  n_tehai i in
    let m4 = 
      if a <= 4 then
        m4 + 1 
      else
        m4
    in
    let m10 = 
      if a <= 10 then
        m10 + 1
      else
        m10
    in
    if i = 0 then
      (m4,m10)
    else
      loop (i-1) m4 m10
  in
  if m = 0 then
    false
  else
    let (m4,m10) = loop (m-1) 0 0 in
    if m4 >=1 && m10 >= 2 then
      true
    else
      false



(*sutehai, tehai, anpai_lst*)
let player_anpai (sutehai:(int*hai*bool)list) tehai =
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

(*sutehai, tehai, anpai_lst*)
let player_anpai_furiten (sutehai:(int*hai)list) tehai =
  let m = List.length tehai in
  let rec loop i tmp = 
    let x = List.nth tehai i in
    let tmp =
      if List.exists (fun (a,b) -> if (a,b) = x then true else false) sutehai then
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


let other_reach yaku_lst sutehai_lst tehai =  
  let rec loop i tmp = 
    let reach = tapl_player yaku_lst i in 
    let tmp = 
      if reach <> [] then
        player_anpai (tapl_player sutehai_lst i) tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  loop 3 tehai

(*立直者の現物を危険度0として((int*hai)*int)listを返す*)
let reach_genbutu_anzen_zero (tehai:((int*hai)*int)list) genbutu_lst = 
  let m = List.length tehai in 
  let rec loop i tmp = 
    let (x,y) = List.nth tehai i in 
    let tmp = 
      if List.exists (fun a -> a = x) genbutu_lst then 
        (x,0)::tmp
      else
        (x,y)::tmp 
    in
    if i = 0 then 
      tmp
    else
      loop (i-1) []
  in
  if m = 0 then 
    []
  else
    loop (m-1) [] 



let reach_genbutu yaku_lst sutehai_lst tehai = 
  let (sute_a,sute_b,sute_c,sute_d) = sutehai_lst in 
  let sutehai_lst = ((rhai_to_hai sute_a),(rhai_to_hai sute_b),(rhai_to_hai sute_c),(rhai_to_hai sute_d)) in 
  let tehai = rhai_to_hai tehai in 
  let rec r_loop i tmp =
    let tmp = 
      if List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player yaku_lst i) then
        i::tmp
      else
        tmp
    in
    if i = 0 then 
      tmp 
    else
      r_loop (i-1) tmp 
  in
  let r_lst = r_loop 3 [] in
  let r_lst_len = List.length r_lst in 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | player::t ->  let sutehai = tapl_player sutehai_lst player in
                    let lst = player_anpai_furiten sutehai tehai in
                    let tmp =  
                      let rec loop' tmp2 t_lst2 = match t_lst2 with 
                        | [] -> tmp2
                        | h::t -> let tmp2 = 
                                    if List.exists (fun x -> x = h) lst then 
                                      h::tmp2 
                                    else 
                                      tmp2 
                                  in 
                                    loop' tmp2 t
                      in
                      loop' [] tmp
                    in
      loop tmp t
  in
  if r_lst_len = 0 then 
    tehai
  else
    let sutehai = tapl_player sutehai_lst (List.hd r_lst) in
    let lst = player_anpai_furiten sutehai tehai in
    loop lst r_lst





let reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst = 
  let n_tehai = reach_genbutu yaku_lst furiten_lst tehai in
  if n_tehai = [] then
    let a_tehai = tehai_to_anzen_in_reach ary zi_ary tehai yaku_lst furiten_lst in
    let ((a,b),c) = minimum_anzen a_tehai in
    hai_to_int tehai (a,b)
  else
    let a_lst = tehai_to_anzen ary zi_ary n_tehai in
    let ((a,b),c) = max_anzen a_lst in
    hai_to_int tehai (a,b)

let anzen_base ary zi_ary tehai sutehai_lst player = 
  let n_tehai1 = tehai_to_anzen ary zi_ary tehai in
  let n_tehai2 = kyoutu_anpai sutehai_lst n_tehai1 player in
  let x = 
    if List.exists (fun ((a,b),c,d) -> c <= 1) n_tehai2 then
      true
    else
      bool_kyoutu_anpai n_tehai2
  in
  if x = true then
    second_anzen n_tehai2
  else
    false


let furo_judge furo_lst sutehai_lst tehai = 
 let rec loop tmp i = match i with 
  | -1 -> tmp
  | _ -> 
    let lst = tapl_player furo_lst i in
    let m = List.length lst in 
    let tmp = 
      if m > 2 then 
        player_anpai (tapl_player sutehai_lst i) tmp
      else
        tmp
    in
    loop tmp (i-1)
  in
  loop tehai 0 
                


let furo_defence ary zi_ary yaku_lst sutehai_lst furo_lst tehai furiten_lst = 
  let n_tehai = reach_genbutu yaku_lst furiten_lst tehai in 
  let n_tehai = furo_judge furo_lst sutehai_lst n_tehai in 
  if n_tehai = tehai then
    -1
  else if n_tehai = [] then
    let a_tehai = tehai_to_anzen ary zi_ary tehai in
    let ((a,b),c) = minimum_anzen a_tehai in
    hai_to_int tehai (a,b)
  else
    let a_lst = tehai_to_anzen ary zi_ary n_tehai in
    let ((a,b),c) = max_anzen a_lst in
    hai_to_int tehai (a,b)



