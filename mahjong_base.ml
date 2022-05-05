type furo = Pon | Ti | Kan
type hai = Manzu | Pinzu | Souzu | Ton | Nan | Sya | Pei | Haku | Hatsu | Tyun | Not_hai
type state = Syuntu | Toitu | Anko | Minko | Ankan | Minkan | Ws
type mati_state = Ryanmen | Kantyan | Pentyan | Tanki | Syanpon | Ns
type yaku = Reach | Ippatu | Menzentumo | Yakuhai | Tanyao | Pinhu | Ipeiko | Haitei | Houtei | Rinsyankaihou | Tyankan
            | Doublereach | Sansyokudouzyun | Sansyokudoukou | Sanankou | Ikkitukan | Titoitu | Toitoi | Tyanta | Sankantu | Syousangen
            | Honroutou | Ryanpeikou | Zyuntyan | Honitu | Tinitu | Suankou | Daisangen | Kokusimusou | Ryuiso | Tuiso | Tinroutou
            | Sukantu | Syoususi | Daisusi | Tyurenpoutou | Tihou | Tenhou
type mode_b = Kokushi | Some | Titoi | CommonB


let rename (m, n) = match n with
  | Manzu -> (Int.to_string m) ^ "m" 
  | Pinzu -> (Int.to_string m) ^ "p" 
  | Souzu -> (Int.to_string m) ^ "s"
  | Ton -> "ton" 
  | Nan -> "nan" 
  | Sya -> "sya" 
  | Pei -> "pei" 
  | Haku -> "haku" 
  | Hatsu -> "ryu"
  | Tyun -> "tyun"
  | _ -> "not"             

let print_list lst = 
    List.iter (fun (m,n) -> Printf.printf "%s " (rename (m,n)); ) lst;
    Printf.printf "\n"

let list_to_ary lst = 
  let m = List.length lst in
  let rec loop i ary zi_ary = 
    let (x,hai) = List.nth lst i in
    let _ = 
      if hai = Manzu then
        let n = ary.(0).(x-1) in
        ary.(0).(x-1) <- n+1;
      else if hai = Pinzu then
        let n = ary.(1).(x-1) in
        ary.(1).(x-1) <- n+1;
      else if hai = Souzu then
        let n = ary.(2).(x-1) in
        ary.(2).(x-1) <- n+1;
      else
        if hai = Ton then
          let n = zi_ary.(0) in
          zi_ary.(0) <- n+1;
        else if hai = Nan then
          let n = zi_ary.(1) in
          zi_ary.(1) <- n+1;
        else if hai = Sya then
          let n = zi_ary.(2) in
          zi_ary.(2) <- n+1;
        else if hai = Pei then
          let n = zi_ary.(3) in
          zi_ary.(3) <- n+1;
        else if hai = Haku then
          let n = zi_ary.(4) in
          zi_ary.(4) <- n+1;
        else if hai = Hatsu then
          let n = zi_ary.(5) in
          zi_ary.(5) <- n+1;
        else 
          let n = zi_ary.(6) in
          zi_ary.(6) <- n+1;
      in
      if i = 0 then
        (ary,zi_ary)
      else
        loop (i-1) ary zi_ary
  in
  loop (m-1) (Array.make_matrix 3 9 0) (Array.make 7 0)

let rec add_tehai list (x,y) = match list with
  | [] -> [(x,y)]
  | [(x1,y1)] -> [(x1,y1);(x,y)]
  | h::t -> h::(add_tehai t (x,y))


let ary_to_hai (x,y) = 
  if x = 0 then
    (y+1,Manzu)
  else if x = 1 then
    (y+1,Pinzu)
  else if x = 2 then
    (y+1,Souzu)
  else
    if y = 0 then
      (0,Ton)
    else if y = 1 then
      (0,Nan)
    else if y = 2 then
      (0,Sya)
    else if y = 3 then
      (0,Pei)
    else if y = 4 then
      (0,Haku)
    else if y = 5 then
      (0,Hatsu) 
    else 
      (0,Tyun)

let hai_to_ary (x,y) = 
  if x = 0 then
    if y = Ton then
      (3,0)
    else if y = Nan then
      (3,1)
    else if y = Sya then
      (3,2)
    else if y = Pei then
      (3,3)
    else if y = Haku then
      (3,4)
    else if y = Hatsu then
      (3,5)
    else 
      (3,6)
  else
    if y = Manzu then
      (0,x-1)
    else if y = Pinzu then
      (1,x-1)
    else
      (2,x-1)

let ary_to_hai_ex (x,y) n = 
  let rec loop i tmp = 
    let tmp = (ary_to_hai (x,y))::tmp in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  if n = 0 then
    []
  else
    loop (n-1) []

let ary_to_list ary zi_ary = 
  let rec loop i j tmp = 
    let n =  ary.(i).(j) in
    let tmp =
      (ary_to_hai_ex (i,j) n)@tmp
    in
    if i = 0 then
      if j = 0 then
        tmp
      else
        loop i (j-1) tmp
    else
      if j = 0 then
        loop (i-1) 8 tmp
      else
        loop i (j-1) tmp
  in
  let rec loop2 i tmp = 
    let n =  zi_ary.(i) in
    let tmp =
      (ary_to_hai_ex (3,i) n)@tmp
    in
    if i = 0 then
      tmp
    else
      loop2 (i-1) tmp
  in
  (loop 2 8 [])@(loop2 6 [])
    


let rec d_tehai list (x,y) = match list with
  | [] -> []
  | [(x1,y1)] -> if (x1,y1) = (x,y) then [] else [(x1,y1)]
  | (x1,y1)::t -> if (x1,y1) = (x,y) then t else (x1,y1)::(d_tehai t (x,y))

let hyouzi_to_dora (x,y) = 
  if x <> 3 then
    if y = 8 then
      (x,0)
    else
      (x,y+1)
  else
    if y = 3 || y = 6 then
      if y = 3 then
        (x,0)
      else
        (x,4)
    else
      (x,y+1)

(*数字から切る牌を選択*)
let int_to_hai tehai a = 
  let (x,y) = List.nth tehai a in
  if a = 13 then
    (x,y,true)
  else
    (x,y,false)

let furo_to_hai (a,(b,(c,d,e))) = 
  let x1 = ary_to_hai (b,c) in
  let x2 = ary_to_hai (b,d) in
  let x3 = ary_to_hai (b,e) in
  [x1;x2;x3]

let ripai2 (list:(int*hai)list) hai = 
  let list = List.filter (fun ((x,y)) -> y = hai ) list in 
  let list = List.sort (fun (x1,y1) (x2,y2) -> if x1 < x2 then -1 else 1) list  in
  list

let ripai list = 
  let listm = ripai2 list Manzu in
  let listp = ripai2 list Pinzu in
  let lists = ripai2 list Souzu in
  let listt = ripai2 list Ton in
  let listn = ripai2 list Nan in
  let listsy = ripai2 list Sya in
  let listpe = ripai2 list Pei in
  let listh = ripai2 list Haku in
  let listr = ripai2 list Hatsu in
  let listty = ripai2 list Tyun in
  let list = listm @ listp @ lists @ listt @ listn @ listsy @ listpe @ listh @ listr @ listty in
  list

let hai_to_int tehai (x,y) =
  let m = List.length tehai in
  let rec loop i = 
    let a = List.nth tehai i in
    if a = (x,y) then
      i
    else if i = 0 then
      (m-1)
    else
      loop (i-1)
  in
  loop (m-1)

(*furo_lstから副露されている牌の配列の座標のリストを返す*)  
let furo_to_ary f_lst = 
  let m = List.length f_lst in
  let rec loop i tmp = 
    let (a,(b,(c,d,e))) = List.nth f_lst i in
    let tmp = 
      if a = Minkan || a = Ankan then
        [(b,c);(b,c);(b,c);(b,c)]@tmp
      else
        [(b,c);(b,d);(b,e)]@tmp
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

let rm_furo_double ary zi_ary furo_double_lst =
  let m = List.length furo_double_lst in 
  let rec loop i = 
    let (x,y) = List.nth furo_double_lst i in
    let  _ = 
      if x = 3 then
        let n = zi_ary.(y) in  
        zi_ary.(y) <-  n + 1;
      else
        let n = ary.(x).(y) in 
        ary.(x).(y) <- n + 1;
    in
    if i = 0 then 
      (ary,zi_ary)
    else
      loop (i-1)
  in
  if m = 0 then 
    (ary,zi_ary)
  else
    loop (m-1)




(*furo_lstから副露されている牌を配列から引いた配列を返す。(ary,zi_ary)*)     
let furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary =
  let a_lst = furo_to_ary (List.nth furo_lst 0) in
  let b_lst = furo_to_ary (List.nth furo_lst 1) in
  let c_lst = furo_to_ary (List.nth furo_lst 2) in
  let d_lst = furo_to_ary (List.nth furo_lst 3) in
  let lst = a_lst@b_lst@c_lst@d_lst in
  let m = List.length lst in
  let rec loop i = 
    let (x,y) = List.nth lst i in
    let _ = 
      if x = 3 then
        let n = zi_ary.(y) in
        zi_ary.(y) <- n - 1;
      else
        let n = ary.(x).(y) in
        ary.(x).(y) <- n - 1;
    in
    if i = 0 then
      (ary,zi_ary)
    else
      loop (i-1)
  in
  if m = 0 then
    (ary,zi_ary)
  else
    let (ary2,zi_ary2) = loop (m-1) in 
    rm_furo_double ary2 zi_ary2 furo_double_lst




let tumogiri tehai =
  (List.length tehai) -1 

let nofuro () =
  "n"

let autoagari () =
  "y"

let autoreach () = 
  "y"

let suzi_lst = [(1,4);(2,5);(3,6);(4,7);(5,8);(6,9)]

let kokushi_lst = [(1,Manzu);(9,Manzu);(1,Pinzu);(9,Pinzu);(1,Souzu);(9,Souzu);(0,Ton);(0,Nan);(0,Sya);(0,Pei);(0,Haku);(0,Hatsu);(0,Tyun)]

let suzi s = match s with
  | 1 -> [4]
  | 2 -> [5]
  | 3 -> [6]
  | 4 -> [4;7]
  | 5 -> [2;8]
  | 6 -> [3;9] 
  | 7 -> [7]
  | 8 -> [8]
  | 9 -> [9]
  | _ -> []

let kabe_to_deleat_suzi s = match s with
  | 1 -> []
  | 2 -> [(1,4)]
  | 3 -> [(1,4);(2,5)]
  | 4 -> [(2,5);(3,6)]
  | 5 -> [(3,6);(4,7)]
  | 6 -> [(4,7);(5,8)] 
  | 7 -> [(5,8);(6,9)]
  | 8 -> [(6,9)]
  | 9 -> []
  | _ -> []

let gukei_lst = [(2,[(1,3)]);(3,[(1,2);(2,4)]);(4,[(3,5)]);(5,[(4,6)]);(6,[(5,7)]);(7,[(6,8);(8,9)]);(8,[(7,9)])]

let possible_furo_patern tehai (x,y) = 
  let (xa,ya) = hai_to_ary (x,y) in
  let tmp = List.filter (fun (a,b) -> b = y) tehai in
  let p_f_lst =
    if List.length (List.filter (fun a -> a = (x,y)) tmp) >= 2 then
      [(Minko,(xa,(ya,ya,ya)))]
    else
      []
  in
  if x = 0 then
    p_f_lst
  else
    if x = 1 then
      if List.exists (fun a -> a = (2,y)) tmp && List.exists (fun a -> a = (3,y)) tmp then
        (Syuntu,(xa,(0,1,2)))::p_f_lst
      else
        p_f_lst
    else if x = 2 then
      let p_f_lst =
        if List.exists (fun a -> a = (1,y)) tmp && List.exists (fun a -> a = (3,y)) tmp then
          (Syuntu,(xa,(0,1,2)))::p_f_lst
        else
          p_f_lst
      in
      let p_f_lst =
        if List.exists (fun a -> a = (3,y)) tmp && List.exists (fun a -> a = (4,y)) tmp then
          (Syuntu,(xa,(1,2,3)))::p_f_lst
        else
          p_f_lst
      in
      p_f_lst
    else if x = 9 then
      if List.exists (fun a -> a = (7,y)) tmp && List.exists (fun a -> a = (8,y)) tmp then
        (Syuntu,(xa,(6,7,8)))::p_f_lst
      else
        p_f_lst
    else if x = 8 then
      let p_f_lst =
        if List.exists (fun a -> a = (7,y)) tmp && List.exists (fun a -> a = (9,y)) tmp then
          (Syuntu,(xa,(6,7,8)))::p_f_lst
        else
          p_f_lst
      in
      let p_f_lst =
        if List.exists (fun a -> a = (6,y)) tmp && List.exists (fun a -> a = (7,y)) tmp then
          (Syuntu,(xa,(5,6,7)))::p_f_lst
        else
          p_f_lst
      in
      p_f_lst
    else
      let p_f_lst =
        if List.exists (fun a -> a = (x-2,y)) tmp && List.exists (fun a -> a = (x-1,y)) tmp then
          (Syuntu,(xa,(ya-2,ya-1,ya)))::p_f_lst
        else
          p_f_lst
      in
      let p_f_lst =
        if List.exists (fun a -> a = (x-1,y)) tmp && List.exists (fun a -> a = (x+1,y)) tmp then
          (Syuntu,(xa,(ya-1,ya,ya+1)))::p_f_lst
        else
          p_f_lst
      in
      let p_f_lst =
        if List.exists (fun a -> a = (x+1,y)) tmp && List.exists (fun a -> a = (x+2,y)) tmp then
          (Syuntu,(xa,(ya,ya+1,ya+2)))::p_f_lst
        else
          p_f_lst
      in
      p_f_lst


let exist_reach yaku_lst player =
  let p_r = List.nth yaku_lst player in
  let x = 
    if List.exists (fun a -> a = Reach || a = Doublereach) p_r then
      1
    else
      0
  in
  let lst = List.filter (fun ls -> (List.exists (fun a -> a = Reach || a = Doublereach) ls) = true) yaku_lst in
  let m = List.length lst in
  if m = x then
    false
  else
    true

let kind_kokushi tehai = 
  let m = List.length tehai in
  let rec loop i k_tmp h_tmp = 
    let x = List.nth tehai i in
    let (k_tmp,h_tmp) = 
      if List.exists (fun a -> a = x) h_tmp then
        let h_tmp = List.filter (fun a -> a <> x) h_tmp in
        (k_tmp,h_tmp)
      else
        let k_tmp = x::k_tmp in
        (k_tmp,h_tmp)
    in
    if i = 0 then
      (k_tmp,h_tmp)
    else
      loop (i-1) k_tmp h_tmp
  in
  if m = 0 then
    ([],13)
  else
    let (k_tmp,h_tmp) = loop (m-1) [] kokushi_lst in
    let n = List.length h_tmp in
    let n = 13 - n in
    (k_tmp,n)

let furo_kind f_lst =
  let m = List.length f_lst in
  let rec loop i tmp = 
    let (a,(b,(c,d,e))) = List.nth f_lst i in
    let tmp = 
      if b = 3 then
        tmp
      else
        b::tmp
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

let which_some n_count m_count p_count s_count = 
  let (hai,count) = 
    if m_count > p_count then
      if m_count > s_count then
        (Manzu,m_count)
      else
        (Souzu,s_count)
    else
      if p_count > s_count then
        (Pinzu,p_count)
      else
        (Souzu,s_count)
  in
  (hai,(n_count-count))



let somete tehai f_lst = 
  let k_furo = furo_kind f_lst in
  let k = if k_furo = [] then 3 else (List.hd k_furo) in
  let k_furo = List.for_all (fun a -> a = k) k_furo in
  if k_furo = true then
    let no_zi_lst = List.filter (fun (a,b) -> a = 0) tehai in
    let n = List.length no_zi_lst in
    let m_lst = List.filter (fun (a,b) -> b = Manzu) no_zi_lst in
    let p_lst = List.filter (fun (a,b) -> b = Pinzu) no_zi_lst in
    let s_lst = List.filter (fun (a,b) -> b = Souzu) no_zi_lst in
    let m_count = List.length m_lst in
    let p_count = List.length p_lst in
    let s_count = List.length s_lst in
    let some_c = 
      if k = 0 then
        (Manzu,(14 - (n-m_count))) 
      else if k = 1 then
        (Pinzu,(14 - (n-p_count)))
      else if k = 2 then
        (Souzu,(14 - (n-s_count)))
      else
        let (hai,count) = which_some n m_count p_count s_count in
        (hai,(14-count))
    in
    some_c
  else
    (Ton,0)


let titoi_allow tehai f_lst = 
  let m = List.length tehai in
  let rec loop i tmp count = 
    let x = List.nth tehai i in
    let count = 
      if List.exists (fun a -> a = x) tmp then
        count + 1
      else
        count
    in
    let tmp = 
      if List.exists (fun a -> a = x) tmp then
        d_tehai tmp x
      else
        x::tmp
    in
    if i = 0 then
      (tmp,count)
    else
      loop (i-1) tmp count
  in
  if m = 0 || f_lst <> [] then
    ([],0)
  else
    loop (m-1) [] 0

    

let uradora dora_lst yama_lst = 
  let m = List.length dora_lst in
  let yama_len = List.length yama_lst in 
  let rec loop i tmp =
    let (x,y,_) = List.nth yama_lst (yama_len - (5+i*2)) in 
    let dora = hyouzi_to_dora (hai_to_ary (x,y)) in 
    let tmp = dora::tmp in 
    if i = 0 then 
      tmp 
    else
      loop (i-1) tmp 
  in
  (loop (m-1) [])@dora_lst


(*ハイテイをずらせるか．ずらせる:true,ずらせない:false*)
let haitei_slide rm_wan yaku_lst player = 
  let rec loop i tmp = 
    let yaku = 
      if player = i then 
        []
      else
        List.nth yaku_lst i
    in
    let tmp = 
      if List.exists (fun a -> a = Reach || a = Doublereach) yaku then
        i::tmp 
      else
        tmp
    in
    if i = 0 then 
      tmp 
    else
      loop (i-1) tmp
  in
  let r_lst = 
    if List.exists (fun a -> a = Reach || a = Doublereach) (List.nth yaku_lst player) then 
      []
    else
      loop 3 []
  in
  let haitei = (rm_wan - 1 + player) mod 4 in
  if List.exists (fun a -> a = haitei) r_lst then 
    if List.exists (fun a -> a = (haitei+1) mod 4) r_lst then 
      false
    else
      true
  else
    false

(*in:副露牌，out:(くいかえ牌,副露面子) list*)
let kuikae (x,y) = match y with 
  | 0 -> [((x,3),(Syuntu,(x,(0,1,2))))]
  | 1 -> [((x,4),(Syuntu,(x,(1,2,3))))]
  | 2 -> [((x,5),(Syuntu,(x,(2,3,4))))]
  | 3 -> [((x,0),(Syuntu,(x,(1,2,3))));((x,6),(Syuntu,(x,(3,4,5))))]
  | 4 -> [((x,1),(Syuntu,(x,(2,3,4))));((x,7),(Syuntu,(x,(4,5,6))))]
  | 5 -> [((x,2),(Syuntu,(x,(3,4,5))));((x,8),(Syuntu,(x,(5,6,7))))]
  | 6 -> [((x,3),(Syuntu,(x,(4,5,6))))]
  | 7 -> [((x,4),(Syuntu,(x,(5,6,7))))]
  | 8 -> [((x,5),(Syuntu,(x,(6,7,8))))]
  | _ -> []

(*(int*hai)*)
let kuikae (x,y) furo_block = 
  if x = 0 then 
    [(x,y)]
  else
    let (x',y') = hai_to_ary (x,y) in 
    let lst = kuikae (x',y') in 
    let m = List.length lst in 
    let rec loop i tmp = 
      let (kuikae_hai,n_block) = List.nth lst i in 
      let tmp = 
        if n_block = furo_block then 
          kuikae_hai::tmp 
        else
          tmp 
      in
      if i = 0 then 
        tmp 
      else
        loop (i-1) tmp 
    in
    if m = 0 then
      [(x,y)]
    else
      let n_lst = loop (m-1) [] in 
      let n_lst = List.map (fun (a,b) -> ary_to_hai (a,b)) n_lst in
      (x,y)::n_lst


let return_what_furo furo = 
  let (x,_) = furo in 
  if x = Syuntu then 
    "t" 
  else if x = Minko then
    "p"
  else
    "k"




