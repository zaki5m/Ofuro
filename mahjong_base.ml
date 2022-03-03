type furo = Pon | Ti | Kan
type hai = Manzu | Pinzu | Souzu | Ton | Nan | Sya | Pei | Haku | Hatsu | Tyun | Not_hai
type state = Syuntu | Toitu | Anko | Minko | Ankan | Minkan | Ws
type mati_state = Ryanmen | Kantyan | Pentyan | Tanki | Syanpon | Ns
type yaku = Reach | Ippatu | Menzentumo | Yakuhai | Tanyao | Pinhu | Ipeiko | Haitei | Houtei | Rinsyankaihou | Tyankan
            | Doublereach | Sansyokudouzyun | Sansyokudoukou | Sanankou | Ikkitukan | Titoitu | Toitoi | Tyanta | Sankantu | Syousangen
            | Honroutou | Ryanpeikou | Zyuntyan | Honitu | Tinitu | Suankou | Daisangen | Kokusimusou | Ryuiso | Tuiso | Tinroutou
            | Sukantu | Syoususi | Daisusi | Tyurenpoutou | Tihou | Tenhou

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

let tumogiri tehai =
  (List.length tehai) -1 

let nofuro () =
  "n"

let autoagari () =
  "y"

let autoreach () = 
  "y"

let suzi_lst = [(1,4);(2,5);(3,6);(4,7);(5,8);(6,9)]

let suzi s = match s with
  | 1 -> [(1,4)]
  | 2 -> [(2,5)]
  | 3 -> [(3,6)]
  | 4 -> [(1,4);(4,7)]
  | 5 -> [(2,5);(5,8)]
  | 6 -> [(3,6);(6,9)] 
  | 7 -> [(4,7)]
  | 8 -> [(5,8)]
  | 9 -> [(6,9)]
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