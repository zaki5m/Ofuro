open M_gragh
open Loop
open Mahjong_base
open Mahjong_haieff
open Tenpai_prob

let seed = try int_of_string Sys.argv.(2) with _ -> 1

let hai_lst = [(1,Manzu);
              (2,Manzu);
              (3,Manzu);
              (4,Manzu);
              (5,Manzu);
              (6,Manzu);
              (7,Manzu);
              (8,Manzu);
              (9,Manzu);
              (1,Pinzu);
              (2,Pinzu);
              (3,Pinzu);
              (4,Pinzu);
              (5,Pinzu);
              (6,Pinzu);
              (7,Pinzu);
              (8,Pinzu);
              (9,Pinzu);
              (1,Souzu);
              (2,Souzu);
              (3,Souzu);
              (4,Souzu);
              (5,Souzu);
              (6,Souzu);
              (7,Souzu);
              (8,Souzu);
              (9,Souzu);
              (0,Ton);
              (0,Nan);
              (0,Sya);
              (0,Pei);
              (0,Haku);
              (0,Hatsu);
              (0,Tyun)]

let hai_lst_red = [(1,Manzu);
              (2,Manzu);
              (3,Manzu);
              (4,Manzu);
              (5,Manzu_red);
              (6,Manzu);
              (7,Manzu);
              (8,Manzu);
              (9,Manzu);
              (1,Pinzu);
              (2,Pinzu);
              (3,Pinzu);
              (4,Pinzu);
              (5,Pinzu_red);
              (6,Pinzu);
              (7,Pinzu);
              (8,Pinzu);
              (9,Pinzu);
              (1,Souzu);
              (2,Souzu);
              (3,Souzu);
              (4,Souzu);
              (5,Souzu_red);
              (6,Souzu);
              (7,Souzu);
              (8,Souzu);
              (9,Souzu);
              (0,Ton);
              (0,Nan);
              (0,Sya);
              (0,Pei);
              (0,Haku);
              (0,Hatsu);
              (0,Tyun)]

let big_hai_lst = hai_lst @ hai_lst @ hai_lst @ hai_lst

let big_hai_lst_red = hai_lst @ hai_lst @ hai_lst @ hai_lst_red

let rec add_tehai (list:(int*hai) list) (x,y) = match list with
  | [] -> [(x,y)]
  | [(x1,y1)] -> [(x1,y1);(x,y)]
  | h::t -> h::(add_tehai t (x,y))

let rec add_tehai2 (list:(int*hai*bool) list) (x,y,z) = match list with
  | [] -> [(x,y,z)]
  | [(x1,y1,z1)] -> [(x1,y1,z1);(x,y,z)]
  | h::t -> h::(add_tehai2 t (x,y,z))

let add_ary_lst (a,b,c,d) ary zi_ary player = 
  if player = 0 then 
    ((ary,zi_ary),b,c,d)
  else if player = 1 then
    (a,(ary,zi_ary),c,d)
  else if player = 2 then 
    (a,b,(ary,zi_ary),d)
  else 
    (a,b,c,(ary,zi_ary))

let add_lst (a,b,c,d) furo_lst player = 
  if player = 0 then 
    (furo_lst,b,c,d)
  else if player = 1 then
    (a,furo_lst,c,d)
  else if player = 2 then 
    (a,b,furo_lst,d)
  else 
    (a,b,c,furo_lst)

let filter_yaku yaku_lst = 
  let a = List.filter (fun x -> x <> Ippatu) (tapl_player_1 yaku_lst) in
  let b = List.filter (fun x -> x <> Ippatu) (tapl_player_2 yaku_lst) in
  let c = List.filter (fun x -> x <> Ippatu) (tapl_player_3 yaku_lst) in
  let d = List.filter (fun x -> x <> Ippatu) (tapl_player_4 yaku_lst) in
  (a,b,c,d)




let add_player_score ((a,b,c,d):(int*int*int*int)) score player = 
  if player = 0 then 
    (score,b,c,d)
  else if player = 1 then
    (a,score,c,d)
  else if player = 2 then 
    (a,b,score,d)
  else 
    (a,b,c,score)


let rename2 (m, n, o) = match n with
  | Manzu -> if o = true then 
              "@" ^ (Int.to_string m) ^ "m"
             else 
              (Int.to_string m) ^ "m"
  | Pinzu -> if o = true then 
              "@" ^ (Int.to_string m) ^ "p" 
             else
              (Int.to_string m) ^ "p" 
  | Souzu -> if o = true then 
              "@" ^ (Int.to_string m) ^ "s"
             else
              (Int.to_string m) ^ "s"
  | Ton -> if o = true then
            "@ton"
           else
            "ton" 
  | Nan -> if o = true then
            "@nan"
           else
            "nan" 
  | Sya -> if o = true then 
            "@sya"
           else
            "sya"
  | Pei -> if o = true then 
            "@pei"
           else
            "pei" 
  | Haku -> if o = true then 
             "@haku"
            else
              "haku" 
  | Hatsu -> if o = true then 
              "@ryu"
             else
              "ryu"
  | Tyun -> if o = true then 
              "@tyun"
            else
              "tyun"
  | _ -> "not"

let rec s_to_hai (x,y) = match y with  
  | 'm' -> let x = int_of_char x - 48 in
              (x,Manzu)
  | 'p' -> let x = int_of_char x - 48 in
              (x,Pinzu)
  | 's' -> let x = int_of_char x - 48 in
              (x,Souzu)
  | _ -> let zi_to_hai (x,y) = match x with
            | 't' -> if y = 'o' then
                        (0,Ton)
                     else 
                        (0,Tyun)
            | 'n' -> (0,Nan)
            | 's' -> (0,Sya)
            | 'p' -> (0,Pei)
            | 'h' -> (0,Haku)
            | 'r' -> (0,Hatsu)
            | _ -> (0,Not_hai)
          in
          zi_to_hai (x,y)  

(*
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
*)


let haipai yama_lst = 
  let rec loop' i j yama_lst tehai_lst_0 tehai_lst_1 tehai_lst_2 tehai_lst_3 = 
    let (t_x,t_y,_) = List.hd yama_lst in
    let yama_lst = List.tl yama_lst in
    let tehai_lst = 
      if i = 0 then
        add_tehai tehai_lst_0 (t_x,t_y) 
      else if i = 1 then
        add_tehai tehai_lst_1 (t_x,t_y)
      else if i = 2 then
        add_tehai tehai_lst_2 (t_x,t_y) 
      else
        add_tehai tehai_lst_3 (t_x,t_y) 
    in
    let (t_x,t_y,_) = List.hd yama_lst in
    let yama_lst = List.tl yama_lst in
    let tehai_lst = add_tehai tehai_lst (t_x,t_y) in
    if i = 3 then
      if j = 5 then
        (yama_lst,tehai_lst_0,tehai_lst_1,tehai_lst_2,tehai_lst)
      else
        loop' 0 (j+1) yama_lst tehai_lst_0 tehai_lst_1 tehai_lst_2 tehai_lst
    else if i = 2 then
      loop' 3 j yama_lst tehai_lst_0 tehai_lst_1 tehai_lst tehai_lst_3
    else if i = 1 then
      loop' 2 j yama_lst tehai_lst_0 tehai_lst tehai_lst_2 tehai_lst_3
    else
      loop' 1 j yama_lst tehai_lst tehai_lst_1 tehai_lst_2 tehai_lst_3
  in
  let (yama_lst,tehai_lst_0,tehai_lst_1,tehai_lst_2,tehai_lst_3) = loop' 0 0 yama_lst [] [] [] [] in
  let (t_x,t_y,_) = List.hd yama_lst in
  let tehai_lst_0 = add_tehai tehai_lst_0 (t_x,t_y) in
  let yama_lst = List.tl yama_lst in
  let (t_x,t_y,_) = List.hd yama_lst in
  let tehai_lst_1 = add_tehai tehai_lst_1 (t_x,t_y)in
  let yama_lst = List.tl yama_lst in
  let (t_x,t_y,_) = List.hd yama_lst in
  let tehai_lst_2 = add_tehai tehai_lst_2 (t_x,t_y) in
  let yama_lst = List.tl yama_lst in
  let (t_x,t_y,_) = List.hd yama_lst in
  let tehai_lst_3 = add_tehai tehai_lst_3 (t_x,t_y) in
  let yama_lst = List.tl yama_lst in
  let len = List.length yama_lst in
  let (x,y,_) = List.nth yama_lst (len-6) in
  let dora = hai_to_ary (x,y) in
  let dora = hyouzi_to_dora dora in
  let tehai_red_0 = tehai_tapl_red tehai_lst_0 in
  let tehai_red_1 = tehai_tapl_red tehai_lst_1 in 
  let tehai_red_2 = tehai_tapl_red tehai_lst_2 in 
  let tehai_red_3 = tehai_tapl_red tehai_lst_3 in 
  global_red := (tehai_red_0,tehai_red_1,tehai_red_2,tehai_red_3);
  (yama_lst,(ripai tehai_lst_0),(ripai tehai_lst_1),(ripai tehai_lst_2),(ripai tehai_lst_3),[dora])

let r_hai2 (lst:(int*hai)list) lsgs = 
  Random.init lsgs;
  let m = List.length lst in 
  let rec loop i tmp = 
    let (a,b) = List.nth lst i in
    let x = Random.int 1000 in
    let tmp = (a,b,x)::tmp in
    if i = 0 then 
      tmp
    else
      loop (i-1) tmp 
  in
  loop (m-1) []



let r_hai (lst:(int*hai)list) = 
  Random.self_init();
  let rec loop tmp = function
    | [] -> tmp 
    | (a,b)::t -> let x = Random.int 1000 in
                  loop ((a,b,x)::tmp) t 
  in
  loop [] lst

let tumo_from_yama (player:int) (yama_lst:(int*hai*int)list) = 
  let (x,y,_) = List.hd yama_lst in
  (player,(x,y))

let possible_furo ary zi_ary (x,y) =
  let tmp =
    if x = 3 then
      if zi_ary.(y) >= 2 then
        if zi_ary.(y) = 3 then
          [Pon;Kan]
        else
          [Pon]
      else
        []
    else
      if ary.(x).(y) >= 2 then
        if ary.(x).(y) = 3 then
          [Pon;Kan]
        else
          [Pon]
      else
        []
  in
  if x <> 3 then
    if y = 0 then
      if ary.(x).(y+1) >= 1 && ary.(x).(y+2) >= 1 then
        Ti::tmp
      else
        tmp
    else if y = 8 then
      if ary.(x).(y-1) >= 1 && ary.(x).(y-2) >= 1 then
        Ti::tmp
      else
        tmp
    else if y = 1 then
      if (ary.(x).(y+1) >= 1 && ary.(x).(y+2) >= 1) || (ary.(x).(y-1) >= 1 && ary.(x).(y+1) >= 1) then
        Ti::tmp
      else
        tmp
    else if y = 7 then
      if (ary.(x).(y-1) >= 1 && ary.(x).(y-2) >= 1) || (ary.(x).(y-1) >= 1 && ary.(x).(y+1) >= 1) then
        Ti::tmp
      else
        tmp
    else 
      if (ary.(x).(y+1) >= 1 && ary.(x).(y+2) >= 1) || (ary.(x).(y-1) >= 1 && ary.(x).(y-2) >= 1) || (ary.(x).(y-1) >= 1 && ary.(x).(y+1) >= 1) then
        Ti::tmp
      else
        tmp
  else
    tmp


let tumo_or_te ((k_x,k_y):(int*hai)) (t_x,t_y) =
  if (k_x,k_y) = (t_x,t_y) then
    let (x,y,z) = (k_x,k_y,true) in
    (x,y,z)
  else
    let (x,y,z) = (k_x,k_y,false) in
    (x,y,z)

let possible_tumo_agari (ary,zi_ary) zi_kaze ba_kaze naki (f_lst:(Mahjong_base.state*(int*(int*int*int))) list) (x,y) yaku_lst dora_lst sutehai yama_lst tehai = 
  let yaku_lst = 
    if sutehai = [] then 
      if zi_kaze = 0 then 
        Tenhou::yaku_lst
      else
        Tihou::yaku_lst
    else
      yaku_lst
  in
  let red = tehai_in_red tehai in 
  let lst = 
  if List.exists (fun a -> a = Reach || a = Doublereach) yaku_lst then
    tehai_to_ten ary zi_ary zi_kaze ba_kaze naki f_lst yaku_lst (uradora dora_lst yama_lst) red 
  else
    tehai_to_ten ary zi_ary zi_kaze ba_kaze naki f_lst yaku_lst dora_lst red 
  in
  let m = List.length lst in
  let rec loop' i tmp = 
    let (agari_hai,ten) = List.nth lst i in
    let agari_hai = ary_to_hai agari_hai in
    let tmp = 
      if agari_hai = (x,y) then
        let (t,_) = ten in
        if t > 0 then
          (true,t)
        else
          tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 then
    (false,0)
  else
    loop' (m-1) (false,0)

let possible_ankan (ary,zi_ary) (x,y) = 
  let (x,y) = hai_to_ary (x,y) in
  if x = 3 then
    if zi_ary.(y) = 3 then
      true
    else
      false
  else
    if ary.(x).(y) = 3 then
      true
    else
      false

let rinsyan dora_lst yama_lst = 
  let m = List.length dora_lst in
  let n = List.length yama_lst in 
  let (x,y,_) = List.nth yama_lst (n-1-(5+m*2)) in
  let dora = hai_to_ary (x,y) in
  let dora = hyouzi_to_dora dora in
  let dora_lst = dora::dora_lst in
  let rinsyan_hai = List.nth yama_lst (n-1) in
  let (x,y,_) = rinsyan_hai in
  ((x,y),yama_lst,dora_lst)


let ankan (x,y) tehai f_lst yama_lst dora_lst = 
  let (a,b) = hai_to_ary (x,y) in
  let rec loop' tehai f_lst =
    (*Printf.printf "Kan?y/n\n";*)
    let y_or_n = nofuro () in
    if y_or_n = "y" then
      let tehai = List.filter (fun c -> c <> (x,y)) tehai in
      let f_lst = (Ankan,(a,(b,b,b)))::f_lst in
      let (rinsyan_hai,yama_lst,dora_lst) = rinsyan dora_lst yama_lst in
      let tehai = add_tehai tehai rinsyan_hai in
      (tehai,f_lst,yama_lst,dora_lst,rinsyan_hai)
    else if y_or_n = "n" then
      (tehai,f_lst,yama_lst,dora_lst,(x,y))
    else
      loop' tehai f_lst
  in
  loop' tehai f_lst

let tumo_agari ten player kyoku honba kyotaku =
  let rec loop' ten = 
    (*Printf.printf "Tumo?y/n\n";*)
    let y_or_n = autoagari() in
    if y_or_n = "y" then
      if player = (kyoku-1) then
        let ten = ten_tumo_oya ten in
        if player = 0 then
          (ten*3+honba*300+kyotaku*1000,-(ten+honba*100),-(ten+honba*100),-(ten+honba*100))
        else if player = 1 then
          (-(ten+honba*100),ten*3+honba*300+kyotaku*1000,-(ten+honba*100),-(ten+honba*100))
        else if player = 2 then
          (-(ten+honba*100),-(ten+honba*100),ten*3+honba*300+kyotaku*1000,-(ten+honba*100))
        else 
          (-(ten+honba*100),-(ten+honba*100),-(ten+honba*100),ten*3+honba*300+kyotaku*1000)
      else
        let (ten_oya,ten_ko) = ten_tumo_ko ten in
        if player = 0 then
          if kyoku = 2 then
            (ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_oya+honba*100),-(ten_ko+honba*100),-(ten_ko+honba*100))
          else if kyoku = 3 then
            (ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_ko+honba*100),-(ten_oya+honba*100),-(ten_ko+honba*100))
          else 
            (ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_ko+honba*100),-(ten_ko+honba*100),-(ten_oya+honba*100))
        else if player = 1 then
          if kyoku = 1 then
            (-(ten_oya+honba*100),ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_ko+honba*100),-(ten_ko+honba*100))
          else if kyoku = 3 then
            (-ten_ko,ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_oya+honba*100),-(ten_ko+honba*100))
          else 
            (-ten_ko,ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_ko+honba*100),-(ten_oya+honba*100))
        else if player = 2 then
          if kyoku = 1 then
            (-(ten_oya+honba*100),-(ten_ko+honba*100),ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_ko+honba*100))
          else if kyoku = 2 then
            (-(ten_ko+honba*100),-(ten_oya+honba*100),ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_ko+honba*100))
          else 
            (-(ten_ko+honba*100),-(ten_ko+honba*100),ten_oya+ten_ko*2+honba*300+kyotaku*1000,-(ten_oya+honba*100))
        else 
          if kyoku = 1 then
            (-(ten_oya+honba*100),-(ten_ko+honba*100),-(ten_ko+honba*100),ten_oya+ten_ko*2+honba*300+kyotaku*1000)
          else if kyoku = 2 then
            (-(ten_ko+honba*100),-(ten_oya+honba*100),-(ten_ko+honba*100),ten_oya+ten_ko*2+honba*300+kyotaku*1000)
          else 
            (-(ten_ko+honba*100),-(ten_ko+honba*100),-(ten_oya+honba*100),ten_oya+ten_ko*2+honba*300+kyotaku*1000)
    else if y_or_n = "n" then
      (0,0,0,0)
    else
      loop' ten
  in
  loop' ten


let ryukyoku_ten tehai_lst = 
  let rec loop i tmp = match i with 
   | -1 -> tmp 
   | _ -> let tehai = tapl_player tehai_lst i in
          let (_,n) = syanten tehai in 
          let tmp = 
            if n = 0 then
              true::tmp
            else
              false::tmp
          in
          loop (i-1) tmp
  in
  loop 3 []

let tenpai_ryo lst = 
  let m = List.length (List.filter (fun x -> x = true) lst) in
  let n = List.length (List.filter (fun x -> x = false) lst) in
  let rec loop' i tmp = 
    let tenpai = List.nth lst i in
    let tmp = 
      if tenpai = true then
        3000/m::tmp
      else
        -3000/n::tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 || m = 4 then
    (0,0,0,0)
  else
    let tmp = loop' 3 [] in
    ((List.nth tmp 0),(List.nth tmp 1),(List.nth tmp 2),(List.nth tmp 3))

(*
let print_player kyoku ba sutehai_lst tehai_lst player = 
  if ba = 0 then
    (Printf.printf "東%d局" kyoku;)
  else
    (Printf.printf "東%d局" kyoku;)
*)

let reach_inq () = 
  let rec loop' () = 
    (*Printf.printf "Reach? y/n\n";*)
    let n = autoreach() in
    if n = "y" then
      true
    else if n = "n" then
      false
    else
      loop'()
    in
    loop' ()


let possible_reach tehai sutehai naki (f_lst:(Mahjong_base.state*(int*(int*int*int))) list) (yaku_lst:Mahjong_base.yaku list) dora_lst zi_kaze ba_kaze kyotaku =
  let (_,n) = syanten tehai in  
  if naki = false && n = 0 && (List.exists (fun a -> a = Reach || a = Doublereach) yaku_lst) = false then
    let x = reach_inq () in
    if x = true then
      let kyotaku = kyotaku + 1 in
      if sutehai = [] then
        (kyotaku,[Doublereach;Ippatu])
      else
        (kyotaku,[Reach;Ippatu])
    else
      (kyotaku,[])
  else
    (kyotaku,[])

let kyoku_to_kaze kyoku player = 
  if kyoku = 1 then
    player
  else if kyoku = 2 then
    if player = 0 then
      3
    else
      player - 1
  else if kyoku = 3 then
    if player = 0 || player = 1 then
      player + 2
    else
      player - 2
  else
    if player = 3 then
      0
    else
      player + 1

let kiriban tehai_lst sutehai_lst ary_lst player f_lst naki yaku_lst dora_lst kyoku ba kyotaku player_score yama_len furo_double_lst furiten_lst  =
  let yaku_player = tapl_player yaku_lst player in
  let yaku_player = List.filter (fun a -> a <> Ippatu) yaku_player in 
  let zi_kaze = kyoku_to_kaze kyoku player in
  let rec loop' tehai sutehai =
    (*t_format tehai_lst f_lst sutehai_lst ba kyoku 0 kyotaku player_score yaku_lst dora_lst; flush stdout;*)
    let n = 
      prob_select sutehai_lst tehai f_lst yaku_lst player yama_len zi_kaze ba naki dora_lst furo_double_lst furiten_lst
    in
    let n = if n >= List.length tehai then List.length tehai - 1 else n in  
    if n < 0 && (List.length tehai) <= n then
      loop' tehai sutehai
    else 
      let (k_x,k_y,k_z) = int_to_hai tehai n in
      let (k_x,k_y,k_z) = select_red_to_black tehai (k_x,k_y,k_z) in 
      let r_tmp_tapl = !global_red in 
      let (r_m,r_p,r_s) = tapl_player r_tmp_tapl player in 
      let r_player_red = if k_y = Manzu_red then (0,r_p,r_s) else if k_y = Pinzu_red then (r_m,0,r_s) else if k_y = Souzu_red then (r_m,r_p,0) else (r_m,r_p,r_s) in 
      let r_tmp_tapl = tapl_player_in r_tmp_tapl r_player_red player in 
      global_red :=  r_tmp_tapl; 
      let tehai1 = d_tehai tehai (k_x,k_y) in
      let (kyotaku,reach) = 
        if naki = false then
          possible_reach tehai1 (tapl_player sutehai_lst player) naki (tapl_player f_lst player) yaku_player dora_lst zi_kaze ba kyotaku 
        else
          (kyotaku,[])
      in
      let sutehai1 = add_tehai2 sutehai (k_x,k_y,k_z) in
      let tehai1 = ripai tehai1 in
      (tehai1,sutehai1,reach,kyotaku)
  in
  let tehai = tapl_player tehai_lst player in
  let sutehai = tapl_player sutehai_lst player in 
  let (tehai,sutehai,reach,kyotaku) = loop' tehai sutehai in
  let player_score =
    if reach <> [] then
      let score = tapl_player player_score player in
      let score = score - 1000 in
      add_player_score player_score score player 
    else
      player_score
  in
  let yaku_player = reach@yaku_player in
  let yaku_lst = add_lst yaku_lst yaku_player player in
  let (ary,zi_ary) = list_to_ary tehai in
  let ary_lst = add_ary_lst ary_lst ary zi_ary player in
  let tmp1 = add_lst tehai_lst tehai player in 
  let tmp2 = add_lst sutehai_lst sutehai player in 
  (tmp1,tmp2,ary_lst,yaku_lst,player_score,kyotaku)

  

let ary_sub_furo ary zi_ary furo = 
  let m = List.length furo in
  let rec loop' i = 
    let (_,(x,(y1,y2,y3)))  = List.nth furo i in
    let _ = 
      if x = 3 then
        (let n = zi_ary.(y1) in
        zi_ary.(y1) <- n - 3;)
      else
        (let n = ary.(x).(y1) in
        ary.(x).(y1) <- n - 1;
        let n = ary.(x).(y2) in
        ary.(x).(y2) <- n - 1;
        let n = ary.(x).(y3) in
        ary.(x).(y3) <- n - 1;)
    in
    if i = 0 then
      ()
    else
      loop' (i-1)
  in
  if m = 0 then
    (ary,zi_ary)
  else
    let _ = loop' (m-1) in
    (ary,zi_ary)
(*not automatic*)
(*
let what_furo lst sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst = 
  let m = List.length lst in
  let rec loop' m =
    if m = 3 then
      ((*Printf.printf "Pon/Ti/Kan/Not? p/t/k/n\n";*)
      let n = purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst in
      if n = "p" || n = "t" || n = "k" || n = "n" then
        if n = "p" then
          [Pon]
        else if n = "t" then
          [Ti]
        else if n = "k" then
          [Kan]
        else
          []
      else
        loop' m)
    else if m = 2 then
      if List.exists (fun a -> Kan = a) lst = true then
        ((*Printf.printf "Pon/Kan/Not? p/k/n\n";*)
        let n = purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst in
        if n = "p" || n = "k" || n = "n" then
          if n = "p" then
            [Pon]
          else if n = "k" then
            [Kan]
          else
            []
        else
         loop' m)
      else
        ((*Printf.printf "Pon/Ti/Not? p/t/n\n";*)
        let n = purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst in
        if n = "p" || n = "t" || n = "n" then
          if n = "p" then
            [Pon]
          else if n = "t" then
            [Ti]
          else
            []
        else
         loop' m)
    else
      if List.exists (fun a -> Ti = a) lst = true then
        ((*Printf.printf "Ti/Not? t/n\n";*)
        let n = purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst in
        if n = "t" || n = "n" then
          if n = "t" then
            [Ti]
          else
            []
        else
         loop' m)
      else
        ((*Printf.printf "Pon/Not? p/n\n";*)
        let n = purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst in
        if n = "p" || n = "n" then
          if n = "p" then
            [Pon]
          else
            []
        else
         loop' m)
  in
  if m = 0 then
    []
  else
    loop' m
*)

let rec ti_furo_number lst = 
  let m = List.length lst in
  let rec loop' i = 
    let (x,y) = List.nth lst i in
    Printf.printf "%d:(%d,%d)" i x y;
    if i = m-1 then
      ()
    else
      loop' (i+1)
  in
  loop' 0;
  Printf.printf "\n";
  let n = read_int() in
  if n >= 0 || n < m then
    n
  else
    ti_furo_number lst 



let furo_naumber ary f (x,y) = 
  if f = Pon then
    (Minko,(x,(y,y,y)))
  else if f = Kan then
    (Minkan,(x,(y,y,y)))
  else
    if y = 0 || y = 8 then
      if y = 0 then
        (Syuntu,(x,(y,y+1,y+2)))
      else
        (Syuntu,(x,(y-2,y-1,y)))
    else if y = 1 || y = 7 then
      if y = 1 then
        let n1 = ary.(x).(y-1) in
        let n3 = ary.(x).(y+2) in
        if n1 > 0 && n3 > 0 then
          let number = [(y-1,y+1);(y+1,y+2)] in
          let n = ti_furo_number number in
          let (m1,m2) = List.nth number n in
          if m1 > y then
            (Syuntu,(x,(y,m1,m2)))
          else
            (Syuntu,(x,(m1,y,m2)))
        else
          if n1 > 0 then
            (Syuntu,(x,(y-1,y,y+1)))
          else
            (Syuntu,(x,(y,y+1,y+2)))
      else
        let n1 = ary.(x).(y-2) in
        let n3 = ary.(x).(y+1) in
        if n1 > 0 && n3 > 0 then
          let number = [(y-2,y-1);(y-1,y+1)] in
          let n = ti_furo_number number in
          let (m1,m2) = List.nth number n in
          if m2 > y then
            (Syuntu,(x,(m1,y,m2)))
          else
            (Syuntu,(x,(m1,m2,y)))
        else
          if n1 > 0 then
            (Syuntu,(x,(y-2,y-1,y)))
          else
            (Syuntu,(x,(y-1,y,y+1)))
    else
      let n1 = ary.(x).(y-2) in
      let n2 = ary.(x).(y-1) in
      let n3 = ary.(x).(y+1) in
      let n4 = ary.(x).(y+2) in
      if n3 = 0  || (n1 = 0 && n4 = 0) || n2 = 0 then
        if n3 = 0 then
          (Syuntu,(x,(y-2,y-1,y)))
        else if n2 = 0 then
          (Syuntu,(x,(y,y+1,y+2)))
        else
          (Syuntu,(x,(y-1,y,y+1)))
      else
        if n1 = 0 then
          let number = [(y-1,y+1);(y+1,y+2)] in
          let n = ti_furo_number number in
          let (m1,m2) = List.nth number n in
          if m1 > y then
            (Syuntu,(x,(y,m1,m2)))
          else
            (Syuntu,(x,(m1,y,m2)))
        else if n4 = 0 then
          let number = [(y-2,y-1);(y-1,y+1)] in
          let n = ti_furo_number number in
          let (m1,m2) = List.nth number n in
          if m2 > y then
            (Syuntu,(x,(m1,y,m2)))
          else
            (Syuntu,(x,(m1,m2,y)))
        else
          let number = [(y-2,y-1);(y-1,y+1);(y+1,y+2)] in
          let n = ti_furo_number number in
          let (m1,m2) = List.nth number n in
          if m2 > y && m1 > y then
            (Syuntu,(x,(y,m1,m2)))
          else if m2 > y then
            (Syuntu,(x,(m1,y,m2)))
          else
            (Syuntu,(x,(m1,m2,y)))

let remove_furo_hai tehai_lst furo_hai (x,y) = 
  let (a,(b,(c,d,e))) = furo_hai in
  let (x,y) = ary_to_hai (x,y) in
  let (c,_) = ary_to_hai (b,c) in
  let (d,_) = ary_to_hai (b,d) in
  let (e,b') = ary_to_hai (b,e) in
  let tehai = 
    if a = Minkan then
      let tmp_lst = List.filter (fun x -> x <> (c,b')) tehai_lst in
      if x = 5 then 
        if y = Manzu then 
          List.filter (fun x -> x <> (c,Manzu_red)) tmp_lst
        else if y = Pinzu then 
          List.filter (fun x -> x <> (c,Pinzu_red)) tmp_lst
        else 
          List.filter (fun x -> x <> (c,Souzu_red)) tmp_lst
      else
        tmp_lst 
    else if a = Minko then
      if x = 5 then 
        if y = Manzu then 
          let tehai_lst = if List.exists (fun tmp -> tmp = (5,Manzu_red)) tehai_lst then  d_tehai tehai_lst (c,Manzu_red) else d_tehai tehai_lst (c,b') in
          let tehai_lst = d_tehai tehai_lst (c,b') in
          tehai_lst
        else if y = Pinzu then 
          let tehai_lst = if List.exists (fun tmp -> tmp = (5,Pinzu_red)) tehai_lst then  d_tehai tehai_lst (c,Pinzu_red) else d_tehai tehai_lst (c,b') in
          let tehai_lst = d_tehai tehai_lst (c,b') in
          tehai_lst
        else 
          let tehai_lst = if List.exists (fun tmp -> tmp = (5,Souzu_red)) tehai_lst then  d_tehai tehai_lst (c,Souzu_red) else d_tehai tehai_lst (c,b') in
          let tehai_lst = d_tehai tehai_lst (c,b') in
          tehai_lst
      else
        let tehai_lst = d_tehai tehai_lst (c,b') in
        let tehai_lst = d_tehai tehai_lst (c,b') in
        tehai_lst
    else
      if (x,y) = (c,b') then
        let (d,b_1) = if (List.exists (fun a -> a = (d,Manzu_red)) tehai_lst) && b' = Manzu  then (d,Manzu_red) else if (List.exists (fun a -> a = (d,Pinzu_red)) tehai_lst) && b' = Pinzu then (d,Pinzu_red) else if (List.exists (fun a -> a = (d,Souzu_red)) tehai_lst) && b' = Souzu then (d,Souzu_red) else (d,b') in 
        let (e,b_2) = if (List.exists (fun a -> a = (e,Manzu_red)) tehai_lst) && b' = Manzu then (e,Manzu_red) else if (List.exists (fun a -> a = (e,Pinzu_red)) tehai_lst) && b' = Pinzu then (e,Pinzu_red) else if (List.exists (fun a -> a = (e,Souzu_red)) tehai_lst) && b' = Souzu then (e,Souzu_red) else (e,b') in 
        let tehai_lst = d_tehai tehai_lst (d,b_1) in
        let tehai_lst = d_tehai tehai_lst (e,b_2) in
        tehai_lst
      else if (x,y) = (d,b') then
        let (c,b_1) = if (List.exists (fun a -> a = (c,Manzu_red)) tehai_lst) && b' = Manzu then (c,Manzu_red) else if (List.exists (fun a -> a = (c,Pinzu_red)) tehai_lst) && b' = Pinzu then (c,Pinzu_red) else if (List.exists (fun a -> a = (c,Souzu_red)) tehai_lst) && b' = Souzu then (c,Souzu_red) else (c,b') in 
        let (e,b_2) = if (List.exists (fun a -> a = (e,Manzu_red)) tehai_lst) && b' = Manzu then (e,Manzu_red) else if (List.exists (fun a -> a = (e,Pinzu_red)) tehai_lst) && b' = Pinzu then (e,Pinzu_red) else if (List.exists (fun a -> a = (e,Souzu_red)) tehai_lst) && b' = Souzu then (e,Souzu_red) else (e,b') in 
        let tehai_lst = d_tehai tehai_lst (c,b_1) in
        let tehai_lst = d_tehai tehai_lst (e,b_2) in
        tehai_lst
      else
        let (d,b_1) = if (List.exists (fun a -> a = (d,Manzu_red)) tehai_lst) && b' = Manzu  then (d,Manzu_red) else if (List.exists (fun a -> a = (d,Pinzu_red)) tehai_lst) && b' = Pinzu then (d,Pinzu_red) else if (List.exists (fun a -> a = (d,Souzu_red)) tehai_lst) && b' = Souzu then (d,Souzu_red) else (d,b') in 
        let (c,b_2) = if (List.exists (fun a -> a = (c,Manzu_red)) tehai_lst) && b' = Manzu then (c,Manzu_red) else if (List.exists (fun a -> a = (c,Pinzu_red)) tehai_lst) && b' = Pinzu then (c,Pinzu_red) else if (List.exists (fun a -> a = (c,Souzu_red)) tehai_lst) && b' = Souzu then (c,Souzu_red) else (c,b') in 
        let tehai_lst = d_tehai tehai_lst (c,b_2) in
        let tehai_lst = d_tehai tehai_lst (d,b_1) in
        tehai_lst
    in
    tehai

let kan yama_lst tehai_lst ary_lst furo_lst naki_lst yaku_lst dora_lst player ba kyoku honba kyotaku sutehai_lst =
  let tehai = tapl_player tehai_lst player in
  let naki = tapl_player naki_lst player in
  let f_lst = tapl_player furo_lst player in
  let zi_kaze = kyoku_to_kaze kyoku player in
  let yaku = tapl_player yaku_lst player in
  let ((x,y),yama_lst,dora_lst) = rinsyan dora_lst yama_lst in
  let (ary,zi_ary) = tapl_player ary_lst player in
  let (t_b,t_ten) = possible_tumo_agari (ary,zi_ary) zi_kaze ba naki f_lst (x,y) (Rinsyankaihou::yaku) dora_lst (tapl_player sutehai_lst player) yama_lst (tapl_player tehai_lst player) in
  let tehai = add_tehai tehai (x,y) in
  let tehai_lst = add_lst tehai_lst tehai player in
  let (ten_0,ten_1,ten_2,ten_3) = 
      if t_b = true then
        (tumo_agari t_ten player kyoku honba kyotaku)
      else
        (0,0,0,0)
  in
  (yama_lst,dora_lst,tehai_lst,(x,y),(ten_0,ten_1,ten_2,ten_3))

(*not automatic*)
(*
let furo_inq2 lst ary_lst sutehai_lst furo_lst yaku_lst yama_len ba kyoku naki_lst dora_lst (x,y) furo_double_lst = 
  let rec loop' i tmp = 
    let (m,n) = List.nth lst i in
    let (ary,zi_ary) = List.nth ary_lst m in
    let tmp =
      if n <> [] && (List.nth yaku_lst m) = [] then
        let tehai = ary_to_list ary zi_ary in
        let zi_kaze = kyoku_to_kaze kyoku m in
        let len = what_furo n sutehai_lst tehai furo_lst yaku_lst m yama_len zi_kaze ba (List.nth naki_lst m) dora_lst (x,y) furo_double_lst in
        if len <> [] then
          let x = (m,furo_naumber ary (List.hd len) (x,y))::tmp in
          x
        else
          tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  loop' 2 []
*)

(*automatic*)
let furo_inq2 (lst:(int*furo list) list) ary_lst sutehai_lst furo_lst yaku_lst yama_len ba kyoku naki_lst dora_lst (x,y) furo_double_lst furoritu_lst furiten_lst  = 
  let rec loop' i tmp = 
    let (m,n) = List.nth lst i in
    let (ary,zi_ary) = tapl_player ary_lst m in
    let tmp =
      if n <> [] && (tapl_player yaku_lst m) = [] then
        let tehai = ary_to_list ary zi_ary in
        let zi_kaze = kyoku_to_kaze kyoku m in
        let furo_t_f = purob_furo sutehai_lst tehai furo_lst yaku_lst m yama_len zi_kaze ba (tapl_player naki_lst m) dora_lst (x,y) furo_double_lst furoritu_lst furiten_lst in
        if furo_t_f = [] then 
          tmp 
        else
          let (_,f_hai) = List.hd furo_t_f in 
          (m,f_hai)::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  loop' 2 []

(*not automatic*)
(*
let furo_inq ary_lst furo_lst naki_lst (x,y) player yama_lst tehai_lst yaku_lst dora_lst ba kyoku kyotaku honba sutehai_lst furo_double_lst = 
  let (x,y) = hai_to_ary (x,y) in
  let rec loop' i tmp = 
    let furo = List.nth furo_lst i in
    let (ary,zi_ary) = List.nth ary_lst i in
    let ary2 = Array.map (fun a -> Array.copy a) ary in
    let zi_ary2 = Array.copy zi_ary in
    let (ary2,zi_ary2) = ary_sub_furo ary2 zi_ary2 furo in
    let lst = possible_furo ary2 zi_ary2 (x,y) in
    if i = 0 then
      lst::tmp
    else
      loop' (i-1) (lst::tmp)
  in
  let p_f_lst = loop' 3 [] in
  let rec loop2' i tmp = 
    let ti = 
      if player = 3 then
        if i = 0 then
          List.nth p_f_lst 0 
        else
          []
      else
        if i = player + 1 then
          List.nth p_f_lst i
        else
          []    
    in
    let pon = List.nth p_f_lst i in
    let pon = List.filter (fun a -> a <> Ti) pon in
    let tmp = 
      if ti <> [] then
        (i,ti)::tmp
      else
        if pon <> [] then
          (i,pon)::tmp
        else
          (i,[])::tmp
    in
    if (i+1) mod 4 = player then
      tmp
    else
      if i = 3 then
        loop2' 0 tmp
      else
        loop2' (i+1) tmp
    in
    let c_f_lst =
      if player = 3 then  
        loop2' 0 []
      else
        loop2' (player+1) []
    in
    let c_f_lst2 = furo_inq2 c_f_lst ary_lst sutehai_lst furo_lst yaku_lst (List.length yama_lst) ba kyoku naki_lst dora_lst (x,y) furo_double_lst in(*一つのfuro_hai*)
    let yaku_lst = 
      if c_f_lst2 = [] then 
        yaku_lst
      else
        List.map (fun a -> List.filter (fun b -> b <> Ippatu) a) yaku_lst
    in
    if c_f_lst2 = [] then
      (furo_lst,naki_lst,player,yama_lst,dora_lst,tehai_lst,(0,0,0,0),furo_double_lst,yaku_lst)
    else if List.length c_f_lst2 = 1 then
      let (j,furo_hai) = List.hd c_f_lst2 in
      let lst =  List.nth furo_lst j in
      let lst = furo_hai::lst in
      let furo_lst = add_furo_lst furo_lst lst j in
      let naki_lst = add_naki_lst naki_lst j in
      let tehai = List.nth tehai_lst j in
      let tehai = remove_furo_hai tehai furo_hai (x,y) in
      let tehai_lst = add_tehai_lst tehai_lst tehai j in
      let (h,_) = furo_hai in
      let (yama_lst,dora_lst,tehai_lst,(a,b,c,d)) = 
        if h = Minkan then
          let (yama_lst,dora_lst,tehai_lst,_,(a,b,c,d)) = kan yama_lst tehai_lst ary_lst furo_lst naki_lst yaku_lst dora_lst j ba kyoku kyotaku honba sutehai_lst in
          (yama_lst,dora_lst,tehai_lst,(a,b,c,d))
        else
          (yama_lst,dora_lst,tehai_lst,(0,0,0,0))
        in
      (furo_lst,naki_lst,j,yama_lst,dora_lst,tehai_lst,(a,b,c,d),(x,y)::furo_double_lst,yaku_lst)
    else
      let (j1,furo_hai1) = List.nth c_f_lst2 0 in
      let (j2,furo_hai2) = List.nth c_f_lst2 1 in
      let (j,furo_hai) = 
        let (s,_) = furo_hai1 in
        if s = Minko || s = Minkan then
          (j1,furo_hai1)
        else
          (j2,furo_hai2)
      in
      let lst =  List.nth furo_lst j in
      let lst = furo_hai::lst in
      let furo_lst = add_furo_lst furo_lst lst j in
      let naki_lst = add_naki_lst naki_lst j in
      let tehai = List.nth tehai_lst j in
      let tehai = remove_furo_hai tehai furo_hai (x,y) in
      let tehai_lst = add_tehai_lst tehai_lst tehai j in
      let (h,_) = furo_hai in
      let (yama_lst,dora_lst,tehai_lst,(a,b,c,d)) = 
        if h = Minkan then
          let (yama_lst,dora_lst,tehai_lst,_,(a,b,c,d)) = kan yama_lst tehai_lst ary_lst furo_lst naki_lst yaku_lst dora_lst j ba kyoku kyotaku honba sutehai_lst in
          (yama_lst,dora_lst,tehai_lst,(a,b,c,d))
        else
          (yama_lst,dora_lst,tehai_lst,(0,0,0,0))
        in
      (furo_lst,naki_lst,j,yama_lst,dora_lst,tehai_lst,(a,b,c,d),(x,y)::furo_double_lst,yaku_lst)

*)

(*automatic*)
let furo_inq ary_lst furo_lst naki_lst (x,y) player yama_lst tehai_lst yaku_lst dora_lst ba kyoku kyotaku honba sutehai_lst furo_double_lst furoritu_lst furiten_lst  = 
  let (x,y) = hai_to_ary (x,y) in
  let rec loop' i tmp = 
    let furo = tapl_player furo_lst i in
    let (ary,zi_ary) = tapl_player ary_lst i in
    let ary2 = Array.map (fun a -> Array.copy a) ary in
    let zi_ary2 = Array.copy zi_ary in
    let (ary2,zi_ary2) = ary_sub_furo ary2 zi_ary2 furo in
    let lst = possible_furo ary2 zi_ary2 (x,y) in
    if i = 0 then
      lst::tmp
    else
      loop' (i-1) (lst::tmp)
  in
  let p_f_lst = loop' 3 [] in
  let rec loop2' i tmp = 
    let ti = 
      if player = 3 then
        if i = 0 then
          List.nth p_f_lst 0 
        else
          []
      else
        if i = player + 1 then
          List.nth p_f_lst i
        else
          []    
    in
    let pon = List.nth p_f_lst i in
    let pon = List.filter (fun a -> a <> Ti) pon in
    let tmp = 
      if ti <> [] then
        (i,ti)::tmp
      else
        if pon <> [] then
          (i,pon)::tmp
        else
          (i,[])::tmp
    in
    if (i+1) mod 4 = player then
      tmp
    else
      if i = 3 then
        loop2' 0 tmp
      else
        loop2' (i+1) tmp
    in
    let c_f_lst =
      if player = 3 then  
        loop2' 0 []
      else
        loop2' (player+1) []
    in
    let c_f_lst2 = furo_inq2 c_f_lst ary_lst sutehai_lst furo_lst yaku_lst (List.length yama_lst) ba kyoku naki_lst dora_lst (x,y) furo_double_lst furoritu_lst furiten_lst  in(*一つのfuro_hai*)
    let yaku_lst = 
      if c_f_lst2 = [] then 
        yaku_lst
      else
        filter_yaku yaku_lst
    in
    if c_f_lst2 = [] then
      (furo_lst,naki_lst,player,yama_lst,dora_lst,tehai_lst,(0,0,0,0),furo_double_lst,yaku_lst)
    else if List.length c_f_lst2 = 1 then
      let (j,furo_hai) = List.hd c_f_lst2 in
      let lst =  tapl_player furo_lst j in
      let lst = furo_hai::lst in
      let furo_lst = add_lst furo_lst lst j in
      let naki_lst = add_lst naki_lst true j in
      let tehai = tapl_player tehai_lst j in
      let tehai = remove_furo_hai tehai furo_hai (x,y) in
      let tehai_lst = add_lst tehai_lst tehai j in
      let (h,_) = furo_hai in
      let (yama_lst,dora_lst,tehai_lst,(a,b,c,d)) = 
        if h = Minkan then
          let (yama_lst,dora_lst,tehai_lst,_,(a,b,c,d)) = kan yama_lst tehai_lst ary_lst furo_lst naki_lst yaku_lst dora_lst j ba kyoku kyotaku honba sutehai_lst in
          (yama_lst,dora_lst,tehai_lst,(a,b,c,d))
        else
          (yama_lst,dora_lst,tehai_lst,(0,0,0,0))
        in
      (furo_lst,naki_lst,j,yama_lst,dora_lst,tehai_lst,(a,b,c,d),(x,y)::furo_double_lst,yaku_lst)
    else
      let (j1,furo_hai1) = List.nth c_f_lst2 0 in
      let (j2,furo_hai2) = List.nth c_f_lst2 1 in
      let (j,furo_hai) = 
        let (s,_) = furo_hai1 in
        if s = Minko || s = Minkan then
          (j1,furo_hai1)
        else
          (j2,furo_hai2)
      in
      let lst =  tapl_player furo_lst j in
      let lst = furo_hai::lst in
      let furo_lst = add_lst furo_lst lst j in
      let naki_lst = add_lst naki_lst true j in
      let tehai = tapl_player tehai_lst j in
      let tehai = remove_furo_hai tehai furo_hai (x,y) in
      let tehai_lst = add_lst tehai_lst tehai j in
      let (h,_) = furo_hai in
      let (yama_lst,dora_lst,tehai_lst,(a,b,c,d)) = 
        if h = Minkan then
          let (yama_lst,dora_lst,tehai_lst,_,(a,b,c,d)) = kan yama_lst tehai_lst ary_lst furo_lst naki_lst yaku_lst dora_lst j ba kyoku kyotaku honba sutehai_lst in
          (yama_lst,dora_lst,tehai_lst,(a,b,c,d))
        else
          (yama_lst,dora_lst,tehai_lst,(0,0,0,0))
        in
      (furo_lst,naki_lst,j,yama_lst,dora_lst,tehai_lst,(a,b,c,d),(x,y)::furo_double_lst,yaku_lst)


let possible_furiten furiten_lst yaku_lst (x,y) player = 
  let rec loop' i tmp furiten_lst' = 
    let furiten = tapl_player furiten_lst i in
    let yaku = tapl_player yaku_lst i in
    let furiten =
      if List.exists (fun a -> a = Reach) yaku then
        (x,y)::furiten
      else
        if i = player then
          []
        else
          (x,y)::furiten
    in
    let furiten_lst' = add_lst furiten_lst' furiten i in
    if i = 0 then
      furiten_lst'
    else
      loop' (i-1) tmp furiten_lst'
  in
  loop' 3 [] ([],[],[],[])

let serch_furiten ten_lst furiten_lst sutehai_lst = 
  let m = List.length ten_lst in
  let rec loop' i tmp =
    let ((a,b),_) = List.nth ten_lst i in 
    let (a,b) = ary_to_hai (a,b) in
    let sutehai = List.map (fun (x,y,z) -> (x,y)) sutehai_lst in
    let tmp = 
      if List.exists (fun x -> x = (a,b)) furiten_lst || List.exists (fun x -> x = (a,b)) sutehai then
        false
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 then
    true
  else
    loop' (m-1) true






let last_sutehai sutehai_lst player = 
  let lst = tapl_player sutehai_lst player in
  let m = List.length lst in
  let (a,b,_) = List.nth lst (m-1) in
  (a,b)

let same_matihai_sutehai ((x,y):(int*int)) lst = 
  let m = List.length lst in
  let rec loop' i tmp = 
    let (a,b) = List.nth lst i in
    let tmp = 
      if a = (x,y) then
        [b]
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop' (m-1) []

let opt_ron player lst honba kyotaku = 
  let m = List.length lst in
  let rec loop' i tmp = 
    let (x,y) = List.nth lst i in
    let (x1,y1) = tmp in
    let tmp =
      if player = 0 || player = 3 then
        if x < x1 then
          (x,y) 
        else
          tmp
      else if player = 1 then
        if x = 0 then
          tmp
        else if x < x1 then
          (x,y)
        else
          tmp
      else 
        if x = 3 then
          (x,y)
        else
          if x < x1 then
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
      (0,0,0,0)
    else
      let n = loop' (m-1) (List.hd lst) in
      let (a,(_,b)) = n in
      if b > 0 then 
        if player = 0 then
          if a = 1 then
            (-(b+honba*300),b+honba*300+kyotaku*1000,0,0)
          else if a = 2 then
            (-(b+honba*300),0,b+honba*300+kyotaku*1000,0)
          else 
            (-(b+honba*300),0,0,b+honba*300+kyotaku*1000)
        else if player = 1 then
          if a = 0 then
            (b+honba*300+kyotaku*1000,-(b+honba*300),0,0)
          else if a = 2 then
            (0,-(b+honba*300),b+honba*300+kyotaku*1000,0)
          else 
            (0,-(b+honba*300),0,b+honba*300+kyotaku*1000)
        else if player = 2 then
          if a = 0 then
            (b+honba*300+kyotaku*1000,0,-(b+honba*300),0)
          else if a = 1 then
            (0,b+honba*300+kyotaku*1000,-(b+honba*300),0)
          else 
            (0,0,-(b+honba*300),b+honba*300+kyotaku*1000)
        else
          if a = 0 then
            (b+honba*300+kyotaku*1000,0,0,-(b+honba*300))
          else if a = 1 then
            (0,b+honba*300+kyotaku*1000,0,-(b+honba*300))
          else 
            (0,0,b+honba*300+kyotaku*1000,-(b+honba*300))
      else
        (0,0,0,0)

let ron_inq i = 
  let rec loop' () = 
    (*plint_player i*)
    (*Printf.printf "Ron/Not? y/n\n";*)
    let n = autoagari() in
    if n = "y" then
      true
    else if n = "n" then
      false
    else
      loop' ()
  in
  loop' ()

let furo_kan_swap lst (x,y) = 
  let (x,y) = hai_to_ary (x,y) in
  let m = List.length lst in
  let rec loop' i tmp = 
    let (a,(b,(c,_,_))) = List.nth lst i in
    let tmp = 
      if a = Minko then
        if (b,c) = (x,y) then
          (Minkan,(b,(c,c,c)))::tmp
        else
          (List.nth lst i)::tmp
      else
        (List.nth lst i)::tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  loop' (m-1) []

let possible_ron ary_lst player naki_lst furo_lst (x,y) kyoku ba yaku_lst dora_lst honba kyotaku furiten_lst sutehai_lst yama_lst tehai_lst = 
  let (x,y) = hai_to_ary (x,y) in
  let rec loop' i tmp = 
    let tmp =
      if i = player then
        tmp
      else
        let (ary,zi_ary) = tapl_player ary_lst i in
        let naki = tapl_player naki_lst i in
        let zi_kaze = kyoku_to_kaze kyoku i in 
        let f_lst = tapl_player furo_lst i in
        let red = tehai_in_red (tapl_player tehai_lst i) in 
        let lst = 
        if List.exists (fun a -> a = Reach || a = Doublereach) (tapl_player yaku_lst i) then 
          tehai_to_ten ary zi_ary zi_kaze ba naki f_lst (tapl_player yaku_lst i) (uradora dora_lst yama_lst) red 
        else  
          tehai_to_ten ary zi_ary zi_kaze ba naki f_lst (tapl_player yaku_lst i) dora_lst red 
        in
        let n = serch_furiten lst (tapl_player furiten_lst i) (tapl_player sutehai_lst i) in
        if lst = [] then
          tmp
        else
          if n = true then
            (i,lst)::tmp
          else
            tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
    in
  let ron = loop' 3 [] in
  let rec loop2' i lst tmp =
    let t_lst = List.nth lst i in
    let (a,b) = t_lst in
    let n = same_matihai_sutehai (x,y) b in
    let tmp =
      if n = [] then
        tmp
      else
        if ron_inq i = true then
          (a,List.hd n)::tmp
        else
          tmp
    in
    if i = 0 then
      tmp
    else
      loop2' (i-1) lst tmp
  in   
  let m = List.length ron in
  if m = 0 then
    (kyotaku,(0,0,0,0))
  else
    let ten = opt_ron player (loop2' (m-1) ron []) honba kyotaku in
    if ten = (0,0,0,0) then
      (kyotaku,ten)
    else
      (0,ten)

let kakan_inq i = 
  let rec loop' () = 
    (*plint_player i*)
    (*Printf.printf "Kan/Not? k/n\n";*)
    let n = nofuro () in
    if n = "k" then
      true
    else if n = "n" then
      false
    else
      loop' ()
  in
  loop' ()

let possible_kakan ary_lst player naki_lst furo_lst (x,y) kyoku ba yaku_lst dora_lst honba kyotaku furiten_lst sutehai_lst yama_lst tehai_lst = 
  let (x,y) = hai_to_ary (x,y) in
  let f_lst = tapl_player furo_lst player in
  let m = List.length f_lst in
  let rec loop' i tmp = 
    let (a,(b,(c,_,_))) = List.nth f_lst i in
    let tmp =
      if a = Minko then
        if (b,c) = (x,y) then
          kakan_inq player
        else
          tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
    in
  let n = 
    if m = 0 then
      false
    else
      loop' (m-1) false 
  in
  if n = true then
    let yaku_lst1 = (Tyankan::(tapl_player_1 yaku_lst),Tyankan::(tapl_player_2 yaku_lst),Tyankan::(tapl_player_3 yaku_lst),Tyankan::(tapl_player_4 yaku_lst)) in
    let (x,y) = ary_to_hai (x,y) in
    let (kyotaku,(a,b,c,d)) = possible_ron ary_lst player naki_lst furo_lst (x,y) kyoku ba yaku_lst1 dora_lst honba kyotaku furiten_lst sutehai_lst yama_lst tehai_lst in
    (true,(kyotaku,(a,b,c,d)))
  else
    (false,(kyotaku,(0,0,0,0)))


(*not automatic*)
(*
let rec furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,y) player furo_lst naki_lst yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst =
  let (n,(kyotaku,(a1,b1,c1,d1))) = possible_kakan ary_lst player naki_lst furo_lst (x,y) kyoku ba yaku_lst dora_lst honba kyotaku furiten_lst sutehai_lst yama_lst in
  let furiten_lst = possible_furiten furiten_lst yaku_lst (x,y) player in
  let (yama_lst',naki_lst',player',dora_lst,furo_lst',tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst) =
    if n = true then
      if (a1,b1,c1,d1) = (0,0,0,0) then
        let furo_lst = add_furo_lst furo_lst (furo_kan_swap (List.nth furo_lst player) (x,y)) player in
        let (yama_lst,dora_lst,tehai_lst,(x,y),(a,b,c,d)) = kan yama_lst tehai_lst ary_lst furo_lst naki_lst yaku_lst dora_lst player ba kyoku honba kyotaku sutehai_lst in
        (yama_lst,naki_lst,player,dora_lst,furo_lst,tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
      else
        (yama_lst,naki_lst,player,dora_lst,furo_lst,tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a1,b1,c1,d1),furo_double_lst)
    else
      let (tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku) = kiriban tehai_lst sutehai_lst ary_lst (x,y) player furo_lst (List.nth naki_lst player) yaku_lst dora_lst kyoku ba kyotaku player_score honba (List.length yama_lst) furo_double_lst in
      let (kyotaku,(a,b,c,d)) = possible_ron ary_lst player naki_lst furo_lst (last_sutehai sutehai_lst player) kyoku ba yaku_lst dora_lst honba kyotaku furiten_lst sutehai_lst yama_lst in
      if (a,b,c,d) <> (0,0,0,0) then
        (yama_lst,naki_lst,player,dora_lst,furo_lst,tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
      else
          let (furo_lst',naki_lst',player',yama_lst,dora_lst,tehai_lst,(a,b,c,d),furo_double_lst,yaku_lst) =
            furo_inq ary_lst furo_lst naki_lst (last_sutehai sutehai_lst player) player yama_lst tehai_lst yaku_lst dora_lst ba kyoku kyotaku honba sutehai_lst furo_double_lst in
          if (a,b,c,d) <> (0,0,0,0) then
            (yama_lst,naki_lst',player',dora_lst,furo_lst',tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
          else
            (yama_lst,naki_lst',player',dora_lst,furo_lst',tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
  in
  if (player = player' &&  (List.length yama_lst) = (List.length yama_lst'))|| (a,b,c,d) <> (0,0,0,0) then
    (tehai_lst,sutehai_lst,ary_lst,furo_lst,naki_lst,player,yama_lst,yaku_lst,dora_lst,(a,b,c,d),kyotaku,player_score,furiten_lst,furo_double_lst)
  else
    if player = player' then
      furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,y) player' furo_lst' naki_lst' yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst
    else
      furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,Not_hai) player' furo_lst' naki_lst' yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst
*)

(*automatic*)
let rec furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,y) player furo_lst naki_lst yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst furoritu_lst =
  let (n,(kyotaku,(a1,b1,c1,d1))) = possible_kakan ary_lst player naki_lst furo_lst (x,y) kyoku ba yaku_lst dora_lst honba kyotaku furiten_lst sutehai_lst yama_lst tehai_lst in
  let furiten_lst = possible_furiten furiten_lst yaku_lst (x,y) player in
  let (yama_lst',naki_lst',player',dora_lst,furo_lst',tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst) =
    if n = true then
      if (a1,b1,c1,d1) = (0,0,0,0) then
        let furo_lst = add_lst furo_lst (furo_kan_swap (tapl_player furo_lst player) (x,y)) player in
        let (yama_lst,dora_lst,tehai_lst,(x,y),(a,b,c,d)) = kan yama_lst tehai_lst ary_lst furo_lst naki_lst yaku_lst dora_lst player ba kyoku honba kyotaku sutehai_lst in
        (yama_lst,naki_lst,player,dora_lst,furo_lst,tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
      else
        (yama_lst,naki_lst,player,dora_lst,furo_lst,tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a1,b1,c1,d1),furo_double_lst)
    else
      let (tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku) = kiriban tehai_lst sutehai_lst ary_lst  player furo_lst (tapl_player naki_lst player) yaku_lst dora_lst kyoku ba kyotaku player_score (List.length yama_lst) furo_double_lst furiten_lst  in
      let (kyotaku,(a,b,c,d)) = possible_ron ary_lst player naki_lst furo_lst (last_sutehai sutehai_lst player) kyoku ba yaku_lst dora_lst honba kyotaku furiten_lst sutehai_lst yama_lst tehai_lst in
      if (a,b,c,d) <> (0,0,0,0) then
        (yama_lst,naki_lst,player,dora_lst,furo_lst,tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
      else
          let (furo_lst',naki_lst',player',yama_lst,dora_lst,tehai_lst,(a,b,c,d),furo_double_lst,yaku_lst) =
            furo_inq ary_lst furo_lst naki_lst (last_sutehai sutehai_lst player) player yama_lst tehai_lst yaku_lst dora_lst ba kyoku kyotaku honba sutehai_lst furo_double_lst furoritu_lst furiten_lst  in
          if (a,b,c,d) <> (0,0,0,0) then
            (yama_lst,naki_lst',player',dora_lst,furo_lst',tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
          else
            (yama_lst,naki_lst',player',dora_lst,furo_lst',tehai_lst,sutehai_lst,ary_lst,yaku_lst,player_score,kyotaku,(x,y),(a,b,c,d),furo_double_lst)
          in
  if (player = player' &&  (List.length yama_lst) = (List.length yama_lst'))|| (a,b,c,d) <> (0,0,0,0) then
    (tehai_lst,sutehai_lst,ary_lst,furo_lst,naki_lst,player,yama_lst,yaku_lst,dora_lst,(a,b,c,d),kyotaku,player_score,furiten_lst,furo_double_lst)
  else
    if player = player' then
      furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,y) player' furo_lst' naki_lst' yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst furoritu_lst
    else
      furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,Not_hai) player' furo_lst' naki_lst' yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst furoritu_lst

let ten_to_player player_score (a,b,c,d) = 
  let a1 = tapl_player_1 player_score in 
  let b1 = tapl_player_2 player_score in 
  let c1 = tapl_player_3 player_score in 
  let d1 = tapl_player_4 player_score in 
  (a1+a,b1+b,c1+c,d1+d)
(*not automatic*)
(*
let kyoku_start_end ba kyoku tehai_lst yama_lst dora_lst honba kyotaku player_score = 
  let furo_lst = [[];[];[];[]] in
  let naki_lst = [false;false;false;false] in
  let ary_lst = List.map (fun a -> list_to_ary a) tehai_lst in
  let yaku_lst = [[];[];[];[]] in
  let sutehai_lst = [[];[];[];[]] in
  let furiten_lst = [[];[];[];[]] in
  let rec loop' player tehai_lst sutehai_lst yama_lst (furo_lst:(Mahjong_base.state*(int*(int*int*int))) list list) ary_lst naki_lst dora_lst (yaku_lst:Mahjong_base.yaku list list) kyotaku player_score furiten_lst furo_double_lst = 
    let zi_kaze = kyoku_to_kaze kyoku player in
    let tehai = List.nth tehai_lst player in
    let (x,y,_) = List.hd yama_lst in
    let yama_lst = List.tl yama_lst in
    let tehai = add_tehai tehai (x,y) in
    let (t_b,t_ten) = possible_tumo_agari (List.nth ary_lst player) zi_kaze ba (List.nth naki_lst player) (List.nth furo_lst player) (x,y) (List.nth yaku_lst player) dora_lst (List.nth sutehai_lst player) yama_lst in
    let (tehai, f, yama_lst, dora_lst,(x',y')) = 
      if possible_ankan (List.nth ary_lst player) (x,y) = true then
        ankan (x,y) tehai (List.nth furo_lst player) yama_lst dora_lst
      else
        (tehai,List.nth furo_lst player,yama_lst,dora_lst,(x,y)) 
    in
    let furo_lst = add_furo_lst furo_lst f player in
    let (t_b,t_ten) = 
      if (x,y) = (x',y') then
        (t_b,t_ten)
      else
        possible_tumo_agari (List.nth ary_lst player) zi_kaze ba (List.nth naki_lst player) (List.nth furo_lst player) (x',y') (Rinsyankaihou::(List.nth yaku_lst player)) dora_lst (List.nth sutehai_lst player) yama_lst
    in
    let (ten_0,ten_1,ten_2,ten_3) = 
      if t_b = true then
        (tumo_agari t_ten player kyoku honba kyotaku)
      else
        (0,0,0,0)
    in
    if (ten_0,ten_1,ten_2,ten_3) <> (0,0,0,0) then
      ((*let tehai_lst = add_tehai_lst tehai_lst tehai player in
      t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst;*)
      (0,(ten_0,ten_1,ten_2,ten_3),player_score))
    else
        let tehai_lst = add_tehai_lst tehai_lst tehai player in
        let (tehai_lst,sutehai_lst,ary_lst,furo_lst,naki_lst,player,yama_lst,yaku_lst,dora_lst,(a,b,c,d),kyotaku,player_score,furiten_lst,furo_double_lst) = furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,y) player furo_lst naki_lst yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst in
        if (a,b,c,d) = (0,0,0,0) then
          let player = 
            if player = 3 then
              0
            else
              player + 1 
          in
          if List.length yama_lst = 14 then
            ((*t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst;*)
            let tenpai_lst = ryukyoku_ten ary_lst furo_lst [] dora_lst in
            let (ten_0,ten_1,ten_2,ten_3) = tenpai_ryo tenpai_lst in
            let player_score = ten_to_player player_score (ten_0,ten_1,ten_2,ten_3) in
            (kyotaku,(ten_0/100,ten_1/100,ten_2/100,ten_3/100),player_score))
          else
            loop' player tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst furo_double_lst
        else
          ((*t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst;*)
          (0,(a,b,c,d),player_score))
    in
    loop' (kyoku-1) tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst []
*)

let make_ary_lst (a,b,c,d) = 
  let a = list_to_ary a in
  let b = list_to_ary b in 
  let c = list_to_ary c in 
  let d = list_to_ary d in 
  (a,b,c,d)

(*
(*automatic*)
let kyoku_start_end ba kyoku tehai_lst yama_lst dora_lst honba kyotaku player_score furoritu_lst = 
  let furo_lst = ([],[],[],[]) in
  let naki_lst = (false,false,false,false) in
  let ary_lst = make_ary_lst tehai_lst in
  let yaku_lst = ([],[],[],[]) in
  let sutehai_lst = ([],[],[],[]) in
  let furiten_lst = ([],[],[],[]) in
  let rec loop' player tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst furo_double_lst = 
    let zi_kaze = kyoku_to_kaze kyoku player in
    let tehai = tapl_player tehai_lst player in
    let (x,y,_) = List.hd yama_lst in
    let yama_lst = List.tl yama_lst in
    let tehai = add_tehai tehai (x,y) in
    let (t_b,t_ten) = possible_tumo_agari (tapl_player ary_lst player) zi_kaze ba (tapl_player naki_lst player) (tapl_player furo_lst player) (x,y) (tapl_player yaku_lst player) dora_lst (tapl_player sutehai_lst player) yama_lst in
    let (tehai, f, yama_lst, dora_lst,(x',y')) = 
      if possible_ankan (tapl_player ary_lst player) (x,y) = true then
        ankan (x,y) tehai (tapl_player furo_lst player) yama_lst dora_lst
      else
        (tehai,(tapl_player furo_lst player),yama_lst,dora_lst,(x,y)) 
    in
    let furo_lst = add_lst furo_lst f player in
    let (t_b,t_ten) = 
      if (x,y) = (x',y') then
        (t_b,t_ten)
      else
        possible_tumo_agari (tapl_player ary_lst player) zi_kaze ba (tapl_player naki_lst player) (tapl_player furo_lst player) (x',y') (Rinsyankaihou::(tapl_player yaku_lst player)) dora_lst (tapl_player sutehai_lst player) yama_lst
    in
    let (ten_0,ten_1,ten_2,ten_3) = 
      if t_b = true then
        (tumo_agari t_ten player kyoku honba kyotaku)
      else
        (0,0,0,0)
    in
    if (ten_0,ten_1,ten_2,ten_3) <> (0,0,0,0) then
      ((*let tehai_lst = add_tehai_lst tehai_lst tehai player in
      t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst; flush stdout;*)
      (0,(ten_0,ten_1,ten_2,ten_3),player_score))
    else
        let tehai_lst = add_lst tehai_lst tehai player in
        let (tehai_lst,sutehai_lst,ary_lst,furo_lst,naki_lst,player,yama_lst,yaku_lst,dora_lst,(a,b,c,d),kyotaku,player_score,furiten_lst,furo_double_lst) = furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,y) player furo_lst naki_lst yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst furoritu_lst in
        if (a,b,c,d) = (0,0,0,0) then
          let player = 
            if player = 3 then
              0
            else
              player + 1 
          in
          if List.length yama_lst = 14 then
            ((*t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst; flush stdout;*)
            let tenpai_lst = ryukyoku_ten ary_lst furo_lst [] dora_lst in
            let (ten_0,ten_1,ten_2,ten_3) = tenpai_ryo tenpai_lst in
            let player_score = ten_to_player player_score (ten_0,ten_1,ten_2,ten_3) in
            (kyotaku,(ten_0/100,ten_1/100,ten_2/100,ten_3/100),player_score))
          else
            loop' player tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst furo_double_lst
        else
          ((*t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst; flush stdout;*)
          (0,(a,b,c,d),player_score))
    in
    loop' (kyoku-1) tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst []
*)


(*automatic kyoku simulate version*)
let kyoku_start_end ba kyoku tehai_lst yama_lst dora_lst honba kyotaku player_score furoritu_lst = 
  let furo_lst = ([],[],[],[]) in
  let naki_lst = (false,false,false,false) in
  let ary_lst = make_ary_lst tehai_lst in
  let yaku_lst = ([],[],[],[]) in
  let sutehai_lst = ([],[],[],[]) in
  let furiten_lst = ([],[],[],[]) in
  let rec loop' player tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst furo_double_lst = 
    let zi_kaze = kyoku_to_kaze kyoku player in
    let tehai = tapl_player tehai_lst player in
    let (x,y,_) = List.hd yama_lst in
    let r_tmp_tapl = !global_red in 
    let (r_m,r_p,r_s) = tapl_player r_tmp_tapl player in 
    let r_player_red = if y = Manzu_red then (1,r_p,r_s) else if y = Pinzu_red then (r_m,1,r_s) else if y = Souzu_red then (r_m,r_p,1) else (r_m,r_p,r_s) in 
    let r_tmp_tapl = tapl_player_in r_tmp_tapl r_player_red player in 
    global_red :=  r_tmp_tapl; 
    let yama_lst = List.tl yama_lst in
    let tehai = add_tehai tehai (x,y) in
    let (t_b,t_ten) = possible_tumo_agari (tapl_player ary_lst player) zi_kaze ba (tapl_player naki_lst player) (tapl_player furo_lst player) (x,y) (tapl_player yaku_lst player) dora_lst (tapl_player sutehai_lst player) yama_lst (tapl_player tehai_lst player) in
    let (tehai, f, yama_lst, dora_lst,(x',y')) = 
      if possible_ankan (tapl_player ary_lst player) (x,y) = true then
        ankan (x,y) tehai (tapl_player furo_lst player) yama_lst dora_lst
      else
        (tehai,(tapl_player furo_lst player),yama_lst,dora_lst,(x,y)) 
    in
    let furo_lst = add_lst furo_lst f player in
    let (t_b,t_ten) = 
      if (x,y) = (x',y') then
        (t_b,t_ten)
      else
        possible_tumo_agari (tapl_player ary_lst player) zi_kaze ba (tapl_player naki_lst player) (tapl_player furo_lst player) (x',y') (Rinsyankaihou::(tapl_player yaku_lst player)) dora_lst (tapl_player sutehai_lst player) yama_lst (tapl_player tehai_lst player)
    in
    let (ten_0,ten_1,ten_2,ten_3) = 
      if t_b = true then
        (tumo_agari t_ten player kyoku honba kyotaku)
      else
        (0,0,0,0)
    in
    if (ten_0,ten_1,ten_2,ten_3) <> (0,0,0,0) then
      ((*let tehai_lst = add_tehai_lst tehai_lst tehai player in
      t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst; flush stdout;*)
      (0,(ten_0,ten_1,ten_2,ten_3),naki_lst,player_score))
    else
        let tehai_lst = add_lst tehai_lst tehai player in
        let (tehai_lst,sutehai_lst,ary_lst,furo_lst,naki_lst,player,yama_lst,yaku_lst,dora_lst,(a,b,c,d),kyotaku,player_score,furiten_lst,furo_double_lst) = furo_loop yama_lst tehai_lst sutehai_lst ary_lst (x,y) player furo_lst naki_lst yaku_lst dora_lst kyoku ba honba kyotaku player_score furiten_lst furo_double_lst furoritu_lst in
        if (a,b,c,d) = (0,0,0,0) then
          let player = 
            if player = 3 then
              0
            else
              player + 1 
          in
          if List.length yama_lst = 14 then
            ((*t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst; flush stdout;*)
            let tenpai_lst = ryukyoku_ten tehai_lst in
            let (ten_0,ten_1,ten_2,ten_3) = tenpai_ryo tenpai_lst in
            let player_score = ten_to_player player_score (ten_0,ten_1,ten_2,ten_3) in
            (kyotaku,(0,0,0,0),naki_lst,player_score))
          else
            loop' player tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst furo_double_lst
        else
          ((*t_format tehai_lst furo_lst sutehai_lst ba kyoku honba kyotaku player_score yaku_lst dora_lst; flush stdout;*)
          (0,(a,b,c,d),naki_lst,player_score))
    in
    loop' (kyoku-1) tehai_lst sutehai_lst yama_lst furo_lst ary_lst naki_lst dora_lst yaku_lst kyotaku player_score furiten_lst []



(*not automatic*)
(*
let kyoku_s ba kyoku honba kyotaku player_score  = 
  let lst2 = hai_lst @ hai_lst @ hai_lst @ hai_lst in
  let lst3 = r_hai lst2 in
  let lst4 = List.sort (fun (x1 ,y1 ,z1) (x2 ,y2 ,z2) -> if z1 < z2 then -1 else 1) lst3 in
  let (yama_lst,tehai_lst_0,tehai_lst_1,tehai_lst_2,tehai_lst_3,dora_lst) = haipai lst4 in
  let tehai_lst = 
    if kyoku = 1 then
      [tehai_lst_0;tehai_lst_1;tehai_lst_2;tehai_lst_3]
    else if kyoku = 2 then
      [tehai_lst_3;tehai_lst_0;tehai_lst_1;tehai_lst_2]
    else if kyoku = 3 then
      [tehai_lst_2;tehai_lst_3;tehai_lst_0;tehai_lst_1]
    else 
      [tehai_lst_1;tehai_lst_2;tehai_lst_3;tehai_lst_0]
  in
  kyoku_start_end ba kyoku tehai_lst yama_lst dora_lst honba kyotaku player_score
*)

(*automatic*)
let kyoku_s ba kyoku honba kyotaku player_score furoritu_lst count = 
  let lst2 = big_hai_lst_red in
  let lst3 = r_hai lst2 in
  (*let lst3 = r_hai2 lst2 count in*) 
  let lst4 = List.sort (fun (_ ,_ ,z1) (_ ,_ ,z2) -> if z1 < z2 then -1 else 1) lst3 in
  let (yama_lst,tehai_lst_0,tehai_lst_1,tehai_lst_2,tehai_lst_3,dora_lst) = haipai lst4 in
  let tehai_lst = 
    if kyoku = 1 then
      (tehai_lst_0,tehai_lst_1,tehai_lst_2,tehai_lst_3)
    else if kyoku = 2 then
      (tehai_lst_3,tehai_lst_0,tehai_lst_1,tehai_lst_2)
    else if kyoku = 3 then
      (tehai_lst_2,tehai_lst_3,tehai_lst_0,tehai_lst_1)
    else 
      (tehai_lst_1,tehai_lst_2,tehai_lst_3,tehai_lst_0)
  in
  kyoku_start_end ba kyoku tehai_lst yama_lst dora_lst honba kyotaku player_score furoritu_lst

(*not automatic*)
(*
let hantyan () = 
  let player_score = [25000;25000;25000;25000] in
  let rec loop' kyoku ba honba kyotaku player_score total_kyoku  = 
    let total_kyoku = total_kyoku + 1 in 
    let (kyotaku,(a,b,c,d),player_score) = kyoku_s ba kyoku honba kyotaku player_score in
    let rentyan = 
      if kyoku = 1 then
        if a > 0 then
          true
        else
          false
      else if kyoku = 2 then
        if b > 0 then
          true
        else
          false
      else if kyoku = 3 then
        if c > 0 then
          true
        else
          false
      else
        if d > 0 then
          true
        else
          false
        in
    let honba = 
      if a < 100 && b < 100 && c < 100 && d < 100 then
        honba + 1
      else if rentyan = true then
        honba + 1
      else
        0
    in
    let player_score = 
      if a < 100 && b < 100 && c < 100 && d < 100 then
        player_score
      else
        ten_to_player player_score (a,b,c,d)
    in
    if rentyan = true then
      loop' kyoku ba honba kyotaku player_score total_kyoku 
    else
      if kyoku = 4 then
        if ba = 0 then
          loop' 1 (ba+1) honba kyotaku player_score total_kyoku
        else
          (total_kyoku,(List.nth player_score 0),(List.nth player_score 1),(List.nth player_score 2),(List.nth player_score 3))
      else
        loop' (kyoku+1) ba honba kyotaku player_score total_kyoku 
    in
    loop' 1 0 0 0 player_score 0 
*)


(*automatic*)
(*
let hantyan furoritu_lst = 
  let furoritu_lst = 
    if (List.length furoritu_lst) = 4 then 
      furoritu_lst
    else
      [25.0;25.0;25.0;25.0]
  in
  let player_score = (25000,25000,25000,25000) in
  let rec loop' kyoku ba honba kyotaku player_score total_kyoku  = 
    let total_kyoku = total_kyoku + 1 in 
    let (kyotaku,(a,b,c,d),player_score) = kyoku_s ba kyoku honba kyotaku player_score furoritu_lst (total_kyoku+seed) in
    Hashtbl.clear myhash;
    let rentyan = 
      if kyoku = 1 then
        if a > 0 then
          true
        else
          false
      else if kyoku = 2 then
        if b > 0 then
          true
        else
          false
      else if kyoku = 3 then
        if c > 0 then
          true
        else
          false
      else
        if d > 0 then
          true
        else
          false
        in
    let honba = 
      if a < 100 && b < 100 && c < 100 && d < 100 then
        honba + 1
      else if rentyan = true then
        honba + 1
      else
        0
    in
    let player_score = 
      if a < 100 && b < 100 && c < 100 && d < 100 then
        player_score
      else
        ten_to_player player_score (a,b,c,d)
    in
    if rentyan = true then
      loop' kyoku ba honba kyotaku player_score total_kyoku 
    else
      if kyoku = 4 then
        if ba = 0 then
          loop' 1 (ba+1) honba kyotaku player_score total_kyoku
        else
          (total_kyoku,(tapl_player_1 player_score),(tapl_player_2 player_score),(tapl_player_3 player_score),(tapl_player_4 player_score))
      else
        loop' (kyoku+1) ba honba kyotaku player_score total_kyoku 
    in
    loop' 1 0 0 0 player_score 0 
*)

(*let _ = hantyan [25.0;25.0;25.0;25.0]*)


