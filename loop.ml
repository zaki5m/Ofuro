open Mahjong_base
(*Manzu,Pinzu,Souzu*)

let tyuren_a = [|[|4;1;1;1;1;1;1;1;3|];
                 [|3;2;1;1;1;1;1;1;3|];
                 [|3;1;2;1;1;1;1;1;3|];
                 [|3;1;1;2;1;1;1;1;3|];
                 [|3;1;1;1;2;1;1;1;3|];
                 [|3;1;1;1;1;2;1;1;3|];
                 [|3;1;1;1;1;1;2;1;3|];
                 [|3;1;1;1;1;1;1;2;3|];
                 [|3;1;1;1;1;1;1;1;4|];
               |]



let ten_oya = [|[|0;0;1500;2000;2400;2900;3400;3900;4400;4800;5300|];
              [|2100;2400;2900;3900;4800;5800;6800;7700;8700;9600;10600|];
              [|3900;4800;5800;7700;9600;11600;12000;12000;12000;12000;12000|];
              [|7700;9600;11600;12000;12000;12000;12000;12000;12000;12000;12000|]|]

let ten_ko = [|[|0;0;1000;1300;1600;2000;2300;2600;2900;3200;3600|];
              [|1300;1600;2000;2600;3200;3900;4500;5200;5800;6400;7100|];
              [|2600;3200;3900;5200;6400;7700;8000;8000;8000;8000;8000|];
              [|5200;6400;7700;8000;8000;8000;8000;8000;8000;8000;8000|]|]

let ten_oya_man = [|12000;18000;24000;36000;48000|]

let ten_ko_man = [|8000;12000;16000;24000;36000|]

let ten_tumo_oya ten =
  if ten mod 3 = 0 then
    ten/3
  else if ten mod 3 = 1 then
    (ten+200)/3
  else
    (ten+100)/3

let ten_tumo_ko ten =
  if ten mod 200 = 0 then
    let oya = ten/2 in
    if oya mod 200 = 0 then
      (oya,oya/2)
    else
      (oya,(oya+100)/2)
  else
    let oya = (ten+100)/2 in
    if oya mod 200 = 0 then
      (oya,oya/2)
    else
      (oya,(oya+100)/2)

let han_of_yaku naki = function  
  | Reach -> 1 | Ippatu -> 1 | Menzentumo -> 1 | Yakuhai -> 1
  | Tanyao -> 1 | Pinhu -> if naki = false then 1 else 0 | Ipeiko -> if naki = false then 1 else 0 | Haitei -> 1
  | Houtei -> 1 | Rinsyankaihou -> 1 | Tyankan -> 1
  | Doublereach -> 2 | Sansyokudouzyun -> if naki = false then 2 else 1 
  | Sansyokudoukou -> 2 | Sanankou -> 2 | Ikkitukan -> if naki = false then 2 else 1 
  | Titoitu -> 2 | Toitoi -> 2 | Tyanta -> if naki = false then 2 else 1 | Sankantu -> 2 |Syousangen -> 2
  | Honroutou -> 2 | Ryanpeikou -> if naki = false then 3 else 0 | Zyuntyan -> if naki = false then 3 else 2 | Honitu -> if naki = false then 3 else 2
  | Tinitu -> if naki = false then 6 else 5
  | Daisangen -> 100 | Kokusimusou -> 100 | Ryuiso -> 100 | Tuiso -> 100 | Tinroutou -> 100
  | Sukantu -> 100 | Syoususi -> 100 | Daisusi -> 100| Tyurenpoutou -> 100 | Tihou -> 100 
  | Tenhou -> 100 | Suankou -> 100

let rec add_state_head list state = match list with
  | [] -> []
  | h::t -> let add_state2 list state = 
              if List.hd list = Toitu then
                list
              else
                state::list
            in
            if h = [] then
              (add_state_head t state)
            else
              (add_state2 h state)::(add_state_head t state)

let rec add_state list list2 = match list with
  | [] -> []
  | h::t -> (list2 @ h)::(add_state t list2)

let rec add_yaku y_lst yaku_lst = match y_lst with
  | [] -> []
  | h::t -> (h@yaku_lst)::(add_yaku t yaku_lst)

let rec remove_ws  = function
  | [] -> []
  | h::t -> let rec check = function
              | [] -> []
              | h::t -> if (List.for_all (fun x -> x <> Ws) h) = true then
                          if (List.length h ) = 5 then
                            h::(check t)
                          else
                            check t
                        else
                            check t 
                        in
            (check h)::(remove_ws t) 

                
let rec remove_to = function
  | [] -> []
  | h::t -> if h = Toitu then
              remove_to t
            else
              h::remove_to t


let rec check_Anko = function
  | [] -> []
  | h::t -> if h <> [] then
              if List.hd h = Anko then
                if List.filter (fun x -> x <> Anko) h = [] then
                  h::check_Anko t
                else
                (List.tl h)::check_Anko t
              else
                h::(check_Anko t)
            else
              h::(check_Anko t)

let sp_kotu list = 
  let lst = List.filter (fun x -> x = Anko) list in
  let lst2 = List.filter (fun x -> x = Minko) list in
  let lst3 = List.filter (fun x -> x = Ankan) list in
  let lst4 = List.filter (fun x -> x = Minkan) list in
  (List.length lst, List.length lst2, List.length lst3, List.length lst4)

let hantei_zi array =
  let rec loop i list =
    let n = array.(i) in 
    let lst =
      if n >= 2 then
        if n >= 3 then
          if n = 3 then
            Anko::list
          else
            list
        else
        Toitu::list
      else
        list
    in
    if i = 6 then
      lst
    else
      loop (i+1) lst
  in
  loop 0 []


  
let hantei_syuntu array list =
  let rec loops i j tmp = 
    let lst =
      let n = array.(i).(j) in
      let n1 = array.(i).(j+1) in 
      let n2 = array.(i).(j+2) in
      if n >= 1 then      
        if n1 >= 1 && n2 >= 1 then
          let tmp = Syuntu::tmp in
          array.(i).(j) <- n - 1;
          array.(i).(j+1) <- n1 - 1;
          array.(i).(j+2) <- n2 - 1;
          loops i j tmp
        else
          [Ws]
      else
        tmp
      in
    if lst = [Ws] then 
      [Ws]
    else
      if i = 2 then
        if j = 6 then
          if array.(i).(7) >= 1 || array.(i).(8) >= 1 then 
            [Ws]
          else
            lst
        else
          loops i (j+1) lst
      else
        if j = 6 then
          if array.(i).(7) >= 1 || array.(i).(8) >= 1 then 
            loops (i+1) 0 [Ws]
          else
            loops (i+1) 0 lst
        else
          loops i (j+1) lst
    in
    loops 0 0 list


let hantei_koutu array = 
  let rec loopk i j array list count = 
    let n = array.(i).(j) in
    let lst =
      if n >= 3 then
        let array2 = Array.map (fun x -> Array.copy x) array in
        array2.(i).(j) <- n - 3;
        let tmp = 
          if count = 12 then
            [[Anko]]
          else
            add_state list [Anko]
        in
        let lst2 = 
          if j = 8 then
            if i = 2 then
              loopk i j array2 tmp (count-3)
            else
              loopk (i+1) 0 array2 tmp (count-3) 
          else
            loopk i (j+1) array2 tmp (count-3) in
        let remove_kotu = hantei_syuntu array2 (List.hd tmp) in
        let remove_kotu = if (List.for_all (fun x -> x <> Ws) remove_kotu) = true then
                            add_state tmp remove_kotu
                          else
                            []
                          in
                        
        if list = [] then
          remove_kotu @ lst2
        else
          let list = List.tl list in
          list @ remove_kotu @ lst2
      else
        list
      in
    if i = 2 then
      if j = 8  then
        let array2 = Array.map (fun x -> Array.copy x) array in
        let remove_kotu = hantei_syuntu array2 [] in
        let remove_kotu = 
          if (List.for_all (fun x -> x <> Ws) remove_kotu) = true then
            if count = 12 then
              remove_kotu::lst
            else
              add_state lst remove_kotu
          else
            lst
        in
        remove_kotu
      else
        loopk i (j+1) array lst count
    else
      if j = 8 then
        loopk (i+1) 0 array lst count
      else
        loopk i (j+1) array lst count
  in
    loopk 0 0 array [] 12

let koutu_count ary anko_lst = 
  let rec loop i j tmp = 
    let tmp = 
      if ary.(i).(j) = 3 then
        tmp+1    
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
  let x = loop 0 0 0 in
  let m = List.length anko_lst in
  let rec loop2 i tmp = 
    let lst = List.nth anko_lst i in
    let y = List.length (List.filter (fun a -> a = Anko) lst) in
    let tmp = 
      if x = y-1 then
        if List.hd lst  = Anko then
          (List.tl lst)::tmp
        else
          lst::tmp
      else
        lst::tmp
    in
    if i = 0 then
      tmp
    else
      loop2 (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop2 (m-1) []






let lp array zi_lst= 
  let list = [] in
  let rec loop10 i j array list zi_lst = 
    let n = array.(i).(j) in
    let lst =
      if n >= 2 then
        let array2 = Array.map (fun x -> Array.copy x) array in
        array2.(i).(j) <- n - 2;
        let nohead = hantei_koutu array2 in
        let nohead = check_Anko nohead in
        let nohead =
          if zi_lst <> [] then
            add_state nohead zi_lst
          else
            nohead
        in
        let nohead = add_state_head nohead Toitu in
        let list = nohead::list in
        list
      else
        list
    in
    

    
    if i = 2 then
      if j = 8 then
        lst
      else
        loop10 i (j+1) array lst zi_lst
    else
      if j = 8 then
        loop10 (i+1) 0 array lst zi_lst
      else
        loop10 i (j+1) array lst zi_lst
  in

  if zi_lst = [] then
    let lst = loop10 0 0 array list [] in
    let lst = remove_ws lst in
    lst
  else
    if (List.exists (fun x -> x = Toitu) zi_lst) = true then
      let lst = hantei_koutu array in
      let lst = check_Anko lst in
      let lst = koutu_count array lst in
      let lst2 = remove_to zi_lst in
      let lst3 = add_state lst lst2 in
      let lst3 = add_state_head lst3 Toitu in
      let lst3 = remove_ws [lst3] in
      lst3
    else
      let lst = loop10 0 0 array list zi_lst in
      let lst = remove_ws lst in
      lst

let titoitu ary zi_lst =
  let rec loop' i j lst = 
    let n = ary.(i).(j) in
    let lst = 
      if n = 2 then
        Toitu::lst
      else
        lst
    in
    if i = 2 then
      if j = 8 then
        lst
      else
        loop' i (j+1) lst 
    else
      if j = 8 then
        loop' (i+1) 0 lst 
      else
        loop' i (j+1) lst 
  in
  let lst = loop' 0 0 [] in
  let lst = List.filter (fun x -> x = Toitu) lst in
  let zi_lst = List.filter (fun x -> x = Toitu) zi_lst in
  let n = (List.length lst) + (List.length zi_lst) in
  if n = 7 then
    true
  else
    false

let kokushi ary zi_ary =
  let lst = [] in
  let zi_lst = [] in
  let rec loop' i j ary lst yn=
    let lst =
      if ary.(i).(j) >= 1 then
          if ary.(i).(j) = 2 then
            if yn = false then
              true::true::lst
            else
              false::lst
          else
            true::lst
      else
        false::lst
      in
    let yn = 
      if ary.(i).(j) = 2 then
        true
      else
        yn
    in
    if i = 2 then
      if j = 8 then
        lst
      else
        loop' i 8 ary lst yn
    else
      if j = 8 then
        loop' (i+1) 0 ary lst yn
      else
        loop' i 8 ary lst yn
      in
  let rec loop2' i zi_ary zi_lst yn=
    let zi_lst =
      if zi_ary.(i) >= 1 then
        if zi_ary.(i) = 2 then
          if yn = false then
            true::true::zi_lst
          else
            false::zi_lst
        else
          true::zi_lst
      else
        false::zi_lst
      in
    let yn = 
      if zi_ary.(i) = 2 then
        true
      else
        yn
      in
    if i = 6 then
      zi_lst
    else
      loop2' (i+1) zi_ary zi_lst yn
        in
  let lst = loop' 0 0 ary lst false in
  let lst2 = 
    if List.exists (fun x -> x = false) lst = true then
      []
    else
      if List.length lst = 6 then
        loop2' 0 zi_ary zi_lst false 
      else
        loop2' 0 zi_ary zi_lst true
  in
  let lst = List.filter (fun x -> x = true) lst in
  let lst2 = List.filter (fun x -> x = true) lst2 in
  let n = (List.length lst) + (List.length lst2) in
  if n = 14 then
    true
  else
    false





let agari lst zi_lst = 
  let rec loop lst =
    if List.hd lst <> [] then
      true
    else
      if (List.length lst) <> 1 then
        loop (List.tl lst) 
      else
        false
      in
  let lst2 = List.filter (fun x -> x = Toitu) zi_lst in
  if List.length lst2 <= 1 then
    if List.length lst <> 0 then
      loop lst
    else
      false
  else
    false

let count_agari zi_lst m = 
  let n = List.length zi_lst in
  let rec loop i tmp = 
    let x = List.nth zi_lst i in
    let tmp =
      if x = Toitu then
        tmp + 2
      else 
        tmp + 3 
    in
    if i = 0 then
      tmp
    else
      loop (i-1) tmp
  in
  let tmp =
    if n = 0 then
      0
    else
      loop (n-1) 0
  in
  if (tmp + m) = 14 then
    true
  else
    false





let tenpai_to_mati s_ary zi_ary =
  let rec looptm i j ary lst zi_lst = 
    let n = ary.(i).(j) in
    let ary2 = Array.map (fun x -> Array.copy x) ary in
    ary2.(i).(j) <- n+1;
    let m = Array.fold_left (fun x y -> x + Array.fold_left (fun a b -> a + b) 0 y) 0 ary2 in
    let lst = 
      if agari (lp ary2 zi_lst) zi_lst = true then
        if count_agari zi_lst m then
          (i,j)::lst
        else
          lst
      else
        if titoitu ary2 zi_lst = true then
          (i,j)::lst
        else
          if kokushi ary2 zi_ary = true then
            (i,j)::lst
          else
            lst
    in

    if i = 2 then
      if j = 8 then
        lst
      else
        looptm i (j+1) ary lst zi_lst
    else
      if j = 8 then
        looptm (i+1) 0 ary lst zi_lst
      else
        looptm i (j+1) ary lst zi_lst
  in
  let m_lst = looptm 0 0 s_ary [] (hantei_zi zi_ary) in

  let rec loop2' i ary zi_ary lst =
    let n = zi_ary.(i) in
    let ary2 = Array.copy zi_ary in
    ary2.(i) <- n+1;
    let zi_lst2 = hantei_zi ary2 in
    let a_lst = lp ary zi_lst2 in
    let lst = 
      if agari a_lst zi_lst2 = true then
        (3,i)::lst
      else
        if titoitu ary zi_lst2 = true then
          (3,i)::lst
        else
          if kokushi ary ary2 = true then
            (3,i)::lst
          else
            lst
    in

    if i = 6 then
      lst
    else
      loop2' (i+1) ary zi_ary lst
  in
  loop2' 0 s_ary zi_ary m_lst


let find_head ary lst = 
  let n = List.length lst in
  let rec loop' i j array lst hd_list n =
    let lst2 =  
      if array.(i).(j) >= 2 then
        if List.nth lst (n-1) <> [] then
          (i,j)::hd_list
        else
          hd_list
      else
        hd_list
    in
    let n = if array.(i).(j) >= 2 then
                n - 1
              else
                n
    in
    if i = 2 then
      if j = 8 then
        lst2
      else
        loop' i (j+1) array lst lst2 n
    else
      if j = 8 then
        loop' (i+1) 0 array lst lst2 n
      else
        loop' i (j+1) array lst lst2 n
  in
  if n = 0 then
    []
  else
    loop' 0 0 ary lst [] n

let find_head_zi zi_ary =
  let rec loop' zi_ary i lst=
    let lst =
      if zi_ary.(i) = 2 then
        (3,i)::lst
      else
        lst
    in

    if i = 6 then
      lst
    else
      loop' zi_ary (i+1) lst
  in
  loop' zi_ary 0 []


let tatu_to_mati ary =
  let rec loop' ary i lst=
    let lst =
      if ary.(i) > 0 then
        i::lst
      else
        lst
      in
    if i = 8 then
      lst
    else
      loop' ary (i+1) lst
    in
  let lst = loop' ary 0 [] in
  if List.length lst = 1 then
    Syanpon
  else
    let x1 = List.nth lst 0 in
    let x2 = List.nth lst 1 in
    if x1 - 1 = x2 then
      if x1 = 8 || x2 = 0 then
        Pentyan
      else
        Ryanmen
    else
      Kantyan



let rm_syuntu ary =
  let ary2 = Array.copy ary in
  let ary3 = Array.copy ary in
  let rec loop' ary i =
    let n1 = ary.(i) in
    let n2 = ary.(i+1) in
    let n3 = ary.(i+2) in
    let n =
      if n1 > 0 && n2 > 0 && n3 >0 then
        (ary.(i) <- n1-1;
        ary.(i+1) <- n2-1;
        ary.(i+2) <- n3-1;
        0)
      else
        (ary.(i) <- n1;
        ary.(i+1) <- n2;
        ary.(i+2) <- n3;
        1)
    in
    if n = 0 then
      loop' ary (i)
    else if i = 6 then
      let x = ary.(0) + ary.(1) + ary.(2) + ary.(3) + ary.(4) + ary.(5) + ary.(6) + ary.(7) + ary.(8) in
      if x = 2 then
        tatu_to_mati ary
      else
        Ns
    else
        loop' ary (i+1)
    in
  let rec loop2' ary i =
    let n1 = ary.(i) in
    let n2 = ary.(i-1) in
    let n3 = ary.(i-2) in
    let n =
      if n1 > 0 && n2 > 0 && n3 >0 then
        (ary.(i) <- n1-1;
        ary.(i-1) <- n2-1;
        ary.(i-2) <- n3-1;
        0)
      else
        (ary.(i) <- n1;
        ary.(i-1) <- n2;
        ary.(i-2) <- n3;
        1)
    in
    if n = 0 then
      loop2' ary (i)
    else if i = 2 then
      let x = ary.(0) + ary.(1) + ary.(2) + ary.(3) + ary.(4) + ary.(5) + ary.(6) + ary.(7) + ary.(8) in      
      if x = 2 then
        tatu_to_mati ary
      else
        Ns
    else
      loop2' ary (i-1)
    in
    let m = loop' ary2 0 in
    let n = loop2' ary3 8 in
    if m = n then
      [m]
    else
      [m;n]


let rm_mentu ary =
  let rec loop' ary i lst =
    let n = ary.(i) in
    let ary2 = Array.copy ary in
    let lst = 
      if n >= 3 then
        (ary2.(i) <- n - 3;
        let lst = loop' ary2 i lst in
        let m = rm_syuntu ary2 in
        m@lst)
      else
        lst
      in

    if i = 8 then
      let m = rm_syuntu ary in
      m@lst
    else
      loop' ary (i+1) lst
  in
  loop' ary 0 []
     

        




let head_mati_same ary x y =
  let n = ary.(x) in
  let m = ary.(y) in
  let ary2 = Array.copy ary in
  if x = y then
    if n >= 3 then
      (ary2.(x) <- n-3;
      let lst = rm_mentu ary2 in
      let lst = List.filter (fun x -> x <> Ns) lst in
      Tanki::lst)
    else
      [Tanki]
  else
    (ary2.(y) <- m-2;
    ary2.(x) <- n-1;
    let lst = rm_mentu ary2 in
    let lst = List.filter (fun x -> x <> Ns) lst in
    lst)

let head_mati_diff ary x =
  let n = ary.(x) in
  let ary2 = Array.copy ary in
  ary2.(x) <- n-1;
  let lst = rm_mentu ary2 in
  List.filter (fun x -> x <> Ns) lst

let head_mati_zi ary x =
  let n = ary.(x) in
  if n = 3 then
    [Syanpon]
  else
    [Tanki]

(*head,mati_state list*)
let find_mati ary (a,b) zi_lst zi_ary = 
  let zi_ary2 = Array.copy zi_ary in
  let ary2 = Array.map (fun x -> Array.copy x) ary in
  let lst = 
    if a = 3 then
      (let n = zi_ary.(b) in
      zi_ary2.(b) <- n + 1;
      lp ary (hantei_zi zi_ary2))
    else
      (let n = ary.(a).(b) in
      ary2.(a).(b) <- n + 1;
      lp ary2 zi_lst)
  in
  let head =
    if find_head_zi zi_ary2 = [] then
      find_head ary2 lst
    else
        find_head_zi zi_ary2
    in
  let n2 = List.length head in
  let rec f h_lst n m_lst = 
    let (x,y) = List.nth h_lst n in
    let lst =
      if a = 3 then
        head_mati_zi zi_ary2 b 
      else
        if x = a then
          head_mati_same ary2.(a) b y
        else
          head_mati_diff ary2.(a) b
      in
      let lst = List.filter (fun c -> c <> Ns) lst in
      if n = 0 then
        ((x,y),lst)::m_lst
      else
        f h_lst (n-1) (((x,y),lst)::m_lst)
      in
    if n2 = 0 then
      []
    else
      f head (n2-1) []




let possible_head ary =
  let rec loop' i j ary lst =
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
        loop' i (j+1) ary lst
    else
      if j = 8 then
        loop' (i+1) 0 ary lst
      else
        loop' i (j+1) ary lst
      
    in
    loop' 0 0 ary []

let possible_head_list lst lst2 =
  let m = List.length lst in
  let m2 = List.length lst2 in
  let rec loop' lst lst2 n lst3 =
    let lst3 = 
      if List.nth lst n <> [] then
        ((List.nth lst2 n),(List.nth lst n))::lst3
      else
        lst3
    in
    if n = 0 then
      lst3
    else
      loop' lst lst2 (n-1) lst3
  in
  if m = 0 then
    []
  else if m <> m2 then
    []
  else
    loop' lst lst2 (m-1) []

let yaku_kotu lst tmp =
  let (a,b,c,d) = sp_kotu lst in
  let y_lst =
    if a + b + c + d = 4 then
      Toitoi::tmp
    else
      tmp
  in
  let y_lst = 
    if a + c = 4 then
      Suankou::y_lst
    else
      if a + c = 3 then
        Sanankou::y_lst
      else
        y_lst
  in
  let y_lst = 
    if c + d = 4 then
      Sukantu::y_lst
    else
      if c + d = 3 then
        Sankantu::y_lst
      else
        y_lst
  in
  y_lst

let combination_list r xs =
  let rec comb r xs a b =
    match (r, xs) with
      (0, _) -> (List.rev a) :: b
    | (_, []) -> b
    | (_, y::ys) -> comb (r - 1) ys (y::a) (comb r ys a b)
  in
    comb r xs [] []


let tapl_to_kotu ary x = 
  let rec loop i j tmp = 
    let tmp = 
      if ary.(i).(j) >= 3 then
        (i,j)::tmp
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
  if x = 0 then
    []
  else
    let lst = loop 0 0 [] in
    if x = List.length lst then
      [lst]
    else
      combination_list x lst




let ary_to_tapl ary a b =
  let ary3 = Array.map (fun x -> Array.copy x) ary in
  let c_lst = tapl_to_kotu ary3 b in
  let rec loop' i j ary2 a lst=
    let n = ary2.(i).(j) in
    let lst = 
      if a > 0 then
        if n >= 1 then
          if n = 2 then
            (ary2.(i).(j) <- n - 2;
            let n1 = ary2.(i).(j+1) in
            let n2 = ary2.(i).(j+2) in
            ary2.(i).(j+1) <- n1 - 2;
            ary2.(i).(j+2) <- n2 - 2;
            (i,(j,j+1,j+2))::(i,(j,j+1,j+2))::lst)
          else
            if n = 1 then
              (ary2.(i).(j) <- n - 1;
              let n1 = ary2.(i).(j+1) in
              let n2 = ary2.(i).(j+2) in
              ary2.(i).(j+1) <- n1 - 1;
              ary2.(i).(j+2) <- n2 - 1;
              (i,(j,j+1,j+2))::lst)
            else
              if n = 3 then
                (ary2.(i).(j) <- n - 3;
                let n1 = ary2.(i).(j+1) in
                let n2 = ary2.(i).(j+2) in
                ary2.(i).(j+1) <- n1 - 3;
                ary2.(i).(j+2) <- n2 - 3;
                (i,(j,j+1,j+2))::(i,(j,j+1,j+2))::(i,(j,j+1,j+2))::lst)
              else
                  (ary2.(i).(j) <- n - 4;
                  let n1 = ary2.(i).(j+1) in
                  let n2 = ary2.(i).(j+2) in
                  ary2.(i).(j+1) <- n1 - 4;
                  ary2.(i).(j+2) <- n2 - 4;
                  (i,(j,j+1,j+2))::(i,(j,j+1,j+2))::(i,(j,j+1,j+2))::(i,(j,j+1,j+2))::lst)
        else
          lst
      else
        lst
    in
    let a = 
      if n >= 1 then
        a - n
      else
        a
    in

    if i = 2 then
      if j = 6 then
        lst
      else
        loop' i (j+1) ary2 a lst
    else
      if j = 6 then
        loop' (i+1) 0 ary2 a lst
      else
        loop' i (j+1) ary2 a lst
  in

  let rec loop2' i a b =
    let ary2 = Array.map (fun x -> Array.copy x) ary in
    let tmp = List.nth c_lst i in
    let rec mini_loop j lst l_tmp = 
      let tmp2 = List.nth lst j in
      let (x,y) = tmp2 in
      let l_tmp = (x,(y,y,y))::l_tmp in
      let n = ary2.(x).(y) in
      ary2.(x).(y) <- n-3;
      if j = 0 then
        l_tmp
      else
        mini_loop (j-1) lst l_tmp
    in
    let l_tmp = mini_loop ((List.length tmp)-1) tmp [] in
    let lst2 = loop' 0 0 ary2 a l_tmp in
    if List.length lst2 = (a+b) then
       lst2
    else 
      loop2' (i-1) a b
  in
  let m = List.length c_lst in
  if m = 0 then
    loop' 0 0 ary3 a []
  else
    loop2' (m-1) a b

let zi_to_tapl ary =
  let rec loop' i ary lst =
    let lst =
      if ary.(i) = 3 then
        (3,(i,i,i))::lst
      else
        lst
    in
    if i = 6 then
      lst
    else
      loop' (i+1) ary lst
  in
  loop' 0 ary []

let hantei_mentu ary zi_ary lst =
  let ary2 = Array.map (fun x -> Array.copy x) ary in
  let zi_ary2 = Array.copy zi_ary in
  let ((x,y),z) = lst in
  let _ =
    if x = 3 then
      let n = zi_ary2.(y) in
      zi_ary2.(y) <- n - 2;
    else
      let n = ary2.(x).(y) in
      ary2.(x).(y) <- n - 2;
  in
  let m = List.length z in
  let rec loop' s_lst len tp_lst=
    let s_lst' = List.nth s_lst len in
    let a = List.length (List.filter (fun x -> x = Syuntu) s_lst') in
    let b = List.length (List.filter (fun x -> x = Anko) s_lst') in
    let c =  zi_to_tapl zi_ary2 in
    let b = b - (List.length c) in
    let ary3 = Array.map (fun x -> Array.copy x) ary2 in
    let tp_lst = ((ary_to_tapl ary3 a b)@c)::tp_lst in
    if len = 0 then
      tp_lst
    else
      loop' s_lst (len-1) tp_lst 
  in
  if m = 0 then
    []
  else
    loop' z (m-1) [] 


let sansyoku (lst:((int*(int*int*int))list)) tmp =
  if List.length lst = 4 then 
    let (a1,b1) = List.nth lst 0 in
    let (a2,b2) = List.nth lst 1 in
    let (a3,b3) = List.nth lst 2 in
    let (a4,b4) = List.nth lst 3 in
    if b1 = b2 then
      if b1 = b3 then
        if a1 = 2 && a2 = 1 && a3 = 0 then
          let (x,y,_) = b1 in
          if x = y then
            Sansyokudoukou::tmp
          else
            Sansyokudouzyun::tmp
        else
          tmp
      else if b1 = b4 then
        if a1 = 2 && a2 = 1 && a4 = 0 then
          let (x,y,_) = b1 in
          if x = y then
            Sansyokudoukou::tmp
          else
            Sansyokudoukou::tmp
        else
          tmp
      else
        tmp
    else if b2 = b3 then
      if b2 = b4 then
        if a2 = 2 && a3 = 1 && a4 = 0 then
          let (x,y,_) = b1 in
          if x = y then
            Sansyokudoukou::tmp
          else
            Sansyokudoukou::tmp
        else
          tmp
      else
        tmp
    else
      tmp
  else
    tmp


let peiko (lst:(int*(int*int*int)) list) tmp = 
  if List.length lst = 4 then
    let (a1,b1) = List.nth lst 0 in
    let (a2,b2) = List.nth lst 1 in
    let (a3,b3) = List.nth lst 2 in
    let (a4,b4) = List.nth lst 3 in
    let n =
      if b1 = b2 && a1 = a2 then
        if b3 = b4 && a3 = a4 then
          Ryanpeikou::tmp
        else
          Ipeiko::tmp
      else if b1 = b3 && a1 = a3 then
        if b2 = b4 && a2 = a4 then
          Ryanpeikou::tmp
        else
          Ipeiko::tmp
      else if b1 = b4 && a1 = a4 then
        if b2 = b3 && a2 = a3 then
          Ryanpeikou::tmp
        else
          Ipeiko::tmp
      else if b2 = b3 && a2 = a3 then
        Ipeiko::tmp
      else if b2 = b4 && a2 = a4 then
        Ipeiko::tmp
      else if b3 = b4 && a3 = a4 then
        Ipeiko::tmp
      else
        tmp
    in
    n
  else
    tmp
    
let tyanta lst head tmp =
  if List.length lst = 4 then
    let (a1,(b1,_,c1)) = List.nth lst 0 in
    let (a2,(b2,_,c2)) = List.nth lst 1 in
    let (a3,(b3,_,c3)) = List.nth lst 2 in
    let (a4,(b4,_,c4)) = List.nth lst 3 in
    let (h1,h2) = head in
    if  a1 <> 3 && a2 <> 3 && a3 <> 3 && a4 <> 3 && h1 <> 3 then 
      if ((b1 = 0 && c1 = 0)||(b1 = 8 && c1 = 8)) && ((b2 = 0 && c2 = 0) ||(b2 = 8 && c2 = 8)) && ((b3 = 0 && c3 = 0) || (b3 = 8 && c3 = 8)) && ((b4 = 0 && c4 = 0) || (b4 = 8 && c4 = 8)) && (h2 = 0 || h2 = 8) then
        Tinroutou::tmp
      else if (b1 = 0 || c1 = 8) && (b2 = 0 || c2 = 8) && (b3 = 0 || c3 = 8) && (b4 = 0 || c4 = 8) && (h2 = 0 || h2 = 8) then
        Zyuntyan::tmp
      else
        tmp
    else if (a1 = 3 || (b1 = 0 && c1 = 0)||(b1 = 8 && c1 = 8)) && (a2 = 3 || (b2 = 0 && c2 = 0) ||(b2 = 8 && c2 = 8)) && (a3 = 3 || (b3 = 0 && c3 = 0) || (b3 = 8 && c3 = 8)) && (a4 = 3 || (b4 = 0 && c4 = 0) || (b4 = 8 && c4 = 8)) && (h1 = 3 || h2 = 0 || h2 = 8) then
      Honroutou::tmp
    else 
      if (b1 = 0 || c1 = 8 || a1 = 3) && (b2 = 0 || c2 = 8 || a2 = 3) && (b3 = 0 || c3 = 8 || a3 = 3) && (b4 = 0 || c4 = 8 || a4 = 3) && (h1 = 3 || h2 = 0 || h2 = 8) then
        Tyanta::tmp
      else
        tmp
  else
    tmp

let tanyao lst head tmp =
  if List.length lst = 4 then
    let (a1,(b1,_,c1)) = List.nth lst 0 in
    let (a2,(b2,_,c2)) = List.nth lst 1 in
    let (a3,(b3,_,c3)) = List.nth lst 2 in
    let (a4,(b4,_,c4)) = List.nth lst 3 in
    let (h1,h2) = head in
    if a1 <> 3 && a2 <> 3 && a3 <> 3 && a4 <> 3 && h1 <> 3 then
      if b1 <> 0 && b2 <> 0 && b3 <> 0 && b4 <> 0 && h2 <> 0 then
        if c1 <> 8 && c2 <> 8 && c3 <> 8 && c4 <> 8 && h2 <> 8 then
          Tanyao::tmp
        else
          tmp
      else
        tmp
    else
      tmp
  else
    tmp



let issyoku lst head tmp =
  let (a1,_) = List.nth lst 0 in
  let (a2,_) = List.nth lst 1 in
  let (a3,_) = List.nth lst 2 in
  let (a4,_) = List.nth lst 3 in
  let (h1,_) = head in
  if a1 = a2 && a1 = a3 && a1 = a4 && a1 = h1 then
    Tinitu::tmp
  else
    if (a1 = a2 || a2 = 3) && (a1 = a3 || a3 = 3) && (a1 = a4 || a4 = 3) && (a1 = h1 || h1 = 3) then
      Honitu::tmp
    else if a1 = 3 then
      Honitu::tmp
  else
    tmp
        
let pinhu lst mati head zi_kaze ba_kaze tmp =
  let (a,b) = head in
  if List.length (List.filter (fun x -> x = Syuntu) lst) = 4 then
    if mati = Ryanmen then
      if a = 3 then
        if b <> zi_kaze && b <> ba_kaze && b <> 4 && b <> 5 && b <> 6 then
          Pinhu::tmp
        else
          tmp
      else
        Pinhu::tmp
    else
      tmp
  else
    tmp

let ikki_tukan lst tmp =
  let (a1,(b1,_,c1)) = List.nth lst 0 in
  let (a2,(b2,_,c2)) = List.nth lst 1 in
  let (a3,(b3,_,c3)) = List.nth lst 2 in
  let (a4,(b4,_,c4)) = List.nth lst 3 in
  if a3 <> 3 then    
    if a1 = a2 && a1 = a3 then
      if (b1 = 6 && c1 = 8) && (b2 = 3 && c2 = 5) && (b3 = 0 && c3 = 2) then
        Ikkitukan::tmp
      else
        tmp
    else if a2 = a3 && a2 = a4 then
      if (b2 = 6 && c2 = 8) && (b3 = 3 && c3 = 5) && (b4 = 0 && c4 = 2) then
        Ikkitukan::tmp
      else
        tmp
    else
      tmp
  else
    tmp

let yakuhai lst zi_kaze ba_kaze tmp =
  let m = List.length lst in
  let rec loop' lst n y_lst =
    let (a,(b,_,_)) = List.nth lst n in 
    let y_lst =
      if a = 3 then
        if b = zi_kaze then
          if b = ba_kaze then
            Yakuhai::Yakuhai::y_lst
          else
            Yakuhai::y_lst
        else if b >= 4 then
          Yakuhai::y_lst
        else if b = ba_kaze then
          Yakuhai::y_lst
        else
          y_lst
      else
        y_lst
    in
    if n = 0 then
      y_lst
    else
      loop' lst (n-1) y_lst
  in
  loop' lst (m-1) tmp

let sangen lst head tmp = 
  let m = List.length lst in
  let rec loop' lst n y_lst = 
    let (a,(b,_,_)) = List.nth lst n in
    let y_lst =
      if a = 3 then
        if b >= 4 then
          b::y_lst
        else
          y_lst
      else
        y_lst
    in
    if n = 0 then
      y_lst
    else
      loop' lst (n-1) y_lst
  in
  let lst2 = 
    if m = 0 then
      []
    else
      loop' lst (m-1) []
  in
  let x = List.fold_left (fun a b -> a + b) 0 lst2  in
  if x = 15 then
    Daisangen::tmp
  else if x = 9 then
    if (3,6) = head then
      Syousangen::tmp
    else
      tmp
  else if x = 10 then
    if (3,5) = head then
      Syousangen::tmp
    else
      tmp
  else if x = 11 then
    if (3,4) = head then
      Syousangen::tmp
    else
      tmp
  else
    tmp

let tyuren ary y_lst = 
  let rec loop ary' j tmp = 
    let tmp = 
      if ary' = tyuren_a.(j) then
        true
      else
        tmp
      in
    if j = 8 then
      tmp
    else
      loop ary' (j+1) tmp
  in 
  let rec loop' i tmp2 = 
    let ary2 = Array.copy ary.(i) in
    let tmp2 = 
      if (loop ary2 0 false) = true then
        Tyurenpoutou::tmp2
      else
        tmp2
    in
    if i = 2 then
      tmp2
    else
      loop' (i+1) tmp2
    in
  loop' 0 y_lst

let ryuiso ary zi_ary y_lst = 
  let rec loop' j tmp = 
    let tmp = ary.(2).(j) + tmp in
    if j = 7 then
      tmp
    else
      if j = 3 || j = 5 then
        loop' (j+2) tmp
      else
        loop' (j+1) tmp
    in
  let tmp = loop' 1 0 in
  let tmp = tmp + zi_ary.(5) in
  if tmp = 14 then
    Ryuiso::y_lst
  else
    y_lst


let sushi lst head y_lst = 
  let (h1,h2) = head in
  let tmp =
    if h1 = 3 then
      if h2 = 0 then
        1
      else if h2 = 1 then
        2
      else if h2 = 2 then
        3
      else if h2 = 3 then
        4
      else
        0
    else
      0
  in
  let rec loop' i tmp = 
    let (a,(b,_,_)) = List.nth lst i in
    let tmp =
      if a = 3 then
        if b = 0 then
          1::tmp
        else if b = 1 then
          2::tmp
        else if b = 2 then
          3::tmp
        else if b = 3 then
          4::tmp
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
  let x = loop' 3 [tmp] in
  if List.fold_left (fun a b -> a + b) 0 x = 10 then
    if h1 = 3 then
      if h2 = 0 then
        Syoususi::y_lst
      else if h2 = 1 then
        Syoususi::y_lst
      else if h2 = 2 then
        Syoususi::y_lst
      else if h2 = 3 then
        Syoususi::y_lst
      else
        Daisusi::y_lst
    else
      Daisusi::y_lst
  else
    y_lst

let tuisou lst head y_lst = 
  let rec loop' i tmp = 
    let (a,_) = List.nth lst i in
    let tmp =
      if a = 3 then
        true::tmp
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
    in
    let (x,_) = head in
    if x = 3 then
      let lst2 = loop' 3 [] in
      if List.length lst2 = 4 then
        Tuiso::y_lst
      else
        y_lst
    else
        y_lst 


let t_tanyao ary zi_ary = 
  let rec loop' i j tmp = 
    let tmp = 
      if ary.(i).(j) = 0 then
        true::tmp
      else
        tmp
      in
    if j = 8 then
      if i = 2 then 
        tmp
      else
        loop' (i+1) 0 tmp
    else
      loop' i 8 tmp
    
  in
  if (Array.fold_left (fun a b -> a + b) 0 zi_ary) = 0 then
    let lst = loop' 0 0 [] in
    if (List.length lst) = 6 then
      [Tanyao]
    else
      []
  else
    []

let t_honrou ary zi_ary = 
    let rec loop' i j tmp = 
      let tmp = 
        if ary.(i).(j) = 2 then
          true::tmp
        else
          tmp
        in
      if j = 8 then
        if i = 2 then 
          tmp
        else
          loop' (i+1) 0 tmp
      else
        loop' i 8 tmp
        
      in
    let x = (Array.fold_left (fun a b -> a + b) 0 zi_ary) in
    let lst = loop' 0 0 [] in
    let y = (List.length lst)*2 in
    if (x+y) = 14 then
        [Honroutou]
      else
        []

let t_honitu ary zi_ary = 
  let rec loop' i tmp = 
    let tmp = 
      if (Array.fold_left (fun a b -> a + b) 0 ary.(i)) = 0 then
        true::tmp
      else
        tmp
      in
    if i = 2 then 
        tmp
    else
        loop' (i+1) tmp
    in
  let x = (Array.fold_left (fun a b -> a + b) 0 zi_ary) in
  let lst = loop' 0 [] in
  if x = 0 then
    if List.length lst = 2 then
      [Tinitu]
    else
      []
  else if x = 14 then
    [Tuiso]
  else
    if List.length lst = 2 then
      [Honitu]
    else
      []    

let min_to_fu lst = 
  let m = List.length lst in
  let rec loop' i x = 
    let (a,(b,(c,_,d))) = List.nth lst i in
    let x' =
      if b <> 3 then
        if c = d then
          if c = 0 || c= 8 then
            8
          else
            4
        else
          0
      else
        0
    in
    let x' = if a = Minko then 
              x'/2 
             else if a = Ankan then 
              x'*4 
             else if a = Minkan then
              x'*2
             else
              0
             in
    if i = 0 then
      x+x'
    else
      loop' (i-1) (x+x')
  in
  if m = 0 then
    0
  else
    loop' (m-1) 0


(*アンコのみ記述済*)
let kotu_to_fu lst =
  let m = List.length lst in
  let rec loop' i x = 
    let (a1,(b1,_,c1)) = List.nth lst i in
    let x' =
      if a1 <> 3 then
        if b1 = c1 then
          if b1 = 0 || b1 = 8 then
            8
          else
            4
        else
          0
      else
        0
    in
    if i = 0 then
      x+x'
    else
      loop' (i-1) (x+x')
    in
  if m = 0 then
    0
  else
    loop' (m-1) 0

let swap_kotu lst lst2 = 
  let m = List.length lst2 in
  let rec loop' i lst' = 
    let (_,(a,(b,c,d))) = List.nth lst2 i in
    let lst' = List.filter (fun x -> x <> (a,(b,c,d))) lst' in
    if i = 0 then
      lst'
    else
      loop' (i-1) lst'
  in
  let lst3 =
    if m = 0 then
      lst
    else
      loop' (m-1) lst
  in
  let x = kotu_to_fu lst3 in
  let y = min_to_fu lst2 in
  x+y





(*tumo,ron*)
let tehai_to_fu naki mati zi_lst lst (min_lst:(state*(int*(int*int*int)))list) f_lst =
  let ft = 
    if naki = true then
      (22,20)
    else
      (22,30)  
  in
  let ft2 = if mati <> Ryanmen && mati <> Syanpon then
              2
            else
              0
  in
  let ft3 = 
    if (List.exists (fun x -> x = Toitu) zi_lst) = true then
      2
    else
      0
  in
  let ft4_t =
    let lst3 = remove_to zi_lst in
    if (List.length lst3) <> 0 then
      let (a,b,c,d) = sp_kotu lst3 in
      (a*8) + (b*4) + (c*32) + (d*16)
    else
      0
  in
  let ft4_r =
    if min_lst <> [] then
      let (_,(h,_)) = List.hd min_lst in
      if h = 3 then
        let lst3 = remove_to zi_lst in
        if (List.length lst3) <> 0 then
          let (a,b,c,d) = sp_kotu lst3 in
          ((a-1)*8) + ((b+1)*4) + (c*32) + (d*16)
        else
          0
      else
        ft4_t
    else
      ft4_t
    in
  let ft5_r =
    swap_kotu lst (min_lst@f_lst)
  in
  let ft5_t =
    swap_kotu lst f_lst
  in 

  let (x,y) = ft in
    (x+ft2+ft3+ft4_t+ft5_t,y+ft2+ft3+ft4_r+ft5_r)



let yaku_to_han lst naki =
  let m = List.length lst in
  let rec loop' n h_lst =
    let tmp = List.nth lst n in
    let h_lst' = List.map (han_of_yaku naki) tmp in
    if n = 0 then 
      h_lst'::h_lst
    else
      loop' (n-1) (h_lst'::h_lst)
  in
  let x = 
    if m = 0 then
      []
    else
      loop' (m-1) [] 
  in
  let rec add_han n lst2 =
    let tmp = List.nth x n in
    let lst2 = (List.fold_left (fun a b -> a + b) 0 tmp)::lst2 in
    if n = 0 then
      lst2
    else
      add_han (n-1) lst2
  in
  if x = [] then
    []
  else
    add_han (m-1) []

let opt_man han =
  if han = 5 then
    0
  else if han = 6 || han = 7 then
    1
  else if han >= 8 && han <= 10 then
    2
  else if han > 100 then
    4
  else 3

let opt_han lst naki oya = 
  let m = List.length lst in
  let rec loop' n tmp =
    let (a,(b,c)) = List.nth lst n in
    let b = if b mod 10 = 0 then b/10 else b/10 + 1 in
    let c = if c mod 10 = 0 then c/10 else c/10 + 1 in
    let c = 
      if c = 2 then
        3
      else
        c
      in
    let tmp' =
      if a = 0 then
        if naki = false then
          if oya = true then
            let y = ten_oya.(a).(b-1) in
            (y,0)
          else
            let y = ten_ko.(a).(b-1) in
            (y,0)
        else
          (0,0)
      else
        if naki = false then
          if b >= 3 then
            if oya = true then
              if a > 4 then
                let x = ten_oya_man.(opt_man (a)) in
                let y = ten_oya_man.(opt_man (a+1)) in
                (y,x)
              else if a+1 > 4 then
                let x = ten_oya.(a-1).(c-1) in
                let y = ten_oya_man.(opt_man (a+1)) in
                (y,x)
              else
                let x = ten_oya.(a-1).(c-1) in
                let y = ten_oya.(a).(b-1) in
                (y,x)
            else
              if a > 4 then
                let x = ten_ko_man.(opt_man (a)) in
                let y = ten_ko_man.(opt_man (a+1)) in
                (y,x)
              else if a+1 > 4 then
                let x = ten_ko.(a-1).(c-1) in
                let y = ten_ko_man.(opt_man (a+1)) in
                (y,x)
              else
                let x = ten_ko.(a-1).(c-1) in
                let y = ten_ko.(a).(b-1) in
                (y,x)
          else
            if oya = true then
              if a > 4 then
                let x = ten_oya_man.(opt_man (a)) in
                let y = ten_oya_man.(opt_man (a+1)) in
                (y,x)
              else if a+1 > 4 then
                let x = ten_oya.(a-1).(c-1) in
                let y = ten_oya_man.(opt_man (a+1)) in
                (y,x)
              else
                let x = ten_oya.(a-1).(c-1) in
                let y = ten_oya.(a).(0) in
                (y,x)
            else
              if a > 4 then
                let x = ten_ko_man.(opt_man (a)) in
                let y = ten_ko_man.(opt_man (a+1)) in
                (y,x)
              else if a+1 > 4 then
                let x = ten_ko.(a-1).(c-1) in
                let y = ten_ko_man.(opt_man (a+1)) in
                (y,x)
              else
                let x = ten_ko.(a-1).(c-1) in
                let y = ten_ko.(a).(0) in
                (y,x)
        else
          if oya = true then
            if a > 4 then
              let x = ten_oya_man.(opt_man (a)) in
              (x,x)
            else
              let x = ten_oya.(a-1).(c-1) in
              let y = ten_oya.(a-1).(b-1) in
              (y,x)
          else
            if a > 4 then
              let x = ten_ko_man.(opt_man (a)) in
              (x,x)
            else
              let x = ten_ko.(a-1).(c-1) in
              let y = ten_ko.(a-1).(b-1) in
              (y,x)
    in
    let (_,d) = tmp in
    let (_,e) = tmp' in
    let tmp = 
      if d > e then
        tmp
      else
        tmp'
    in
    if n = 0 then
      tmp
    else
      loop' (n-1) tmp
  in
  if m = 0 then
    (0,0)
  else
    loop' (m-1) (0,0)

let rec swap_anko state = function 
  | [] -> []
  | h::t -> if h = Anko then
              state::t
            else
              h::(swap_anko state t)

let furo_to_state lst f_lst = 
  let n = List.length f_lst in
  let rec loop' lst i = 
    let (a,_) = List.nth f_lst i in
    let lst = 
      if a <> Syuntu then
        List.map (swap_anko a) lst
      else
        lst
    in
    if i = 0 then
      lst
    else
      loop' lst (i-1)
  in
  if n = 0 then
    lst
  else
    loop' lst (n-1) 

let furo_to_state_zi lst f_lst = 
  let n = List.length f_lst in
  let rec loop' lst i = 
    let (a,(b,_)) = List.nth f_lst i in
    let lst = 
      if b = 3 then
        swap_anko a lst
      else
        lst
    in
    if i = 0 then
      lst
    else
      loop' lst (i-1)
  in
  if n = 0 then
    lst
  else
    loop' lst (n-1) 

let dora_count lst ary zi_ary = 
  let m = List.length lst in
  let rec loop' i tmp = 
    let (x,y) = List.nth lst i in
    let n = 
      if x = 3 then
        zi_ary.(y)
      else
        ary.(x).(y)
    in
    let tmp = tmp + n in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  loop' (m-1) 0



let dora_in lst dora_lst ary zi_ary red = 
  let m = List.length lst in
  let rec loop' i tmp = 
    let han = List.nth lst i in
    let n = 
      if han <> 0 then
        (dora_count dora_lst ary zi_ary) + red  
      else
        0
    in
    let tmp = (han + n)::tmp in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop' (m-1) []



let opt_yaku ary zi_ary lst lst2 head mati zi_kaze ba_kaze naki zi_lst oya min_lst f_lst yaku_lst dora_lst red = 
  let lst = furo_to_state lst f_lst in
  let zi_lst = furo_to_state_zi zi_lst f_lst in
  let m = List.length lst in
  let lst' =
      if List.filter (fun x -> x <> Syanpon) mati = [] then
        List.map (swap_anko Minko) lst
      else
        lst
      in
  let rec loop' n y_lst lst3 =
    let tmp = yaku_kotu (List.nth lst3 n) [] in
    let tmp = (sansyoku (List.nth lst2 n)) tmp in
    let tmp = (tyanta(List.nth lst2 n) head) tmp in
    let tmp = (tanyao(List.nth lst2 n) head) tmp in
    let tmp = (issyoku(List.nth lst2 n) head) tmp in
    let tmp = (ikki_tukan(List.nth lst2 n)) tmp in
    let tmp = (yakuhai(List.nth lst2 n) zi_kaze ba_kaze) tmp in
    let tmp = (sangen(List.nth lst2 n) head) tmp in
    let tmp = (sushi (List.nth lst2 n) head) tmp in
    let tmp = (tuisou (List.nth lst2 n) head) tmp in
    let tmp = (ryuiso ary zi_ary) tmp in
    let tmp = if naki = false then 
                tyuren ary tmp 
              else
                tmp
    in
    let tmp = if naki = false then
                peiko (List.nth lst2 n) tmp 
              else
                tmp
    in
    let tmp = 
      if naki = false then
        if List.exists (fun a -> a = Ryanmen ) mati then
          pinhu (List.nth lst3 n) Ryanmen head zi_kaze ba_kaze tmp 
        else
          tmp
      else
        tmp
    in
    if n = 0 then
      tmp::y_lst
    else
      loop' (n-1) (tmp::y_lst) lst3
  in
  let y_lst = (loop' (m-1) []) lst in
  let y_lst = add_yaku y_lst yaku_lst in
  let y_lst' = (loop' (m-1) []) lst' in
  let y_lst' = add_yaku y_lst' yaku_lst in
  let h_lst = yaku_to_han y_lst naki in
  let h_lst = dora_in h_lst dora_lst ary zi_ary red in
  let h_lst' = yaku_to_han y_lst' naki in
  let h_lst' = dora_in h_lst' dora_lst ary zi_ary red in
  let rec ex_pinhu x y z n n_lst=
    let x' = List.nth x n in
    let y' = List.nth y n in
    let z' = List.nth z n in
    let n_lst =
      if List.exists (fun a -> a = Pinhu) x' then
        (y',(20,30))::n_lst
      else
        let rm_lst = List.filter (fun a -> a <> Ryanmen && a <> Syanpon) mati in
        if rm_lst <> [] then
          (y',(tehai_to_fu naki (List.hd rm_lst) zi_lst z' min_lst f_lst))::n_lst
        else
          (y',(tehai_to_fu naki (List.hd mati) zi_lst z' min_lst f_lst))::n_lst
    in

    if n = 0 then
      n_lst
    else
      ex_pinhu x y z (n-1) n_lst
  in
  let han = ex_pinhu y_lst h_lst lst2 (m-1) [] in
  let han' = ex_pinhu y_lst' h_lst' lst2 (m-1) [] in
  let (a,_) = opt_han han naki oya in
  let (_,b) = opt_han han' naki oya in
  (a,b)


let opt_ten lst = 
  let m = List.length lst in
  let rec loop' i tmp =
    let (x,y) = List.nth lst i in
    let (z,_) = tmp in
    let tmp = 
      if x > z then
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

let opt_ten2 lst =
  let m = List.length lst in
  let rec loop' i tmp =
    let (z,(x,y)) = List.nth lst i in
    let (_,(x2,_)) = tmp in
    let tmp = 
      if x >= x2 then
        (z,(x,y))
      else
        tmp
    in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
      
  if m = 0 then
    ((-1,-1),(0,0))
  else
    loop' (m-1) ((-1,-1),(0,0))


let tehai_to_yaku ary zi_ary lst mati zi_kaze ba_kaze naki oya min_lst f_lst yaku_lst dora_lst red =
  let m = List.length lst in
  let zi_lst = hantei_zi zi_ary in
  let rec loop' n new_lst =
    let (a,b) = List.nth lst n in
    let m_lst =  hantei_mentu ary zi_ary (a,b) in
    let tmp = opt_yaku ary zi_ary b m_lst a mati zi_kaze ba_kaze naki zi_lst oya min_lst f_lst yaku_lst dora_lst red in
    let new_lst = tmp::new_lst in
    if n = 0 then
      new_lst
    else
      loop' (n-1) new_lst
      
  in
  if m = 0 then
    []
  else
    loop' (m-1) []


let furo_to_tehai ary zi_ary f_lst = 
  let m = List.length f_lst in
  let rec loop' i = 
    let (_,(a,(b,c,d))) = List.nth f_lst i in
    let _ = 
      if a = 3 then
        let n = zi_ary.(b) in
        zi_ary.(b) <- n+3;
      else
        if b <> c then
          (let n1 = ary.(a).(b) in
          let n2 = ary.(a).(c) in
          let n3 = ary.(a).(d) in
          ary.(a).(b) <- n1+1;
          ary.(a).(c) <- n2+1;
          ary.(a).(d) <- n3+1;)
        else
          (let n = ary.(a).(b) in
          ary.(a).(b) <- n+3;)
    in
    if i = 0 then
      ()
    else
      loop' (i-1) 
    
  in
  if m = 0 then
    ()
  else
    loop' (m-1)






let tehai_to_ten_2 ary zi_ary (x,y) lst zi_kaze ba_kaze naki oya f_lst yaku_lst dora_lst red =
  let ary2 = Array.map (fun x -> Array.copy x) ary in
  let zi_ary2 = Array.copy zi_ary in
  let _ =
    if x = 3 then
      (let n = zi_ary2.(y) in
      zi_ary2.(y) <- n+1;)
    else
      (let n = ary2.(x).(y) in
      ary2.(x).(y) <- n+1;)
  in
  let zi_lst = hantei_zi zi_ary2 in
  let lst2 = lp ary2 zi_lst in
  let h_lst = possible_head ary2 in
  let head = 
    if (List.exists (fun x -> x = Toitu) zi_lst) = true then
      possible_head_list lst2 (find_head_zi zi_ary2)
    else
      possible_head_list lst2 h_lst 
  in
  let m = List.length lst in
  let rec loop' i tmp = 
    let (_,z) = List.nth lst i in
    let min_lst =
      if List.filter (fun x -> x <> Syanpon) z = [] then
        [(Minko,(x,(y,y,y)))]
      else
        []
      in
    let tmp2 = tehai_to_yaku ary2 zi_ary2 head z zi_kaze ba_kaze naki oya min_lst f_lst yaku_lst dora_lst red in
    let tmp = 
      ((x,y),(opt_ten tmp2))::tmp in
    if i = 0 then
      tmp
    else
      loop' (i-1) tmp
  in
  if m = 0 then
    []
  else
    loop' (m-1) []


let titoitu_ten ary zi_ary (a,b) oya yaku_lst dora_lst red = 
  let zi_ary2 = Array.copy zi_ary in
  let ary2 = Array.map (fun x -> Array.copy x) ary in
  let _ =
    if a = 3 then
      (let n = zi_ary.(b) in
      zi_ary2.(b) <- n+1;)
    else
      (let n = ary.(a).(b) in
      ary2.(a).(b) <- n+1;)
  in
  let zi_lst = hantei_zi zi_ary2 in
  if titoitu ary2 zi_lst = false then
    ((a,b),(0,0))
  else
    let tmp = [Titoitu] in
    let tmp = (t_tanyao ary2 zi_ary2)@tmp in
    let tmp = (t_honrou ary2 zi_ary2)@tmp in
    let tmp = (t_honitu ary2 zi_ary2)@tmp in
    let tmp = add_yaku [tmp] yaku_lst in
    let lst = yaku_to_han tmp false in
    let lst = dora_in lst dora_lst ary zi_ary red in
    let x = List.hd lst in
    let tmp2 =
      if x > 4 then
        if oya = true then
          let x' = ten_oya_man.(opt_man x) in
          let y' = ten_oya_man.(opt_man (x+1)) in
          (y',x')
        else
          let x' = ten_ko_man.(opt_man x) in
          let y' = ten_ko_man.(opt_man (x+1)) in
          (y',x')
      else if (x+1) > 4 then
        if oya = true then
          let x' = ten_oya.(x-1).(1) in
          let y' = ten_oya_man.(opt_man (x+1)) in
          (y',x')
        else
          let x' = ten_ko.(x-1).(1) in
          let y' = ten_ko_man.(opt_man (x+1)) in
          (y',x')
      else
        if oya = true then
          let x' = ten_oya.(x-1).(1) in
          let y' = ten_oya.(x).(1) in
          (y',x')
        else
          let x' = ten_ko.(x-1).(1) in
          let y' = ten_ko.(x).(1) in
          (y',x')
    in
    ((a,b),tmp2)


let kokushi_ten ary zi_ary (a,b) oya = 
  let zi_ary2 = Array.copy zi_ary in
  let ary2 = Array.map (fun x -> Array.copy x) ary in
  let _ =
    if a = 3 then
      (let n = zi_ary.(b) in
      zi_ary2.(b) <- n+1;)
    else
      (let n = ary.(a).(b) in
      ary2.(a).(b) <- n+1;)
  in
  if kokushi ary2 zi_ary2 = true then
    if oya = true then
      ((a,b),(48000,48000))
    else
      ((a,b),(36000,36000))
  else
    ((a,b),(0,0))

(*ary,zi_aryはf_lstの物は加えない*)
(*return ((int*int)和了牌*(int*int)(ツモ，ロン))list *)
let tehai_to_ten ary zi_ary zi_kaze ba_kaze naki (f_lst:(Mahjong_base.state*(int*(int*int*int)))list) (yaku_lst:Mahjong_base.yaku list) dora_lst red =
  let oya = if zi_kaze = 0 then true else false in 
  let ary2 = Array.map (fun x -> Array.copy x) ary in
  let zi_ary2 = Array.copy zi_ary in
  furo_to_tehai ary2 zi_ary2 f_lst;
  let zi_count = Array.fold_left (fun a b -> a + if b = 1 then 1 else 0) 0 zi_ary2 in 
  let zi_lst = hantei_zi zi_ary2 in
  let lst = 
    if zi_count > 1 && zi_count < 6 then 
      []
    else
      tenpai_to_mati ary2 zi_ary2 in
  let rec loop tmp = function
    | [] -> tmp
    | h::t -> let kokushi_lst = kokushi_ten ary2 zi_ary2 h oya in 
              let titoi_lst = titoitu_ten ary2 zi_ary2 h oya yaku_lst dora_lst red in
              let lst2 = find_mati ary2 h zi_lst zi_ary2 in
              let tmp2 = tehai_to_ten_2 ary2 zi_ary2 h lst2 zi_kaze ba_kaze naki oya f_lst yaku_lst dora_lst red in
              let tmp2 = titoi_lst::tmp2 in
              let tmp2 = kokushi_lst::tmp2 in
              loop (opt_ten2 tmp2::tmp) t
  in
  loop [] lst


(*let _ = 
  let a = [|[|0;0;2;0;0;0;0;0;0|];
            [|3;0;0;0;0;2;0;0;0|];
            [|0;1;1;1;0;0;0;0;0|]|] in 
  let zi = [|0;0;0;0;0;3;0|] in
  let zi_lst = hantei_zi zi in  
  tehai_to_ten a zi 2 1 false [] [] [(3,5)]*)
