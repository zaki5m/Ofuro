open Mahjong_base


let haku = ["+---+";
            "|   |";
            "|   |";
            "|   |";
            "+---+"]

let ryu = ["+---+";
           "|   |";
           "| h |";
           "|   |";
           "+---+"]

let tyun = ["+---+";
            "| t |";
            "| y |";
            "| u |";
            "+---+"]

let m1 = ["+---+";
          "|   |";
          "|-1-|";
          "|   |";
          "+---+"]
  
let m2 = ["+---+";
          "|   |";
          "|-2-|";
          "|   |";
          "+---+"]
let m3 = ["+---+";
          "|   |";
          "|-3-|";
          "|   |";
          "+---+"]

let m4 = ["+---+";
          "|   |";
          "|-4-|";
          "|   |";
          "+---+"]

let m5 = ["+---+";
          "|   |";
          "|-5-|";
          "|   |";
          "+---+"]

let m6 = ["+---+";
          "|   |";
          "|-6-|";
          "|   |";
          "+---+"]

let m7 = ["+---+";
          "|   |";
          "|-7-|";
          "|   |";
          "+---+"]
      
let m8 = ["+---+";
          "|   |";
          "|-8-|";
          "|   |";
          "+---+"]

let m9 = ["+---+";
          "|   |";
          "|-9-|";
          "|   |";
          "+---+"]

let s1 = ["+---+";
          "|+++|";
          "| 1 |";
          "|+++|";
          "+---+"]

let s2 = ["+---+";
          "|+++|";
          "| 2 |";
          "|+++|";
          "+---+"]

let s3 = ["+---+";
          "|+++|";
          "| 3 |";
          "|+++|";
          "+---+"]
    
let s4 = ["+---+";
          "|+++|";
          "| 4 |";
          "|+++|";
          "+---+"]

let s5 = ["+---+";
          "|+++|";
          "| 5 |";
          "|+++|";
          "+---+"]

let s6 = ["+---+";
          "|+++|";
          "| 6 |";
          "|+++|";
          "+---+"]

let s7 = ["+---+";
          "|+++|";
          "| 7 |";
          "|+++|";
          "+---+"]

let s8 = ["+---+";
          "|+++|";
          "| 8 |";
          "|+++|";
          "+---+"]

let s9 = ["+---+";
          "|+++|";
          "| 9 |";
          "|+++|";
          "+---+"]


let p1 = ["+---+";
          "|ooo|";
          "| 1 |";
          "|ooo|";
          "+---+"]

let p2 = ["+---+";
          "|ooo|";
          "| 2 |";
          "|ooo|";
          "+---+"]

let p3 = ["+---+";
          "|ooo|";
          "| 3 |";
          "|ooo|";
          "+---+"]

let p4 = ["+---+";
          "|ooo|";
          "| 4 |";
          "|ooo|";
          "+---+"]


let p5 = ["+---+";
          "|ooo|";
          "| 5 |";
          "|ooo|";
          "+---+"]
    
let p6 = ["+---+";
          "|ooo|";
          "| 6 |";
          "|ooo|";
          "+---+"]

let p7 = ["+---+";
          "|ooo|";
          "| 7 |";
          "|ooo|";
          "+---+"]

let p8 = ["+---+";
          "|ooo|";
          "| 8 |";
          "|ooo|";
          "+---+"]


let p9 = ["+---+";
          "|ooo|";
          "| 9 |";
          "|ooo|";
          "+---+"]

let ton = ["+---+";
           "| t |";
           "| o |";
           "| n |";
           "+---+"]

let nan = ["+---+";
           "| n |";
           "| a |";
           "| n |";
           "+---+"]

let sya = ["+---+";
           "| s |";
           "| y |";
           "| a |";
           "+---+"]

let pei = ["+---+";
           "| p |";
           "| e |";
           "| i |";
           "+---+"]



let cat filename = 
  let n = open_in filename in
  let rec loop' () = 
    print_string (input_line n);
    loop' ()
  in
  try
    loop' ()
  with
    End_of_file -> close_in n 

let change_gragh (x,y) = 
  if y = Manzu then
    if x = 1 then
      m1
    else if x = 2 then
      m2
    else if x = 3 then
      m3
    else if x = 4 then
      m4
    else if x = 5 then
      m5
    else if x = 6 then
      m6
    else if x = 7 then
      m7
    else if x = 8 then
      m8
    else
      m9
  else if y = Pinzu then
    if x = 1 then
      p1
    else if x = 2 then
      p2
    else if x = 3 then
      p3
    else if x = 4 then
      p4
    else if x = 5 then
      p5
    else if x = 6 then
      p6
    else if x = 7 then
      p7
    else if x = 8 then
      p8
    else
      p9
  else if y = Souzu then
    if x = 1 then
      s1
    else if x = 2 then
      s2
    else if x = 3 then
      s3
    else if x = 4 then
      s4
    else if x = 5 then
      s5
    else if x = 6 then
      s6
    else if x = 7 then
      s7
    else if x = 8 then
      s8
    else
      s9
  else if y = Ton then
    ton
  else if y = Nan then
    nan
  else if y = Sya then
    sya
  else if y = Pei then
    pei
  else if y = Haku then
    haku
  else if y = Hatsu then
    ryu
  else
    tyun



let print_number tehai = 
  let m = List.length tehai in
  let rec loop' i =
    let _ =
      if i < 10 then
        (Printf.printf "  %d  " i;)
      else
        Printf.printf " %d  " i;
    in
    if i = m - 1 then
      (Printf.printf "\n";)
    else
      loop' (i+1)
  in
  loop' 0

let print_hai lst x = 
  let n = List.length lst in
  let rec loop i = 
    let m = List.nth lst i in
    Printf.printf "%s" (List.nth m x);
    if i = (n-1) then
      (Printf.printf "  ";)
    else
      loop (i+1)
  in
  if n = 0 then
    ()
  else
    loop 0

let print_furo lst x = 
  let n = List.length lst in
  let rec loop i = 
    let m = List.nth lst i in
    print_hai m x;
    if i = (n-1) then
      ()
    else
      loop (i+1)
  in
  if n = 0 then
    ()
  else
    loop 0

let print_info ba kyoku honba kyotaku player_score yaku_lst dora_lst = 
  let _ = 
    if ba = 0 then
      (Printf.printf "東%d局 %d本場 供託:%d A:%d B:%d C:%d D:%d\n" kyoku honba kyotaku (tapl_player_1 player_score) (tapl_player_2 player_score) (tapl_player_3 player_score) (tapl_player_4 player_score);)
    else
      Printf.printf "南%d局 %d本場 供託:%d A:%d B:%d C:%d D:%d\n" kyoku honba kyotaku (tapl_player_1 player_score) (tapl_player_2 player_score) (tapl_player_3 player_score) (tapl_player_4 player_score);
  in
  let rec loop i = 
    let n = tapl_player yaku_lst i in
    let _ = 
      if List.exists (fun a -> a = Reach) n then
        if i = 0 then
          (Printf.printf "A:Reach ";)
        else if i = 1 then
          (Printf.printf "B:Reach ";)
        else if i = 2 then 
          (Printf.printf "C:Reach ";)
        else
          (Printf.printf "D:Reach ";)
      else
        ()
    in
    if i = 3 then
      (Printf.printf "\n";)
    else
      loop (i+1)
  in
  let n = List.length dora_lst in
  let rec loop2 i tmp = 
    let dora = List.nth dora_lst i in
    let dora = ary_to_hai dora in
    let tmp = dora::tmp in
    if i = 0 then
      tmp
    else
      loop2 (i-1) tmp
  in
  let dora_lst = loop2 (n-1) [] in
  let dora_lst = List.map (fun a -> change_gragh a ) dora_lst in
  let rec loop3 i = 
    print_hai dora_lst i;
    Printf.printf "\n";
    if i = 4 then
      ()
    else
      loop3 (i+1)
  in
  loop3 0;
  loop 0




let t_format tehai_lst furo_lst (sutehai_lst:(int*hai*bool)list list) ba kyoku honba kyotaku player_score yaku_lst dora_lst =
  print_info ba kyoku honba kyotaku player_score yaku_lst dora_lst;
  let tehai_lst = List.map (fun a -> (List.map change_gragh a))tehai_lst in
  let sutehai_lst = List.map (fun a -> (List.map (fun (x,y,_) -> change_gragh (x,y))) a) sutehai_lst in
  let furo_lst = List.map (fun a -> (List.map (fun b -> furo_to_hai b) a))furo_lst in
  let rec loop' i x = 
    let tehai = List.nth tehai_lst i in
    let furo = List.nth furo_lst i in
    let furo = List.map (fun a -> (List.map change_gragh a)) furo in
    let sutehai = List.nth sutehai_lst i in
    print_hai tehai x;
    print_furo furo x;
    print_hai sutehai x;
    Printf.printf "\n";
    if x = 4 then
      if i = 3 then
        print_number tehai
      else
        (print_number tehai;
        loop' (i+1) 0)
    else
      loop' i (x+1) 
  in
  loop' 0 0 

let tehai_onry tehai_lst = 
  let rec loop i = 
    print_hai tehai_lst i;
    if i = 4 then 
      (Printf.printf "\n";)
    else
      (Printf.printf "\n";
      loop (i+1)
      )
  in
  loop 0
    


