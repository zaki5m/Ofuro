open Unix
open Yojson
open Mahjong_base
open Mahjong_admin
open Tenpai_prob
open Loop

exception End_game
exception Error

let bufsize = 80
let port = 11600

let rec d_tehai_s (list:string list) d_hai = match list with
  | [] -> []
  | [x] -> if x = d_hai then [] else [x]
  | x::t -> if x = d_hai then t else x::(d_tehai_s t d_hai)


let convert_mjai_to_myai hai = match hai with 
  | "1m" -> (1,Manzu)
  | "2m" -> (2,Manzu)
  | "3m" -> (3,Manzu)
  | "4m" -> (4,Manzu)
  | "5m" -> (5,Manzu)
  | "5mr" -> (5,Manzu)
  | "6m" -> (6,Manzu)
  | "7m" -> (7,Manzu)
  | "8m" -> (8,Manzu)
  | "9m" -> (9,Manzu)
  | "1p" -> (1,Pinzu)
  | "2p" -> (2,Pinzu)
  | "3p" -> (3,Pinzu)
  | "4p" -> (4,Pinzu)
  | "5p" -> (5,Pinzu)
  | "5pr" -> (5,Pinzu)
  | "6p" -> (6,Pinzu)
  | "7p" -> (7,Pinzu)
  | "8p" -> (8,Pinzu)
  | "9p" -> (9,Pinzu)
  | "1s" -> (1,Souzu)
  | "2s" -> (2,Souzu)
  | "3s" -> (3,Souzu)
  | "4s" -> (4,Souzu)
  | "5s" -> (5,Souzu)
  | "5sr" -> (5,Souzu)
  | "6s" -> (6,Souzu)
  | "7s" -> (7,Souzu)
  | "8s" -> (8,Souzu)
  | "9s" -> (9,Souzu)
  | "E" -> (0,Ton)
  | "S" -> (0,Nan)
  | "W" -> (0,Sya)
  | "N" -> (0,Pei)
  | "P" -> (0,Haku)
  | "F" -> (0,Hatsu)
  | "C" -> (0,Tyun)
  | _ -> (1,Not_hai)

let convert_myai_to_mjai tehai_s hai = match hai with 
  | (1,Manzu) -> "1m"
  | (2,Manzu) -> "2m"
  | (3,Manzu) -> "3m"
  | (4,Manzu) -> "4m"
  | (5,Manzu) -> if List.exists (fun a -> a = "5m") tehai_s then "5m" else "5mr"
  | (6,Manzu) -> "6m"
  | (7,Manzu) -> "7m"
  | (8,Manzu) -> "8m"
  | (9,Manzu) -> "9m"
  | (1,Pinzu) -> "1p"
  | (2,Pinzu) -> "2p"
  | (3,Pinzu) -> "3p"
  | (4,Pinzu) -> "4p"
  | (5,Pinzu) -> if List.exists (fun a -> a = "5p") tehai_s then "5p" else "5pr"
  | (6,Pinzu) -> "6p"
  | (7,Pinzu) -> "7p"
  | (8,Pinzu) -> "8p"
  | (9,Pinzu) -> "9p"
  | (1,Souzu) -> "1s"
  | (2,Souzu) -> "2s"
  | (3,Souzu) -> "3s"
  | (4,Souzu) -> "4s"
  | (5,Souzu) -> if List.exists (fun a -> a = "5s") tehai_s then "5s" else "5sr"
  | (6,Souzu) -> "6s"
  | (7,Souzu) -> "7s"
  | (8,Souzu) -> "8s"
  | (9,Souzu) -> "9s"
  | (0,Ton) -> "E"
  | (0,Nan) -> "S"
  | (0,Sya) -> "W"
  | (0,Pei) -> "N"
  | (0,Haku) -> "P"
  | (0,Hatsu) -> "F"
  | (0,Tyun) -> "C"
  | _ -> "C"

let pre_prob_select tehai_s sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst = 
  let hai_number = prob_select sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst in
  let hai = List.nth tehai hai_number in 
  convert_myai_to_mjai tehai_s hai

let calc_yama_len sutehai_lst = 
  let x1 = tapl_player_1 sutehai_lst in 
  let x2 = tapl_player_2 sutehai_lst in 
  let x3 = tapl_player_1 sutehai_lst in 
  let x4 = tapl_player_2 sutehai_lst in
  let sum = (List.length x1) + (List.length x2) + (List.length x3) + (List.length x4) in
  136 - 53 - sum 


let sutehai_bool hai tsumogiri = 
  let (x,y) = hai in 
  (x,y,tsumogiri)

let hello () = 
  Printf.printf "hello\n"; flush Stdlib.stdout;
  `Assoc
    [
      ("type", `String "join");
      ("name", `String "furo_ai");
      ("room", `String "default");
    ]
let response_none () = 
  `Assoc
  [
    ("type", `String "none");
  ]

  let hora id kiriban = 
    `Assoc
    [
      ("type", `String "hora");
      ("actor", `Int id);
      ("target", `Int kiriban);
    ]

let tsumo tehai_s sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst = 
  let k_hai = pre_prob_select tehai_s sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst in 
  let tsumogiri = if (List.nth tehai_s (List.length tehai_s - 1)) = k_hai then true else false in 
  `Assoc
  [
    ("type", `String "dahai");
    ("actor", `Int player);
    ("pai", `String k_hai);
    ("tsumogiri", `Bool tsumogiri);
  ]

let furo_k tehai_s sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst = 
  let k_hai = pre_prob_select tehai_s sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst in 
  `Assoc
  [
    ("type", `String "dahai");
    ("actor", `Int player);
    ("pai", `String k_hai);
    ("tsumogiri", `Bool false);
  ]  

let pon id tehai_s target dahai_s (new_y1,new_y2) = 
  let pai1 = convert_myai_to_mjai tehai_s new_y1 in 
  let pai2 = convert_myai_to_mjai tehai_s new_y2 in 
  `Assoc
    [
      ("type", `String "pon");
      ("actor", `Int id);
      ("target", `Int target);
      ("pai", `String dahai_s);
      ("consumed", `List [`String pai1;`String pai2]);
    ]

let chi id tehai_s target dahai_s (new_y1,new_y2) = 
  let pai1 = convert_myai_to_mjai tehai_s new_y1 in 
  let pai2 = convert_myai_to_mjai tehai_s new_y2 in 
  `Assoc
    [
      ("type", `String "chi");
      ("actor", `Int id);
      ("target", `Int target);
      ("pai", `String dahai_s);
      ("consumed", `List [`String pai1;`String pai2]);
    ]

let minkan id tehai_s target dahai_s (new_y1,new_y2) = 
  let pai1 = convert_myai_to_mjai tehai_s new_y1 in 
  let pai2 = convert_myai_to_mjai tehai_s new_y2 in 
  `Assoc
    [
      ("type", `String "daiminkan");
      ("actor", `Int id);
      ("target", `Int target);
      ("pai", `String dahai_s);
      ("consumed", `List [`String pai1;`String pai2]);
    ]

let dahai tehai_s sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst furoritu target dahai_s =
  let x = purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst furoritu in
  if x = [] then 
    response_none ()
  else
    let (_,(state,(x,(y1,y2,y3)))) = List.hd x in
    let (new_y1,new_y2) = if y1 = y then (y2,y3) else if y2 = y then (y1,y3) else (y1,y2) in 
    let new_y1 = ary_to_hai (x,new_y1) in
    let new_y2 = ary_to_hai (x,new_y2) in    
    if state = Minko then 
      pon player tehai_s target dahai_s (new_y1,new_y2)
    else if state = Syuntu then 
      chi player tehai_s target dahai_s (new_y1,new_y2)
    else
      minkan player tehai_s target dahai_s (new_y1,new_y2)





let client_fun ic oc = 
  let open Yojson.Basic.Util in
  try
    let rec loop tehai tehai_s sutehai_lst id ba_kaze kyoku honba kyotaku furo_lst yaku_lst dora_lst naki furoritu furo_double_lst = 
      Printf.printf "flush\n"; flush Stdlib.stdout;
      let r = input_line ic in 
      let json = Yojson.Basic.from_string r in
      let first = json |> member "type" |> to_string in
      let id = if first = "start_game" then json |> member "id" |> to_int else id in 
      let ba_kaze = if first = "start_kyoku" then let ba = json |> member "bakaze" |> to_string in if ba = "E" then 0 else 1 else ba_kaze in
      let kyoku = if first = "start_kyoku" then json |> member "kyoku" |> to_int else kyoku in
      let honba = if first = "start_kyoku" then json |> member "honba" |> to_int else honba in
      let kyotaku = if first = "start_kyoku" then json |> member "kyotaku" |> to_int else kyotaku in
      let dora_lst = if first = "start_kyoku" then let dr = json |> member "dora_marker" |> to_string in [hai_to_ary (convert_mjai_to_myai dr)] else dora_lst in
      let tehai_lst =  if first = "start_kyoku" then json |> member "tehais" |> convert_each (fun a -> a |> convert_each (fun b -> b |> to_string) )  else [] in
      let tehai = if tehai_lst = [] then tehai else List.map ( fun a -> convert_mjai_to_myai a) (List.nth tehai_lst id) in 
      let sutehai_lst = if first = "dahai" then 
                          let dahai = json |> member "pai" |> to_string in 
                          let player = json |> member "actor" |> to_int in 
                          let tsumogiri = json |> member "tsumogiri" |> to_bool in 
                          let sutehai = sutehai_bool (convert_mjai_to_myai dahai) tsumogiri in 
                          let tmp = tapl_player sutehai_lst player in 
                          tapl_player_in sutehai_lst (sutehai::tmp) player
                        else
                          sutehai_lst 
      in 
      let da_hai = if first = "dahai" then json |> member "pai" |> to_string  else "null" in
      let (k_x,k_y) = if first = "dahai" then
                        convert_mjai_to_myai da_hai
                      else
                        (1,Not_hai)
      in
      let tehai_s = if tehai_lst = [] then tehai_s else List.nth tehai_lst id in
      let tsumo_ban = if first = "tsumo" then json |> member "actor" |> to_int else 4 in
      let kiri_ban = if first = "dahai" then json |> member "actor" |> to_int else 4 in 
      let hora_lst = if tehai = [] then 
                      []
                     else
                      let (t_ary,t_zi_ary) = list_to_ary tehai in
                      let zi_kaze = kyoku_to_kaze kyoku id in  
                      tehai_to_ten t_ary t_zi_ary zi_kaze ba_kaze naki (tapl_player furo_lst id) (tapl_player yaku_lst id) dora_lst
      in
      let tsumo_hai = if first = "tsumo" then json |> member "pai" |> to_string else "null" in
      let (t_x,t_y) = if tsumo_ban = id then convert_mjai_to_myai tsumo_hai else (1,Not_hai) in 
      let tehai = if tsumo_ban = id then 
                    add_tehai tehai (t_x,t_y)
                  else
                    tehai
      in
      Printf.printf "flush\n"; flush Stdlib.stdout; 
      let tehai_s = if tsumo_ban = id then 
                      tsumo_hai::tehai_s
                    else
                      tehai_s
      in
      let tehai = if kiri_ban = id then 
                    d_tehai tehai (k_x,k_y)
                  else
                    tehai
      in
      let tehai_s = if kiri_ban = id then 
                    d_tehai_s tehai_s da_hai
                  else
                    tehai_s
      in
      let yaku_lst = if first = "reach" then 
                    let player = json |> member "actor" |> to_int in 
                    let x = tapl_player yaku_lst player in 
                    tapl_player_in yaku_lst (Reach::x) player 
                  else
                    yaku_lst 
      in
      let furo_double_lst = if first = "pon" || first = "chi" || first = "daiminkan" then 
                              let pai = json |> member "pai" |> to_string in 
                              let pai = convert_mjai_to_myai pai in
                              let pai = hai_to_ary pai in  
                              pai::furo_double_lst
                            else
                              furo_double_lst
      in 
      let furo_player = if first = "pon" || first = "chi" || first = "daiminkan" || first = "ankan" || first = "kakan" then
                          json |> member "actor" |> to_int 
                        else
                          4
      in 
      let furo_lst = if first = "pon" || first = "chi" || first = "daiminkan" || first = "ankan" || first = "kakan" then
                        let pai = json |> member "pai" |> to_string in 
                        let pai = convert_mjai_to_myai pai in
                        let pai = hai_to_ary pai in  
                        let consumed = json |> member "consumed" |> convert_each (fun a -> a |> to_string) in
                        let consumed = List.map (fun a -> convert_mjai_to_myai a) consumed in 
                        let consumed = List.map (fun a -> hai_to_ary a) consumed in
                        let player = json |> member "actor" |> to_int in 
                        let furo = tapl_player furo_lst player in 
                        if first = "pon" then 
                          let (x,y) = pai in 
                          let furo = (Minko,(x,(y,y,y)))::furo in 
                          tapl_player_in furo_lst furo player
                        else if first = "chi" then
                          let (x,y) = pai in 
                          let consumed = List.sort (fun (_,a) (_,b) -> if a > b then 1 else -1) (pai::consumed) in
                          let (x,y) = List.hd consumed in  
                          let furo = (Syuntu,(x,(y,y+1,y+2)))::furo in 
                          tapl_player_in furo_lst furo player
                        else if first = "daiminkan" then 
                          let (x,y) = pai in 
                          let furo = (Minkan,(x,(y,y,y)))::furo in 
                          tapl_player_in furo_lst furo player
                        else if first = "ankan" then 
                          let (x,y) = pai in 
                          let furo = (Ankan,(x,(y,y,y)))::furo in 
                          tapl_player_in furo_lst furo player
                        else
                          let (x,y) = pai in 
                          let furo = List.filter (fun (_,(b,(c,_,_))) -> b <> x && c <> y) furo in 
                          let furo = (Minkan,(x,(y,y,y)))::furo in 
                          tapl_player_in furo_lst furo player
                      else
                        furo_lst
      in
      let naki = if first = "pon" || first = "chi" || first = "daiminkan" || first = "kakan" then
                  let player = json |> member "actor" |> to_int in
                  if player = id then
                    true
                  else
                    naki
                else
                  naki
      in
      let match_type first = match first with 
        | "end_game" -> raise End_game
        | "error" -> raise Error
        | "hello" -> hello()
        | "start_game" -> response_none ()
        | "tsumo" -> if id = tsumo_ban then 
                        if List.exists (fun (a,b) -> a = (hai_to_ary (t_x,t_y))) hora_lst then 
                          hora id id
                        else
                          let yama_len = calc_yama_len sutehai_lst in
                          let zi_kaze = kyoku_to_kaze kyoku id in  
                          tsumo tehai_s sutehai_lst tehai furo_lst yaku_lst id yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst
                    else
                      response_none()
        | "dahai" -> if kiri_ban = id then 
                        response_none ()
                     else
                        let yama_len = calc_yama_len sutehai_lst in
                        let zi_kaze = kyoku_to_kaze kyoku id in
                        let (x,y) = hai_to_ary (k_x,k_y) in  
                        if List.exists (fun (a,b) -> a = (x,y)) hora_lst then 
                          hora id kiri_ban 
                        else
                          let dahai_s = json |> member "pai" |> to_string in
                          dahai tehai_s sutehai_lst tehai furo_lst yaku_lst id yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst furoritu kiri_ban dahai_s 
        | "pon" -> if furo_player = id then 
                    let yama_len = calc_yama_len sutehai_lst in
                    let zi_kaze = kyoku_to_kaze kyoku id in 
                    furo_k tehai_s sutehai_lst tehai furo_lst yaku_lst id yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst
                  else
                    response_none ()
        | "chi" -> if furo_player = id then 
                    let yama_len = calc_yama_len sutehai_lst in
                    let zi_kaze = kyoku_to_kaze kyoku id in 
                    furo_k tehai_s sutehai_lst tehai furo_lst yaku_lst id yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst
                  else
                    response_none ()
        | "daiminkan" -> if furo_player = id then 
                          let yama_len = calc_yama_len sutehai_lst in
                          let zi_kaze = kyoku_to_kaze kyoku id in 
                          furo_k tehai_s sutehai_lst tehai furo_lst yaku_lst id yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst
                        else
                          response_none ()
        | "ankan" -> if furo_player = id then 
                      let yama_len = calc_yama_len sutehai_lst in
                      let zi_kaze = kyoku_to_kaze kyoku id in 
                      furo_k tehai_s sutehai_lst tehai furo_lst yaku_lst id yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst
                    else
                      response_none ()                  
        | _ -> response_none ()
      in
      let new_json = match_type first in
      (*Yojson.Basic.to_channel oc new_json;*)
      let new_json = Yojson.Basic.to_string new_json in 
      output_string oc (new_json ^ "\n");
      Out_channel.flush oc;
      loop tehai tehai_s sutehai_lst id ba_kaze kyoku honba kyotaku furo_lst yaku_lst dora_lst naki furoritu furo_double_lst
    in
    loop [] [] ([],[],[],[]) 4 0 1 0 0 ([],[],[],[]) ([],[],[],[]) [] false 25. []
  with
    | End_game -> exit 0
    | Error -> exit 0



let main () =
  (*let s = Unix.socket PF_INET SOCK_STREAM 0 in*)
  let addr = ADDR_INET(inet_addr_loopback, port) in
  let ic , oc = open_connection addr in 
  client_fun ic oc;
  shutdown_connection ic
  (*let buf = Bytes.create bufsize in
  let rec loop () =
    let inp = read_line () in
    let inp = Bytes.of_string inp in 
    let len = Bytes.length inp in
    if len <> 0 && Unix.write s inp 0 len = len then 
      let len = Unix.read s buf 0 bufsize in
      print_endline (String.sub (Bytes.to_string buf) 0 len)
    else
      ()
  in
  loop ();
  Unix.close s*)

let () =
  try main ()
  with Unix_error (err, f, s) ->
    Printf.eprintf "Unix error in %s: %s (%s)\n%!"
      f (Unix.error_message err) s

