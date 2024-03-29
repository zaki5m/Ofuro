open Mahjong_base
open Loop
open Domainslib
open Mahjong_haieff
open Mahjong_safty
open M_gragh
open Tenpai_ary2
open Yojson
open Unix

let ic_kitaiti = ref (Stdlib.stdin:in_channel)

let oc_kitaiti = ref (Stdlib.stdout:out_channel)

module C = Domainslib.Chan

let num_domains = 2 

type 'a message = Task of 'a | Quit

let c = C.make_unbounded ()

let lk = Mutex.create ()

let create_work tasks =
  Array.iter (fun t -> C.send c (Task t)) tasks;
  for _ = 1 to num_domains do
    C.send c Quit
  done

let create_work_lst tasks =
  List.iter (fun t -> C.send c (Task t)) tasks

let rec worker f () =
  match C.recv c with
  | Task a ->
      f a;
      worker f ()
  | Quit -> ()

let myhash = Hashtbl.create 12345




let hash_number_manzu x = match x with 
  | 1 -> 1
  | 2 -> 4
  | 3 -> 7
  | 4 -> 100
  | 5 -> 111
  | 6 -> 231
  | 7 -> 283
  | 8 -> 313
  | 9 -> 541
  | _ -> 0

let hash_number_pinzu x = match x with 
  | 1 -> 17
  | 2 -> 31
  | 3 -> 621
  | 4 -> 677
  | 5 -> 857
  | 6 -> 317
  | 7 -> 463
  | 8 -> 71
  | 9 -> 751
  | _ -> 0

let hash_number_souzu x = match x with 
  | 1 -> 21
  | 2 -> 32
  | 3 -> 45
  | 4 -> 56
  | 5 -> 69
  | 6 -> 86
  | 7 -> 97
  | 8 -> 108
  | 9 -> 115
  | _ -> 0

let hash_number_match (x,y) = match y with 
  | Manzu -> hash_number_manzu x
  | Pinzu -> hash_number_pinzu x 
  | Souzu -> hash_number_souzu x
  | Ton -> 11
  | Nan -> 12
  | Sya -> 13
  | Pei -> 14
  | Haku -> 15
  | Hatsu -> 16
  | Tyun -> 17
  | _ -> 0

let hash_number tehai  = 
  let rec loop tmp t_lst = match t_lst with
    | [] -> tmp 
    | (x,y)::t -> let tmp2 = hash_number_match (x,y) in 
                  loop (tmp+tmp2) t
  in
  let score = loop 0 tehai in
  score

(*recieve (k_lst,tumo_lst,k_count,current_tehai) list
   return (k_lst,tumo_lst,rest_tumo_lst,k_count,current_tehai) list*)  
let make_rest_tumo_lst ary zi_ary lst= 
  let rec filter_loop ((x,y):(int*int)) m t_lst = match t_lst with 
    | [] ->  m
    | h::t -> if h = (x,y) then filter_loop (x,y) (m+1) t else filter_loop (x,y) m t 
  in
  let rec rest_tumo_loop tmp tmp2 t_lst = match t_lst with 
    | [] -> tmp
    | (x,y)::t -> let (x,y) = hai_to_ary (x,y) in
                  let m = filter_loop (x,y) 0 tmp2 in 
                  let n = 
                      if x = 3 then 
                        zi_ary.(y) - m
                      else
                        ary.(x).(y) - m
                  in
                  if n = 0 then
                    []
                  else 
                    let tmp = n::tmp in
                    rest_tumo_loop tmp ((x,y)::tmp2) t
  in
  let rec rest_yukou_loop tmp t_lst = match t_lst with
    | [] -> tmp
    | h::t -> let (x,y) = hai_to_ary h in
              let n = 
                if x = 3 then 
                  zi_ary.(y) 
                else
                  ary.(x).(y) 
              in
              let tmp = n + tmp in 
              rest_yukou_loop tmp t 
  in
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (k_lst,tumo_lst,k_count,yukou_hai_lst,_,current_tehai)::t -> let tmp2 = rest_tumo_loop [] [] tumo_lst in
                                                                 let tmp3 = List.map (fun a -> rest_yukou_loop 0 a) yukou_hai_lst in 
                                                                  if tmp2 = [] then 
                                                                    loop tmp t 
                                                                  else
                                                                    loop ((k_lst,tumo_lst,tmp2,k_count,tmp3,current_tehai)::tmp) t 
  in
  let rec loop2 tmp2 t_lst2 = match t_lst2 with
    | [] -> tmp2
    | h::t -> let tmp2 = loop tmp2 h in 
              loop2 tmp2 t 
  in
  loop2 [] lst

let parallel_rest_tumo_lst ary zi_ary tenpai_lst = 
  let n = Array.length tenpai_lst in 
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let update p r i = p.(i) <- make_rest_tumo_lst ary zi_ary r.(i) in 
  let pre =  Array.make n [] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre tenpai_lst))) in
  worker (update pre tenpai_lst) ();
  Array.iter Domain.join domains;
  pre

  

let yukou_to_ritu yama_len tumo_len yukou_lst tumo_lst = 
  let yama_len = yama_len + 14 in 
  let tumo_len_len = List.length tumo_lst in
  let rec make_lst tmp i =
    if i <= 14 then 
      tmp
    else
      make_lst (i::tmp) (i-4)
  in
  let lst = make_lst [] (yama_len+1) in
  let lst_len =  List.length lst in 
  let rec make_x n tmp yama i = match i with 
    | 0 -> tmp
    | _ ->  let tmp = tmp +. (n /. float_of_int (yama-i)) in
            make_x n tmp yama (i-1)
  in
  let rec loop len max_i tmp tmp2 x i j = match len with 
    | 0 ->  tmp
    | _ ->  let n = List.nth yukou_lst j in 
            let yama = List.nth lst (lst_len-len) in 
            let yama2 = List.nth lst (lst_len-len-1) in 
            let x = x -. ((i /. max_i) *. (make_x (float_of_int n) 0. yama 3)) in 
            let tumo_tmp = ((float_of_int yama2) -. x) /. (float_of_int yama2) in 
            let tumo_tmp = if tumo_tmp < 0. then 1. else tumo_tmp in  
            let tmp = ((i-.1.)/.max_i)*.tumo_tmp*.tmp in
            let tmp2 = 
              if  (lst_len-len-1) < 0 then 
                tmp+.tmp2
              else if j = (List.length yukou_lst) - 1 then 
                tmp+.tmp2
              else
                let y = (List.nth yukou_lst (j+1)) in 
                let y = if n = 0 then 0. else (float_of_int y) -. (((float_of_int n) -. x) *. (float_of_int y) /.(float_of_int n)) in
                if y <= 0. then 
                  0.
                else
                  tmp2 +. (loop (len-1) i tmp 0. y i (j+1))
            in
            if (i-.2.) <= 0. then 
              loop 0 max_i tmp tmp2 x (i-.1.) j
            else
              loop (len-1) max_i tmp tmp2 x (i-.1.) j
  in
  if (tumo_len - tumo_len_len + 1) < 1 then 
    0.0
  else 
    loop tumo_len (float_of_int (tumo_len - tumo_len_len + 1)) 1. 0. (float_of_int (List.nth yukou_lst 0)) (float_of_int (tumo_len - tumo_len_len + 1)) 0 

let self_t_ritu yama_len tumo_len yukou_lst tumo_lst = 
  let yama_len = yama_len +. 14. in 
  let n = List.length yukou_lst in 
  let rec loop2 len max_len under top tmp =
    let tmp = tmp *. top /. under in
    if len > max_len then 
      loop2 (len-1) max_len (under-.1.) (top-.1.) tmp
    else
      tmp /. (under-.4.)
  in
  let rec loop len end_len i yama tmp sum t_lst = match t_lst with
    | [] -> sum
    | h::t -> let tumo = List.nth tumo_lst i in 
              let result = None (*Tenpai_ary2.serch len end_len yama h*) in
              let tmp2 = if result = None then 
                          loop2 len end_len yama (yama -. float_of_int h) 1. 
                        else
                          Option.get result
              in 
              let tmp2 = tmp *. tmp2 *. (float_of_int tumo) in
              let tmp3 = if i = n-1 then tmp2 else loop (end_len-1) (end_len-1) (i+1) (yama-.(float_of_int (len - end_len+1))) tmp2 0. t in
              if end_len > (n-i) then 
                loop len (end_len-1) i yama tmp (sum +. tmp3) (h::t)
              else
                sum +. tmp3
  in
  if tumo_len < n then
    0. 
  else
    loop tumo_len tumo_len 0 yama_len 1. 0. yukou_lst


(*ary,zi_aryは残り枚数のテーブル。　返り値(kitaiti,agariritu) 0th: reach無し 1th:reachあり*)
let tenpai_kitaiti lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary tumo_l rm_wan =
  let (t_ary,t_zi_ary) = list_to_ary lst in
  let ten_lst = tehai_to_ten t_ary t_zi_ary zi_kaze ba_kaze naki f_lst yaku_lst dora_lst 0 in
  let m = List.length ten_lst in
  let tumo_times = Int.to_float tumo_l in
  let rec loop tmp_kitaiti tmp_agari t_lst = match t_lst with
    | [] -> (tmp_kitaiti,tmp_agari)
    | ((a,b),(c,_))::t -> let n = 
                              if a = 3 then
                                zi_ary.(b)
                              else
                                ary.(a).(b)
                          in
                          let n = Int.to_float n in
                          let c = Int.to_float c in
                          let tmp_kitaiti = (n /. rm_wan) *. c *. tumo_times +. tmp_kitaiti in
                          let tmp_agari = (n /. rm_wan) *. tumo_times +. tmp_agari in
                          loop tmp_kitaiti tmp_agari t
  in
  if naki = false then
    let ten_lst2 = tehai_to_ten t_ary t_zi_ary zi_kaze ba_kaze naki f_lst (Reach::yaku_lst) dora_lst 0 in
    if m = 0 then
      []
    else
      [(loop 0.0 0.0 ten_lst);(loop 0.0 0.0 ten_lst2)]
  else
    if m = 0 then
      []
    else
      [(loop 0.0 0.0 ten_lst)]

let tenpai_kitaiti_pp lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst red =
  let (t_ary,t_zi_ary) = list_to_ary lst in
  let ten_lst = tehai_to_ten t_ary t_zi_ary zi_kaze ba_kaze naki f_lst yaku_lst dora_lst red in
  let m = List.length ten_lst in
  let rec loop tmp_kitaiti t_lst = match t_lst with
    | [] -> tmp_kitaiti
    | ((a,b),(c,_))::t -> let c = Int.to_float c in
                          loop (((a,b),c)::tmp_kitaiti) t
  in
  if naki = false then
    let ten_lst2 = tehai_to_ten t_ary t_zi_ary zi_kaze ba_kaze naki f_lst (Reach::yaku_lst) dora_lst red in
    if m = 0 then
      []
    else
      loop [] ten_lst2
  else
    if m = 0 then
      []
    else
      loop [] ten_lst

let max_kitaiti_p lst = 
  let rec loop tmp t_lst = match t_lst with
    | [] -> tmp 
    | (i,(x,y))::t -> let (_,(t_x,t_y)) = tmp in 
                      if y >= t_y then 
                        loop (i,(x,y)) t 
                      else
                        loop tmp t 
  in
  loop (13,(0.0,0.0)) lst

let max_kitaiti_p_f lst = 
  let rec loop tmp t_lst = match t_lst with
    | [] -> tmp 
    | (i,(x,y))::t -> let (_,(t_x,t_y)) = tmp in 
                      if y >= t_y then 
                        loop (i,(x,y)) t 
                      else
                        loop tmp t 
  in
  loop ((1,Not_hai),(0.0,0.0)) lst

let tenpai_to_opt_agariritu_kitaiti lst sum_lst tumo_l rm_wan = 
  let len = 69 - (int_of_float rm_wan) in 
  let rec loop tmp j t_lst = match t_lst with 
    | [] -> tmp
    | (i,t_lst2)::t -> let rec loop2 (agariritu,kitaiti) t_lst22 = match t_lst22 with 
                        | [] -> (agariritu,kitaiti)
                        | (n,z)::t2 -> let a_ritu = if n = 0 then 
                                                      0.0
                                                    else if tumo_l = 0 then 
                                                      0.0
                                                    else
                                                      (*syanten_1_ary.(len).(4-n) *)
                                                      self_t_ritu rm_wan tumo_l [(List.nth sum_lst j)] [n]
                                                    in
                                       let k_ritu = a_ritu *. z in
                                       loop2 ((agariritu+.a_ritu),(k_ritu+.kitaiti)) t2 
                        in
                        loop ((i,(loop2 (0.0,0.0) t_lst2))::tmp) (j+1) t
  in
  let n_lst = loop [] 0 lst in 
  max_kitaiti_p n_lst

let tenpai_to_opt_agariritu_kitaiti_f lst sum_lst patern tumo_l rm_wan = 
  let len = 69 - (int_of_float rm_wan) in 
  let patern = float_of_int patern in 
  let rec loop tmp j t_lst = match t_lst with 
    | [] -> tmp
    | (i,t_lst2)::t -> let rec loop2 (agariritu,kitaiti) t_lst22 = match t_lst22 with 
                        | [] -> (agariritu,kitaiti)
                        | (n,z)::t2 -> let a_ritu = if n = 0 then 
                                                      0.0
                                                    else if tumo_l = 0 then 
                                                      0.0
                                                    else
                                                      (*syanten_1_ary.(len).(4-n)*)
                                                      self_t_ritu rm_wan tumo_l [(List.nth sum_lst j)] [n]
                                                    in
                                       let a_ritu = (1. /. patern) *. a_ritu in 
                                       let k_ritu = a_ritu *. z in
                                       loop2 ((agariritu+.a_ritu),(k_ritu+.kitaiti)) t2 
                        in
                        loop ((i,(loop2 (0.0,0.0) t_lst2))::tmp) (j+1) t
  in
  let n_lst = loop [] 0 lst in 
  max_kitaiti_p_f n_lst





let tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary = 
  let tehai = rhai_to_hai tehai in 
  let rec loop tmp double_lst i t_lst = match t_lst with 
    | [] -> tmp
    | h::t -> if List.exists (fun a -> a = h) double_lst then 
                loop tmp double_lst (i+1) t
              else
                let lst = d_tehai tehai h in
                let (_,n) = syanten lst in
                let tmp = 
                  if n = 0 then
                    let red = tehai_in_red lst in 
                    (i,(tenpai_kitaiti_pp lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst red))::tmp
                  else
                    tmp
                in
                loop tmp (h::double_lst) (i+1) t
  in
  let rec loop2 tmp sum_lst t_lst = match t_lst with
    | [] -> (sum_lst,tmp)
    | (i,t_lst)::t -> let rec loop3 tmp2 sum t_lst = match t_lst with
                      | [] -> (sum,tmp2)
                      | ((x,y),z)::t2 -> 
                                        let n = 
                                          if x = 3 then
                                            zi_ary.(y)
                                          else 
                                            ary.(x).(y)
                                        in
                                        loop3 ((n,z)::tmp2) (sum+n) t2
                      in
                      let (sum,tmp2) = loop3 [] 0 t_lst in 
                      loop2 ((i,tmp2)::tmp) (sum::sum_lst) t
  in 
  let tmp = loop [] [] 0 tehai in
  if tmp = [] then 
    (13,(0.,0.))
  else
    let (sum,tmp) = loop2 [] [] tmp in 
    tenpai_to_opt_agariritu_kitaiti tmp sum tumo_l rm_wan
  
  
(*
let tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary = 
  let rec loop tmp double_lst i t_lst = match t_lst with 
    | [] -> tmp
    | h::t -> if List.exists (fun a -> a = h) double_lst then 
                loop tmp double_lst (i+1) t
              else
                let lst = d_tehai tehai h in
                let (_,n) = syanten lst in
                let tmp = 
                  if n = 0 then
                    (i,(tenpai_kitaiti lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary tumo_l rm_wan))::tmp
                  else
                    tmp
                in
                loop tmp (h::double_lst) (i+1) t
  in
  (*reach判断を加える*)
  let rec loop2 tmp a_lst = match a_lst with 
    | [] -> tmp 
    | (a,lst)::t -> let (b,c) = 
                      if lst = [] then
                        (0.0,0.0)
                      else if (List.length lst) = 2 then 
                        List.nth lst 1
                      else
                        List.hd lst 
                    in
                    let (_,(y,_)) = tmp in
                    let tmp =
                      if y > b then
                        tmp
                      else
                        (a,(b,c))
                    in
                    loop2 tmp t
  in
  let t_lst = loop [] [] 0 tehai in
  loop2 (13,(0.0,0.0)) t_lst 
*)

let calc_k_ritu_not_naki lst = 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp
    | h::[] -> tmp
    | h::t -> let n = float_of_int h in 
              let tmp = tmp *. (1./.n) in 
              loop tmp t
  in
  loop 1. lst 

let calc_k_ritu_naki lst = 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp
    | h::t -> let n = float_of_int h in 
              let tmp = tmp *. (1./.n) in 
              loop tmp t
  in
  loop 1. lst 

let tenpai_tumo_lst current_tehai =
  let rec loop i j tmp = 
    let tmp = 
        let (x,y) = ary_to_hai (i,j) in
        let tmp_tehai = add_tehai current_tehai (x,y) in
        let (_,new_n) = syanten tmp_tehai in 
        if new_n = -1 then
          (x,y)::tmp
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
        let (x,y) = ary_to_hai (3,i) in
        let tmp_tehai = add_tehai current_tehai (x,y) in
        let (_,new_n) = syanten tmp_tehai in 
        if new_n = -1 then
          (x,y)::tmp
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


let tenpai_to_opt_p tehai f_lst zi_kaze ba_kaze naki yaku_lst dora_lst = 
  let rec loop tmp double_lst i t_lst = match t_lst with 
    | [] -> (i,tmp)
    | h::t -> if List.exists (fun a -> a = h) double_lst then 
                loop tmp double_lst i t
              else
                let lst = d_tehai tehai h in
                let (_,n) = syanten lst in
                let tmp = 
                  if n = 0 then
                    let red = tehai_in_red tehai in 
                    (tenpai_kitaiti_pp lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst red)::tmp
                  else
                    tmp
                in
                let i = 
                  if n = 0 then 
                    i+1
                  else
                    i
                  in
                loop tmp (h::double_lst) i t
  in
  loop [] [] 0 tehai 


let tenpai_to_opt_f tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary kuikae_lst = 
  let rec loop tmp double_lst i t_lst = match t_lst with 
    | [] -> (i,tmp)
    | h::t -> if List.exists (fun a -> a = h) double_lst then 
                loop tmp double_lst i t
              else
                let lst = d_tehai tehai h in
                let (_,n) = syanten lst in
                let tmp = 
                  if List.exists (fun a -> a = h) kuikae_lst then 
                    tmp
                  else if n = 0 then
                    let red = tehai_in_red tehai in
                    (h,(tenpai_kitaiti_pp lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst red))::tmp
                  else
                    tmp
                in
                let i = 
                  if List.exists (fun a -> a = h) kuikae_lst then 
                    i
                  else if n = 0 then
                    i+1
                  else
                    i
                in
                loop tmp (h::double_lst) i t
  in
  let rec loop2 tmp sum_lst t_lst = match t_lst with
    | [] -> (sum_lst,tmp)
    | (i,t_lst)::t -> let rec loop3 tmp2 sum t_lst = match t_lst with
                      | [] -> (sum,tmp2)
                      | ((x,y),z)::t2 -> 
                                        let n = 
                                          if x = 3 then
                                            zi_ary.(y)
                                          else 
                                            ary.(x).(y)
                                        in
                                        loop3 ((n,z)::tmp2) (sum+n) t2
                      in
                      let (sum,tmp2) = loop3 [] 0 t_lst in 
                      loop2 ((i,tmp2)::tmp) (sum::sum_lst) t
  in 
  let (i,tmp) = loop [] [] 0 tehai in
  let (sum,tmp) = loop2 [] [] tmp in
  tenpai_to_opt_agariritu_kitaiti_f tmp sum i tumo_l rm_wan 
(*
let tenpai_to_opt_f tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary kuikae_lst = 
  let rec loop tmp double_lst i t_lst = match t_lst with 
    | [] -> (i,tmp)
    | h::t -> if List.exists (fun a -> a = h) double_lst then 
                loop tmp double_lst i t
              else
                let lst = d_tehai tehai h in
                let (_,n) = syanten lst in
                let tmp = 
                  if List.exists (fun a -> a = h) kuikae_lst then 
                    tmp
                  else if n = 0 then
                    (h,(tenpai_kitaiti_pp lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst))::tmp
                  else
                    tmp
                in
                let i = 
                  if List.exists (fun a -> a = h) kuikae_lst then 
                    i
                  else if n = 0 then
                    i+1
                  else
                    i
                in
                loop tmp (h::double_lst) i t
  in
  let rec loop2 tmp t_lst = match t_lst with
    | [] -> tmp
    | (i,t_lst)::t -> let rec loop3 tmp2 t_lst = match t_lst with
                      | [] -> tmp2
                      | ((x,y),z)::t2 -> 
                                        let n = 
                                          if x = 3 then
                                            zi_ary.(y)
                                          else 
                                            ary.(x).(y)
                                        in
                                        loop3 ((n,z)::tmp2) t2
                      in
                      let tmp2 = loop3 [] t_lst in 
                      loop2 ((i,tmp2)::tmp) t
  in 
  let (i,tmp) = loop [] [] 0 tehai in
  tenpai_to_opt_agariritu_kitaiti_f (loop2 [] tmp) i tumo_l rm_wan  
*)
(*
let tenpai_to_opt_f tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary kuikae_lst = 
  let rec loop tmp double_lst t_lst = match t_lst with 
    | [] -> tmp
    | h::t -> if List.exists (fun a -> a = h) double_lst  then 
                loop tmp double_lst t
              else
                let lst = d_tehai tehai h in
                let (_,n) = syanten lst in
                let tmp = 
                  if List.exists (fun a -> a = h) kuikae_lst then 
                    tmp 
                  else if n = 0 then
                    (h,(tenpai_kitaiti lst f_lst zi_kaze ba_kaze naki yaku_lst dora_lst ary zi_ary tumo_l rm_wan))::tmp
                  else
                    tmp
                in
                loop tmp (h::double_lst) t
  in
  let rec loop2 tmp a_lst = match a_lst with
    | [] -> tmp
    | (a,lst)::t -> let (b,c) = 
                      if lst = [] then
                        (0.0,0.0)
                      else
                        List.hd lst 
                    in
                    let (_,(y,_)) = tmp in
                    let tmp =
                      if y > b then
                        tmp
                      else
                        (a,(b,c))
                    in
                    loop2 tmp t
  in
  let t_lst = loop [] [] tehai in
  loop2 ((1,Not_hai),(0.0,0.0)) t_lst 
*)     

let ary_opt ary zi_ary lst = 
  let lst = List.map (fun (a,b) -> hai_to_ary (a,b)) lst in
  let ary2 = Array.map (fun a -> Array.copy a) ary in
  let zi_ary2 = Array.copy zi_ary in
  let rec loop t_lst = match t_lst with 
    | [] -> ()
    | (x,y)::t -> let _ =
                    if x = 3 then
                      let n = zi_ary2.(y) in
                      zi_ary2.(y) <- n-1;
                    else
                      let n = ary2.(x).(y) in
                      ary2.(x).(y) <- n-1;
                  in
                  loop t
  in
  loop lst;
  (ary2,zi_ary2)


let all_tumo (k_lst,tumo_lst,k_count,yukou_hai_lst,flag,current_tehai) = 
  let (_,n) = syanten current_tehai in
  let rec loop i j tmp tmp2 = 
    let (x,y) = ary_to_hai (i,j) in
    let tmp_tehai = add_tehai current_tehai (x,y) in
    let (_,new_n) = syanten tmp_tehai in 
    let tmp = 
        if new_n = (n - 1) then
          (k_lst,(x,y)::tumo_lst,k_count,yukou_hai_lst,flag,tmp_tehai)::tmp
        else
          if flag = 0 then
            tmp
          else
            (k_lst,(x,y)::tumo_lst,k_count,yukou_hai_lst,(flag-1),tmp_tehai)::tmp
    in
    let tmp2 = 
      if new_n = (n - 1) then 
        (x,y)::tmp2
      else
        if flag = 0 then
          tmp2
        else
          (x,y)::tmp2
    in
    if i = 2 then
      if j = 8 then
        (tmp,tmp2)
      else
        loop i (j+1) tmp tmp2
    else
      if j = 8 then
        loop (i+1) 0 tmp tmp2
      else
        loop i (j+1) tmp tmp2
  in
  let rec loop2 i tmp tmp2 = 
    let (x,y) = ary_to_hai (3,i) in
    let tmp_tehai = add_tehai current_tehai (x,y) in
    let (_,new_n) = syanten tmp_tehai in 
    let tmp = 
        if new_n = (n - 1) then
          (k_lst,(x,y)::tumo_lst,k_count,yukou_hai_lst,flag,tmp_tehai)::tmp
        else
          if flag = 0 then
            tmp
          else
            (k_lst,(x,y)::tumo_lst,k_count,yukou_hai_lst,(flag-1),tmp_tehai)::tmp
    in
    let tmp2 = 
      if new_n = (n - 1) then 
        (x,y)::tmp2
      else
        if flag = 0 then
          tmp2
        else
          (x,y)::tmp2
    in
    if i = 6 then
      (tmp,tmp2)
    else
      loop2 (i+1) tmp tmp2
  in
  let (t_lst,tmp2) = loop 0 0 [] [] in
  let (t_lst,tmp2) = loop2 0 t_lst tmp2 in 
  List.map (fun (a,b,c,d,e,f) -> (a,b,c,tmp2::d,e,f)) t_lst 

let k_fase (k_lst,tumo_lst,k_count,yukou_hai_lst,flag,current_tehai) = 
  let (_,n) = syanten current_tehai in
  let rec loop tmp double_lst i t_lst = match t_lst with 
    | [] -> (i,tmp)
    | h::t -> if List.exists (fun a -> a = h) double_lst then 
                loop tmp double_lst i t
              else
                let new_tehai = d_tehai current_tehai h in
                let (_,new_n) = syanten new_tehai in
                let tmp = 
                  if new_n = n then
                    all_tumo (h::k_lst,tumo_lst,k_count,yukou_hai_lst,flag,new_tehai)@tmp
                  else
                    tmp 
                in
                let i = 
                  if new_n = n then 
                    i+1
                  else
                    i
                in
                loop tmp (h::double_lst) i t
  in
  let (i,tmp) = loop [] [] 0 current_tehai in 
  List.map (fun (a,b,c,d,e,f) -> (a,b,i::c,d,e,f)) tmp

let hash_serch tehai = 
  let tehai2 = ripai tehai in 
  (*let x = hash_number tehai in*)
  let x = Hashtbl.hash tehai in
  let lst = Hashtbl.find_all myhash x in
  let rec loop t_lst = match t_lst with 
   | [] -> []
   | (current_tehai,info_lst)::t -> if current_tehai = tehai2 then 
                                      info_lst
                                    else
                                      loop t
  in
  let tmp = loop lst in 
  tmp




let operate_tenpai_ritu_parallel tenpai_lst = 
  let (_,_,_,_,_,current_tehai) = tenpai_lst in 
  let judge_hash = hash_serch current_tehai in 
  if judge_hash = [] then 
    let (_,n) = syanten current_tehai in 
    let x = hash_number current_tehai in 
    let tenpai_lst = [[tenpai_lst]] in 
    let rec loop3 tmp3 t_lst = match t_lst with
      | [] -> tmp3 
      | (k_lst,tumo_lst,k_count,yukou_hai_lst,flag,current_tehai)::t ->  let tmp3 = (k_fase (k_lst,tumo_lst,k_count,yukou_hai_lst,flag,current_tehai))::tmp3 in
                                                            loop3 tmp3 t
    in
    let rec loop i tmp =
      let rec loop2 tmp2 lst = match lst with
        | [] -> tmp2 
        | h::t ->  let tmp2 = loop3 tmp2 h in
                  loop2 tmp2 t
        in
      if tmp = [] then 
        ([],[])
      else
        let tmp' = loop2 [] tmp in
        if i = 0 then
          (tmp,tmp')
        else
          loop (i-1) tmp'
    in
    let (tmp,tmp') = loop (n-1) tenpai_lst in
    if tmp = [] then 
      tmp'
    else
      (*let all_t = all_k_fase tmp in
      let tmp = syanten_to_tenpai all_t tmp' in*)
      let tehai2 = ripai current_tehai in 
      Mutex.lock lk;
      Hashtbl.add myhash x (tehai2,tmp');
      Mutex.unlock lk;
      tmp
  else
    judge_hash

let hash_serch_lst_to_ary lst = 
  let m = List.length lst in 
  let ary = Array.make m [] in 
  let rec loop i t_lst = match t_lst with
    | [] -> ()
    | h::t -> ary.(i) <- [h];
              loop (i+1) t
  in
  loop 0 lst;
  ary

let parallel tmp =
  let n = List.length tmp in 
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let update p r i = p.(i) <- operate_tenpai_ritu_parallel r.(i) in 
  let results = Array.of_list tmp in
  let pre =  Array.make n [] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre results))) in
  worker (update pre results) ();
  Array.iter Domain.join domains;
  pre

let print_de lst = 
  let rec loop2 x t_lst = match x with 
    | 4 -> ()
    | _ -> print_hai t_lst x;Printf.printf "\n"; loop2 (x+1) t_lst 
in
  let rec loop t_lst = match t_lst with
    | [] -> ()
    | (_,_,_,_,h)::t -> let h = List.map (fun a -> change_gragh a)h in loop2 0 h; loop t 
in
loop lst
let print_de2 lst = 
  let rec loop2 x t_lst = match x with 
    | 4 -> ()
    | _ -> print_hai t_lst x;Printf.printf "\n"; loop2 (x+1) t_lst 
  in
  let h = List.map (fun a -> change_gragh a) lst in 
  loop2 0 h

let judge_parallel tehai = 
  let lst = hash_serch tehai in 
  if lst = [] then 
    let (_,n) = syanten tehai in
    if n  >=  1 then 
      let tenpai_lst = [([],[],[],[],tehai)] in 
      let (k_lst,tumo_lst,k_count,yukou_hai_lst,current_tehai) = List.hd tenpai_lst in
      let tmp = k_fase (k_lst,tumo_lst,k_count,yukou_hai_lst,1,current_tehai) in
      let res = parallel tmp in 
      res
    else
      [|[]|] 
  else
    hash_serch_lst_to_ary lst  

let make_agariritu_kitaiti rest_tumo_lst (new_tumo_lst:(int*float)list list) tumo_len rm_wan yukou_lst sum_lst = 
  let m = List.length rest_tumo_lst in 
  let rec loop2 (tmp,tmp2) y_lst t_lst = match t_lst with 
    | [] -> (tmp,tmp2) 
    | (h1,h2)::t -> 
      let t_ritu = 
                self_t_ritu rm_wan tumo_len y_lst (rest_tumo_lst@[h1]) 
              in
              let kitaiti = t_ritu *. h2 in 
              loop2 ((t_ritu+.tmp),(kitaiti+.tmp2)) y_lst t 
  in
  let rec loop (tmp,tmp2) i t_lst = match t_lst with 
    | [] -> (tmp,tmp2) 
    | h::t -> let sum = List.nth sum_lst i in  
              let (agariritu,kitaiti) = loop2 (0.,0.) (yukou_lst@[sum]) h in 
              loop ((agariritu+.tmp),(kitaiti+.tmp2)) (i+1) t 
  in
  if m = 0 then 
    loop (0.0,0.0) 0 new_tumo_lst
  else if (m+1) > tumo_len then 
    (0.0,0.0)
  else
    loop (0.0,0.0) 0 new_tumo_lst

let tenpai_to_kitaiti_p ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan = 
  let m = Array.length tenpai_lst in
  let tasks = Array.init m (fun i -> i) in 
  create_work tasks;
  let rec loop2 new_rest_tumo_lst sum_lst ary2 zi_ary2 a_k_lst = match a_k_lst with 
    | [] -> (sum_lst,new_rest_tumo_lst)
    | h::t -> 
      let rec lp1 tmp2 sum t_lst2 = match t_lst2 with 
                | [] -> (sum,tmp2)
                | h1::t1 -> 
                            let ((x,y),z) = h1 in 
                            let n = 
                              if x = 3 then 
                                zi_ary2.(y)
                              else
                                ary2.(x).(y)
                            in
                            if n <= 0 then 
                              lp1 tmp2 sum t1
                            else 
                              lp1 ((n,z)::tmp2) (sum+n) t1
              in
              let (sum,tmp2) = lp1 [] 0 h in 
              loop2 (tmp2::new_rest_tumo_lst) (sum::sum_lst) ary2 zi_ary2 t
  in
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (k_lst,tumo_lst,rest_tumo_lst,k_ritu,yukou_lst,current_tehai,t_ritu)::t -> let (ary2,zi_ary2) = ary_opt ary zi_ary tumo_lst in
                                                                let current_tehai = ripai current_tehai in
                                                                let (i,a_k_lst) = tenpai_to_opt_p current_tehai f_lst zi_kaze ba_kaze naki [] dora_lst in
                                                                let (sum_lst,lst) = loop2 [] [] ary2 zi_ary2 a_k_lst in
                                                                let k_ritu = k_ritu *. (1. /. (float_of_int i)) in 
                                                                (*let (agariritu,kitaiti) = make_agariritu_kitaiti rest_tumo_lst lst tumo_l rm_wan in*)
                                                                let (agariritu,kitaiti) = make_agariritu_kitaiti rest_tumo_lst lst tumo_l rm_wan yukou_lst sum_lst in
                                                                loop ((k_lst,t_ritu,agariritu*.k_ritu,kitaiti*.k_ritu)::tmp) t 
  in
  let update p r i = p.(i) <- loop [] r.(i) in 
  let pre =  Array.make m [] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre tenpai_lst))) in
  worker (update pre tenpai_lst) ();
  Array.iter Domain.join domains;
  pre


(*最初の捨て牌が同じものを一つのindexにまとめる
  recieve(k_lst,t_ritu,agariritu,kitaiti)array
  return(k_lst,t_ritu,agariritu,kitaiti)lst *)
let first_sutehai_to_one_index tenpai_array = 
  let n = Array.length tenpai_array in 
  let rec loop i (a_lst, b_tmp, c_tmp, d_tmp) tmp_lst (most_lst, most_a, most_b, most_c) = 
    let (k_lst,t_ritu,agariritu,kitaiti) = tenpai_array.(i) in 
    if k_lst = [] then 
      if i = 0 then
        (most_lst, b_tmp, c_tmp, d_tmp)::tmp_lst
      else
        loop (i-1) (a_lst, b_tmp, c_tmp, d_tmp) tmp_lst (most_lst, most_a, most_b, most_c)
    else
      let first_sutehai = List.nth k_lst (List.length k_lst - 1) in
      if a_lst = [] || List.nth a_lst (List.length a_lst - 1) = first_sutehai then
        let (b_tmp, c_tmp, d_tmp) = (b_tmp+.t_ritu, c_tmp+.agariritu, d_tmp+.kitaiti) in 
        let (most_lst, most_a, most_b, most_c) =  
          if most_c > kitaiti then 
            (most_lst, most_a, most_b, most_c)
          else if most_c < kitaiti then 
            (k_lst, t_ritu,agariritu,kitaiti)
          else if most_b > agariritu then
            (most_lst, most_a, most_b, most_c)
          else if most_b < agariritu then
            (k_lst, t_ritu,agariritu,kitaiti)
          else if most_c > t_ritu then
            (most_lst, most_a, most_b, most_c)
          else 
            (k_lst, t_ritu,agariritu,kitaiti)
        in
        if i = 0 then 
          (most_lst, b_tmp, c_tmp, d_tmp)::tmp_lst
        else
          loop (i-1) (most_lst, b_tmp, c_tmp, d_tmp) tmp_lst (most_lst, most_a, most_b, most_c)
      else
        let n_first_sutehai = List.nth a_lst (List.length a_lst - 1) in 
        let tmp_lst = if let (x,_) = n_first_sutehai in x = 0 && d_tmp <> 0. then (most_lst, b_tmp, c_tmp, d_tmp+.5.)::tmp_lst else (most_lst, b_tmp, c_tmp, d_tmp)::tmp_lst in 
        let (a_lst, b_tmp, c_tmp, d_tmp) = (k_lst,t_ritu,agariritu,kitaiti) in 
        if i = 0 then 
          (a_lst, b_tmp, c_tmp, d_tmp)::tmp_lst
        else
          loop (i-1) (a_lst, b_tmp, c_tmp, d_tmp) tmp_lst (a_lst, b_tmp, c_tmp, d_tmp)
  in
  loop (n-1) ([], 0.0, 0.0, 0.0) [] ([], 0.0, 0.0, 0.0)




    

(*recieve(k_lst,t_ritu,agariritu,kitaiti) lst array, return(k_lst,t_ritu,agariritu,kitaiti) array*)
let opt_tenpai_form_p tenpai_lst = 
  let n = Array.length tenpai_lst in
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let rec kitaiti_find (tmp_a, tmp_b, tmp_c) (tmp_lst, most_a, most_b, most_c) tenpai_lst = match tenpai_lst with
    | [] -> (*let _ = if true then (Printf.printf "t_ritu:%F agariritu:%F kitaiti:%F\n" tmp_a tmp_b tmp_c;)else () in*) (tmp_lst, tmp_a, tmp_b, tmp_c)
    | (k_lst,t_ritu,agariritu,kitaiti)::t ->
      let (tmp_a, tmp_b, tmp_c) = (tmp_a+.t_ritu, tmp_b+.agariritu, tmp_c+.kitaiti) in
      let (tmp_lst, most_a, most_b, most_c) =  
        if most_c > kitaiti then 
          (tmp_lst, most_a, most_b, most_c)
        else if most_c < kitaiti then 
          (k_lst, t_ritu,agariritu,kitaiti)
        else if most_b > agariritu then
          (tmp_lst, most_a, most_b, most_c)
        else if most_b < agariritu then
          (k_lst, t_ritu,agariritu,kitaiti)
        else if most_c > t_ritu then
          (tmp_lst, most_a, most_b, most_c)
        else 
          (k_lst, t_ritu,agariritu,kitaiti)
      in
      kitaiti_find (tmp_a, tmp_b, tmp_c) (tmp_lst, most_a, most_b, most_c) t
  in
  let update p r i = p.(i) <- kitaiti_find (0.0, 0.0, 0.0) ([], 0.0, 0.0, 0.0) r.(i) in 
  let pre =  Array.make n ([],0.0,0.0,0.0) in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre tenpai_lst))) in
  worker (update pre tenpai_lst) ();
  Array.iter Domain.join domains;
  first_sutehai_to_one_index pre
  

(*recieve (k_lst,t_ritu,agariritu,kitaiti,anzendo)lst
   return (k_lst,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti,total_kitaiti)lst*)
let minus_kitaiti_p lst = 
  (*期待値の合計を計算*)
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (_,_,_,kitaiti,_)::t -> loop (tmp +. kitaiti) t
  in
  let sum = loop 0.0 lst in 
  let rec loop2 tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (k_lst,t_ritu,agariritu,kitaiti,anzendo)::t -> let minus_kitaiti = sum -. kitaiti in 
                                                     (*Printf.printf "sum :%f anzendo: %d\n" sum anzendo; *)
                                                     let anzendo_f = float_of_int anzendo in 
                                                     let total_kitaiti = kitaiti -.  anzendo_f*.0.0 in
                                                     loop2 ((k_lst,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti,total_kitaiti)::tmp) t 
  in
  loop2 [] lst



let max_kitaiti_p tenpai_lst = 
  let rec loop lst tmp = match lst with
    | [] -> tmp
    | h::t -> let (_,_,_,_,_,_,a) = h in
              let (_,_,_,_,_,_,b) = tmp in
              if a > b then
                loop t h
              else
                loop t tmp
  in
  loop tenpai_lst ([],0.0,0.0,0.0,0,0.0,0.0)

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

let max_tenpairitu_p tenpai_lst = 
  let rec loop lst tmp = match lst with
    | [] -> tmp
    | h::t -> let (_,a,_,_,_,_,_) = h in
              let (_,b,_,_,_,_,_) = tmp in
              if a > b then
                loop t h
              else
                loop t tmp
  in
  loop tenpai_lst ([],0.0,0.0,0.0,0,0.0,0.0)


let max_agariritu_p tenpai_lst = 
  let rec loop lst tmp = match lst with
    | [] -> tmp
    | h::t -> let (_,_,a,_,_,_,_) = h in
              let (_,_,b,_,_,_,_) = tmp in
              if a > b then
                loop t h
              else
                loop t tmp
  in
  loop tenpai_lst ([],0.0,0.0,0.0,0,0.0,0.0)


(*(k_lst,tumo_lst,rest_tumo_lst,k_count,current_tehai)を受け取って(k_lst,tumo_lst,rest_tumo_lst,k_ritu,current_tehai,t_ritu) を返す*)
let col_tenpai_parallel tenpai_lst_ary tumo_l rm_wan = 
  let n = Array.length tenpai_lst_ary in 
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let rec loop tmp lst = match lst with
    | [] -> tmp
    | (k_lst,tumo_lst,rest_tumo_lst,k_count,yukou_lst,current_tehai)::t -> 
            (*let t_ritu = tenpai_ritu rest_tumo_lst tumo_l rm_wan in *)
            let t_ritu = self_t_ritu rm_wan tumo_l yukou_lst rest_tumo_lst in 
            let k_ritu = calc_k_ritu_not_naki k_count in 
            (*let k_ritu = k_ritu *. (yukou_to_ritu (int_of_float rm_wan) tumo_l yukou_lst rest_tumo_lst) in*)
            let t_ritu = t_ritu *. k_ritu in 
            (*let _ =  if true then (Printf.printf "%d, " tumo_l; Printf.printf "%f, " t_ritu; List.iter (fun a -> Printf.printf "%d "a) rest_tumo_lst; Printf.printf "\n") else () in*)
            if t_ritu <= 0.0 then
              loop tmp t
            else
              loop ((k_lst,tumo_lst,rest_tumo_lst,k_ritu,yukou_lst,current_tehai,t_ritu)::tmp) t
    in 
  let update p r i = p.(i) <- loop [] r.(i) in 
  let pre =  Array.make n [] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre tenpai_lst_ary))) in
  worker (update pre tenpai_lst_ary) ();
  Array.iter Domain.join domains;
  pre

let col_tenpai_parallel_f tenpai_lst_ary tumo_l rm_wan = 
  let n = Array.length tenpai_lst_ary in 
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let rec loop tmp lst = match lst with
    | [] -> tmp
    | (k_lst,tumo_lst,rest_tumo_lst,k_count,yukou_lst,current_tehai)::t -> 
            (*let t_ritu = tenpai_ritu rest_tumo_lst tumo_l rm_wan in*)
            let t_ritu = self_t_ritu rm_wan tumo_l yukou_lst rest_tumo_lst in 
            let k_ritu = calc_k_ritu_not_naki k_count in 
            (*let k_ritu = k_ritu *. (yukou_to_ritu (int_of_float rm_wan) tumo_l yukou_lst rest_tumo_lst) in*)
            let t_ritu = t_ritu *. k_ritu in 
            (*let _ =  if true then (Printf.printf "%d, " tumo_l; Printf.printf "%f, " t_ritu; List.iter (fun a -> Printf.printf "%d "a) rest_tumo_lst; Printf.printf "\n") else () in*)
            if t_ritu <= 0.0 then
              loop tmp t
            else
              loop ((k_lst,tumo_lst,rest_tumo_lst,k_ritu,yukou_lst,current_tehai,t_ritu)::tmp) t
    in 
  let update p r i = p.(i) <- loop [] r.(i) in 
  let pre =  Array.make n [] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre tenpai_lst_ary))) in
  worker (update pre tenpai_lst_ary) ();
  Array.iter Domain.join domains;
  pre

(*list list array*)
let col_tenpai ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst = 
  let tenpai_lst = judge_parallel tehai in (*list list Array*)
  let tenpai_lst = parallel_rest_tumo_lst ary zi_ary tenpai_lst in 
  let a_len = Array.length tenpai_lst in 
  let rm_wan = yama_len-14 in
  let tumo_l =  rm_wan / 4 in
  let rm_wan = Int.to_float rm_wan in
  if a_len = 1 && List.length tenpai_lst.(0) = 0 || a_len = 0 then
    ([],0.0,0.0,0.0,0,0.0,0.0)
  else
    let tenpai_lst = col_tenpai_parallel tenpai_lst tumo_l rm_wan in
    let tenpai_lst = tenpai_to_kitaiti_p ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
    let last_form_tenpai_lst = opt_tenpai_form_p tenpai_lst in
    if last_form_tenpai_lst = [] then 
      ([],0.0,0.0,0.0,0,0.0,0.0)
    else
      let add_anzendo_lst = List.map (fun (a,b,c,d) -> (a,b,c,d,(anzen ary zi_ary a))) last_form_tenpai_lst in
      let final_form = minus_kitaiti_p add_anzendo_lst in 
      let max = max_kitaiti_p final_form in 
      if max = ([],0.0,0.0,0.0,0,0.0,0.0) then 
        max_tenpairitu_p final_form
      else
        max


(*(k_lst,tumo_lst,rest_tumo_lst,current_tehai,t_ritu,agariritu,kitaiti,anzendo,minus_kitaiti,total_kitaiti)*)    
let col_tenpai ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst = 
  let tenpai_lst = judge_parallel tehai in (*list list Array*)
  let tenpai_lst = parallel_rest_tumo_lst ary zi_ary tenpai_lst in 
  let a_len = Array.length tenpai_lst in 
  let rm_wan = yama_len-14 in
  let tumo_l =  rm_wan / 4 in
  let rm_wan = Int.to_float rm_wan in
  if a_len = 1 && List.length tenpai_lst.(0) = 0 || a_len = 0 then
    ([],0.0,0.0,0.0,0,0.0,0.0)
  else
    let tenpai_lst = col_tenpai_parallel tenpai_lst tumo_l rm_wan in
    let tenpai_lst = tenpai_to_kitaiti_p ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
    let last_form_tenpai_lst = opt_tenpai_form_p tenpai_lst in
    (*List.iter (fun (a,c,d,e) -> let (x,y) = List.nth a (List.length a - 1) in Prinif.printf "(%d,_) %f %f %f\n" x c d e) last_form_tenpai_lst; *)
    (*List.iter (fun (a,b,c,d) -> let (a1,a2) = hai_to_ary (List.nth a (List.length a -1)) in Printf.printf "(%d,%d)) %f %f %f \n"a1 a2 b c d;) last_form_tenpai_lst;*)
    if last_form_tenpai_lst = [] then 
      ([],0.0,0.0,0.0,0,0.0,0.0)
    else
      let add_anzendo_lst = List.map (fun (a,b,c,d) -> (a,b,c,d,(anzen ary zi_ary a))) last_form_tenpai_lst in
      let final_form = minus_kitaiti_p add_anzendo_lst in 
      let max = max_kitaiti_p final_form in 
      if max = ([],0.0,0.0,0.0,0,0.0,0.0) then 
        max_tenpairitu_p final_form
      else
        max

let kuikae_check kuikae_lst n rm_wan tumo_l tenpai_lst_ary = 
  let tasks = Array.init n (fun i -> i) in
  create_work tasks;
  let rec loop tmp lst = match lst with 
    | [] -> tmp
    | (k_lst,tumo_lst,rest_tumo_lst,k_count,yukou_lst,current_tehai)::t -> let tmp = 
                                                          let k_lst_len = List.length k_lst in 
                                                          if k_lst_len = 0 then
                                                            tmp 
                                                          else
                                                            if List.exists (fun a -> a = (List.nth k_lst (k_lst_len-1))) kuikae_lst then
                                                              tmp
                                                            else
                                                              (*let t_ritu = tenpai_ritu rest_tumo_lst tumo_l rm_wan in*)
                                                              let t_ritu = self_t_ritu rm_wan tumo_l yukou_lst rest_tumo_lst in 
                                                              let k_ritu = calc_k_ritu_not_naki k_count in 
                                                              (*let k_ritu = k_ritu *. (yukou_to_ritu (int_of_float rm_wan) tumo_l yukou_lst rest_tumo_lst) in*)
                                                              let t_ritu = t_ritu *. k_ritu in 
                                                              if t_ritu <= 0.0 then 
                                                                tmp
                                                              else
                                                                (k_lst,tumo_lst,rest_tumo_lst,k_ritu,yukou_lst,current_tehai,t_ritu)::tmp 
                                                        in
                                                        loop tmp t
  in
  let update p r i = p.(i) <- loop [] r.(i) in 
  let pre =  Array.make n [] in 
  let domains = Array.init (num_domains - 1)
              (fun _ -> Domain.spawn(worker (update pre tenpai_lst_ary))) in
  worker (update pre tenpai_lst_ary) ();
  Array.iter Domain.join domains;
  pre  


let col_tenpai_f_kuikae ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst kuikae_lst = 
  let tenpai_lst = judge_parallel tehai in (*list list Array*)
  let tenpai_lst = parallel_rest_tumo_lst ary zi_ary tenpai_lst in 
  let a_len = Array.length tenpai_lst in 
  let rm_wan = yama_len-14 in
  let tumo_l =  rm_wan / 4 in
  let rm_wan = Int.to_float rm_wan in
  if a_len = 1 && List.length tenpai_lst.(0) = 0 || a_len = 0 then
    ([],0.0,0.0,0.0,0,0.0,0.0)
  else
    let tenpai_lst = kuikae_check kuikae_lst a_len rm_wan tumo_l tenpai_lst in
    let tenpai_lst = tenpai_to_kitaiti_p ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
    let last_form_tenpai_lst = opt_tenpai_form_p tenpai_lst in
    if last_form_tenpai_lst = [] then 
      ([],0.0,0.0,0.0,0,0.0,0.0)
    else
      let add_anzendo_lst = List.map (fun (a,b,c,d) -> (a,b,c,d,(anzen ary zi_ary a))) last_form_tenpai_lst in
      let final_form = minus_kitaiti_p add_anzendo_lst in 
      let max = max_kitaiti_p final_form in 
      if max = ([],0.0,0.0,0.0,0,0.0,0.0) then 
        max_tenpairitu_p final_form
      else
        max
    

let mode_kokushi ary zi_ary k_lst = 
  let k_lst = tehai_to_anzen ary zi_ary k_lst in
  let rec loop tmp t_lst = match t_lst with 
    | [] -> let (x,_) = tmp in
            x 
    | ((x,y),z)::t -> let (_,z') = tmp in
                      let tmp = 
                        if z' < z then
                          ((x,y),z)
                        else
                          tmp
                      in
                      loop tmp t 
  in  
  loop ((1,Not_hai),0) k_lst

let mode_somete ary zi_ary tehai s_hai =
  let n_tehai = List.filter (fun (_,b) -> b <> s_hai) tehai in
  let n_tehai = List.filter (fun (a,_) -> a <> 0 ) n_tehai in
  let n_tehai = tehai_to_anzen ary zi_ary n_tehai in
  let rec loop tmp t_lst = match t_lst with 
    | [] -> let (x,_) = tmp in
            x
    | ((x,y),z)::t -> let (_,z') = tmp in
                      let tmp = 
                        if z' < z then
                          ((x,y),z)
                        else
                          tmp
                      in
                      loop tmp t 
  in
  loop ((1,Not_hai),0) n_tehai

(*anzen_baseの処理未実装*)
let mode_titoi ary zi_ary t_lst tehai = 
  let t_lst = tehai_to_anzen ary zi_ary t_lst in
  let rec loop tmp lst = match lst with 
    | [] -> let (x,_) = tmp in
            x
    | ((x,y),z)::t -> let (_,z') = tmp in
                      let tmp = 
                        if z' < z then
                          ((x,y),z)
                        else
                          tmp
                      in
                      loop tmp t 
  in
  loop ((1,Not_hai),0) t_lst



let mode_common_b ary zi_ary sutehai_lst tehai player = 
  let (_,n) = syanten tehai in
  let rec loop tmp double_lst t_lst = match t_lst with
    | [] -> tmp 
    | h::t -> if List.exists (fun a -> a = h) double_lst then
                loop tmp double_lst t 
              else
                let n_tehai = d_tehai tehai h in 
                let (_,n') = syanten n_tehai in 
                let tmp = 
                  if n = n' then
                    if anzen_base ary zi_ary n_tehai sutehai_lst player then
                      h::tmp
                    else
                      tmp
                  else
                    tmp
                in 
                loop tmp (h::double_lst) t
  in
  let k_lst = loop [] [] tehai in
  if k_lst = [] then 
    let n_tehai = tehai_to_anzen ary zi_ary tehai in
    let ((x,y),_) = max_anzen n_tehai in
    (x,y)
  else
    let n_tehai = tehai_to_anzen ary zi_ary k_lst in
    let ((x,y),_) = max_anzen n_tehai in
    (x,y)

let mode_attack_common_b ary zi_ary sutehai_lst tehai player dora = 
  let dora = List.map (fun a -> hyouzi_to_dora a) dora in 
  let (_,n) = syanten tehai in
  let (_,normal) = common_syanten tehai in 
  let rec loop (count,tmp) t_lst = match t_lst with 
    | [] -> tmp
    | (x,y)::t -> let new_tehai = d_tehai tehai (x,y) in 
                  let (_,n') = syanten new_tehai in
                  if n = n' && (List.exists (fun a -> if a = (hai_to_ary (x,y)) then true else false) dora ) = false then 
                    let new_count = count_yukouhai ary zi_ary (hai_to_ary (x,y)) in 
                    if new_count < count then 
                      loop (new_count,(x,y)) t
                    else
                      loop (count,tmp) t
                  else
                    loop (count,tmp) t
  in
  let rec loop_normal (count,tmp) t_lst = match t_lst with 
    | [] -> tmp
    | (x,y)::t -> let new_tehai = d_tehai tehai (x,y) in 
                  let (_,n') = syanten new_tehai in
                  let (_,normal') = common_syanten tehai in 
                  if n = n' && normal = normal' && (List.exists (fun a -> if a = (hai_to_ary (x,y)) then true else false) dora ) = false then 
                    let new_count = count_yukouhai ary zi_ary (hai_to_ary (x,y)) in 
                    if new_count < count then 
                      loop (new_count,(x,y)) t
                    else
                      loop (count,tmp) t
                  else
                    loop (count,tmp) t
  in
  let trush = if n = normal then 
                loop (30,(1,Not_hai)) tehai 
              else if (normal - n) = 1 then 
                loop_normal (30,(1,Not_hai)) tehai
              else
                loop (30,(1,Not_hai)) tehai 
              in           
  if trush <> (1,Not_hai) then
    trush
  else 
    mode_common_b ary zi_ary sutehai_lst tehai player



let common_b ary zi_ary tehai sutehai_lst tumo_len (f_lst:(state*(int*(int*int*int)))list) player dora furiten_lst = 
  let (_,n) = syanten tehai in
  if tumo_len >= n then
    let (k_lst,kind_k) = kind_kokushi tehai in
    let (s_hai,s_count) = somete tehai f_lst in
    let (t_lst,t_count) = titoi_allow tehai f_lst in
    let mode = 
      if tumo_len > 9 then 
          if kind_k >= 9 then
            Kokushi
          else if s_count >= 11 then
            Some
          else if t_count >= 4 then
            Titoi
          else
            Attack_common_b    
      else
          if t_count >= 3 then
            Titoi
          else if s_count >= 11 then
            Some
          else if kind_k >= 9 then
            Kokushi
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
      else if mode = Attack_common_b then
        mode_attack_common_b ary zi_ary sutehai_lst tehai player dora
      else
        mode_common_b ary zi_ary sutehai_lst tehai player
    in
    hai_to_int tehai k_hai
  else
    let yaku_lst = 
      if player = 0 then 
        ([],[Reach],[Reach],[Reach])
      else if player = 1 then
        ([Reach],[],[Reach],[Reach])
      else if player = 2 then
        ([Reach],[Reach],[],[Reach])
      else
        ([Reach],[Reach],[Reach],[])
    in
    reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst 


(*true:攻撃的な戦術,false:守備的な戦術*)
let mode_choice count tumo_len = 
  if tumo_len > 15 then
    if count <= 3 then
      true
    else
      false 
  else if tumo_len > 12 then
    if count <= 3 then
      true
    else
      false 
  else if tumo_len > 9 then
    if count <= 3 then
      true
    else
      false  
  else if tumo_len > 6 then
    if count <= 2 then 
      true
    else
      false
  else 
    if count <= 2 then 
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



  

let convert_hai_to_int = function 
  | (1,Manzu) -> 0
  | (2,Manzu) -> 1
  | (3,Manzu) -> 2
  | (4,Manzu) -> 3
  | (5,Manzu) -> 4
  | (6,Manzu) -> 5
  | (7,Manzu) -> 6
  | (8,Manzu) -> 7
  | (9,Manzu) -> 8
  | (1,Pinzu) -> 9
  | (2,Pinzu) -> 10
  | (3,Pinzu) -> 11
  | (4,Pinzu) -> 12
  | (5,Pinzu) -> 13
  | (6,Pinzu) -> 14
  | (7,Pinzu) -> 15
  | (8,Pinzu) -> 16
  | (9,Pinzu) -> 17
  | (1,Souzu) -> 18
  | (2,Souzu) -> 19
  | (3,Souzu) -> 20
  | (4,Souzu) -> 21
  | (5,Souzu) -> 22
  | (6,Souzu) -> 23
  | (7,Souzu) -> 24
  | (8,Souzu) -> 25
  | (9,Souzu) -> 26
  | (0,Ton) -> 27
  | (0,Nan) -> 28
  | (0,Sya) -> 29
  | (0,Pei) -> 30
  | (0,Haku) -> 31
  | (0,Hatsu) -> 32
  | (0,Tyun) -> 33
  | (5,Manzu_red) -> 34
  | (5,Pinzu_red) -> 35
  | (5,Souzu_red) -> 36


let hand_convert hand = 
  let m = List.length hand in 
  if m = 14 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3);`Int (List.nth hand 4);`Int (List.nth hand 5);`Int (List.nth hand 6);`Int (List.nth hand 7);`Int (List.nth hand 8);`Int (List.nth hand 9);`Int (List.nth hand 10);`Int (List.nth hand 11);`Int (List.nth hand 12);`Int (List.nth hand 13)])
  else if m = 13 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3);`Int (List.nth hand 4);`Int (List.nth hand 5);`Int (List.nth hand 6);`Int (List.nth hand 7);`Int (List.nth hand 8);`Int (List.nth hand 9);`Int (List.nth hand 10);`Int (List.nth hand 11);`Int (List.nth hand 12)])
  else if m = 11 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3);`Int (List.nth hand 4);`Int (List.nth hand 5);`Int (List.nth hand 6);`Int (List.nth hand 7);`Int (List.nth hand 8);`Int (List.nth hand 9);`Int (List.nth hand 10)])
  else if m = 10 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3);`Int (List.nth hand 4);`Int (List.nth hand 5);`Int (List.nth hand 6);`Int (List.nth hand 7);`Int (List.nth hand 8);`Int (List.nth hand 9)])
  else if m = 8 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3);`Int (List.nth hand 4);`Int (List.nth hand 5);`Int (List.nth hand 6);`Int (List.nth hand 7)])
  else if m = 7 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3);`Int (List.nth hand 4);`Int (List.nth hand 5);`Int (List.nth hand 6)])
  else if m = 5 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3);`Int (List.nth hand 4)])
  else if m = 4 then 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1);`Int (List.nth hand 2);`Int (List.nth hand 3)])
  else 
    ("hand", `List [`Int (List.nth hand 0);`Int (List.nth hand 1)])

let dora_convert dora = 
  let m = List.length dora in 
  if m = 1 then 
    ("dora", `List [`Int (List.nth dora 0)])
  else if m = 2 then 
    ("dora", `List [`Int (List.nth dora 0);`Int (List.nth dora 1)])
  else if m = 3 then 
    ("dora", `List [`Int (List.nth dora 0);`Int (List.nth dora 1);`Int (List.nth dora 2)])
  else  
    ("dora", `List [`Int (List.nth dora 0);`Int (List.nth dora 1);`Int (List.nth dora 2);`Int (List.nth dora 2)])
    
let furo_convert f_lst = 
  let m = List.length f_lst in 
  let rec loop tmp t_lst = match t_lst with
    | [] -> tmp 
    | (_,(a,(b,c,d)))::t -> let n1 = convert_hai_to_int (ary_to_hai (a,b)) in 
                            let n2 = convert_hai_to_int (ary_to_hai (a,c)) in 
                            let n3 = convert_hai_to_int (ary_to_hai (a,d)) in 
                            loop (n1::n2::n3::tmp) t 
  in
  let n_f_lst = loop [] f_lst in 
  if m = 0 then 
    ("furo", `List [`Int 37])
  else if m = 1 then 
    ("furo", `List [`Int (List.nth n_f_lst 0);`Int (List.nth n_f_lst 1);`Int (List.nth n_f_lst 2)])
  else if m = 2 then  
    ("furo", `List [`Int (List.nth n_f_lst 0);`Int (List.nth n_f_lst 1);`Int (List.nth n_f_lst 2);`Int (List.nth n_f_lst 3);`Int (List.nth n_f_lst 4);`Int (List.nth n_f_lst 5)])
  else if m = 3 then  
    ("furo", `List [`Int (List.nth n_f_lst 0);`Int (List.nth n_f_lst 1);`Int (List.nth n_f_lst 2);`Int (List.nth n_f_lst 3);`Int (List.nth n_f_lst 4);`Int (List.nth n_f_lst 5);`Int (List.nth n_f_lst 6);`Int (List.nth n_f_lst 7);`Int (List.nth n_f_lst 8)])
  else  
    ("furo", `List [`Int (List.nth n_f_lst 0);`Int (List.nth n_f_lst 1);`Int (List.nth n_f_lst 2);`Int (List.nth n_f_lst 3);`Int (List.nth n_f_lst 4);`Int (List.nth n_f_lst 5);`Int (List.nth n_f_lst 6);`Int (List.nth n_f_lst 7);`Int (List.nth n_f_lst 8);`Int (List.nth n_f_lst 9);`Int (List.nth n_f_lst 10);`Int (List.nth n_f_lst 11)])

let table_json ary zi_ary (m_red,p_red,s_red) =
  let m5 = if m_red then 0 else 1 in 
  let p5 = if p_red then 0 else 1 in 
  let s5 = if s_red then 0 else 1 in 
  let n5m = ary.(0).(4) - m5 in 
  let n5p = ary.(1).(4) - p5 in 
  let n5s = ary.(2).(4) - s5 in 
  ("table", `List [`Int ary.(0).(0); `Int ary.(0).(1); `Int ary.(0).(2); `Int ary.(0).(3); `Int n5m; `Int ary.(0).(5); `Int ary.(0).(6); `Int ary.(0).(7); `Int ary.(0).(8);
                   `Int ary.(1).(0); `Int ary.(1).(1); `Int ary.(1).(2); `Int ary.(1).(3); `Int n5p; `Int ary.(1).(5); `Int ary.(1).(6); `Int ary.(1).(7); `Int ary.(1).(8);
                   `Int ary.(2).(0); `Int ary.(2).(1); `Int ary.(2).(2); `Int ary.(2).(3); `Int n5s; `Int ary.(2).(5); `Int ary.(2).(6); `Int ary.(2).(7); `Int ary.(2).(8);
                   `Int zi_ary.(0); `Int zi_ary.(1); `Int zi_ary.(2); `Int zi_ary.(3); `Int zi_ary.(4); `Int zi_ary.(5); `Int zi_ary.(6); `Int m5; `Int p5; `Int s5]) 

let dahaikouho hand f_lst ary zi_ary dora zi_kaze ba_kaze turn (m_red,p_red,s_red) = 
  let hand = List.map (fun a -> convert_hai_to_int a) hand in 
  let dora = List.map (fun a -> convert_hai_to_int (ary_to_hai a)) dora in 
  let dahai = hand_convert hand in
  let doraindicators = dora_convert dora in  
  let furo_json = furo_convert f_lst in
  let table = table_json ary zi_ary (m_red,p_red,s_red) in 
  `Assoc
    [
      dahai;
      furo_json;
      table;
      doraindicators;
      ("zikaze", `Int zi_kaze);
      ("bakaze", `Int ba_kaze);
      ("turn", `Int turn);
    ]


let client_fun ic oc hand f_lst ary zi_ary dora zi_kaze ba_kaze turn (m_red,p_red,s_red) = 
  let open Yojson.Basic.Util in
  let zi_kaze = if zi_kaze = 0 then 27 else if zi_kaze = 1 then 28 else if zi_kaze = 2 then 29 else 30 in 
  let ba_kaze = if ba_kaze = 0 then 27 else 28 in 
  let turn = 18 - turn in 
  (*Yojson.Basic.to_channel oc new_json;*)
  let new_json = dahaikouho hand f_lst ary zi_ary dora zi_kaze ba_kaze turn (m_red,p_red,s_red) in 
  let new_json = Yojson.Basic.to_string new_json in 
  output_string !oc (new_json ^ "\n");
  Out_channel.flush !oc;
  let r = input_line !ic in 
  let json = Yojson.Basic.from_string r in
  let new_list = json |> member "send_list" |> convert_each (fun a -> let dahai = a |> member "hai" |> to_int in 
                                                                      let t_ritu = a |> member "tenpai" |> to_float in
                                                                      let a_ritu = a |> member "agari" |> to_float in
                                                                      let kitaiti = a |> member "kitaiti" |> to_float in  
                                                                      if dahai = 37 then 
                                                                        ((1,Not_hai),0.,0.,0.)
                                                                      else
                                                                        let t_ritu = if t_ritu = 1.1 then 1.0 else if t_ritu = -0.1 then 0.0 else t_ritu in 
                                                                        let a_ritu = if a_ritu = -0.1 then 0.0 else a_ritu in
                                                                        let kitaiti = if kitaiti = -0.1 then 0.0 else kitaiti in
                                                                        let dahai = convert_int_to_hai dahai in
                                                                        (dahai,t_ritu,a_ritu,kitaiti) ) in 
                                                                        Printf.printf "step3\n";
  if new_list = [] then 
    [((1,Not_hai),0.,0.,0.)]
  else
    new_list
  (*let dahai = json |> member "hai" |> to_int in
  let t_ritu = json |> member "tenpai" |> to_float in
  let a_ritu = json |> member "agari" |> to_float in
  let kitaiti = json |> member "kitaiti" |> to_float in
  if dahai = 34 then 
    ((1,Not_hai),0.,0.,0.)
  else
    let t_ritu = if t_ritu = 1.1 then 1.0 else if t_ritu = -0.1 then 0.0 else t_ritu in 
    let a_ritu = if a_ritu = -0.1 then 0.0 else a_ritu in
    let kitaiti = if kitaiti = -0.1 then 0.0 else kitaiti in
    let dahai = convert_int_to_hai dahai in 
    (dahai,t_ritu,a_ritu,kitaiti)*)

let judge_reach_opt lst ary zi_ary g_lst tehai yaku_lst furiten_lst = 
  let a_tehai = tehai_to_anzen_in_reach ary zi_ary tehai yaku_lst furiten_lst in 
  let tehai = rhai_to_hai tehai in 
  (*  List.iter (fun (a,c) -> let (a,b) = hai_to_ary a in Printf.printf "(%d,%d):%d\n" a b c) a_tehai;*)
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (k_hai,_,_,kitaiti)::t -> if List.exists (fun (a,b) -> k_hai = (a,b)) tehai then 
                                  if List.exists (fun x -> x =k_hai) g_lst || (let (_,c) = List.find (fun ((a,b),c) -> if (a,b) = k_hai then true else false) a_tehai in c < 4) then
                                    let (t_k_hai,_) = tmp in  
                                    if -1 = t_k_hai then 
                                      ((hai_to_int tehai k_hai),kitaiti) 
                                    else
                                      let (_,t_kitaiti) = tmp in 
                                      if (t_kitaiti -. kitaiti) > 500. then 
                                        tmp 
                                      else
                                        ((hai_to_int tehai k_hai),kitaiti)
                                  else if (*judge_suzi k_hai ary zi_ary g_lst &&*) (let (_,c) = List.find (fun ((a,b),c) -> if (a,b) = k_hai then true else false) a_tehai in c < 6) then 
                                    let (t_k_hai,_) = tmp  in
                                    if -1 = t_k_hai then
                                      loop ((hai_to_int tehai k_hai),kitaiti) t 
                                    else
                                      loop tmp t
                                  else
                                    loop tmp t 
                                else
                                  loop tmp t 

  in
  let (k,_) = loop (-1,0.) lst in 
  k  
                            


let judge_reach ary zi_ary tehai sutehai_lst yaku_lst yama_len f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan yaku furiten_lst (m_red,p_red,s_red) = 
  let (_,n) = syanten tehai in 
  let n' = titoi_syanten tehai in
  let n'' = kokushi_syanten tehai in
  if tumo_l >= n then 
    if n = 0 then 
      let (x,(_,kitaiti)) = tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku dora_lst ary zi_ary in
      if kitaiti > 0.0 then
        if x = 13 then 
          List.length tehai - 1
        else
          x 
      else
        reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst
    else if n = n' || n = n'' then 
      reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst
    else if tumo_l < 1 then 
      let c_lst = client_fun ic_kitaiti oc_kitaiti tehai f_lst ary zi_ary dora_lst zi_kaze ba_kaze (tumo_l+1) (m_red,p_red,s_red) in
      let c_lst = List.map (fun (a,b,c,d) -> (rhai_to_hai_single a,b,c,d)) c_lst in 
      let g_lst = reach_genbutu yaku_lst furiten_lst tehai in
      let k_hai = judge_reach_opt c_lst ary zi_ary g_lst tehai yaku_lst furiten_lst in 
      if k_hai = -1 then  
          reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst
      else
        k_hai
    else 
      let c_lst = client_fun ic_kitaiti oc_kitaiti tehai f_lst ary zi_ary dora_lst zi_kaze ba_kaze tumo_l (m_red,p_red,s_red) in
      let c_lst = List.map (fun (a,b,c,d) -> (rhai_to_hai_single a,b,c,d)) c_lst in 
      let g_lst = reach_genbutu yaku_lst furiten_lst tehai in
      let k_hai = judge_reach_opt c_lst ary zi_ary g_lst tehai yaku_lst furiten_lst in 
      if k_hai = -1 then  
          reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst
      else
        k_hai
  else
    reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst

let dis_add_main_pre tehai ary zi_ary tumo_l dora_lst =
  let dora = List.map (fun a -> hyouzi_to_dora a) dora_lst in 
  let (dahai,(_,_,_)) = dis_add_main tehai ary zi_ary tumo_l dora in
  hai_to_int tehai dahai
    


(*m change*)
let change_yaku_lst yaku_lst furo_lst = 
  let furo_1 = if List.length (tapl_player_1 furo_lst) > 2 then true else false in
  let furo_2 = if List.length (tapl_player_2 furo_lst) > 2 then true else false in 
  let furo_3 = if List.length (tapl_player_3 furo_lst) > 2 then true else false in 
  let furo_4 = if List.length (tapl_player_4 furo_lst) > 2 then true else false in  
  let (yaku_1,yaku_2,yaku_3,yaku_4) = yaku_lst in 
  let yaku_1 = if furo_1 = true then [Reach] else yaku_1 in 
  let yaku_2 = if furo_2 = true then [Reach] else yaku_2 in 
  let yaku_3 = if furo_3 = true then [Reach] else yaku_3 in 
  let yaku_4 = if furo_4 = true then [Reach] else yaku_4 in 
  (yaku_1,yaku_2,yaku_3,yaku_4)
  
let prob_select sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst furo_double_lst furiten_lst =
  let yaku = tapl_player yaku_lst player in
  let reach_q_1 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_1 yaku_lst) in
  let reach_q_2 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_2 yaku_lst) in
  let reach_q_3 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_3 yaku_lst) in
  let reach_q_4 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_4 yaku_lst) in
  let (_,n) = syanten tehai in
  let n' = titoi_syanten tehai in
  let n'' = kokushi_syanten tehai in
  let (_,n''') = common_syanten tehai in 
  let (ary,zi_ary) = create_table sutehai_lst tehai in
  let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in
  let (m_red,p_red,s_red) = create_taable_in_red sutehai_lst tehai in 
  let furo_q = furo_defence ary zi_ary yaku_lst sutehai_lst furo_lst tehai furiten_lst in
  let f_lst = tapl_player furo_lst player in
  let rm_wan = (yama_len-14) in
  let tumo_l = (rm_wan)/4 in
  let a_lst = tehai_to_anzen ary zi_ary (rhai_to_hai tehai) in 
  let rm_wan = Int.to_float rm_wan in
  if mode_choice n tumo_l then 
    if List.exists (fun a -> a = Reach || a = Doublereach) yaku = true then
      tumogiri tehai 
    else if reach_q_1 || reach_q_2 || reach_q_3 || reach_q_4 then 
      if furo_q <> -1 then 
        let yaku_lst = change_yaku_lst yaku_lst furo_lst in
        judge_reach ary zi_ary tehai sutehai_lst yaku_lst yama_len f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan yaku furiten_lst (m_red,p_red,s_red)
      else
        judge_reach ary zi_ary tehai sutehai_lst yaku_lst yama_len f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan yaku  furiten_lst (m_red,p_red,s_red)
    else if n = n'' || (n = n' && n <> n''') then 
      if n = 0 then 
        let (x,_) = tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku dora_lst ary zi_ary in
        if x = 13 then 
          (List.length tehai - 1)
        else
          x
      else
        common_b ary zi_ary tehai sutehai_lst tumo_l f_lst player dora_lst furiten_lst
    else if tumo_l < 1 then 
      let (dahai,_,_,_) = List.hd (client_fun ic_kitaiti oc_kitaiti tehai (tapl_player furo_lst player) ary zi_ary dora_lst zi_kaze ba_kaze (tumo_l+1) (m_red,p_red,s_red)) in 
      if dahai = (1,Not_hai) then 
        if n = 1 && (List.length tehai) = 14 then 
          dis_add_main_pre tehai ary zi_ary tumo_l dora_lst
        else
          hai_to_int tehai (mode_attack_common_b ary zi_ary sutehai_lst tehai player dora_lst)
      else
        hai_to_int tehai dahai
    else if n = 0 then
      let (x,_) = tenpai_to_opt tehai tumo_l rm_wan f_lst zi_kaze ba_kaze naki yaku dora_lst ary zi_ary in
      if x = 13 then 
        List.length tehai - 1
      else
        x
      (*let (dahai,_,_,_) = client_fun ic_kitaiti oc_kitaiti tehai dora_lst zi_kaze ba_kaze tumo_l in 
      hai_to_int tehai dahai*)
    else 
      (*let (a,_,_,_,_,_,_) = col_tenpai ary zi_ary tehai yama_len f_lst zi_kaze ba_kaze naki dora_lst in
      let a_len = List.length a in
      if a_len = 0 then 
        let a_tehai = tehai_to_anzen ary zi_ary tehai in
        let ((a,b),_) = minimum_anzen a_tehai in
        hai_to_int tehai (a,b)
      else
      let x = List.nth a (a_len-1) in
      hai_to_int tehai x*)
      let kouho_lst =  (client_fun ic_kitaiti oc_kitaiti tehai (tapl_player furo_lst  player) ary zi_ary dora_lst zi_kaze ba_kaze tumo_l (m_red,p_red,s_red)) in 
      let (dahai1,_,_,kitaiti1) = if List.length kouho_lst = 0 then ((1,Not_hai),0.,0.,0.) else List.hd kouho_lst in 
      let (dahai2,_,_,kitaiti2) = if List.length kouho_lst = 1 then ((1,Not_hai),0.,0.,0.) else List.nth kouho_lst 1 in 
      if dahai1 = (1,Not_hai) then 
        if n = 1 && (List.length tehai) = 14 then 
          dis_add_main_pre tehai ary zi_ary tumo_l dora_lst
        else
          hai_to_int tehai (mode_attack_common_b ary zi_ary sutehai_lst tehai player dora_lst)
      else if dahai2 = (1,Not_hai) then 
        hai_to_int tehai dahai1
      else
        let (_,anzen1) = List.find (fun ((a,b),c) -> if (a,b) = (rhai_to_hai_single dahai1) then true else false) a_lst in
        let (_,anzen2) = List.find (fun ((a,b),c) -> if (a,b) = (rhai_to_hai_single dahai2) then true else false) a_lst in
        if (kitaiti1 -. kitaiti2) < 150. then 
          if anzen1 > anzen2 then
            hai_to_int tehai dahai1
          else
            hai_to_int tehai dahai2
        else
          hai_to_int tehai dahai1
  else
    if reach_q_1 || reach_q_2 || reach_q_3 || reach_q_4 then
      reach_defence ary zi_ary yaku_lst sutehai_lst tehai furiten_lst
    else
      common_b ary zi_ary tehai sutehai_lst tumo_l f_lst player dora_lst furiten_lst

(*(agariritu,kitaiti),furohai*)
let f_kitaiti p_f_lst tehai f_lst (x,y) ary zi_ary yama_len zi_kaze ba_kaze dora_lst =
  let n = List.length tehai in
  let rm_wan = ((yama_len-14)-3) in
  let tumo_l = rm_wan/4 in
  let rm_wan = Int.to_float rm_wan in
  let rec loop tmp t_lst = match t_lst with
    | [] -> tmp 
    | (s,(a,(b,c,d)))::t -> let n_f_lst = (s,(a,(b,c,d)))::f_lst in
                            let (_,ya) = hai_to_ary (x,y) in
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
                                let (agariritu,kitaiti) = x in
                                (1.0,agariritu,kitaiti,k_hai)
                              else 
                                let (k_lst,t_ritu,agariritu,_,_,_,total_kitaiti) = col_tenpai_f_kuikae ary zi_ary n_tehai yama_len n_f_lst zi_kaze ba_kaze true dora_lst kuikae_lst in 
                                let k_lst_len = List.length k_lst in 
                                if k_lst_len = 0 then 
                                  (t_ritu,agariritu,total_kitaiti,(1,Not_hai))
                                else
                                  (t_ritu,agariritu,total_kitaiti,(List.nth k_lst (k_lst_len-1)))
                            in
                            let tmp = ((t_ritu,agariritu,total_kitaiti,k_hai),(s,(a,(b,c,d))))::tmp in
                            loop tmp t 
                          in
  loop [] p_f_lst

let judge_parallel_f tehai = 
  let (_,n) = syanten tehai in 
  if n  >=  1 then 
    let tenpai_lst = [([],[],[],[],tehai)] in 
    let (k_lst,tumo_lst,k_count,yukou_hai_lst,current_tehai) = List.hd tenpai_lst in
    let tmp = all_tumo (k_lst,tumo_lst,k_count,yukou_hai_lst,2,current_tehai) in
    let res = parallel tmp in
    res
  else
    (*[|operate_tenapai_ritu_f ary zi_ary tehai|]*)
    [|[]|]

let col_tenpai_f ary zi_ary yama_len f_lst zi_kaze ba_kaze naki dora_lst tenpai_lst  = 
  let a_len = Array.length tenpai_lst in
  let rm_wan = yama_len-14 in
  let tumo_l =  rm_wan / 4 in
  let rm_wan = Int.to_float rm_wan in
  if a_len = 1 && List.length tenpai_lst.(0) = 0 || a_len = 0 then
    ([],0.0,0.0,0.0,0,0.0,0.0)
  else
    let tenpai_lst = col_tenpai_parallel_f tenpai_lst tumo_l rm_wan in
    let tenpai_lst = tenpai_to_kitaiti_p ary zi_ary tenpai_lst f_lst zi_kaze ba_kaze naki dora_lst tumo_l rm_wan in
    let last_form_tenpai_lst = opt_tenpai_form_p tenpai_lst in
    if last_form_tenpai_lst = [] then 
      ([],0.0,0.0,0.0,0,0.0,0.0)
    else
      let add_anzendo_lst = List.map (fun (a,b,c,d) -> (a,b,c,d,(anzen ary zi_ary a))) last_form_tenpai_lst in
      let final_form = minus_kitaiti_p add_anzendo_lst in 
      let max = max_agariritu_p final_form in
      max
         

let max_f_agariritu_a lst = 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | ((a,x,y,z),b)::t -> let ((_,x',_,_),_) = tmp in 
                          let tmp = 
                            if x < x' then 
                              tmp
                            else
                              ((a,x,y,z),b) 
                          in
                          loop tmp t 
  in
  loop ((0.0,0.0,0.0,(1,Not_hai)),(Minko,(0,(0,0,0)))) lst 

let tenpai_hai_lst n_tehai = 
  let rec loop tmp double_lst t_lst = match t_lst with 
    | [] -> tmp 
    | h::t -> if List.exists (fun a -> a = h) double_lst then
                    loop tmp double_lst t
                  else
                    let tehai = d_tehai n_tehai h in 
                    let (_,n) = syanten tehai in
                    let tmp = 
                      if n = 0 then
                        h::tmp 
                      else
                        tmp
                    in
                    loop tmp (h::double_lst) t
  in
  loop [] [] n_tehai




let keiten tehai sutehai_lst p_f_lst yama_len (x,y) yaku_lst ary zi_ary = 
  let t_len = List.length tehai in
  let rm_wan = (yama_len-14) in
  let tumo_l = rm_wan/4 in
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (s,(a,(b,c,d)))::t -> let (_,ya) = hai_to_ary (x,y) in
                            let n_tehai =
                              if s = Minko then
                                let n_tehai = List.filter (fun z -> z <> (x,y)) tehai in
                                let n_tehai = 
                                  if (t_len-2) = List.length n_tehai then
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
                                let ((_,b1),_) = tmp in 
                                let (_,b2) = min_den in 
                                if b1 > b2 then 
                                        (min_den,(s,(a,(b,c,d))))
                                else
                                        tmp
                            in
                            loop tmp t 
  in
  if tumo_l > 6 then
    (((1,Not_hai),100),(Minko,(0,(0,0,0))))
  else
    loop (((1,Not_hai),100),(Minko,(0,(0,0,0))))p_f_lst


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
(*
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
  if tumo_len > 15 then 
    if ((100000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (600. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if ((60000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (800. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if ((10000000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (20000. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if ((10000000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (50000. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else
    if ((10000000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (180000. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false

let threthhold_furo_25 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if ((20000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (1000. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if ((20000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (1000. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if ((20000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (1000. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if ((30000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || (agariritu > 0. && (1000. *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
  else
    if ((10000000.0 *. agariritu *. agariritu *. agariritu) > kitaiti && agariritu < 0.) || agariritu > 0. then 
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
    (*if (48000.0 *. agariritu *. agariritu) > kitaiti then 
      true
    else*)
      false
  else if tumo_len >= 6 then 
    if ((10000000.0 *. agariritu *. agariritu*.agariritu) > kitaiti && agariritu < 0.) || ((0.1 *. agariritu *. agariritu) > kitaiti && agariritu > 0.) then 
      true
    else
      false
  else
    true
*)
(*
let threthhold_furo_10 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 750.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 850.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 1110.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && (0.1 *. agariritu *. agariritu -. 530.) > kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && (1.0 *. agariritu *. agariritu -. 40.) > kitaiti) then 
      true
    else
      false

let threthhold_furo_15 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 500.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 600.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 780.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && (0.1 *. agariritu *. agariritu -. 420.) > kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && (1.0 *. agariritu *. agariritu -. 20.) > kitaiti) then 
      true
    else
      false


let threthhold_furo_20 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 320.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 420.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 630.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && (0.1 *. agariritu *. agariritu -. 250.) > kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && (17. *. agariritu *. agariritu -. 10.) > kitaiti) then 
      true
    else
      false

let threthhold_furo_25 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 220.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 320.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 480.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && (0.1 *. agariritu *. agariritu -. 180.) > kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && (1.2 *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false


let threthhold_furo_30 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 130.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 200.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 360.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && (0.1 *. agariritu *. agariritu -. 90.) > kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && (1.4 *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false

let threthhold_furo_35 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && 0. > kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 100.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && (0. *. agariritu *. agariritu -. 1800.) > kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && (0.1 *. agariritu *. agariritu -. 60.) > kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && (1.5 *. agariritu *. agariritu) > kitaiti) then 
      true
    else
      false
*)

let threthhold_furo_10 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && ((-150.) *. agariritu *. agariritu +. 80.) < kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && ((-150.) *. agariritu *. agariritu +. 94.) < kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && ((-100.) *. agariritu *. agariritu +. 57.) < kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && ((-100.) *. agariritu *. agariritu +. 50.) < kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && ((-100.) *. agariritu *. agariritu +. 17.) < kitaiti) then 
      true
    else
      false

let threthhold_furo_15 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if (agariritu > 0. && ((-140.) *. agariritu *. agariritu +. 14.) < kitaiti) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if (agariritu > 0. && ((-80.) *. agariritu *. agariritu +. 9.) < kitaiti) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if (agariritu > 0. && ((-80.) *. agariritu *. agariritu +. 3.) < kitaiti) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0. && ((-80.) *. agariritu *. agariritu ) < kitaiti) then 
      true
    else
      false
  else
    if (agariritu > 0. && ((-10000.) *. agariritu *. agariritu) < kitaiti) then 
      true
    else
      false


let threthhold_furo_20 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if ((agariritu > 0. && ((-6180.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.)  then 
      true
    else
      false
  else if tumo_len > 12 then 
    if ((agariritu > 0. && ((-6180.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.)  then 
      true
    else
      false
  else if tumo_len > 9 then 
    if ((agariritu > 0. && ((-6880.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.)  then 
      true
    else
      false
  else if tumo_len > 6 then 
    if ((agariritu > 0. && ((-18880.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.)  then 
      true
    else
      false
  else
    if (agariritu > 0.) then 
      true
    else
      false

let threthhold_furo_25 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if ((agariritu > 0. && ((-20500.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if ((agariritu > 0. && ((-13500.) *. agariritu *. agariritu +. 0.) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if ((agariritu > 0. && ((-20000.) *. agariritu *. agariritu +. 40.) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0.) then 
      true
    else
      false
  else
    if (agariritu > 0.) then 
      true
    else
      false


let threthhold_furo_30 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if ((agariritu > 0. && ((-227000.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if ((agariritu > 0. && ((-52000.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if ((agariritu > 0. && ((-112700.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0.) then 
      true
    else
      false
  else
    if (agariritu > 0.) then 
      true
    else
      false

let threthhold_furo_35 agariritu kitaiti tumo_len = 
  if tumo_len > 15 then 
    if ((agariritu > 0. && ((-789000.) *. agariritu *. agariritu ) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 12 then 
    if ((agariritu > 0. && ((-341000.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 9 then 
    if ((agariritu > 0. && ((-8090000.) *. agariritu *. agariritu) < kitaiti) || kitaiti > 0.) then 
      true
    else
      false
  else if tumo_len > 6 then 
    if (agariritu > 0.) then 
      true
    else
      false
  else
    if (agariritu > 0.) then 
      true
    else
      false

(*let furoritu_to_furo furoritu agariritu kitaiti tumo_len = 
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
    false*)

let furoritu_to_furo furoritu agariritu kitaiti tumo_len = 
  if furoritu >= 32.5 then 
    kitaiti > (-1037.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 27.5 then 
    kitaiti > (-129.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0.
  else if furoritu >= 22.5 then 
    kitaiti > (-11.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 17.5 then 
    kitaiti > 0.  && agariritu > 0.
  else if furoritu >= 12.5 then 
    kitaiti > (-114.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu +. 500.  && agariritu > 0.
  else if furoritu >= 7.5 then 
    kitaiti > (-8.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu +. 1000. && agariritu > 0. 
  else
    false

(*15%*)
(*
let furoritu_to_furo furoritu agariritu kitaiti tumo_len = 
  if furoritu >= 32.5 then 
    kitaiti > (-16.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 27.5 then 
    kitaiti > (-0.28)*.(2.**float_of_int(18-tumo_len))*.(sqrt agariritu)  && agariritu > 0. 
  else if furoritu >= 22.5 then 
    kitaiti > (-1.28)*.(2.**float_of_int(18-tumo_len))*.agariritu  && agariritu > 0. 
  else if furoritu >= 17.5 then 
    kitaiti > (-0.24)*.(3.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 12.5 then 
    kitaiti > (-17.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 7.5 then 
    kitaiti > (-1.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else
    false
*)
(*20%*)
(*
let furoritu_to_furo furoritu agariritu kitaiti tumo_len = 
  if furoritu >= 32.5 then 
    kitaiti > (-325.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 27.5 then 
    kitaiti > (-4.35)*.(2.**float_of_int(18-tumo_len))*.(sqrt agariritu)  && agariritu > 0. 
  else if furoritu >= 22.5 then 
    kitaiti > (-18.1)*.(2.**float_of_int(18-tumo_len))*.agariritu  && agariritu > 0. 
  else if furoritu >= 17.5 then 
    kitaiti > (-12.3)*.(3.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 12.5 then 
    kitaiti > (-17.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 7.5 then 
    kitaiti > (-1.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else
    false
*)
(*25%*)
(*
let furoritu_to_furo furoritu agariritu kitaiti tumo_len = 
  if furoritu >= 32.5 then 
    kitaiti > (-3860.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 27.5 then 
    kitaiti > (-38.)*.(2.**float_of_int(18-tumo_len))*.(sqrt agariritu)  && agariritu > 0. 
  else if furoritu >= 22.5 then 
    kitaiti > (-179.)*.(2.**float_of_int(18-tumo_len))*.agariritu  && agariritu > 0. 
  else if furoritu >= 17.5 then 
    kitaiti > (-410.)*.(3.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 12.5 then 
    kitaiti > (-17.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 7.5 then 
    kitaiti > (-1.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else
    false
    *)
(*30%*)
(*
let furoritu_to_furo furoritu agariritu kitaiti tumo_len = 
  if furoritu >= 32.5 then 
    kitaiti > (-5000000.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 27.5 then 
    kitaiti > (-1500.)*.(2.**float_of_int(18-tumo_len))*.(sqrt agariritu)  && agariritu > 0. 
  else if furoritu >= 22.5 then 
    kitaiti > (-15170.)*.(2.**float_of_int(18-tumo_len))*.agariritu  && agariritu > 0. 
  else if furoritu >= 17.5 then 
    kitaiti > (-1250000.)*.(3.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 12.5 then 
    kitaiti > (-17.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else if furoritu >= 7.5 then 
    kitaiti > (-1.)*.(2.**float_of_int(18-tumo_len))*.agariritu *.agariritu  && agariritu > 0. 
  else
    false
*)


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
(*
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
    if Array.length tenpai_lst = 0 || Array.length tenpai_lst = 1 && List.length tenpai_lst.(0) = 0 then 
      []
    else if reach_q = false then
      let not_naki = col_tenpai_f ary zi_ary tehai yama_len (List.nth furo_lst player) zi_kaze ba_kaze naki dora_lst tenpai_lst in
      if f_kitaiti_lst = [] then 
        []
      else
        let ((f_t_ritu,f_agariritu,f_kitaiti,k_hai),f_hai) = max_f_agariritu_a f_kitaiti_lst in  
        let (k_lst,t_ritu,agariritu,kitaiti,anzendo,_,_) = not_naki in
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
 *)  
 
let kuikae_filter kuikae_lst lst = 
  let rec loop tmp t_lst = match t_lst with 
    | [] -> tmp 
    | (dahai,tmp_a,tmp_b,tmp_c)::t -> if List.exists (fun a -> a = dahai) kuikae_lst || dahai = (1,Not_hai) then 
                                        loop tmp t 
                                      else
                                        loop ((dahai,tmp_a,tmp_b,tmp_c)::tmp) t 
  in
  List.rev (loop [] lst) 

let patern_to_kitaiti tehai (x,y) f_lst p_f_lst ary zi_ary dora zi_kaze ba_kaze turn (m_red,p_red,s_red)= 
  let t_len = List.length tehai in
  let rec loop (tmp,t_ritu,agariritu,kitaiti,lst_2) t_lst = match t_lst with 
    | [] -> (tmp,t_ritu,agariritu,kitaiti,lst_2)
    | (s,(a,(b,c,d)))::t -> let (_,ya) = hai_to_ary (x,y) in
              let n_tehai =
                if s = Minko then
                  let n_tehai = List.filter (fun z -> z <> (x,y)) tehai in
                  let n_tehai = 
                    if (t_len-2) = List.length n_tehai then
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
              let tmp_l = client_fun ic_kitaiti oc_kitaiti n_tehai ((s,(a,(b,c,d)))::f_lst) ary zi_ary dora zi_kaze ba_kaze turn (m_red,p_red,s_red) in 
              let kuikae_lst = kuikae (x,y) (s,(a,(b,c,d))) in
              let tmp_l = kuikae_filter kuikae_lst tmp_l in  
              if tmp_l = [] then 
                loop (tmp,t_ritu,agariritu,kitaiti,lst_2) t 
              else 
                let (_,tmp_a,tmp_b,tmp_c) = List.hd tmp_l in 
                if tmp_c > kitaiti then 
                  loop ((s,(a,(b,c,d))),tmp_a,tmp_b,tmp_c,tmp_l) t 
                else
                  loop (tmp,t_ritu,agariritu,kitaiti,lst_2) t 
  in
  let (furo_mentsu,t_ritu,agariritu,kitaiti,mentsu_lst) = loop ((Syuntu,(0,(0,1,2))),0.0,0.0,0.0,[]) p_f_lst in
  let t_ritu = if t_ritu = 1.1 then 1.0 else t_ritu in
  (furo_mentsu,t_ritu,agariritu,kitaiti,mentsu_lst)

let judge_reach_defence_furo tmp_l tehai furiten_lst yaku_lst ary zi_ary = 
  let g_lst = reach_genbutu yaku_lst furiten_lst tehai in
  let k_hai = judge_reach_opt tmp_l ary zi_ary g_lst tehai yaku_lst furiten_lst in 
  if k_hai = -1 then 
    false 
  else
    true



(*data scan*)

let purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst furoritu_lst furiten_lst =
  let (_,n) = syanten tehai in
  let reach_q_1 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_1 yaku_lst) in
  let reach_q_2 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_2 yaku_lst) in
  let reach_q_3 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_3 yaku_lst) in
  let reach_q_4 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_4 yaku_lst) in
  let n' = kokushi_syanten tehai in
  let tn = titoi_syanten tehai in
  let tumo_len = (yama_len-14)/4 in
  if n = 0 || tumo_len = 0 || n = n' then
    []
  else if n > 3 || (n = tn && tn >= 3) || (tn = 1 && (tn - n) >= 1) then 
    []
  else 
    let (x,y) = ary_to_hai (x,y) in
    let (ary,zi_ary) = create_table sutehai_lst tehai in
    let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in
    let (m_red,p_red,s_red) = create_taable_in_red sutehai_lst tehai in
    let p_f_lst = possible_furo_patern tehai (x,y) in
    let furo_q = furo_defence ary zi_ary yaku_lst sutehai_lst furo_lst tehai furiten_lst in
    let (f_hai,f_t_ritu,f_agariritu,f_kitaiti,mentsu_lst) = patern_to_kitaiti tehai (x,y) (tapl_player furo_lst player) p_f_lst ary zi_ary dora_lst zi_kaze ba_kaze tumo_len (m_red,p_red,s_red) in  
    if (reach_q_1 || reach_q_2 || reach_q_3 || reach_q_4 || furo_q <> -1) = false then
      let (dahai,t_ritu,agariritu,kitaiti) = List.hd (client_fun ic_kitaiti oc_kitaiti tehai (tapl_player furo_lst player) ary zi_ary dora_lst zi_kaze ba_kaze (tumo_len-1) (m_red,p_red,s_red)) in 
        let _ = 
          if f_agariritu > 0.0 && f_kitaiti > 0.0 then
            (Printf.printf "%d %d %f %f %f %f %f %f\n"player tumo_len f_agariritu f_kitaiti f_t_ritu (f_agariritu -. agariritu) (f_kitaiti -. kitaiti) (f_t_ritu -. t_ritu);)
          else
            ()
          in
        
        if naki = false then 
          if f_agariritu > 0.0 && f_kitaiti > 0.0 && furoritu_to_furo (List.nth furoritu_lst player) (f_agariritu -. agariritu) (f_kitaiti -. kitaiti) tumo_len then
            [(dahai,f_hai)]
          else
            (*let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in 
            if k_hai = (1,Not_hai) || den > 5 then 
              if furoritu_to_keiten furoritu f_t_ritu tumo_len && (f_t_ritu -. t_ritu) >= 0.0 && k_hai <> (1,Not_hai) then
                [(k_hai,f_hai)]
              else
                []
            else
              [(k_hai,f_hai)]
              *)[]
        else if (f_agariritu -. agariritu) > 0.0 then 
          [(dahai,f_hai)]
        else
          (*let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in 
          if k_hai = (1,Not_hai) || den > 5 then 
            if furoritu_to_keiten furoritu f_t_ritu tumo_len && (f_t_ritu -. t_ritu) >= 0.0 && k_hai <> (1,Not_hai) then
              [(k_hai,f_hai)]
            else
              []
          else
            [(k_hai,f_hai)]
            *)
            []
    else 
      let yaku_lst = if furo_q = -1 then yaku_lst else change_yaku_lst yaku_lst furo_lst in 
      if judge_reach_defence_furo mentsu_lst tehai furiten_lst yaku_lst ary zi_ary then 
        let (dahai,t_ritu,agariritu,kitaiti) = List.hd (client_fun ic_kitaiti oc_kitaiti tehai (tapl_player furo_lst player) ary zi_ary dora_lst zi_kaze ba_kaze (tumo_len-1) (m_red,p_red,s_red)) in 
      let _ = 
        if f_agariritu > 0.0 && f_kitaiti > 0.0 && tumo_len > 6 then
          (let tehai = List.map ( fun a -> change_gragh a) tehai in 
          let _ = if f_agariritu> 1.0 then (print_hai tehai 0; Printf.printf "\n"; print_hai tehai 1; Printf.printf "\n"; print_hai tehai 2; Printf.printf "\n"; print_hai tehai 3; Printf.printf "\n"; print_hai tehai 4; Printf.printf "\n";) else () in 
            Printf.printf "%d %d %f %f %f %f %f %f\n"player tumo_len f_agariritu f_kitaiti f_t_ritu (f_agariritu -. agariritu) (f_kitaiti -. kitaiti) (f_t_ritu -. t_ritu);
)
        else if tumo_len <= 6 then 
          (let tehai = List.map ( fun a -> change_gragh a) tehai in 
          let _ = if f_agariritu> 1.0 then (print_hai tehai 0; Printf.printf "\n"; print_hai tehai 1; Printf.printf "\n"; print_hai tehai 2; Printf.printf "\n"; print_hai tehai 3; Printf.printf "\n"; print_hai tehai 4; Printf.printf "\n";) else () in 
            Printf.printf "%d %d %f %f %f %f %f %f\n"player tumo_len f_agariritu f_kitaiti f_t_ritu (f_agariritu -. agariritu) (f_kitaiti -. kitaiti) (f_t_ritu -. t_ritu);
)
      in
        if naki = false then 
          if f_agariritu > 0.0 && f_kitaiti > 0.0 && furoritu_to_furo (List.nth furoritu_lst player) (f_agariritu -. agariritu) (f_kitaiti -. kitaiti) tumo_len then
            [(dahai,f_hai)]
          else
            []
        else if (f_agariritu -. agariritu) > 0.0 then 
          [(dahai,f_hai)]
        else
          []
      else
        []



(*mjai*)
(*
let purob_furo sutehai_lst tehai furo_lst yaku_lst player yama_len zi_kaze ba_kaze naki dora_lst (x,y) furo_double_lst furoritu furiten_lst =
  let tehai = rhai_to_hai tehai in 
  let (_,n) = syanten tehai in
  let reach_q_1 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_1 yaku_lst) in
  let reach_q_2 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_2 yaku_lst) in
  let reach_q_3 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_3 yaku_lst) in
  let reach_q_4 =  List.exists (fun b -> b = Reach || b = Doublereach) (tapl_player_4 yaku_lst) in
  let n' = kokushi_syanten tehai in
  let tn = titoi_syanten tehai in
  let (_,n''') = common_syanten tehai in
  let tumo_len = (yama_len-14)/4 in
  if n = 0 || tumo_len = 0 || n = n' then
    []
  else if n > 3 || (n <> n''' && n''' > 3)  ||(n = tn && tn >= 3) || (tn = 1 && (tn - n) >= 1) then 
    []
  else 
    let (x,y) = ary_to_hai (x,y) in
    let (ary,zi_ary) = create_table sutehai_lst tehai in
    let (ary,zi_ary) = furo_lst_to_rm_ary furo_lst furo_double_lst ary zi_ary in
    let (m_red,p_red,s_red) = create_taable_in_red sutehai_lst tehai in
    let p_f_lst = possible_furo_patern tehai (x,y) in
    let furo_q = furo_defence ary zi_ary yaku_lst sutehai_lst furo_lst tehai furiten_lst in
    let (f_hai,f_t_ritu,f_agariritu,f_kitaiti,mentsu_lst) = patern_to_kitaiti tehai (x,y) (tapl_player furo_lst player) p_f_lst ary zi_ary dora_lst zi_kaze ba_kaze tumo_len (m_red,p_red,s_red) in  
    if (reach_q_1 || reach_q_2 || reach_q_3 || reach_q_4 || furo_q <> -1) = false then
      let (dahai,t_ritu,agariritu,kitaiti) = List.hd (client_fun ic_kitaiti oc_kitaiti tehai (tapl_player furo_lst player) ary zi_ary dora_lst zi_kaze ba_kaze (tumo_len-1) (m_red,p_red,s_red)) in 
        (*let _ = 
          if f_agariritu > 0.0 && f_kitaiti > 0.0 then
            (Printf.printf "%d %f %f %f %f \n"tumo_len f_agariritu f_kitaiti (f_agariritu -. agariritu) (f_kitaiti -. kitaiti);)
          else
            ()
          in
        *)
        if naki = false then 
          if f_agariritu > 0.0 && f_kitaiti > 50.0 && furoritu_to_furo furoritu (f_agariritu -. agariritu) (f_kitaiti -. kitaiti) tumo_len then
            [(dahai,f_hai)]
          else
            (*let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in 
            if k_hai = (1,Not_hai) || den > 5 then 
              if furoritu_to_keiten furoritu f_t_ritu tumo_len && (f_t_ritu -. t_ritu) >= 0.0 && k_hai <> (1,Not_hai) then
                [(k_hai,f_hai)]
              else
                []
            else
              [(k_hai,f_hai)]
              *)[]
        else if (f_agariritu -. agariritu) > 0.0 then 
          [(dahai,f_hai)]
        else
          (*let ((k_hai,den),f_hai) = keiten tehai sutehai_lst (List.nth furo_lst player) p_f_lst yama_len (x,y) yaku_lst ary zi_ary in 
          if k_hai = (1,Not_hai) || den > 5 then 
            if furoritu_to_keiten furoritu f_t_ritu tumo_len && (f_t_ritu -. t_ritu) >= 0.0 && k_hai <> (1,Not_hai) then
              [(k_hai,f_hai)]
            else
              []
          else
            [(k_hai,f_hai)]
            *)
            []
    else 
      let yaku_lst = if furo_q = -1 then yaku_lst else change_yaku_lst yaku_lst furo_lst in 
      if judge_reach_defence_furo mentsu_lst tehai furiten_lst yaku_lst ary zi_ary then 
        let (dahai,t_ritu,agariritu,kitaiti) = List.hd (client_fun ic_kitaiti oc_kitaiti tehai (tapl_player furo_lst player) ary zi_ary dora_lst zi_kaze ba_kaze (tumo_len-1) (m_red,p_red,s_red)) in 
        if naki = false then 
          if f_agariritu > 0.0 && f_kitaiti > 50.0 && furoritu_to_furo furoritu (f_agariritu -. agariritu) (f_kitaiti -. kitaiti) tumo_len then
            [(dahai,f_hai)]
          else
            []
        else if (f_agariritu -. agariritu) > 0.0 then 
          [(dahai,f_hai)]
        else
          []
      else
        []
*)


(*
let _ = let tehai = [(4,Manzu);(4,Manzu);(6,Manzu);(7,Manzu);(4,Pinzu);(7,Pinzu);(8,Pinzu);(8,Pinzu);(6,Souzu);(7,Souzu);(8,Souzu);(0,Ton);(0,Nan);(0,Tyun)] in 
  let (ary,zi_ary) = create_table ([],[],[],[]) tehai in
(* let tenpai_lst = judge_parallel tehai in *)
  col_tenpai ary zi_ary tehai 80 [] 0 0 false [(0,3)]
*)
