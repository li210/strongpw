type comp =  
  | Lowercase of string  
  | Uppercase of string 
  | Digit of int 
  | Special of char  
  | DeletorL of comp
  | DeletorR of comp
  | Deletor of comp
  | Empty
  | CutL
  | CutR
  | Iterator
  | Password of comp list
  | Pos
  | Input
  | Switcher

type sorted_pwd = {
  lowercase: string list;
  uppercase: string list;
  digits: int list;
  special: char list;
}
let append_after_position lst pos elements_to_append =
  let rec aux i acc = function
    | [] -> List.rev acc
    | x :: xs as l ->
      if i = pos then
        List.rev_append acc (x :: elements_to_append @ xs)
      else
        aux (i + 1) (x :: acc) xs
  in
  aux 0 [] lst
let print_comp a = 
  match a with 
  | Lowercase s -> Printf.printf "Lowercase %s\n" s 
  | Uppercase s -> Printf.printf "Uppercase %s\n" s 
  | Digit s -> Printf.printf "Digit %d\n" s 
  | Special s -> Printf.printf "Special %c\n" s 
  | _ -> Printf.printf "other\n"

let references = ref []
let active_pw = ref []

let rec flatten_passwords comps acc =
  match comps with
  | [] -> List.rev acc
  | Password pw :: rest -> flatten_passwords rest (List.rev_append (flatten_passwords pw []) acc)
  | comp :: rest -> flatten_passwords rest (comp :: acc)

 
let flatten_passwords_in_list comps =
  flatten_passwords comps []
let sort_pwd x = 
  let a = ref {lowercase=[]; uppercase=[]; digits=[]; special=[];} in  
  List.iter (fun e -> 
    match e with 
    | Lowercase s -> a := {!a with lowercase = !a.lowercase @ [s]} 
    | Uppercase s -> a := {!a with uppercase = !a.uppercase @ [s]} 
    | Digit s -> a := {!a with digits = !a.digits @ [s]} 
    | Special s -> a := {!a with special = !a.special @ [s]} 
    | _ -> ()
  ) x; 
  !a
let exnr = Failure "emptydel"

 let input_read () = print_string "Enter input: "; flush stdout;
  try
    let line = input_line stdin in
    match line with
    | "" -> raise End_of_file
    | _ -> line
  with
  | End_of_file -> raise exnr

let switch g =  List.mapi (fun i x -> let m = List.find_index (fun y -> match y with |Switcher -> true | _ -> false) g in match m with |None -> x | Some f -> if i = f - 1 then (List.nth g (f+1)) else if i = f + 1 then (List.nth g (f-1)) else if i=f then Empty else x) g

let exp_deletor   b = let v =List.mapi (fun i x ->
  match x with 
  | DeletorL s -> s 
  | DeletorR s -> s 
  | Deletor s -> s 
   |   s-> s) b 
in List.filter (fun x -> match x with | Empty   -> false | _ -> true) v 
let delete a =   let pow2 = List.filteri (fun i x -> 
  match x with 
  | Empty -> false
  | _ -> 
    let d = 
      let e = 
        match List.find_index (fun v -> match v with | CutL -> true | _ -> false) a with 
        | None -> true 
        | Some qa -> if i > qa then true else false 
      in 
      if e then 
        let f = 
          match List.find_index (fun v -> match v with | CutR -> true | _ -> false) a with 
          | None -> true 
          | Some qa -> if i < qa then true else false 
        in 
        if f then 
          let g = 
            let gg = 
              try List.nth a (i + 1) with _ -> Digit 0 
            in 
            match gg with 
            | DeletorL _ -> false 
            | Deletor _ -> false 
            | _ -> 
              let h = try List.nth a (i - 1) with _ -> Digit 0 in 
              match h with 
              | DeletorR _ -> false 
              | Deletor _ -> false 
              | _ -> true 
          in 
          g 
        else false 
      else false 
    in 
    d
) a in 

let pow3 = List.mapi (fun i x -> 
  match x with 
  | Deletor c -> c   
  | DeletorR c -> c 
  | DeletorL c -> c
  | _ -> x
) pow2 in let pow4 = flatten_passwords_in_list pow3 in exp_deletor pow4

let pw_string p = 
  let str = ref "" in 
  List.iter (fun x -> 
    match x with 
    | Digit s -> str := !str ^ Int.to_string s 
    | Uppercase s -> str := !str ^ s 
    | Lowercase s -> str := !str ^ s 
    | Special s -> str := !str ^ Char.escaped s
    | _ -> ()
  ) p; 
  !str

let rec char_operation c lg i = let ls = delete lg in
  let va : comp = 
    match c with 
    | '=' -> 
      let prms = List.filteri (fun j _ -> Int.abs(i - j) = 1) ls in 
      if List.length prms < 2 then 
        Uppercase "EQUAL" 
      else 
        let eq = 
          match  List.nth prms 0  with 
          | Digit s -> 
            (match  List.nth prms 1  with 
            | Digit t -> if s = t then 1 else 0 
            | Uppercase t -> let u = Bytes.get_uint16_ne (String.to_bytes t) 0 in if s = u then 1 else 0  
            | Lowercase t -> let u = Bytes.get_uint16_ne (String.to_bytes t) 0 in if s = u then 1 else 0  
            | Special t -> let u = Char.code t in if s = u then 1 else 0)
          | Uppercase s -> 
            (match List.nth prms 1  with 
            | Digit t -> let u = Int.to_string t in if String.compare s u = 0 then 1 else 0  
            | Lowercase t -> if String.compare s t = 0 then 1 else 0  
            | Uppercase t -> if String.compare s t = 0 then 1 else 0
            | Special t -> let u = Char.escaped t in if String.compare s u = 0 then 1 else 0)
          | Special s -> 
            (match List.nth prms 1  with 
            | Digit t -> let u = Char.chr t in if s = u then 1 else 0  
            | Lowercase t -> let u = String.get t 0 in if s = u then 1 else 0
            | Uppercase t -> let u = String.get t 0 in if s = u then 1 else 0  
            | Special t -> if s = t then 1 else 0)
          | Lowercase s -> 
            (match List.nth prms 1  with 
            | Digit t -> let u = Int.to_string t in if String.compare s u = 0 then 1 else 0  
            | Lowercase t -> if String.compare s t = 0 then 1 else 0  
            | Uppercase t -> if String.compare s t = 0 then 1 else 0
            | Special t -> let u = Char.escaped t in if String.compare s u = 0 then 1 else 0)
        in 
        Deletor (Digit eq)
    | '>' -> 
      let prms = List.filteri (fun j _ -> Int.abs(i - j) = 1) ls in 
      if List.length prms < 2 then 
        Uppercase "GREATER" 
      else 
        let eq = 
          match List.nth prms 0 with 
          | Digit s -> 
            (match List.nth prms 1  with 
            | Digit t -> if s > t then 1 else 0 
            | Lowercase t -> let u = Bytes.get_uint16_ne (String.to_bytes t) 0 in if s > u then 1 else 0  
            | Uppercase t -> let u = Bytes.get_uint16_ne (String.to_bytes t) 0 in if s > u then 1 else 0  
            | Special t -> let u = Char.code t in if s > u then 1 else 0)
          | Uppercase s -> 
            (match List.nth prms 1  with 
            | Digit t -> let u = Int.to_string t in if String.compare s u = 1 then 1 else 0  
            | Lowercase t -> if String.compare s t = 1 then 1 else 0  
            | Uppercase t -> if String.compare s t = 1 then 1 else 0
            | Special t -> let u = Char.escaped t in if String.compare s u = 1 then 1 else 0)
          | Special s -> 
            (match List.nth prms 1  with 
            | Digit t -> let u = Char.chr t in if s > u then 1 else 0  
            | Lowercase t -> let u = String.get t 0 in if s > u then 1 else 0
            | Uppercase t -> let u = String.get t 0 in if s > u then 1 else 0  
            | Special t -> if s = t then 1 else 0)
          | Lowercase s -> 
            (match   List.nth prms 1  with 
            | Digit t -> let u = Int.to_string t in if String.compare s u > 0 then 1 else 0  
            | Lowercase t -> if String.compare s t = 1 then 1 else 0  
            | Uppercase t -> if String.compare s t = 1 then 1 else 0
            | Special t -> let u = Char.escaped t in if String.compare s u = 1 then 1 else 0)
          
        in 
        Deletor (Digit eq)
    | '<' -> 
      let prms = List.filteri (fun j _ -> Int.abs(i - j) = 1) ls in 
      if List.length prms < 2 then 
        Uppercase "LOWER" 
      else 
        let eq = 
          match List.nth prms 0 with 
          | Digit s -> 
            (match List.nth prms 1 with 
            | Digit t -> if s < t then 1 else 0 
            | Lowercase t -> let u = Bytes.get_uint16_ne (String.to_bytes t) 0 in if s < u then 1 else 0  
            | Uppercase t -> let u = Bytes.get_uint16_ne (String.to_bytes t) 0 in if s < u then 1 else 0  
            | Special t -> let u = Char.code t in if s < u then 1 else 0)
          | Uppercase s -> 
            (match List.nth prms 1 with 
            | Digit t -> let u = Int.to_string t in if String.compare s u = -1 then 1 else 0  
            | Lowercase t -> if String.compare s t = -1 then 1 else 0  
            | Uppercase t -> if String.compare s t = -1 then 1 else 0
            | Special t -> let u = Char.escaped t in if String.compare s u = -1 then 1 else 0)
          | Special s -> 
            (match List.nth prms 1 with 
            | Digit t -> let u = Char.chr t in if s < u then 1 else 0  
            | Lowercase t -> let u = String.get t 0 in if s < u then 1 else 0
            | Uppercase t -> let u = String.get t 0 in if s < u then 1 else 0  
            | Special t -> if s < t then 1 else 0)
          | Lowercase s -> 
            (match List.nth prms 1 with 
            | Digit t -> let u = Int.to_string t in if String.compare s u = -1 then 1 else 0  
            | Lowercase t -> if String.compare s t = -1 then 1 else 0  
            | Uppercase t -> if String.compare s t = -1 then 1 else 0
            | Special t -> let u = Char.escaped t in if String.compare s u = -1 then 1 else 0)
          
        in 
        Deletor (Digit eq)
    | '?' -> 
      let prms = List.filteri (fun j _ -> Int.abs(i - j) = 1) ls in 
      if List.length prms < 2 then 
        Uppercase "DECLARATION" 
      else if List.exists (fun a -> fst a = (List.nth prms 0)) !references then
        (references := ((List.nth prms 0), (List.nth prms 1)) :: !references; 
        Deletor Empty) else begin references := (((List.nth prms 0), (List.nth prms 1)):: (List.filter (fun a -> fst a <> List.nth prms 0) !references)   );   Deletor (Empty) end
    | '!' -> begin
      let prms = List.filteri (fun j _ -> i - j = 1) ls in 
      if List.length prms < 1 then 
        Digit 0 
      else 
        let eq = List.filter (fun a -> fst a = List.nth prms 0) !references in  
        try 
          DeletorL (snd (List.nth eq 0)) 
        with _ -> 
          DeletorL (Uppercase "REFERENCE") end
    | '[' -> CutL
    | ']' -> CutR  | '^' -> Pos
    | '#' -> begin let prm1 = List.filteri (fun j _ -> i-j = 1) ls in let prm2 = List.filteri (fun y _ -> y > i) ls in  Printf.printf "declarin %s   \n" ((pw_string   prm1  )^(pw_string   prm2  )) ;     if List.length prm1 <> 1 then 
      Uppercase "DECLARATION"   else begin if (List.exists (fun a -> fst a = (List.nth prm1 0)) !references) then  begin references:=((List.nth  prm1 0,Password(prm2)):: !references  );   DeletorL (Empty )   end else   begin references := (((List.nth prm1 0), Password(prm2)):: (List.filter (fun a -> fst a <> List.nth prm1 0) !references)   ); DeletorL (Empty) end  end
  
  end
  | '$' -> Input
  |'%' -> let prms = List.filteri (fun j _ -> Int.abs(i - j) = 1) ls in 
    if List.length prms < 2 then Uppercase("SWITCH") else Switcher
    | _ -> Uppercase "UNKNOWN" 
  in 
  va


 

let invalid_password = Failure "Invalid password"

let switch_case s =
  let switch c =
    if Char.uppercase_ascii c = c then
      Char.lowercase_ascii c
    else
      Char.uppercase_ascii c
  in
  String.map switch s
let check_password q = 
  let p = sort_pwd q in
  let check = ref true in
  if List.length p.digits = 0 then check := false;
  if List.length p.lowercase = 0 then check := false;
  if List.length p.uppercase = 0 then check := false;
  if List.length p.special = 0 then check := false;
  if String.length (pw_string q) < 8 then check := false; 
  !check

let check_pw p = 
  if check_password p then p else raise invalid_password

let merge p =
  let rec merge_aux acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x :: acc)
    | x :: (y :: _ as rest) ->
      match x, y with
      | Lowercase s1, Lowercase s2 -> merge_aux acc (Lowercase (s1 ^ s2) :: List.tl rest)
      | Uppercase s1, Uppercase s2 -> merge_aux acc (Uppercase (s1 ^ s2) :: List.tl rest)
      | Digit d1, Digit d2 -> 
        let str = string_of_int d1 ^ string_of_int d2 in
        merge_aux acc (Digit (int_of_string str) :: List.tl rest)
      | _ -> merge_aux (x :: acc) rest
  in
  merge_aux [] p

let remove_element p x = 
  List.filteri (fun i y -> 
    match List.find_index (fun z -> z = x) p with 
    | None -> true 
    | Some w -> if i = w then false else true
  ) p 

let unsort p s = 
  List.filter (fun x -> 
    match x with 
    | Lowercase t -> 
      if List.exists (fun y -> y = t) !s.lowercase then 
        (s := {!s with lowercase = remove_element !s.lowercase t}; true) 
      else false 
    | Uppercase t -> 
      if List.exists (fun y -> y = t) !s.uppercase then 
        (s := {!s with uppercase = remove_element !s.uppercase t}; true) 
      else false 
    | Digit t -> 
      if List.exists (fun y -> y = t) !s.digits then 
        (s := {!s with digits = remove_element !s.digits t}; true) 
      else false 
    | Special t -> 
      if List.exists (fun y -> y = t) !s.special then 
        (s := {!s with special = remove_element !s.special t}; true) 
      else false  
    | _ -> true
  ) p   

let append_rest z b l=  let a = exp_deletor z in

  let er = ref [] in (
      List.iter (fun x -> er:= !er @ [Lowercase(x)]) !b.lowercase; List.iter (fun x -> er:= !er @ [Uppercase(x)]) !b.uppercase; List.iter (fun x -> er:= !er @ [Digit(x)]) !b.digits; List.iter (fun x -> er:= !er @ [Special(x)]) !b.special) ;  
  
  let osk = ref [] in let rec ooo nn= try begin
 match (List.find (fun x -> List.exists (fun h ->h=x) !er ) nn ) with 
 Lowercase _ ->  
 
 begin ( 
      if l then
        osk := !osk @ [Lowercase(List.fold_left (fun g h ->    (String.cat g h)
           
        ) (  "") !b.lowercase)]
      else
        osk := !osk @ [Uppercase(switch_case(List.fold_left (fun g h ->    (String.cat g h)
           
        ) (  "") !b.lowercase))]  )  ;
        ooo (List.filter (fun x -> match x with Lowercase _ -> false |_ ->true ) nn);    ()
    end 
    | Uppercase _ -> 
    begin (
      if l then
        osk := !osk @ [Uppercase(List.fold_left (fun g h ->    (String.cat g h)
           
        ) (  "") !b.uppercase)]
      else
        osk := !osk @ [Lowercase(switch_case(List.fold_left (fun g h ->    (String.cat g h)
           
        ) (  "") !b.uppercase))]);
        ooo (List.filter (fun x -> match x with Uppercase _ -> false |_ ->true ) nn); ()
    end
 
    | Digit _ -> 
    begin 
      osk := !osk @ [Digit (List.fold_left (+) 0 !b.digits)];
      ooo (List.filter (fun x -> match x with Digit _ -> false |_ ->true ) nn); ()
    end
    | Special _ ->
    begin List.iter (fun x -> osk := !osk @ [Special x]) !b.special;
        ooo (List.filter (fun x -> match x with Special _ -> false |_ ->true ) nn); ()
    end
  | _ -> () end with _ ->  ()  in ooo a;
let ko =
 
  flatten_passwords_in_list  !osk in    ko



let negative_substring a = Failure a
let stringsub a b =  

  let aseq = ref (List.of_seq (String.to_seq a )) in
  let bseq =    (List.of_seq (String.to_seq b)) in let b2 = 
 List.mapi (fun i x -> match (List.find_index (fun y -> y = x) !aseq)
 with |None -> x |Some j -> aseq := ( List.filteri (fun g y -> g <> j) !aseq); '*' 
 )  bseq
in let b3 = List.filter (fun x -> x <> '*') b2 in (String.of_seq (List.to_seq !aseq)),  (String.of_seq (List.to_seq b3))

let add_passwords primary secondary = 
  let sorted_secondary = ref (sort_pwd secondary) in
  let updated_primary = List.mapi (fun i comp ->
    match comp with
    | Digit value ->
      begin
        try
          let head_digit = List.hd (!sorted_secondary.digits) in
          let new_value = value + head_digit in try
          sorted_secondary := {!sorted_secondary with digits = List.tl !sorted_secondary.digits};
          Digit(new_value) with _ -> sorted_secondary := {!sorted_secondary with digits =[]}; Digit(new_value)
        with _ -> Digit(value)
      end
    | Uppercase str ->
      begin
        try
          let head_uppercase = List.hd (!sorted_secondary.uppercase) in
          let result = String.cat str head_uppercase in try  
          sorted_secondary := {!sorted_secondary with uppercase = List.tl !sorted_secondary.uppercase} ;  Uppercase(  result) with _ -> sorted_secondary := {!sorted_secondary with uppercase =[]};
            Uppercase(  result)
        with _ -> Uppercase(str)
      end
    | Lowercase str ->
      begin
        try
          let head_lowercase = List.hd (!sorted_secondary.lowercase) in
          let result = String.cat str head_lowercase in try
            sorted_secondary := {!sorted_secondary with lowercase = List.tl !sorted_secondary.lowercase};
          Lowercase(  result) with _ -> sorted_secondary := {!sorted_secondary with lowercase =[]}; Lowercase(  result)
        with _ -> Lowercase(str)
      end
    | Special str -> Special(str)
    | _ -> comp
  ) primary in

  let pow = List.mapi (fun i comp ->
    match comp with
    | Special str -> char_operation str updated_primary i
    | _ -> comp
  ) updated_primary in let swi = switch pow in
 

    let pow3 = delete swi in let pos =  List.find_index (fun x -> x = Pos) swi in let append a b  = match pos with | None -> a@b | Some x -> append_after_position a x b in let pwnew = append pow3 (append_rest secondary sorted_secondary true)

 
  in 
  let pp4 = List.filter(fun x -> match x with |Pos ->false | _ ->true) pwnew in let ppi = 
  if List.exists (fun x -> match x with Input ->true | _ -> false) pp4 then
  let separate_string s =
    let rec aux i (comps) =
      if i < 0 then (comps)
      else
        let c = s.[i] in
        if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' then
          aux (i - 1) (Lowercase(Char.escaped c ):: comps)
        else if Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' then
          aux (i - 1) ( Uppercase(Char.escaped c ) :: comps)
        else if Char.code c >= Char.code '0' && Char.code c <= Char.code '9' then
          aux (i - 1) ( Digit(int_of_char c ):: comps)
        else
          aux (i - 1) ( Special(c) ::comps)
    in
    aux (String.length s - 1) ( [])
  in  let   input = input_read () in
    let (comps) = separate_string input in let pp5 =
     
  List.map (fun x -> match x with Input -> Password  (comps) | _ -> x) pp4 in pp5 else pp4  
  in let ppp = flatten_passwords_in_list ppi in let ppo = merge ppp in
  if check_password ppo then ppo else raise (Failure "Password does not meet requirements") 

let get_active_pw () a = if List.is_empty !active_pw then a else !active_pw
let sub_passwords primary secondary =
  let sorted_secondary = ref (sort_pwd secondary) in
  let updated_primary = List.mapi (fun i comp ->
    match comp with
    | Digit value ->
      begin
        try
          let head_digit = List.hd (!sorted_secondary.digits) in
          let new_value = value - head_digit in
          sorted_secondary := {!sorted_secondary with digits = List.tl !sorted_secondary.digits};
          Digit(new_value)
        with _ -> Digit(value)
      end
    | Uppercase str ->
      begin
        try
          let head_uppercase = List.hd (!sorted_secondary.uppercase) in
          let result = stringsub str head_uppercase in
          match snd result with
          | "" -> sorted_secondary := {!sorted_secondary with uppercase = List.tl !sorted_secondary.uppercase}; Uppercase(fst result)
          | remaining -> sorted_secondary := {!sorted_secondary with uppercase = (List.tl !sorted_secondary.uppercase @ [remaining] )}; Uppercase(fst result)
        with _ -> Uppercase(str)
      end
    | Lowercase str ->
      begin
        try
          let head_lowercase = List.hd (!sorted_secondary.lowercase) in
          let result = stringsub str head_lowercase in
          match snd result with
          | "" -> sorted_secondary := {!sorted_secondary with lowercase = List.tl !sorted_secondary.lowercase}; Uppercase(fst result)
          | remaining -> sorted_secondary := {!sorted_secondary with lowercase = (List.tl !sorted_secondary.lowercase @ [remaining] )}; Uppercase(fst result)
        with _ -> Lowercase(str)
      end
    | Special str -> Special(str)
    | _ -> comp
  ) primary in

  let processed_password = List.mapi (fun i comp ->
    match comp with
    | Special str -> char_operation str updated_primary i
    | _ -> comp
  ) updated_primary in let swi = switch processed_password in



  let pow3 = delete swi in let pos =  List.find_index (fun x -> x = Pos) swi in let append a b  = match pos with | None -> a@b | Some x -> append_after_position a x b in let pwnew = append pow3 (append_rest secondary sorted_secondary true)

 
in 
let pp4 = List.filter(fun x -> match x with |Pos ->false | _ ->true) pwnew in let ppi = 
if List.exists (fun x -> match x with Input ->true | _ -> false) pp4 then
let separate_string s =
  let rec aux i (comps) =
    if i < 0 then (comps)
    else
      let c = s.[i] in
      if Char.code c >= Char.code 'a' && Char.code c <= Char.code 'z' then
        aux (i - 1) (Lowercase(Char.escaped c ):: comps)
      else if Char.code c >= Char.code 'A' && Char.code c <= Char.code 'Z' then
        aux (i - 1) ( Uppercase(Char.escaped c ) :: comps)
      else if Char.code c >= Char.code '0' && Char.code c <= Char.code '9' then
        aux (i - 1) ( Digit(int_of_char c ):: comps)
      else
        aux (i - 1) ( Special(c) ::comps)
  in
  aux (String.length s - 1) ( [])
in  let   input = input_read () in
  let (comps) = separate_string input in let pp5 =
   
List.map (fun x -> match x with Input -> Password  (comps) | _ -> x) pp4 in pp5 else pp4  
in let ppp = flatten_passwords_in_list ppi in let ppo = merge ppp in
if check_password ppo then ppo else raise (Failure "Password does not meet requirements") 
let decide_iter a =
  match List.hd a with
  | Iterator -> if List.is_empty !active_pw then List.tl a else !active_pw
  | _ -> a

let rec run_sequence a i =
  List.iter (fun x ->
    match fst x with
    | '+' ->
      let k = add_passwords (decide_iter (fst (snd x))) (decide_iter (snd (snd x))) in
      active_pw := k
    | '-' ->
      let k = sub_passwords (decide_iter (fst (snd x))) (decide_iter (snd (snd x))) in
      active_pw := k
    | _ -> ()
  ) a;
  print_string (pw_string !active_pw);
  print_newline();
  match i with
  | None -> run_sequence a None
  | Some i -> if i = 0 then () else run_sequence a (Some (i - 1))