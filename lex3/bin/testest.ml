let stringsub () =
  let a = "FFFGUUUA" in
  let b = "UUUGGG" in
  let aseq = ref (List.of_seq (String.to_seq a )) in
  let bseq =    (List.of_seq (String.to_seq b)) in let b2 = 
 List.mapi (fun i x -> match (List.find_index (fun y -> y = x) !aseq)
 with |None -> x |Some j -> aseq := ( List.filteri (fun g y -> g <> j) !aseq); '*' 
 )  bseq
in let b3 = List.filter (fun x -> x <> '*') b2 in (String.of_seq (List.to_seq !aseq)),  (String.of_seq (List.to_seq b3))

let () = 
  let a,b = stringsub () in
  Printf.printf "The first string is %s and the second string is %s\n" a b