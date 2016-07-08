let rec build_string f n accu =
  if n <= 0
    then String.concat "" accu
    else build_string f (n-1) (f (n-1) :: accu)
;;

let char n = String.make 1 (Char.chr n);;

let reference n =
  if n = 8 then "\\b"
  else if n = 9 then "\\t"
  else if n = 10 then "\\n"
  else if n = 13 then "\\r"
  else if n = Char.code '\"' then "\\\""
  else if n = Char.code '\\' then "\\\\"
  else if n < 32 || n > 126 then Printf.sprintf "\\%03d" n
  else char n
;;

let raw_string = build_string char 256 [];;
let ref_string = build_string reference 256 [];;

if String.escaped raw_string <> ref_string then failwith "test:String.escaped";;


let check_split ?limit sep s =
  let l = String.split ?limit sep s in
  assert(List.length l > 0);
  assert(String.concat (String.make 1 sep) (String.split sep s) = s);

  let has_no_sep = String.iter (fun c -> assert (c <> sep)) in
  match limit with
  | None -> List.iter has_no_sep l
  | Some limit ->
      assert(List.length l <= limit + 1);
(*      Printf.printf "limit = %i, s = %S, l = %S\n%!" limit s (String.concat "|" l); *)
      List.iteri (fun i chunk -> if i < limit then has_no_sep chunk) l
;;

let () =
  let test s =
    for i = 0 to String.length s do
      check_split '.' (String.sub s 0 i);
      for limit = 0 to 10 do
        check_split ~limit '.' (String.sub s 0 i);
      done
    done
  in
  test ".abc.def..hij.k..";
  test "";
  test ".";
  test "a";
  test "a.b"
;;
