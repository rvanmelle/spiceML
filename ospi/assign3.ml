open Printf
open ExtList
open Util.ListHelpers
module PP = Util.PrettyPrintHelpers

let p_list_of_ints x =
  List.iter (fun r ->
    Format.printf "%a\n" (PP.pp_list PP.pp_int) r) x

(* Performs a box trunction using the box truncation indices defined
   by ks... returns a int list list of the indices *)
let box_truncation ks : int list list =
  let rec box_aux ks last =
    let start cur = if last = 0 then 0 else -cur in
    match ks with
      | kcur :: [] -> (* last index... non-recursive base case *)
	  let rec aux pos =
	    if pos > kcur then [] else [pos] :: (aux (pos + 1)) in
	  aux (start kcur)
      | kcur :: rest ->
	  let result = ref [] in
	  for i = (start kcur) to kcur do
	    let ith = List.map (fun r -> i :: r) (box_aux rest i) in
	    result := List.append ith !result
	  done;
	  !result
      | [] -> failwith "this should not happen"
  in
  List.sort (box_aux ks 0)

(* Performs a diamond trunction with truncation index k and dimension
   dims... returns a int list list of the indices *)
let diamond_truncation k dims : int list list =
  let rec dmd_aux dim last =
    match dim with
      | d when d = dims -> (* last dimension... non-recursive base case *)
	  let start = if last < 0 then 1 else 0 in
	  let rec aux pos =
	    if (pos + (abs last)) > k then [] else [pos] :: (aux (pos + 1)) in
	  aux start
      | d when d < dims ->
	  let result = ref [] in
	  for i = -(k-last) to (k-last) do
	    let ith = List.map (fun r -> i :: r) (dmd_aux (d+1) i) in
	    result := List.append ith !result
	  done;
	  !result
      | _ -> failwith "this is very very bad"
  in
  List.sort (List.map (fun x -> List.rev x) (dmd_aux 1 0))

let freq_of_indices fs ks =
  abs_float (List.fold_left2 (fun acc f k -> acc +. (f *. (float k))) 0. fs ks)
    
(* Performs a frequency mapping for the box-truncation defined by
   the ks.  Returns two association lists which can be used to look stuff
   up in either direction *)
let frequency_mapping ks fs =
  let trunc = box_truncation ks in
  let ks' = List.take ((List.length ks) - 1) ks in
  List.split (List.map (fun t ->
    let p = abs((List.hd t) + List.fold_left2 (fun p k big_k ->
      p * k * (2*big_k + 1)) 1 (List.tl t) ks') in
    (p, (t, (freq_of_indices fs t))), ((freq_of_indices fs t), p)
  ) trunc)
    
let test_dmd () =
  let k = 4 and dims = 2 in
  let r = diamond_truncation k dims in
  p_list_of_ints r

let test_mapping () =
  (* forward: artifical -> (ks, real frequency *)
  (* reverse: real frequency -> artifical *)
  let forward, reverse = frequency_mapping [3;4] [10.1; 9.9] in
  List.iter (fun (p,(ks,f)) ->
    printf "p=%d --> " p;
    list_iteri (fun i k -> printf "k(%d)=%d " (i+1) k) ks;
    printf " f=%g\n%!" f) forward;
  List.iter (fun (f, p) -> printf "f=%g --> %d\n" f p) reverse

let html_of_list titles ls outc =
  fprintf outc "<table>\n";
  fprintf outc "<tr>";
  List.iter (fun h -> fprintf outc "<th>%s</th>" h) titles;
  fprintf outc "</tr>";
  List.iter (fun row ->
    fprintf outc "<tr>";
    List.iter (function
      | `Int x -> fprintf outc "<td>%d</td>" x
      | `Float x -> fprintf outc "<td>%g</td>" x
    ) row;
    fprintf outc "</tr>\n";
  ) ls;
  fprintf outc "</table>\n"

let format_list fs ks =
  let freq_exists l freq =
    List.exists (fun x ->
      match (List.nth x ((List.length x) - 1)) with
	| `Float f -> (Gsl_math.fcmp freq f ~epsilon:1e-12) = 0
	| _ -> failwith "wtf") l
  in
  List.fold_left (fun acc is ->
    let freq = freq_of_indices fs is in
    if freq_exists acc freq then (printf "YEAH\n%!"; acc) else
      (List.append (List.map (fun i -> `Int i) is)
	  [(`Float freq)]) :: acc) [] ks


let question_2 () =
  let forward, reverse = frequency_mapping [2;3] [10.; 1.] in
  let outc = open_out "ospi/assign3/q2_forward.html" in
  fprintf outc "<table>\n";
  fprintf outc "<tr><th>Harmonic</th><th>k(1)</th><th>k(2)</th><th>Freq</th></tr>";
  List.iter (fun (p,(ks,f)) ->
    fprintf outc "<tr><td>%d</td>" p;
    List.iter (fun k -> fprintf outc "<td>%d</td>" k) ks;
    fprintf outc "<td>%g</td></tr>" f) forward;
  fprintf outc "</table>\n";
  let outc = open_out "ospi/assign3/q2_reverse.html" in
  fprintf outc "<table>\n";
  fprintf outc "<tr><th>Freq</th><th>Harmonic</th></tr>";
  List.iter (fun (f, p) ->
    fprintf outc "<tr><td>%g</td><td>%d</td></tr>" f p) reverse;
  fprintf outc "</table>\n"
  
let question_1 () =
  let ks = [3;4;5]
  and fs = [10.; 10.1; 9.9] in
  let r = box_truncation ks in
  html_of_list ["k1"; "k2"; "k3"; "Freq"] (format_list fs r)
    (open_out "ospi/assign3/q1_box.html");

  let d = diamond_truncation 4 3 in
  html_of_list ["k1"; "k2"; "k3"; "Freq"] (format_list fs d)
    (open_out "ospi/assign3/q1_diamond.html")
  
let test_box () =
  let ks = [3;4] in
  let r = box_truncation ks in
  let expected = ((List.fold_left (fun acc i -> acc * (2 * i + 1)) 1 ks) - 1) / 2 in
  (*Format.printf "%a" (PP.pp_list PP.pp_int) r*)
  p_list_of_ints r;
  html_of_list ["k1"; "k2"; "Freq"] (format_list [4.; 5.] r)
    (open_out "test.html");
  printf "\nexpected=%d found=%d\n" expected (List.length r)

let main () =
  question_1 ();
  question_2 ()
	  
