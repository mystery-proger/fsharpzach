open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

(* «¿ƒ¿Õ»≈ 1 *)

let explode (str : string) : (char list) = [for char in str -> char]

let compress string =
  let rec compress' (acc : ((char * int) list) * (char option) * int) (next : char list) : ((char * int) list) =
    let (current, last, freq) = acc
    match next with
    | char :: rest when not (last.IsSome) -> compress' ([], Some(char), 1) rest
    | char :: rest when last.IsSome && char = last.Value -> compress' (current, last, freq + 1) rest
    | char :: rest when last.IsSome -> compress' ((last.Value, freq) :: current, Some(char), 1) rest
    | [] when last.IsSome -> (last.Value, freq) :: current
    | _ -> []
  List.rev (compress' ([], None, 0) (explode string))

compress "abraaacadabra"

(* «¿ƒ¿Õ»≈ 2 *)

let implode (list : char list) = List.fold (+) "" (List.map string list)

let rec repeat char = function
  | empty when empty < 1 -> []
  | freq -> char :: (repeat char (freq - 1))

let decompress list =
  let rec decompress' = function
    | [] -> []
    | (char, freq) :: rest -> (repeat char freq) :: (decompress' rest)
  decompress' list |> List.concat |> implode

decompress (List.zip ['a'; 'b'; 'c'; 'd'] [1; 0; 3; 4])

(* «¿ƒ¿Õ»≈ 3 *)

open System.IO

let ReadLines fn = seq {
  use inp = File.OpenText fn in
    while not(inp.EndOfStream) do
      yield (inp.ReadLine()) }

let commas = explode " `!@#$%^&*()_+!\"π;%:?*()~,./\\[]{}\n\t\f\b\r'"

let words path =
  ReadLines path
  |> Seq.map (fun s -> s.Split(commas |> List.toArray))
  |> Seq.concat  
  |> Seq.filter (fun s -> s.Length > 0)
  |> Seq.filter (fun s -> s.Length > 1 || not  (List.exists ((=) (char s)) commas))
  |> Seq.map (fun s -> s.ToLower())

let hamlet = "C:\Users\EvgenyShlykov\Desktop\Funcpro\Hamlet.txt"

words hamlet

(* «¿ƒ¿Õ»≈ 4 *)

type compressor =
  | Dictionary of int
  | Word of string

let oftens words =
  words
  |> Seq.groupBy (fun s -> s)
  |> Seq.map (fun (s, l) -> (s, Seq.length l))
  |> Seq.filter (fun (s, n) -> n > 100) 
  |> Seq.map (fun (s, freq) -> s)
  |> Seq.toList

hamlet |> words |> oftens

let enumerate (list : string list) = 
  let rec enumerate' i (list : string list) =
    match list with
    | x :: xs -> (i, x) :: enumerate' (i + 1) xs
    | _ -> []
  enumerate' 1 list

enumerate ["aba"; "cdb"; "efg"]

let replaceword (dictionary : (int * string) list) (word : string) =
  match List.tryFind (snd >> ((=) word)) dictionary with
  | None -> Word(word)
  | Some((i, _)) -> Dictionary(i)
  
let comressfile wordseq =
  let dictionary = wordseq |> oftens |> enumerate
  wordseq
  |> Seq.map (fun s -> replaceword dictionary s)

hamlet |> words |> comressfile

(* «¿ƒ¿Õ»≈ 5 *) 

let unreplace (dictionary : (int * string) list) = function
  | Word(word) -> word
  | Dictionary(i) -> snd dictionary.[i]

let decompressfile dictionary = Seq.map (unreplace dictionary)

let dict = hamlet |> words |> oftens |> enumerate
decompressfile dict (hamlet |> words |> comressfile)

(* «¿ƒ¿Õ»≈ 6 *) 

let asyncrun f file = async {
  let! c = f file
  return c }

let asyncfull f list =
  list
  List.map (asyncrun f)
  |> Async.Parallel
  |> Async.RunSynchronously
  |> Array.toList

syncfull comressfle [hamlet; hamlet; hamlet]