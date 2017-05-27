let explode (s : string) = [for c in s -> c]

let kompress (cs : char list) =
    match cs with
    | (x::xs) ->
        let rec kompress' acc acc' cs =
            match cs with
            | (x::xs) when x = fst(acc') -> kompress' acc (fst(acc'), snd(acc') + 1) xs
            | [] -> acc' :: acc
            | (x::xs) -> kompress' (acc' :: acc) (x, 1) xs
        List.rev( kompress' [] (x, 1) xs)
    | [] -> []

let k = explode "aaaababaaa"
let l = kompress k

let unkompress cs =
    let rec unkompress' acc cs =
        match cs with
        | (x::xs) when snd(x) = 1 -> unkompress' (fst(x) :: acc) xs
        | (x::xs) -> unkompress' (fst(x) :: acc) ((fst(x), snd(x) - 1) :: xs)
        | [] -> acc
    List.rev (unkompress' [] cs)

let m = unkompress l

open System
open System.IO

let readFile (fileName : string) =               
        fileName 
        |> File.ReadLines 
        |> Seq.map (fun (x : string) -> explode (x + " "))
        |> Seq.concat
        |> Seq.toList

let input = readFile "C:\Users\Admin\Documents\s.txt"

let getClear cs =
    let rec getClear' curr_word acc cs =
        match cs with
        | (x::xs) when x = ' ' || x = ',' -> if curr_word <> [] then getClear' [] (List.rev(curr_word) :: acc) xs else getClear' [] acc xs
        | (x::xs) -> getClear' (x :: curr_word) acc xs
        | [] -> match curr_word with
                | (x::xs) -> (List.rev(curr_word) :: acc)
                | [] -> acc
    List.rev(getClear' [] [] cs)

let x = getClear input
let result = 
        x
        |> Seq.ofList
        |> Seq.map(fun x -> new string [|for c in x -> c|])
        |> Seq.toList