// Дополнительные сведения о F# см. на http://fsharp.org
// Дополнительную справку см. в проекте "Учебник по F#".

open System
open System.IO

let pairSimilar = 
    let rec pair' acc last count=
        function
        | x::xs when x = last -> pair' acc last (count + 1) xs
        | x::xs -> pair' ((last, count)::acc) x 1 xs
        | [] -> (last, count)::acc
    function 
    | x::xs -> List.rev (pair' [] x 1 xs)
    | [] -> []

let rec unpairSimilar =
    let rec multChar acc =
        function 
        | (a, b) when b <= 0 -> acc
        | (a, b) -> multChar (a::acc) (a, (b-1))
    function 
    | x::xs -> (multChar [] x)@(unpairSimilar xs)
    | [] -> []

let readWords file = 
    File.ReadLines file |> Seq.collect(fun x -> x.Split(',', '!', '?', '-', ' ') ) |> Seq.filter(fun x -> x.Length > 0)

let getDict words =
    words |> Seq.groupBy(fun x -> x) |> Seq.map(fun (a, b) -> a, (Seq.length b)) |> Seq.toList

type ComprWord =
    | Num of int
    | Word of string

let numWord dict word =
    let rec numWord' acc i =
        match acc with
        | (w, x)::xs when w = word -> i
        | x::xs -> numWord' xs (i + 1) 
        | [] -> -1
    numWord' dict 0

let compressWords words =
    let dict = getDict words
    let changeWord word = 
        match (dict |> Seq.filter(fun (x, y) -> x = word) |> Seq.head) with
        | (a, b) when b > 2 -> ComprWord.Num(numWord dict a)
        | (a, b) -> ComprWord.Word(a) 
    words |> Seq.map(fun x -> (changeWord x))

let getWord dict w = 
    let rec numWord' acc num i =
        match acc with
        | (w, x)::xs when i = num -> w
        | x::xs -> numWord' xs num (i + 1)
        | [] -> ""
    match w with
    | ComprWord.Word(s) -> s
    | ComprWord.Num(n) -> numWord' dict n 0

let decompr dict words = 
    words |> Seq.map(fun x -> getWord dict x)

let main argv = 
    printfn "%A" argv
    0 // возвращение целочисленного кода выхода

unpairSimilar (pairSimilar ['a';'a';'b';'b'; 'a'])

let words = (readWords @"D:\repos\func\funczach\funczach\test.txt") |> Seq.toList
decompr (getDict (words |> List.toSeq)) (compressWords (words |> List.toSeq))