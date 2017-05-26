open System
open FSharp.Data
open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized
open System
open System.Net
open System.Threading


let charListPack (str:char list) =
    let rec charListPack' sourse dest =
        match (sourse, dest) with 
        | (s::ss, (ch, cnt)::ds) -> if s = ch then charListPack' ss ((ch, cnt + 1)::ds)
                                    else charListPack' ss ((s, 1)::(ch, cnt)::ds)
        | ([], ds) -> ds
        | (s::ss, []) -> charListPack' ss [(s, 1)]

    // потестить:
    // unique' [1;1;2;3;4;4;5;5;6;7;7;7;8;9;9] []
    // unique [4;1;2;3;4;5;5;6;7;7;8;9;1;9;7]
    match str with
    | [] -> []
    | _ -> List.rev (charListPack' str [])


/////////////////////////////////////////////////////////////////
let t1 = charListPack ['1';'2';'3';'4';'5';'5';'5']
let t2 = charListPack ['1';'1';'1';'2';'3';'4';'5';'5';'5']
let t3 = charListPack ['1';'2';'2';'3';'3';'4';'5';]
let t4 = charListPack ['1';'2';'2';'3';'3';'3';'4';'5';]

let (el:char list) = [] 
let t5 = charListPack el


/// Код с семинара:
let explode (s:string) = [for i in s -> i]
let implode (list : char list) = 
    List.fold(fun acc c -> acc + (string c)) "" list

let stringPack x = charListPack (explode x)
let ts1 = stringPack "abbraccaddabbra"
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

let charListUnpack (a : (char * int) list) =
    let rec clu a buffer = 
        match a with
        | (ch, 1) :: atail -> clu atail (ch::buffer)
        | (ch, cnt) :: atail -> clu ((ch, cnt - 1)::atail) (ch::buffer)
        | _ -> buffer
    List.rev (clu a [])

/////////////////////////////////////////////////////////////////

let t6 = charListUnpack t1
let t7 = charListUnpack t2
let t8 = charListUnpack t3
let t9 = charListUnpack t4
let t10 = charListUnpack t5

let stringUnpack x = implode (charListUnpack x)
let ts2 = implode (charListUnpack ts1)

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


let dropChars = [' '; '.'; ','; '?'; '!'] // etc....
let dropSpaceAndCommas (list: char list) =
    let rec drp (list: char list) buf =  
        let bad c =
            let rec bad' = function
            | (v :: tail) when (c = v) -> true
            | (v :: tail) -> bad' tail
            | _ -> false
            bad' dropChars

        match list with
        | (c :: tail) when (bad c) ->  drp tail buf
        | (c :: tail) ->  drp tail (c::buf)
        | _ -> buf
    List.rev (drp list []) 
      
let t11 = dropSpaceAndCommas (explode "Hello, world!")

/////////////////////////////////////////////////////////////////
let bad c =
    let rec bad' = function
    | (v :: tail) when (c = v) -> true
    | (v :: tail) -> bad' tail
    | _ -> false
    bad' dropChars
let good c = (bad c) <> false
let dropSpaceAndCommas2 (list: char list) =
    List.filter (fun x -> (bad x)) list

let t11' = dropSpaceAndCommas (explode "Hello, world!")
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////


type Compressed = Str of string | ID of int

let histogram list = 
    list
    |> Seq.groupBy (fun c -> c)
    |> Map.ofSeq
    |> Map.map (fun k v -> Seq.length v)

let v = histogram ["aa"; "aa"; "bb";"cc"; "bb"];;

let compress (l:string list) =
    let rec dictBuild (l:string list) (i:int) (mp:Map<string, int>) =
        match l with
        | (l :: ls) -> dictBuild ls (i + 1) (Map.add l i mp)
        | _ -> mp
    let dict = dictBuild l 0 (Map.ofList [])
    let magic_cost = 2
    let hist = histogram l
    let rec cc' (l:string list) (buf:Compressed list) =
        match l with
        | (v :: ls) when ((Map.find v hist) < magic_cost) -> cc' ls (Str(v)::buf)
        | (v :: ls) -> cc' ls (ID(Map.find v dict)::buf)
        | _ -> buf
    (List.rev (cc' l []), dict)

//////////////////////////////////////////
let tC1_l, tC1_d = compress ["aa"; "aa"; "bb";"cc"; "bb"]
let tC2_l, tC2_d = compress ["aa"; "aa"; "aa"; "bb";"cc"; "bb"; "fsharp"]

//////////////////////////////////////////
//////////////////////////////////////////

let mapReverse (mp:Map<string, int>) =
    let arr = Map.toList mp
    let rec build arr (buf:Map<int, string>) =
        match arr with
        | (k, v) :: tail -> build tail (Map.add v k buf)
        | _ -> buf
    build (Map.toList mp) (Map.ofList [])

let unCompress (list:Compressed list) dict =
    let rdict = mapReverse dict
    let rec uc (list:Compressed list) (buf:string list)=
        match list with
        | ID(id)::tail -> uc tail ((Map.find id rdict)::buf)
        | Str(s)::tail -> uc tail (s::buf)
        | _ -> buf
    List.rev (uc list [])

let tC1_s = unCompress tC1_l tC1_d
let tC2_s = unCompress tC2_l tC2_d

//////////////////////////////////////////
//////////////////////////////////////////

open System.Threading

let compress_async task =
    let async_compress x = async {
        return compress x
    }
    task
        |> List.map async_compress
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.toList


//////////////////////////////////////////
let task = [tC1_s; tC2_s; ["abac"; "helllo"; "mipt"; "abac"]]
let solution = compress_async task;;
//////////////////////////////////////////
//////////////////////////////////////////
let path = "fsharp.txt"

let dropSymbols = ['.'; ','; '?'; '!'] // etc....
let dropCommas (list: char list) =
    let rec drp (list: char list) buf =  
        let bad c =
            let rec bad' = function
            | (v :: tail) when (c = v) -> true
            | (v :: tail) -> bad' tail
            | _ -> false
            bad' dropSymbols

        match list with
        | (c :: tail) when (bad c) ->  drp tail buf
        | (c :: tail) ->  drp tail (c::buf)
        | _ -> buf
    List.rev (drp list []) 

let packFile path = 
    let lines = (File.ReadLines(path))
                    |> Seq.toList

        
    let dropString s =
        let rec drp (l:char list) (buf:char list list) =
            match l with 
            | ' '::ls -> let h :: t = buf 
                         drp ls ([] :: (List.rev h) :: t)
            | c::ls -> let h :: t = buf 
                       drp ls ((c::h) :: t)
            | _ -> let h :: t = buf
                   ((List.rev h) :: t)
        List.rev (drp s [[]])

    let lines_dropped = List.collect (fun x -> ' '::(explode x)) lines

    let dropped = dropString lines_dropped

    let craft (dropped:char list list) =
        List.map (fun x -> implode x) (List.filter (fun x -> x.IsEmpty <> true) dropped)

    let craft_lines = craft dropped

    let result = compress craft_lines
    result

//packFile path;;



let compress_async_file task =
    let async_packFile path = async {
        return packFile path
    }
    task
        |> List.map async_packFile
        |> Async.Parallel
        |> Async.RunSynchronously
        |> Array.toList

let task_file = ["fsharp.txt"; "fsharp2.txt"]
let result = compress_async_file task_file