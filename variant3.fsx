open System
open System.IO


let readLines (path:string) = seq {
    use sr = new StreamReader (path)
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}  


let testPath = "/run/media/ilya/Data/Projects/MIPT/FSharp/mipt2017/test.txt"
let test2Path = "/run/media/ilya/Data/Projects/MIPT/FSharp/mipt2017/test1.txt"

let maxLength (path : string) = 
    (readLines path) |> Seq.map(fun (x : string) -> x.Length) |> Seq.max


let formatLine (line : string) (needLength : int) = 
    let words = line.Split [|' '|] |> Array.filter (fun (x : string) -> x.Length > 0)
    let wordsCnt = words.Length
    let stringLength = (words |> Array.map(fun x -> x.Length + 1) |> Array.sum) - 1

    if wordsCnt < 2 || stringLength >= needLength then
        if wordsCnt = 0 then  
            ""
        else
            words |> Array.map (fun x -> x + " ") |> String.Concat
    else
        let additionalSpaces = String.init ((needLength - stringLength) / (wordsCnt - 1)) (fun _ -> " ") 
    
        let rec appendString (str : string) = function 
            | (x : string) :: xs when xs.Length > 0 -> x + str :: (appendString str xs) 
            | x -> x
        
        let result = appendString additionalSpaces (words |> Array.toList) |> String.Concat
        result + String.init (needLength % (wordsCnt - 1)) (fun _ -> " ")

formatLine "asd" 25

maxLength testPath


let writeLines (path : string) (lines : string list) = 
    use sw = new StreamWriter (path)
    lines |> List.map (fun x -> sw.WriteLine(x)) |> ignore


let formatFile pathRead pathWrite =
    let lines = readLines pathRead
    let mxLen = maxLength pathRead
    let formatedLines = lines |> Seq.map(fun x -> formatLine x mxLen) |> Seq.toList
    writeLines pathWrite formatedLines

formatFile testPath test2Path


let getPrimaryWords path = 
    let lines = readLines path
    let words = lines 
                    |> Seq.collect( fun (x : string) -> x.Split [|' '|]) 
                    |> Seq.filter (fun x -> x.Length > 0)
                    |> Seq.toList
    let wordsSet = set words


    
    let lowerFirstLetter str =
        let explode (s : string) = [ for i in s -> i]
        let chars = explode str 
        let lower' = function
            | x :: xs ->  (Char.ToLower x) :: xs
            | [] -> []
        chars |> lower' |> String.Concat
        

    set (words
            |> List.filter (fun (x : string) -> Char.IsUpper (x.Chars 0) )
            |> List.filter (fun (x : string) -> not (wordsSet.Contains (lowerFirstLetter x))))

getPrimaryWords testPath


