open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// почтовый адрес
let email = "nadysha1997@yandex.ru"

let explode (s:string) = [for c in s -> c]

type Token =
    | OpenBracket | CloseBracket  // [, ]
    | OpenBrace | CloseBrace      // {, }
    | Colon                       // :
    | Comma                       // ,
    | String of string
    | Number of int
    | Boolean of bool
    | Null

let tokenize source =

    let rec parseString acc = function
        | '\\' :: 'n' :: x -> parseString (acc + "\n") x
        | '\\' :: '"' :: x -> parseString (acc + "\"") x
        | '"' :: x -> acc, x
        | str :: x -> parseString (acc + str.ToString()) x
        | _ -> failwith "Invalid string"
 
    let rec token acc = function
        | [] -> acc, [] 
        | (t :: _) as x when List.exists ((=)t) [')'; ':'; ','; ']'] -> acc, x
        | w :: x when Char.IsWhiteSpace(w) -> acc, x
        | c :: x -> token (acc + (c.ToString())) x

    let rec tokenize' acc = function
        | w :: x when Char.IsWhiteSpace(w) -> tokenize' acc x
        | '[' :: x -> tokenize' (OpenBracket :: acc) x
        | ']' :: x -> tokenize' (CloseBracket :: acc) x
        | '{' :: x -> tokenize' (OpenBrace :: acc) x
        | '}' :: x -> tokenize' (CloseBrace :: acc) x
        | ':' :: x -> tokenize' (Colon :: acc) x
        | ',' :: x -> tokenize' (Comma :: acc) x
        | '"' :: x -> 
            let s, y = parseString "" x
            tokenize' (String s :: acc) y   
        | 't' :: 'r' :: 'u' :: 'e' :: x -> tokenize' (Boolean true :: acc) x
        | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: x -> tokenize' (Boolean false :: acc) x
        | 'n' :: 'u' :: 'l' :: 'l' :: x -> tokenize' (Null :: acc) x
        | d :: x -> 
            let n, y = token (d.ToString()) x
            tokenize' (Number (try Convert.ToInt32 n with e -> 0)  :: acc) y
        | [] -> List.rev acc
        | _ -> failwith "Tokinzation error"

    tokenize' [] source

type JSON =
    | Object of (string * JSON) list
    | Array of JSON list
    | String of string
    | Boolean of bool
    | Number of int
    | Null

let rec parse json =
    
    let rec parse' json =

        let rec parseObject list = function
            | CloseBrace :: x -> (Object (List.rev list)), x
            | Comma :: x -> parseObject list x
            | Token.String s :: Colon :: x ->
                let a, x = parse' x
                parseObject ((s, a) :: list) x
            | _ -> failwith "Incorrect object"

        let rec parseArray list = function
            | CloseBracket :: x -> (Array (List.rev list)), x
            | Comma :: x -> parseArray list x
            | ob -> 
                let a, x = parse' ob
                parseArray (a :: list) x  

        match json with
            | OpenBracket :: x -> parseArray [] x
            | OpenBrace :: x -> parseObject [] x
            | Token.String s :: x -> JSON.String s, x
            | Token.Boolean s :: x -> JSON.Boolean s, x
            | Token.Number s :: x -> JSON.Number s, x
            | Token.Null :: x -> JSON.Null, x
            | _ -> failwith "Incorrect identification"
  
    match parse' json with
        | res, [] -> res
        | _ -> failwith "Wrong JSON structure"


//variant 17
let lab3 x =  
    let rec lab3' level x = 
        match x with
          | Object list -> List.fold (fun levels x -> levels @ (lab3'(level + 1) (snd x))) [] list
          | Array list -> List.fold (fun levels x -> levels @ (lab3' level x)) [] list
          | Number int -> [level]
          | _ -> []

    let list = lab3' 0 x
    List.length (List.distinct list) <= 1


let rec stringify = function
    | Object list -> "{" + (String.concat ", " (List.map (fun (a, b) -> "\"" + a + "\": " + (stringify b)) list)) + "}"
    | Array list -> "[" + (String.concat ", " (List.map stringify list)) + "]"
    | String string -> "\"" + string + "\""
    | Boolean bool -> if bool then "true" else "false"
    | Number int -> string int
    | Null -> "null"


let generate () = 

    let rnd = new Random()

    let randomString length = 
        let chars = Array.append [|'A'..'Z'|] [|'0'..'9'|]
        let randomChars = [|for i in 1..rnd.Next(length) -> chars.[rnd.Next(chars.Length)]|]
        System.String(randomChars)

    let rec generate' n =
        match rnd.Next(n) with
            | 0 -> Array[for i in 1..rnd.Next(n) -> generate' (i - 1)]
            | 1 -> String(randomString (rnd.Next(n)))
            | 2 -> Boolean(false)
            | 3 -> Boolean(true)
            | 4 -> Number(rnd.Next(n))
            | 5 -> Null
            | _ -> Object[for i in 1..rnd.Next(n) -> (randomString(rnd.Next(n)), generate' (i - 1))]

    generate' 42


// Tests
let tree0 = generate()
lab3 tree0
stringify tree0

let string1 = """
{
    "a": 1
    "b": {
        "c": [1,2,3]
    }
}
"""

let tree1 = string1 |> explode |> tokenize |> parse
lab3 tree1
stringify tree1

let string2 = """
{
    "a": 1,
    "b": "2",
    "c": {
        "d": 1
    }
}
"""

let tree2 = string2 |> explode |> tokenize |> parse
lab3 tree2
stringify tree2

let string3 = """
{
    "a": 1,
    "b": "2",
    "c": {
        "d": "1"
    },
    "e": 23
}
"""

let tree3 = string3 |> explode |> tokenize |> parse
lab3 tree3
stringify tree3


let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab2"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString