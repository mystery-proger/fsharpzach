// плучить выражеие, равное 0

let rec meaning nums signs = 
    match (nums, signs) with
    | (x, []) -> List.item 0 x 
    | (x::xs, z::zs) -> z x (meaning xs zs)
    | _ -> 0
    
let rec signs n = 
    if n = 0 then [[]]
    else [ for i in [(+); (/); (-); (*)] do for j in (signs (n-1)) -> i::j] 


let zero list =
    if list = [] then false 
    else
        signs ((List.length list) - 1) 
        |> List.map (fun x -> meaning list x) 
        |> List.filter (fun x -> (x = 0))
        |> List.length > 0