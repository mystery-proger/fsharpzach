// çíà÷åíèå àðèôì. âûðàæåíèÿ 0 ÍÅÄÎÄÅËÀÍÎ

let rec del_i_el i = function
    | [] -> []
    | x::xs when x = i -> del_i_el i xs
    | x::xs -> x::del_i_el i xs

let rec permute = function
    | [] ->[[]]
    | list -> [for i in list do for j in permute (del_i_el i list) -> i::j]

let meaning list = 
    list |> List.fold (fun (acc, f) x -> 
        if x = "+" then (acc, (+))
        elif x = "-" then (acc, (-))
        elif x = "*" then (acc, (*))
        elif x = "/" then (acc, (/))
        else (f acc (int x), (+))
    ) (0, (+)) |> fst

let zero_meaning list =
    (permute list) 
    |> List.iter (fun x -> 
        if meaning x = 0 
        then x |> List.iter (fun y -> printf "%s" y)
        else printf ""
        printf " "
    )
   