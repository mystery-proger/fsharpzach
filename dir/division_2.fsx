// список элементов на 2 части

let a = [1; 3; -4; 1; -4; 5]

let division_by_i list i = 
    [(List.take i list); (List.skip i list)]

let division = 
    [0 .. (List.length a)] 
    |> List.map (fun x -> division_by_i a x)
