// задача о нахождении максимального значения выражения, 
// полученного расстановкой знаков +, -, * и скобок 
// в последовательности a_1, a_2, .. , a_n

let a = [1; 3; -4; 1; -4; 5]

let rec d i j = 
    if i = j then List.item i a
    else
        let max_line k = 
            ((d i k) + (d (k+1) j)) 
            |> max ((d i k) - (d (k+1) j)) 
            |> max ((d i k) * (d (k+1) j))
        [i .. j-1] 
            |> List.map (fun x -> max_line x) 
            |> List.max

let max_val =
    d 0 ((List.length a) - 1)