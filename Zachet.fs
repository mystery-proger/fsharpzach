let Comb mylist =  
    let rec myfunc acc1 acc2 count = function
        | s :: t when (count % 2 = 1) -> myfunc acc1 (s :: acc2) ((count - 1) / 2) t
        | s :: t when (count % 2 = 0) -> myfunc (s :: acc1) acc2 (count / 2) t
        | [] -> (acc1, acc2)
    
    let rec func acc1 mylist = function
        | num when (num > 0) -> func ((myfunc [] [] num mylist) :: acc1) mylist (num - 1)//(acc2 < List.length mylist) then func ((myfunc [] [] count mylist) :: acc1) (acc2 + 1) 
        | 0 -> acc1
    
    func [] mylist (pown 2 (List.length mylist))

let ans = Comb [1..5]

let maxExpr mylist =
    let sorted = List.sort mylist
    let rec countMax acc1 = function
        | s :: t when (s = 0) -> countMax acc1 t
        | s :: t when (s > 0) -> countMax (acc1 * s) t
        | s :: t when (s < 0) -> countMax (acc1 * (-s)) t
        | [] -> acc1 
    countMax 1 sorted

maxExpr [-1;1;5;-4;57;0]