module lab1

open System
open System.Net
open System.IO
open System.Collections.Specialized

// почтовый адрес
let email = "nadysha1997@yandex.ru"
// общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = float * int
let delta = 1e-10

// *** Первая часть

let fTailor (x : float) = 1. / 2. * log(x) // функция, которую раскладываем

let n, a, b = 20., 0.2, 0.7 // интервал

let tailor x = 
    let s = (x - 1.) / (x + 1.)
    let ss = s ** 2.
    let rec tailor' acc x i =
        match ((x - 1.) / (x + 1.)) ** (2. * (float)i + 1.) with 
        | rest when abs(rest) <= delta -> ((acc + (1. / (2. * (float)i + 1.)) * rest), i + 1)
        | rest -> tailor' (acc + (1. / (2. * (float)i + 1.)) * rest) x (i + 1)
    tailor' 0. x 0

let tailorA x = 
    let s = (x - 1.) / (x + 1.)
    let ss = s ** 2.
    let rec tailorA' acc x i rest =
        match rest with 
        | rest when abs(rest) <= delta -> ((acc + (1. / (2. * (float)i + 1.)) * rest), i + 1)
        | _ -> tailorA' (acc + (1. / (2. * (float)i + 1.)) * rest) x (i + 1) (rest * ss)
    tailorA' 0. x 0 s

let printTailor () = 
    [a .. (b-a)/n .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = tailor x, tailorA x in (x, firstRes, firstCou, secondRes, secondCou, fTailor x))
    |> List.iter (fun (a,b,c,d,e,f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

printTailor ()

// *** Вторая часть

let fSolve k = fun x -> 
    if (k = 1) then 0.25 * x * x * x + x - 1.2502 // функция, решение которой ищем
    elif (k = 2) then x + x ** (1. / 2.) + x ** (1. / 3.) - 2.5
    else x - 1. / (3. + sin (3.6 * x))

let a1, b1 = 0., 2.
let a2, b2 = 0.4, 1.
let a3, b3 = 0., 0.85

let iter f a b k : Result =
    let fSolveIter = fun x ->
        if (k = 1) then 1.2502 - 0.25 * x * x * x
        elif (k = 2) then 2.5 - x ** (1. / 2.) - x ** (1. / 3.)
        else 1. / (3. + sin (3.6 * x))
    let rec iter' f x i =
        let x1 = f x
        if (abs(x1 - x) <= delta) then (x1, i + 1)
        else iter' f x1 (i + 1)
    iter' fSolveIter ((a + b) / 2.) 1

let newton f a b k : Result = 
    let derivative = fun x -> 
        if k = 1 then 0.75 * x * x + 1.
        elif k = 2 then 1. + (1. / 2.) * x ** (-1. / 2.) + (1. / 3.) * x ** (-2. / 3.)
        else 1. + 3.6 * cos (3.6 * x) / ((3. + sin (3.6 * x)) ** 2.)
    let rec newton' f a b i = 
        let c = (a + b) / 2.
        let x = c - ((f c) / (derivative c))
        if (abs(x - c) <= delta) then (x, i)
        else newton' f x (x - (f x) / (derivative x)) (i + 1)
    newton' f a b 1

let dichotomy =
    // для функций с аккумулятором удобно ставить его в начало
    let rec dichotomyA i (f:float->float) (a:float) (b:float) (k : int) : Result =
        let c = (a + b) / 2.
        if (b - a <= delta) || (f c = 0.) then (c, i + 1)
        else
            if ((f c) * (f a) < 0.) 
            then dichotomyA (i + 1) f a c k
            else dichotomyA (i + 1) f c b k
    dichotomyA 0 // чтобы воспользоваться каррированием

let printSolve () =
    [iter; newton; dichotomy] 
    |> List.map (fun f -> f (fSolve 1) a1 b1 1) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

    [iter; newton; dichotomy] 
    |> List.map (fun f -> f (fSolve 2) a2 b2 2) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

    [iter; newton; dichotomy] 
    |> List.map (fun f -> f (fSolve 3) a3 b3 3) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)

printSolve()




let main () = 
  let values = new NameValueCollection()
  values.Add("email", email)
  values.Add("content", File.ReadAllText(__SOURCE_DIRECTORY__ + @"/" + __SOURCE_FILE__))

  let client = new WebClient()
  let response = client.UploadValues(new Uri("http://91.239.142.110:13666/lab1"), values)
  let responseString = Text.Encoding.Default.GetString(response)

  printf "%A\n" responseString