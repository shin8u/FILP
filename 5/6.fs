open System

let rec F a b = 
    if a=b then a
    else 
        if a>b then F (a-b) b 
        else F a (b-a) 

let Obhod x F init =
    let rec obhod x F init number =
        if number=0 then init
        else 
            if (F x number) = 1 then obhod x F (F init number) (number-1)
            else obhod x F init (number-1)   
    obhod x F init x
          
let PrimeNumber x f init = 
    let rec PrimeNumberProcessing1 x f init current = 
        if current = 0 then init
        else
            let newInit = if F x current = 1 then f init current
                          else init
            let newCurrent = current - 1
            PrimeNumberProcessing1 x f newInit newCurrent
    PrimeNumberProcessing1 x f init x

let Eiler x = 
    PrimeNumber x (fun x y-> x + 1) 0

[<EntryPoint>]
let main argv =
    let x = System.Convert.ToInt32(System.Console.ReadLine())
    System.Console.WriteLine(Obhod x (fun x y -> x*y) 1)
    System.Console.WriteLine(Obhod x (fun x y -> x+y) 1)
    System.Console.WriteLine(Eiler x )
    0 
