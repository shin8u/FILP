open System

let rec Func a b = 
    if a=b then a
    else 
        if a>b then Func (a-b) b 
        else Func a (b-a) 

let Obhod x F init y =
    let rec obhod x F init number =
        if number=0 then init
        else 
            if (Func x number) = 1 && y number then obhod x F (F init number) (number-1)
            else obhod x F init (number-1)   
    obhod x F init x
          
let DivObhod x y F init =
    let rec divobhod x f init number = 
        if number = 0 then init 
        else 
        if x%number=0&& y number then divobhod x F (F init number) (number-1)
        else divobhod x F init (number-1)
    divobhod x F init x


[<EntryPoint>]
let main argv =
    System.Console.Write("Введите ваше число: ")
    let x = System.Convert.ToInt32(System.Console.ReadLine())
    System.Console.WriteLine(DivObhod x (fun x -> x%2=1) (fun x y -> x*y) 1)
    System.Console.WriteLine(DivObhod x (fun x -> x%2=0) (fun x y -> x*y) 1)
    System.Console.WriteLine(Obhod x (fun x y -> x+y) 0 (fun x -> x%2=0)) 
    System.Console.WriteLine(Obhod x (fun x y -> x+y) 0 (fun x -> x%2=1))
    0
