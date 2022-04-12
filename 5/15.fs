open System

let rec Func x y =
    if x=y then x
    else 
        if x>y then Func (x-y) y 
        else Func x (y-x) 

let Obhod n f init =
    let rec obhod n f init number =
        if number=0 then init
        else 
            if (Func n number) = 1 then obhod n f (f init number) (number-1)
            else obhod n f init (number-1)   
    obhod n f init n
          

[<EntryPoint>]
let main argv =
    let n = Convert.ToInt32(Console.ReadLine())
    Console.WriteLine(Obhod n (fun x y -> x+y) 0)
    0 
