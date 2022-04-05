
open System
let rec PrUp n =
    if n=0 then 1
    else (n%10)*PrUp(n/10)

let PrTail n =
    let rec PT n pr =
        if n=0 then pr
        else 
          let newPr =pr*(n%10)
          let newn=n/10
          PT newn newPr
    PT n 1

let rec MinUp n=
    if n<10 then n
    else 
        if MinUp(n/10)<n%10 then MinUp(n/10)
        else n%10

let MinTail n=
    let rec MT n min =
        if n=0 then min
        else 
            if min>n%10 then (MT (n/10)(n%10))
            else MT (n/10) min
    MT n 10

let rec MaxUp n=
    if n<10 then n
    else 
        if MaxUp(n/10)>n%10 then MaxUp(n/10)
        else n%10

let MaxTail n=
    let rec MaxT n max=
        if n=0 then max
        else if n%10>max then MaxT (n/10)(n%10)
            else MaxT(n/10)max
    MaxT n 0

[<EntryPoint>]
let main argv =
   let n = Convert.ToInt32(Console.ReadLine())
   Console.WriteLine( PrUp n)
   Console.WriteLine( PrTail n)
   Console.WriteLine( MinUp n)
   Console.WriteLine( MinTail n)
   Console.WriteLine( MaxUp n)
   Console.WriteLine( MaxTail n)
   0 
