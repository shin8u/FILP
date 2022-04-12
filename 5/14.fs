open System
let Del n f init = 
    let rec FDel n f init del = 
        if del=0 then init
        else 
            let NextInit=
                if n%del=0 then f init del
                else init
            let newDel=del-1
            FDel n f NextInit newDel
    FDel n f init n            
[<EntryPoint>]
let main argv =
    let n=Convert.ToInt32(Console.ReadLine())
    Console.WriteLine(Del n (fun x y -> x+y) 0)
    0
