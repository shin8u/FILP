open System

let obhod n predicate func init=
    let rec obhod1 n func init b=
        if b=0 then init else
        let newInit= if n%b=0 && predicate b then func init b else init
        let newb=b-1
        obxod1 n func newInit newb 
    obhod1 n func init n

let rec nod n m=
    if n=0||m=0 then n+m 
    else
    let newn=if n>m then n%m else n
    let newm=if n<=m then m%n else m
    nod newn newm

let obr1 n predicate func init=
    let rec obr2 n func init cand=
        if cand<=0 then init else
        let newInit= if nod n cand=1 && predicate cand then func init cand else init
        let newcand=cand-1
        obr n func newInit newcand
    obr2 n func init n

[<EntryPoint>]
let main argv =
    System.Console.WriteLine("Введите число:")
    let a = System.Convert.ToInt32(System.Console.ReadLine())
    Console.WriteLine(obhod a (fun x -> x%2=0) (fun x y ->x*y) 1)
    Console.WriteLine(obr1 a (fun x->x%2=1) (fun x y->x+y) 0 )
    0
