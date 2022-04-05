open System

[<EntryPoint>]
let main argv =
   let otvet (otvet:string) =
        match otvet.Trim() with
          |"F#"|"Prolog" -> printfn "Подлиза"
          |"python"-> printfn "попался, питонист!"
          |other -> printfn "помойся трижды с мылом"
    
   printfn "на чём прогаешь?"
   //суперпозиция
   (Console.ReadLine>>otvet>>Console.WriteLine)()
   //каррирование
   let f otvet func out = out(func(otvet()))
   f Console.ReadLine otvet Console.WriteLine 
   0 

