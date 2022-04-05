let otvet otvet =
    match otvet with
      |"F#"|"Prolog" -> printfn "Подлиза"
      |"python"-> printfn "попался, питонист!"
      |other -> printfn "помойся трижды с мылом"

[<EntryPoint>]
let main argv =
   printfn "на чём прогаешь?"
   let yaz = Console.ReadLine()

   otvet yaz
   0 
