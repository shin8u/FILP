open System

// Ввод элементов
let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail

let Min a b = if a<b then a else b//Сравнения двух чисел для поиска минимального

let Min_list list  =  //Функция ищет минимальный элемент списка. Если список пуст возвращает 0
    let rec Min_list_rec list (func:int->int->int) min = 
        match list with 
        | []->min
        | h::t-> Min_list_rec t func (func h min)
    match list with
    |[]->0
    |h::t->Min_list_rec t Min h

let kol_min list =
    let rec kol_min_rec list min kol =
        match list with
        |[]-> kol
        |h::t when min=h->
            let kol1 = kol+1
            kol_min_rec t min kol1
        |h::t-> kol_min_rec t min kol
    kol_min_rec list (Min_list list) 0

  // Вывод элементов
let rec vivod = function    
    |[]->0
    |h::tail->
        printfn $"{h}"
        vivod tail

[<EntryPoint>]
let main argv =
    printfn $"Введите количество элементов"
    let list = vvod (System.Convert.ToInt32(System.Console.ReadLine()))
    printfn $"Количество минимальных элемнтов: {kol_min list}"
    0 // return an integer exit code
