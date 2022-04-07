open System

// Ввод элементов
let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail

let max a b = if a>b then a else b//Сравнения двух чисел для поиска максимального

let Max list  =  //Поиск максимального элемента списка. Если список пуст возвращает 0
    let rec Max_rec list (func:int->int->int) max = 
        match list with 
        | []->max
        | h::t-> Max_rec t func (func h max)
    match list with
    |[]->0
    |h::t->Max_rec t max h
  
  //Количество элементов после максимального
let Kolvo list = 
    let rec Kolvo_rec list max kol =
        match list with
        |[]-> kol
        |t::h->
            match t with
            |t when t=max  -> Kolvo_rec h max 0
            |_-> 
                let kol2=kol+1
                Kolvo_rec h max kol2
    Kolvo_rec list (Max list) 0  

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
    let kol = Kolvo list
    printfn $"ответ:{kol}"
    0
