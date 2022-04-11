open System

// Ввод элементов
let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail

let min a b = if a<b then a else b//Сравнения двух чисел для поиска максимального

let Min list  =  //Поиск минимального элемента списка. Если список пуст возвращает 0
    let rec Min_rec list (func:int->int->int) min = 
        match list with 
        | []->min
        | h::t-> Min_rec t func (func h min)
    match list with
    |[]->0
    |h::t->Min_rec t min h
  
let F list index =
    let rec P list index beg min=
        match list with
        | h::t when ((index = beg)&&(h=min)) -> printfn"Локальный минимум"
        | h::t when ((index = beg)&&(h<>min)) -> printfn"Не локальный минимум"
        | h::t-> 
            let beg1=beg+1
            P t index beg1 min
    P list index 0 (Min list)

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
    printfn$"Введите индекс"
    let index = System.Convert.ToInt32(System.Console.ReadLine())
    F list index
    0
