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
  
  // Поиск индекса минимального
let index list =
    let rec index_Min_rec list min beg  = 
        match list with
        | h::t when h=min-> beg
        | h::t->
            let beg1 = beg+1
            index_Min_rec t min beg1
    index_Min_rec list (Min list) 1

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
    let ind = index list
    printfn $"ответ:{ind}"
    0
