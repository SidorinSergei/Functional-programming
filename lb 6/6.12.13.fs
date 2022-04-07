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
  
let f list =
    let rec f_rec list new_list min = 
        match list with
        |h::t when h=min -> [h]@t@new_list
        |h::t-> 
            let new_list1 = new_list@[h]
            f_rec t new_list1 min
    f_rec list [] (Min list)

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
    printfn"Обработанный список:"
    f list |> vivod 
    0
