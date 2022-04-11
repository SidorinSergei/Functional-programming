open System

// Ввод элементов
let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail

let Max a b = if a>b then a else b//Сравнения двух чисел для поиска максимального

let Max1 list  =  //Функция ищет максимальный элемент списка. Если список пуст возвращает 0
    let rec Max_rec list (func:int->int->int) min = 
        match list with 
        | []->min
        | h::t-> Max_rec t func (func h min)
    match list with
    |[]->0
    |h::t->Max_rec t Max h
 
let F list =
    let rec F_rec list pred list_index beg =
        match list with
        |[]->list_index
        |h::t when h<pred -> 
            let new_list_index = list_index@[beg]
            let beg1 = beg+1
            F_rec t h new_list_index beg1
        |h::t->
             let beg1 = beg+1
             F_rec t h list_index beg1
    match list with
    |h::t-> F_rec t h [] 1
    
let  Length list = 
    let rec Length_rec list kol =
        match list with
        | []->kol
        | h::t->
            let kol1 = kol+1
            Length_rec t kol1
    Length_rec list 0

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
    let new_lst = F list
    printfn $"Количество чисел {Length new_lst}"
    printfn"Идексы: "
    vivod new_lst
    0
