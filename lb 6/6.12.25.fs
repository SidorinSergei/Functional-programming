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
 
//выводит из списка заданный интервал
let f list inter = 
    let rec f_rec list inter beg new_list =
        match list with
        |list when beg>(snd inter)-> Max1 new_list
        |h::t when ((beg>(fst inter))&&(beg<(snd inter))) -> 
            let new_list1 = new_list@[h]
            let beg1 = beg+1
            f_rec t inter beg1 new_list1
        |h::t-> 
            let beg1 = beg+1
            f_rec t inter beg1 new_list
    f_rec list inter 1 [] 

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
    printfn $"Введите интервал [a;b] для поиска максимального"
    let inter = ((System.Convert.ToInt32(System.Console.ReadLine())),(System.Convert.ToInt32(System.Console.ReadLine())))
    let max = f list inter
    printfn $"Максимум {max}"
    0 
