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
 
let inter_max list = //функция ищет интервал между первым и последним максимальным элеметами
    let rec one_inter_max_rec list max beg = //Функция ищет 1 максимальный элемент
        match list with
        | h::t when h=max-> beg
        | h::t ->
            let beg1=beg+1
            one_inter_max_rec t max beg1 //Функция ищет последний максимальный элемент
    let  rec end_inter_max_rec list max beg inter =
        match list with
        | []-> inter
        | h::t when h=max ->
            let beg1= beg+1
            end_inter_max_rec  t max beg1 beg
        | h::t->
            let beg1 = beg+1
            end_inter_max_rec t max beg1 inter
    let a = (one_inter_max_rec list (Max1 list) 1)
    let b = (end_inter_max_rec list (Max1 list) 1 a)  
    (a,b)

let rec writeline_inter list =
    let rec writeline_iter_rec list beg inter new_list= 
        match list with
        |list when beg>(snd inter)-> new_list
        |h::t when ((beg>(fst inter))&&(beg<(snd inter))) -> 
            let new_list1 = new_list@[h]
            let beg1 = beg+1
            writeline_iter_rec t beg1 inter new_list1
        |h::t-> 
            let beg1 = beg+1
            writeline_iter_rec t beg1 inter new_list
    writeline_iter_rec list 1 (inter_max list) [] 

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
    let new_list = writeline_inter list
    printfn "Элементы списка между первым и последним максимальными элементами:"
    printfn $"{vivod new_list}"
    printfn""
    0 // return an integer exit code
