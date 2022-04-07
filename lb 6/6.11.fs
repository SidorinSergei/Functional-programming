open System

// Ввод элементов
let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail

let f list func = 
    let rec f_rec list func new_list = 
        match list with
        | []->new_list
        | h::t->
            let el_1 = h //первый элемент
            let el_2 = if t<>[]then t.Head else 1 //  2 элемент
            let el_3 = if t <> [] then (if t.Tail <> [] then t.Tail.Head else 1) else 1 // 3 элемент
            let sum_3 = func el_1 el_2 el_3 // ищем сумму 
            let new_list_2 = new_list@ [sum_3]//Добавляем элемент в список
            let shifted_list = if t <> [] then (if t.Tail <> [] then t.Tail.Tail else []) else [] //Сдвиг на 2 элемента
            f_rec shifted_list func new_list_2
    f_rec list func []
  
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
    let new_list = f list (fun a b c -> a+b+c)
    printfn $"Результат"
    vivod new_list
    0
