let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail
// Вывод элемнмтов
let rec vivod = function    
    |[]->0
    |h::tail->
        printfn $"{h}"
        vivod tail
 
//Количество элементов после максимального
let F list = List.length list - (List.findIndexBack (fun x -> x = (List.max list)) list)-1 // Функция вычитает из общей длины списка индекс последнего максимального элемента списка и 1 


[<EntryPoint>]
let main argv =
    printfn $"Введите количество элементов"
    let list = vvod (System.Convert.ToInt32(System.Console.ReadLine()))    
    printfn $"ответ:{list|>F}"
    0
