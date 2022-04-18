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
 
let Fun list = List.find(fun x->List.length(List.filter(fun y->y=x)list)=1)list

[<EntryPoint>]
let main argv =
    printfn $"Введите количество элементов"
    let list = vvod (System.Convert.ToInt32(System.Console.ReadLine()))    
    printfn $"ответ:{list|>Fun}"
    0
    
