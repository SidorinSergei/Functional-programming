let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail
// Вывод элементов
let rec vivod = function    
    |[]->0
    |h::tail->
        printfn $"{h}"
        vivod tail
let rec vivod2 = function    
|[]->0
|h::tail->
    printfn $"({fst h},{snd h})"
    vivod tail
 
    
[<EntryPoint>]
let main argv =
    printfn $"Введите количество элементов"
    let list = vvod (System.Convert.ToInt32(System.Console.ReadLine()))    

    let res = List.map(fun x -> List.length(List.filter(fun y ->y=x)list))list
    let list2 = List.zip list res
    let L1 = List.filter(fun x->(snd x)=1)list2
    let L2 = List.map(fun x->(x, List.length(List.filter(fun y->y= x)list)))list

    
    printfn $"L1 ответ:{L1|>vivod2}"
    printfn $"L2 ответ:{L2|>vivod2}"
    0
