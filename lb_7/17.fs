open System

let rec vvod n = 
    if n=0 then []
    else
    let Head = System.Convert.ToInt32(System.Console.ReadLine())
    let Tail = vvod (n-1)
    Head::Tail

let readData1 = 
    Console.WriteLine("Введите размерность первой последовательности:  ")
    let n=System.Convert.ToInt32(System.Console.ReadLine())
    Console.WriteLine("Введите первую последовательность: ")
    vvod n

let readData2 = 
    Console.WriteLine("Введите размерность второй последовательности:  ")
    let n=System.Convert.ToInt32(System.Console.ReadLine())
    Console.WriteLine("Введите вторую последовательность: ")
    vvod n
 

let rec vivod = function
    [] ->   let z = System.Console.ReadKey()
            0
    | (head : int)::tail -> 
                       System.Console.WriteLine(head)
                       vivod tail
//Даны две последовательности, найти наибольшую по длине общую подпоследовательность.
let circleLeft (list: 'int list) =
    list.Tail @ [list.Head]

let findPosl1 list1 list2=
    fst (List.fold2 (fun s x1 x2->
        if x1=x2 then
            let new_c = (snd s)@[x1]
            if List.length new_c >= List.length (fst s) then
                (new_c, new_c)
            else 
                (fst s, new_c)
        else
            (fst s, [])    
    ) ([], []) list1 list2)

let findPosl list1 list2 = 
    let rec ff list1 list2 iter (new_list:'int List)= 
        if iter=List.length list2 then new_list
        else 
            let newPosl =
                if List.length new_list<List.length (findPosl1 list1 list2) then findPosl1 list1 list2
                else new_list
            ff list1 (circleLeft list2) (iter+1) newPosl
    ff list1 list2 1 []

[<EntryPoint>]
let main argv =
    let list1= readData1
    let list2= readData2
    let result = findPosl list1 list2
    printfn"Ответ:"
    vivod result
    0
