open System

//1.Дана строка.Необходимо найти общее количество русских символов.

let NumberOfRuschar str = 
    let res1 = String.length(String.filter (fun x -> x>='А' && x<='я') str)
    Console.WriteLine("Количество русских символов в строке: {0}",res1)


[<EntryPoint>]
let main argv = 
    let n = Console.ReadLine() ;
    NumberOfRuschar(n);

    0
