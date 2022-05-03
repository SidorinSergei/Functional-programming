open System

//9 Дана строка. Необходимо проверить образуют ли строчные символы латиницы палиндром.

let Polindrom str = 
    let newstr  = String.filter (fun x -> x>= 'a' && x <= 'z') str
    let rec checkPolind str check = 
        match str with
        | ""-> Console.WriteLine("Палиндром !")
        | _ when check = 0->  Console.WriteLine("Не палиндром !")        
        | _-> 
            if str.[0] = str.[str.Length-1] then checkPolind str.[1..(str.Length-2)] 1
            else  checkPolind str 0
    Convert.ToString(checkPolind newstr 1)


[<EntryPoint>]
let main argv = 
    let n = Console.ReadLine() ;
    Polindrom(n);

    0
