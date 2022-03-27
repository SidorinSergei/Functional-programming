open System

[<EntryPoint>]
let main argv =
    let a (x: String) = x.ToLower()
    let answer x =
        match x with
        | "f#" | "prolog" -> "Пoдлизa!"
        | "python" -> "Ленивый"
        | "java" -> "Братик"
        | "c#" -> "Молодец"
        | "c++" -> "Красавчик"
        | "php" -> "Ты кто такой?"
        | "c" -> "Здорово"
        | _ -> "Ты банан"
    //12.1
 printfn "Какoй твoй любимый язык?"
    (Console.ReadLine >> a >> answer >> Console.WriteLine)()
    //12.2
 printfn "Какoй твoй любимый язык?"
 let proc input (output:string->unit) chooser = output (chooser (input ()))
 proc Console.ReadLine Console.WriteLine answer
