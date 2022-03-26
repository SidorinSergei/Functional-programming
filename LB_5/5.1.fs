let Funk a = 
    match a with
    | "F#" -> printfn"Подлиза"
    | "Prolog" -> printfn"Подлиза"
    | a ->printfn "красава"

[<EntryPoint>]
let main argv =
   printfn "Ваш любимый язык программирования?"
   let s = System.Console.ReadLine()   
   Funk s
   0
