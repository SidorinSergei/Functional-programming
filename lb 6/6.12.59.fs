open System

// Ввод элементов
let rec vvod n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Convert.ToInt32(System.Console.ReadLine())
          let Tail = vvod (n-1)
          Head::Tail

let povtor list h =
    let rec rec_povtor list h1 pov =
        match list with
        |[]->pov
        |h::tail->
            let pov1 = if h1=h then pov+1 else pov
            rec_povtor tail h1 pov1
    rec_povtor list h 1
    
let f list =
     let rec f_rec list new_list=
         match list with
         |[]->new_list
         |h::t when (h>0 && h<100 && (povtor list h)>2) -> 
              let new_el = h*h
              let new_new_list = new_list@[new_el]
              f_rec t new_new_list
         |h::t -> f_rec t new_list
     f_rec list []

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
    printfn "Новый список:" 
    printfn $"{f list|>vivod}"
    
    0 // return an integer exit code
