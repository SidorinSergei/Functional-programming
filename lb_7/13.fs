open System

let rec outPutList list =
    match list with
    |head::[] -> printfn "%A" head
    |head::tail-> printf "%A," head; outPutList tail
    |[]-> printfn ""

let indexMaxElm elm max = (max=elm)     

[<EntryPoint>]
let main argv =
    let list = [4;5;7;3;2;3;7;1;2]
    outPutList list
    let maxElmList =  List.max(list)
    let index = List.findIndex(fun elm -> indexMaxElm elm maxElmList) (List.rev(list))
    let list2 = List.filter(fun elm ->  index<fst(elm) ) (List.indexed(list))
    let list3 = List.map(fun x -> snd x) list2
    outPutList list3
    printfn " Количество элементов %d " (List.length(list3)) 
    0 // return an integer exit code
