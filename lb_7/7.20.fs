open System
let rec readList n = 
    match n with
       | 0-> []
       | _ -> 
          let Head = System.Console.ReadLine()
          let Tail = readList (n-1)
          Head::Tail

// Вывод элемнмтов
let rec writeList = function    
    |[]->0
    |h::tail->
        printfn $"{h}"
        writeList tail
//1 В порядке увеличения разницы между средним количеством согласных и средним количеством гласных букв в строке

let raz List = 
    let rec rec_raz list new_List = 
        match list with 
        |[]->new_List
        |str::tail ->
             let razn = (String.length str)/(String.length(String.filter(fun x->x='a'||x='e'||x='y'||x='u'||x='o'||x='i')str))-(String.length str)/(String.length(String.filter(fun x->x<>'a'||x<>'e'||x<>'y'||x<>'u'||x<>'o'||x<>'i')str)) //разницa между средним количеством согласных и средним количеством гласных букв в строке
             let new_list1 = new_List @[(str,razn)]
             rec_raz tail new_list1
    rec_raz List []


let sortList list =
     let new_list =List.sort (raz list)
     let List = List.map(fun x->fst(x))new_list
     List

//6 В порядке увеличения медианного значения выборки строк
//(прошлое медианное значение удаляется из выборки и производится поиск
//нового медианного значения)

let findMedian list = List.item ((List.length list)/2) (List.sort list)

let sortMedian list = 
    let rec sort list sortList =
        match list with
        |[]->sortList
        |_ ->
            let nowMed = findMedian list
            let indMed =List.findIndex (fun x->x=nowMed) list
            let newList = List.filter (fun x -> x<>indMed) list
            sort newList (sortList @ [nowMed])
    sort list []
            
let choose n list =
    match n with 
    |1-> sortList list
    |2-> sortMedian list

[<EntryPoint>]
let main argv =   
    printfn $"Введите количество строк"
    let list = readList (System.Convert.ToInt32(System.Console.ReadLine()))
    printfn"ответ: 
                   "
    list|>sortList|>writeList
    0 // return an integer exit code
