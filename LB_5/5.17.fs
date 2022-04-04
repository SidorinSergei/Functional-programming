open System

//Обход делителей числа
let rec Divid_rec (a:int,init:int,beg:int,func: int->bool):int = 
    match beg with
    | beg when beg>a/2 -> init
    | beg when ((a%beg=0)&&(func beg)) ->
          printfn $"{init}) {beg}"
          Divid_rec(a,init+1 , beg+1, func)              
    | beg -> Divid_rec(a,init,beg+1,func)

let UnFunDivid (a:int,init:int,func: int->bool) = 
    printfn"Делители удовлетворяющие условию 'a<10':"
    Divid_rec(a,init,1,func)

//Обход взаимопростых чисел
let rec Nod a b =
    match a with 
    | _ when a%b =0 -> b
    | _-> Nod b (a%b)


let rec Prime_rec (a:int,init:int,beg:int,func: int->bool):int = 
    match beg with
    | beg when beg=a-> init
    | beg when ((Nod a beg = 1)&&(func beg)) -> 
          printfn $"{init}) {beg}"
          Prime_rec(a, init+1 , beg+1, func)

    | beg -> Prime_rec(a,init,beg+1,func)

(*let rec Eyler (a:int,beg:int,init):int = 
    match a with
    | a when beg=a -> init
    | a when Nod a beg =1 ->
        printfn $"Взимно простое число {beg}"
        Eyler (a,beg+1,init+1)
    | a-> Eyler(a,beg+1,init)
    
    let UnEyler a init = Eyler (a, 1, init)
*)

let UnFunPrime (a:int,init:int,func: int->bool) = 
    printfn"Взаимно промтые кампаненты удовлетворяющие условию 'a<10':"
    Prime_rec(a,init,1,func)

[<EntryPoint>]
let main ergv = 
    printfn"Ведите число:"
    let a = System.Convert.ToInt32(Console.ReadLine())
    printfn $"Количсетво делителей числа {a}, удавлитворяющих условию : {UnFunDivid(a,0,fun a->a<10)}"
    printfn $"Количество взаимно простых кампанентов числа {a} удавлитворяющих условию: {UnFunPrime(a,0,fun  a->a<10)}"
    0
