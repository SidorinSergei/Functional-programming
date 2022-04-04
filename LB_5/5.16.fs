open System

let rec Nod a b =
 match a with 
 | _ when a%b =0 -> b
 | _-> Nod b (a%b)


let rec Divid_rec (a:int,init:int,beg:int,func: int->int-> int):int = 
 match beg with
 | beg when beg=a-> init
 | beg when Nod a beg = 1 -> Divid_rec(a,func init beg, beg+1, func)
 | beg -> Divid_rec(a,init,beg+1,func)

let rec E (a:int,beg:int,init):int = 
 match a with
 | a when beg=a -> init
 | a when Nod a beg =1 ->
 printfn $"Взимно простое число {beg}"
 E (a,beg+1,init+1)
 | a-> E(a,beg+1,init)

let UnFun (a:int,init:int,func: int->int->int) = Divid_rec(a,init,1,func)
let UnEyler a init = E (a, 1, init)

let ergv = 
 printfn"Ведите число:"
 let a = System.Convert.ToInt32(Console.ReadLine())
 printfn $"Сумма взаимно простых кампанентов числа {a}: {UnFun(a,0,fun a b->a+b)}"
 printfn $"Количество взаимно простых кампанентов числа {a}: {UnEyler a 0}"
    
