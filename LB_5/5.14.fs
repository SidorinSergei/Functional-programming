open System

let rec Divid_rec (a:int,init:int,beg:int,func: int->int-> int):int = 
 match beg with
 | beg when beg>a/2 -> init
 | beg when a%beg = 0 -> Divid_rec(a,func init beg, beg+1, func)//beg считать отдельно
 | beg -> Divid_rec(a,init,beg+1,func)

let UnFun (a:int,init:int,func: int->int->int) = Divid_rec(a,init,1,func)

let ergv = 
 printfn"Ведите число:"
 let a = System.Convert.ToInt32(Console.ReadLine())
 printfn $"Сумма делителей числа {a}: {UnFun(a,0,fun a b->a+b)}"
 printfn $"Произведение делителей числа {a}: {UnFun(a,1,fun a b->a*b)}"
    0
