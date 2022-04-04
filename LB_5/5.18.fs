open System
// Сумма делителей числа(для проверки на простоту) 
let rec sum_Div (a:int,c:int,beg:int):int =
    match beg with
    | beg when beg>a/2 -> c
    | beg when a%beg=0 -> sum_Div(a,c+beg,beg+1) 
    | beg -> sum_Div(a,c,beg+1)

// Сумма простых делителей числа
let rec Div_rec (a:int,init:int,beg:int,func: int->int->int):int = 
    match beg with
    | beg when beg>a/2 -> init
    | beg when ((a%beg=0)&&(sum_Div(beg,0,2)=0)) -> Div_rec(a, func init beg ,beg+1,func)
    | beg -> Div_rec(a,init ,beg+1,func)
//Голова рекурсии 1
let UnFunDiv (a:int,init:int,func:int->int->int):int = Div_rec(a,init,1,func) 

 //   Количество нечётных цифр числа ,tkmit 3
let rec Odd_rec (a:int,init:int,beg:int):int =
    match beg with
    | 0 when a=0-> init
    | beg when ((beg%2=1)&&(beg>3)) ->Odd_rec(a/10,init+1,a%10)
    | beg->Odd_rec(a/10,init,a%10)

let rec UnFunOdd (a:int):int  = Odd_rec(a/10,0,a%10)
//Сумма цифр
let rec Sum_Digits (a:int,init:int,beg:int):int = 
    match beg with
    | 0 when a=0 ->init
    | beg -> Sum_Digits(a/10,init+beg,a%10)

let rec UnFinSun_Digits(a:int) = Sum_Digits(a,0,0)
// Задача №3
let rec Mult_Div (a:int,init:int,beg:int):int =
    match a with
    | a when beg>a/2->init
    | a when ((a%beg=0)&&(UnFinSun_Digits(a)<UnFinSun_Digits(beg))) -> Mult_Div(a,init*beg,beg+1)
    | a-> Mult_Div(a,init,beg+1)
let rec UnFunMult_Divid (a) = Mult_Div(a,1,1)

[<EntryPoint>]
let main argv =
   printfn"Ведите число:"
   let a = System.Convert.ToInt32(Console.ReadLine())
   printfn $"Сумма простых делителей числа {a}: {UnFunDiv(a,0,fun a b -> a+b)}"
   printfn $"Количество нечётных цифр числа {a}, больших 3:{UnFunOdd(a)}"
   printfn $"Прозведение таких делителей числа {a}, сумма цифр которых меньше, чем сумма цифр исходного числа: {UnFunMult_Divid(a)} " 
   0
