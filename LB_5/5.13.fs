open System

// Произведение цифр числа, рекурсия вниз
let m_vniz x =
    let rec m_vniz1 x current =
        if x = 0 then current
        else
            let curent1 = current * (x % 10)
            let x1 = x / 10
 m_vniz1 x1 curent1
 m_vniz1 x1 x 1

// Произведение цифр числа, рекурсия вверх
let rec m_vverx x =
    if x = 0 then 1
    else (x % 10) * m_vverx(x / 10)

// Минимальная цифра числа, рекурсия вниз
let min_nomer_vniz x =
    let rec min_nomer_vniz1 x current_min =
        if x = 0 then current
        else
            let min = if x % 10 < current_min then x % 10 else current
            let x1 = x / 10
 min_nomer_vniz1 x1 min
 min_nomer_vniz1 x (x % 10)

// Мин. цифра числа, рекурсия вверх
let rec min_nomer_vverx x =
    if x < 10 then x
    else min (x % 10) (min_nomer_vverx (x / 10))
    
// Макс. цифра числа, рекурсия вверх
let rec max_nomer_vverx x =
    if x < 10 then x
    else max (x % 10) (max_noner_vverx (x / 10))

// Максимальная цифра числа, рекурсия вниз
let max_nomer_vniz x =
    let rec max_nomer_vniz1 x current_max =
        if x = 0 then current_max
        else
            let max = if x % 10 > current_max then x % 10 else current_max
            let x1 = x / 10
            max_nomer_vniz1 x1 max

    max_nomer_vniz1 x (x % 10)

[<EntryPoint>]
let main argv =
    Console.WriteLine("Введите число")
    let x = Console.ReadLine() |> Int32.Parse
    Console.WriteLine("Произведение цифр (вверх): {0}", m_vverx x)
    Console.WriteLine("Произведение цифр (вниз): {0}", m_vniz x)
    Console.WriteLine("Минимальная цифра числа (вверх): {0}", min_nomer_vverx x)
    Console.WriteLine("Минимальная цифра числа (вниз): {0}", min_nomer_vniz x)
    Console.WriteLine("Максимальная цифра числа (вверх): {0}", max_nomer_vverx x)
    Console.WriteLine("Максимальная цифра числа (вниз): {0}", max_nomer_vniz x)
    0
