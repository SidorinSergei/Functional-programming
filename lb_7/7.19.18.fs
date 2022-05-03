open System

//18 Найти в тексте даты формата «день.месяц.год».

let Date (str:string)= 
    let day = str.Remove(2,8)
    let month = str.Remove(0,3).Remove(2,5)
    let year = str.Remove(0,6)
    day<="31"&&day>="01" && month>="01"&& month<="12" && year >"0000" && year<"9999"

let FindData (str:string) = 
    let rec finddate (stroka:string) (strNow: string) (strResult: string) = 
        match stroka with
        |"" -> strResult
        |_-> 
            let newStroka = 
                if strNow.Length<10 then
                    strNow + stroka.Remove(1,stroka.Length-1)
                else strNow.Remove(0,1)+(stroka.Remove(1,stroka.Length-1))
            let newResult = 
                if (newStroka.Length = 10 && Date newStroka) then strResult+"\n"+newStroka
                else strResult
            finddate (stroka.Remove(0,1)) newStroka newResult
    if finddate str "" "" = "" then "В тексте дат нет" else finddate str "" ""
    Console.WriteLine("Искомая дата:{0}",finddate str "" "")


[<EntryPoint>]
let main argv = 
    let n = Console.ReadLine() ;
    Convert.ToString(FindData n)

    0
