
type Passport(first:string ,sur: string,last: string,ser:int, nomer: int,date: string) =
    //параметры конструктора
    member this.Firstname:string  = first
    member this.Surname:string = sur
    member this.Lastname:string = last
    member this.nomber:int = nomer
    member this.seriya:int = ser
    member this.Date = date
