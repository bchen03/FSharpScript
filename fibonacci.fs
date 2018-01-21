open System

[<EntryPoint>]
let main argv = 
//    let a = Console.ReadLine() |> int
//    let b = Console.ReadLine() |> int
//    printfn "%d" (a+b)

    let maxNum = Console.ReadLine() |> int

    let fibonacci = 
        Seq.unfold(fun (current, next) -> Some(current, (next, current + next))) (1,2)
        |> Seq.takeWhile(fun i -> i < maxNum)
        |> Seq.map(fun i -> i.ToString())
        |> Seq.reduce(fun state item -> state + "," + item)

    printfn "%i: %s" maxNum fibonacci

    0 // return an integer exit code
    

    