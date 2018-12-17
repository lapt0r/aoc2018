open System
open System.IO;
open System.Collections.Generic
open System.CodeDom.Compiler

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
let solution1a (input : string[]) =
    let solution = 
        input
        |> Array.map(fun s -> int s)
        |> Seq.sum

    string solution

let solution1b (input : string[]) = 
    let seen_sums : HashSet<int> = new HashSet<int>()
    let int_input = input |> Seq.map (int)
    let int_items = seq {while true do yield! int_input}
 
    let solution = 
        int_items
        |> Seq.scan (+) 0
        |> Seq.skipWhile(fun x -> seen_sums.Add(x) = true)
        |> Seq.head

    string solution

let extractLetterCount (input : string) =
    input
    |> Seq.toList
    |> Seq.countBy id 
    |> Seq.toList

let pairWiseCompare (string1 : string, string2 : string) =
    Seq.map2 (fun x y -> x = y) string1 string2
    |> Seq.length

let solution2a (input : string[]) =
    let twosies = 
        input
        |> Array.map(fun x -> x |> extractLetterCount)
        |> Array.map(fun y -> y |> Seq.filter(fun x -> snd x = 2) |> Seq.length)
        |> Array.filter(fun y -> y > 0)
    
    let threesies = 
        input
        |> Array.map(fun x -> x |> extractLetterCount)
        |> Array.map(fun y -> y |> Seq.filter(fun x -> snd x = 3) |> Seq.length)
        |> Array.filter(fun y -> y > 0)

    string (twosies.Length * threesies.Length)

let solution2b (input : string[]) =
    let range = input.[0].Length - 1
    let combinations = seq {for i in 0..range do yield! input |> Array.map(fun x -> x.Trim()) |> Array.map(fun x -> x.[0..i-1] + "_" + x.[i+1..range])}
    let comb_list = combinations |> Seq.toList
    let listofsolutions = 
        combinations
        |> Seq.countBy id
        |> Seq.filter(fun x -> (snd x) = 2)
        |> Seq.toList

    listofsolutions
    |> List.head
    |> (fun x -> (fst x).Replace("_",""))

[<EntryPoint>]
let main argv = 
    printfn "Select challenge: "
    let challenge = Console.ReadLine()
    let solution = match challenge with
        | "1a" -> 
            File.ReadAllLines("solution1_input.txt") |> solution1a
        | "1b_test" ->
            File.ReadAllLines("1b_test.txt") |> solution1b
        | "1b" ->
            File.ReadAllLines("solution1_input.txt") |> solution1b
        | "2a_test" ->
            File.ReadAllLines("2a_test.txt") |> solution2a
        | "2a" ->
            File.ReadAllLines("solution2_input.txt") |> solution2a
        | "2b_test" -> 
            File.ReadAllLines("2b_test.txt") |> solution2b
        | "2b" ->
            File.ReadAllLines("solution2_input.txt") |> solution2b
        | _ -> "bad arg"
    printfn "Solution : %s" solution
    Console.ReadLine() |> ignore
    0 // return an integer exit code


