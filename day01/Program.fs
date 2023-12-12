open System
open System.IO
open System.Text.RegularExpressions
    
let lines = File.ReadAllLines("input.txt")

let extractValue digitsExtractor line =
    let digits = digitsExtractor line
    if Seq.isEmpty digits then 0
    else int $"{Seq.head digits}{Seq.last digits}"

let extractDigits line = line |> Seq.filter Char.IsDigit       

let replaceDigitWords options line =
    let wordToDigit = function
        | "one" -> "1" | "two" -> "2" | "three" -> "3" | "four" -> "4" | "five" -> "5"
        | "six" -> "6" | "seven" -> "7" | "eight" -> "8" | "nine" -> "9"
        | digit -> digit
    let numberPattern = @"(one|two|three|four|five|six|seven|eight|nine|\d)"
    Regex.Replace(line, numberPattern, (fun (m: Match) -> wordToDigit m.Value), options)

let extractDigitsWithWords line =
    let first = line |> replaceDigitWords RegexOptions.None |> extractDigits  
    let last = line |> replaceDigitWords RegexOptions.RightToLeft |> extractDigits
    Seq.append first last
    
let calibrate (digitsExtractor: string -> char seq) lines = Seq.sumBy (extractValue digitsExtractor) lines
       
printfn $"Part 1: %A{calibrate extractDigits lines}"
printfn $"Part 2: %A{calibrate extractDigitsWithWords lines}"