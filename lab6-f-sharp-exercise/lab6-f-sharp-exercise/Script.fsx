#r "nuget: FSharp.Collections.ParallelSeq"
open FSharp.Collections.ParallelSeq


let y = 0
let numsI = [ 1; 2; 3; 4; 5 ]
let numsF = [ 1.0; 2.0; 3.0; 4.0; 5.0 ]

let sqrInt x = x * x

sqrInt 3 ;;

let inline sqr x = x * x

sqr 3   ;;
sqr 3.5 ;;


let sumOfSquaresI (nums : int list) =
    let mutable acc = 0
    for n in nums do
        acc <- acc + sqrInt n
    acc

sumOfSquaresI numsI ;;

let inline sumOfSquaresI' (nums : ^T list) =
    let mutable acc = LanguagePrimitives.GenericZero< ^T >
    for n in nums do
        acc <- acc + sqr n
    acc

sumOfSquaresI' numsF ;;


let inline sumOfSquaresR (nums : ^T list) =
    let inline add x y = x + y
    let zero = LanguagePrimitives.GenericZero< ^T >

    let rec loop acc list =
        match list with
        | []      -> acc
        | h :: t  -> loop (add acc (sqr h)) t

    loop zero nums

sumOfSquaresR numsI ;;
sumOfSquaresR numsF ;;


let inline sumOfSquaresF (nums : seq< ^T >) =
    nums
    |> Seq.map sqr
    |> Seq.sum

sumOfSquaresF numsI ;;
sumOfSquaresF numsF ;;


#r "nuget: FSharp.Collections.ParallelSeq"
open FSharp.Collections.ParallelSeq

let sumOfSquaresP (nums : seq<float>) =
    nums
    |> PSeq.map sqr
    |> PSeq.sum

sumOfSquaresP numsF ;;


#load "Stocks.fsi" "Stocks.fs"
open MyProject

open System
open System.Net
open System.IO

let startDate = DateTime(2000,1,1).ToString("yyyyMMdd")
let endDate   = DateTime.Now.ToString("yyyyMMdd")

let msft = Prices.loadPrices "MSFT" ;;

#r "nuget: XPlot.Plotly"
open XPlot.Plotly

msft
|> Chart.Line
|> Chart.WithTitle "MSFT – cena zamknięcia"
|> Chart.Show

[ "MSFT"; "ORCL"; "EBAY" ]
|> Seq.iter (fun t ->
    Prices.loadPrices t
    |> Chart.Line
    |> Chart.WithTitle (sprintf "%s – cena zamknięcia" t)
    |> Chart.Show)


open System.Net.Http

let tickers = [ "MSFT"; "ORCL"; "EBAY" ]

let allPrices : (DateTime * float)[][] =
    tickers
    |> Seq.map Prices.loadPricesAsync
    |> Async.Parallel
    |> Async.RunSynchronously

let parseCsvParallel (csv:string) =
    csv.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.skip 1
    |> PSeq.map (fun line -> line.Split(','))
    |> PSeq.toArray

printfn "\n--- Uruchamianie Stocks.fs ---"

let tickersToAnalyze = [| "MSFT"; "GOOGL" |]
let daysToAnalyze = 10

printfn "Pobieranie i analiza dla: %A" tickersToAnalyze

let analyzers = StockAnalyzerFactory.GetAnalyzers(tickersToAnalyze, daysToAnalyze)
analyzers |> Array.iter (fun a -> printfn "%O" a)

printfn "--- Koniec Stocks.fs ---"