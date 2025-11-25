// =============================
// Script.fsx  – lab 6 i 7
// =============================

#r "nuget: FSharp.Collections.ParallelSeq"
open FSharp.Collections.ParallelSeq

// --- podstawowe let/fun i listy ---

let y = 0              // zwykłe wiązanie wartości
let numsI = [ 1; 2; 3; 4; 5 ]
let numsF = [ 1.0; 2.0; 3.0; 4.0; 5.0 ]

// funkcja sqr – domyślnie na int
let sqrInt x = x * x

sqrInt 3 ;;            // FSI: val it : int = 9

// wersja generyczna, inline – będzie działać na int/float/decimal
let inline sqr x = x * x

sqr 3   ;;             // int
sqr 3.5 ;;             // float


// -------------------------------------------
// Imperatywne sumowanie kwadratów (mutable)
// -------------------------------------------

let sumOfSquaresI (nums : int list) =
    let mutable acc = 0
    for n in nums do
        acc <- acc + sqrInt n
    acc

sumOfSquaresI numsI ;;

// ta sama idea, ale wersja inline, działająca na dowolnym typie numerycznym
let inline sumOfSquaresI' (nums : ^T list) =
    let mutable acc = LanguagePrimitives.GenericZero< ^T >
    for n in nums do
        acc <- acc + sqr n
    acc

sumOfSquaresI' numsF ;;


// -------------------------------------------
// Rekurencyjna wersja funkcyjna (match / ::)
// -------------------------------------------

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


// -------------------------------------------
// Wersja funkcyjna bez rekurencji (Seq.map/sum)
// + operator potokowy |> (Pyramid of Doom -> linear flow)
// -------------------------------------------

let inline sumOfSquaresF (nums : seq< ^T >) =
    nums
    |> Seq.map sqr
    |> Seq.sum

sumOfSquaresF numsI ;;
sumOfSquaresF numsF ;;


// -------------------------------------------
// ParallelSeq – sumowanie równoległe
// -------------------------------------------

#r "nuget: FSharp.Collections.ParallelSeq"
open FSharp.Collections.ParallelSeq

// wersja z PSeq (w instrukcji parametr ma typ seq<float>)
let sumOfSquaresP (nums : seq<float>) =
    nums
    |> PSeq.map sqr
    |> PSeq.sum

sumOfSquaresP numsF ;;


// =============================
// Pobieranie kursów akcji ze stooq
// =============================

#load "Stocks.fsi" "Stocks.fs"
open MyProject

open System
open System.Net
open System.IO

// zakres dat – od 2000-01-01 do dziś
let startDate = DateTime(2000,1,1).ToString("yyyyMMdd")
let endDate   = DateTime.Now.ToString("yyyyMMdd")

// --- Funkcje przeniesione do Stocks.fs ---
// // budowanie URL (ticker np. "MSFT" -> "msft.us")
// let stooqUrl (ticker:string) =
//     let t = ticker.ToLower() + ".us"
//     sprintf "https://stooq.pl/q/d/l/?s=%s&i=d&d1=%s&d2=%s" t startDate endDate

// // pobranie CSV synchronicznie
// let getCsv (ticker:string) =
//     let url = stooqUrl ticker
//     let req  = WebRequest.Create(url) :?> HttpWebRequest
//     use resp = req.GetResponse()
//     use stream = resp.GetResponseStream()
//     use reader = new StreamReader(stream)
//     reader.ReadToEnd()

// // parsowanie CSV -> tablica (DateTime * float)
// // (kolumny: Date,Open,High,Low,Close,Volume)
// let prices (csv:string) =
//     csv.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
//     |> Seq.skip 1
//     |> Seq.map (fun line ->
//         let parts = line.Split(',')
//         let date  = DateTime.Parse(parts.[0])
//         let close = float parts.[4]
//         date, close)
//     |> Seq.toArray

// // funkcja loadPrices z instrukcji
// let loadPrices (ticker:string) : (DateTime * float)[] =
//     ticker
//     |> getCsv
//     |> prices

// sprawdzenie:
let msft = Prices.loadPrices "MSFT" ;;

// =============================
// Wykres: XPlot.Plotly
// =============================

#r "nuget: XPlot.Plotly"
open XPlot.Plotly

msft
|> Chart.Line
|> Chart.WithTitle "MSFT – cena zamknięcia"
|> Chart.Show

// kilka spółek w pętli:
[ "MSFT"; "ORCL"; "EBAY" ]
|> Seq.iter (fun t ->
    Prices.loadPrices t
    |> Chart.Line
    |> Chart.WithTitle (sprintf "%s – cena zamknięcia" t)
    |> Chart.Show)


// =============================
// Asynchronizm: loadPricesAsync + Async.Parallel
// =============================

open System.Net.Http

// --- Funkcja przeniesiona do Stocks.fs ---
// // asynchroniczna wersja – blok async { .. }, let!, return
// let loadPricesAsync (ticker:string) : Async<(DateTime * float)[]> =
//     async {
//         use client = new HttpClient()
//         let url = stooqUrl ticker
//         let! csv = client.GetStringAsync(url) |> Async.AwaitTask
//         let ps = prices csv
//         return ps
//     }

// pobranie trzech spółek równolegle
let tickers = [ "MSFT"; "ORCL"; "EBAY" ]

let allPrices : (DateTime * float)[][] =
    tickers
    |> Seq.map Prices.loadPricesAsync
    |> Async.Parallel
    |> Async.RunSynchronously   // tu JEDYNE blokowanie

// zrównoleglenie przetwarzania samych linii CSV (PSeq)
let parseCsvParallel (csv:string) =
    csv.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.skip 1
    |> PSeq.map (fun line -> line.Split(','))
    |> PSeq.toArray

// =============================
// Uruchomienie kodu z Stocks.fs
// =============================

// #load "Stocks.fsi" "Stocks.fs" // Już załadowane wyżej
// open MyProject // Już otwarte wyżej

printfn "\n--- Uruchamianie Stocks.fs ---"

// Przykładowe użycie StockAnalyzerFactory
let tickersToAnalyze = [| "MSFT"; "GOOGL" |]
let daysToAnalyze = 10

printfn "Pobieranie i analiza dla: %A" tickersToAnalyze

// Synchronicznie
let analyzers = StockAnalyzerFactory.GetAnalyzers(tickersToAnalyze, daysToAnalyze)
analyzers |> Array.iter (fun a -> printfn "%O" a)

printfn "--- Koniec Stocks.fs ---"