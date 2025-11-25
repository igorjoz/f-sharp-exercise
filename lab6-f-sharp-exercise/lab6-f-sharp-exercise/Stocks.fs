namespace MyProject

open System
open System.Net
open System.IO
open System.Net.Http

module Prices =

    let startDate = DateTime(2000,1,1).ToString("yyyyMMdd")
    let endDate   = DateTime.Now.ToString("yyyyMMdd")

    let stooqUrl (ticker:string) =
        let t = ticker.ToLower() + ".us"
        sprintf "https://stooq.pl/q/d/l/?s=%s&i=d&d1=%s&d2=%s" t startDate endDate

    let getCsv (ticker:string) =
        let url = stooqUrl ticker
        let req  = WebRequest.Create(url) :?> HttpWebRequest
        use resp = req.GetResponse()
        use stream = resp.GetResponseStream()
        use reader = new StreamReader(stream)
        reader.ReadToEnd()

    let parsePrices (csv:string) : (DateTime * float)[] =
        csv.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        |> Seq.skip 1
        |> Seq.map (fun line ->
            let parts = line.Split(',')
            let date  = DateTime.Parse(parts.[0])
            let close = float parts.[4]
            date, close)
        |> Seq.toArray

    let loadPrices (ticker:string) : (DateTime * float)[] =
        ticker |> getCsv |> parsePrices

    let loadPricesAsync (ticker:string) : Async<(DateTime * float)[]> =
        async {
            use client = new HttpClient()
            let url = stooqUrl ticker
            let! csv = client.GetStringAsync(url) |> Async.AwaitTask
            return parsePrices csv
        }


// ==============================================
// III. F# w ujęciu obiektowym – klasa StockAnalyzer
// ==============================================

type StockAnalyzer(ticker:string, days:int, prices:(DateTime * float)[]) =

    // bierzemy ostatnie "days" notowań, posortowane po dacie rosnąco
    let window =
        prices
        |> Array.sortBy fst
        |> Array.rev
        |> Array.truncate days
        |> Array.rev

    let closes = window |> Array.map snd

    let returns =
        closes
        |> Array.pairwise
        |> Array.map (fun (prev, curr) -> (curr - prev) / prev)

    member _.Ticker = ticker
    member _.Days   = days
    member _.Prices = window

    /// odchylenie standardowe dziennych stóp zwrotu
    member _.StdDev =
        if returns.Length = 0 then 0.0
        else
            let mean = returns |> Array.average
            returns
            |> Array.averageBy (fun r ->
                let d = r - mean
                d * d)
            |> sqrt

    /// całkowita stopa zwrotu za zadany okres
    member _.TotalReturn =
        if closes.Length = 0 then 0.0
        else
            let first = closes.[0]
            let last  = closes.[closes.Length-1]
            (last - first) / first

    override this.ToString() =
        sprintf "%s (%d dni): σ = %.4f  R = %.2f%%"
                ticker days this.StdDev (this.TotalReturn * 100.0)


/// Fabryka: synchroniczne i asynchroniczne tworzenie analizatorów
[<Sealed>]
type StockAnalyzerFactory private () =

    /// wersja synchroniczna – GetAnalyzers
    static member GetAnalyzers (tickers:string[], days:int) : StockAnalyzer[] =
        tickers
        |> Array.map (fun t ->
            let prices = Prices.loadPrices t
            StockAnalyzer(t, days, prices))

    /// wersja asynchroniczna – GetAnalyzersAsync
    static member GetAnalyzersAsync (tickers:string[], days:int)
        : Async<StockAnalyzer[]> =

        tickers
        |> Seq.map (fun t ->
            async {
                let! prices = Prices.loadPricesAsync t
                return StockAnalyzer(t, days, prices)
            })
        |> Async.Parallel