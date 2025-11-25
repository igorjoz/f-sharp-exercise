namespace MyProject

open System

module Prices =
    val loadPrices      : string -> (DateTime * float)[]
    val loadPricesAsync : string -> Async<(DateTime * float)[]>

type StockAnalyzer =
    new : ticker:string * days:int * prices:(DateTime * float)[] -> StockAnalyzer
    member Ticker      : string
    member Days        : int
    member Prices      : (DateTime * float)[]
    member StdDev      : float
    member TotalReturn : float
    override ToString  : unit -> string

[<Sealed>]
type StockAnalyzerFactory =
    static member GetAnalyzers :
        tickers:string[] * days:int -> StockAnalyzer[]

    static member GetAnalyzersAsync :
        tickers:string[] * days:int -> Async<StockAnalyzer[]>
