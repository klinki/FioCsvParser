namespace FioCsvParser

open FSharp.Data
open System.Text
open System.Text.RegularExpressions
open System

module Stocks =
    type BaseTransfer = { Date: DateTime; Amount: decimal; Currency: string }
    type BaseStockTrade = { Date: DateTime; Ticker: string; Count: int; Price: decimal; Fee: decimal; Total: decimal; Currency: string; }
    type StockDividend = { Date: DateTime; Ticker: string; Amount: decimal; Fee: decimal; Currency: string; Tax: decimal; }

    type StockTrade =
        | Buy of BaseStockTrade
        | Sell of BaseStockTrade

    type MoneyTransfer =
        | Deposit of BaseTransfer
        | Withdrawal of BaseTransfer

    type TransactionType =
        | TransferType of MoneyTransfer
        | StockTradeType of StockTrade
        | DividendType of StockDividend

module Fio =
    open Stocks

    Encoding.RegisterProvider CodePagesEncodingProvider.Instance

    let [<Literal>] csvDefinitionFile = __SOURCE_DIRECTORY__ + "/Data/Obchody.csv"

    type TradeRows = CsvProvider<csvDefinitionFile, Separators = ";", SkipRows = 3, HasHeaders = true, Encoding = "1250", Culture = "cs-CZ">
    
    type FioData =
        | Transfer of TradeRows.Row
        | Trade of TradeRows.Row
        | DividendAmount of TradeRows.Row
        | DividendTax of TradeRows.Row
        | DividendFee of TradeRows.Row
        | Other of TradeRows.Row

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let printHeaders headers = 
        match headers with 
            | Some headers -> for header in headers do printf "%s " header
            | None -> printf "No headers"

    let mapRow (row: TradeRows.Row) =
        match row.``Text FIO`` with
            | Regex @"Nákup" [] -> Trade row
            | Regex @"Prodej" [] -> Trade row
            | Regex @"Převod" [] -> Transfer row
            | Regex @"Dividenda" [] -> DividendAmount row
            | Regex @"Vloženo" [] ->  DividendAmount row
            | Regex @"Poplatek" [] -> DividendFee row
            | Regex @"Daň" [] -> DividendTax row
            | Regex @"ADR Fee" [] -> DividendFee row
            | _ -> Other row

    let testFun =
        let data = TradeRows.Load csvDefinitionFile

        let reduceDivCollection acc el =
            match el with
                | DividendAmount amount -> { acc with Amount = decimal amount.Počet }
                | DividendTax tax -> { acc with Tax = decimal tax.Počet }
                | DividendFee fee -> { acc with Fee = decimal fee.Počet }
                | _ -> acc

        let groupByDividendType item =
            match item with
                | DividendAmount _ | DividendTax _ | DividendFee _ -> true
                | _ -> false

        let filterOther item =
            match item with
                | Other _ -> false
                | _ -> true

        let getFee (row: TradeRows.Row) =
            match row.``Měna`` with
                | "USD" -> decimal row.``Poplatky v USD``
                | "CZK" -> decimal row.``Poplatky v CZK``
                | "EUR" -> decimal row.``Poplatky v EUR``
                | _     -> 0m

        let getAmount (row: TradeRows.Row) =
            match row.``Měna`` with
                | "CZK" -> decimal row.``Objem v USD``
                | "USD" -> decimal row.``Objem v CZK``
                | "EUR" -> decimal row.``Objem v EUR``
                | _     -> 0m

        let convertToBaseTrade (row: TradeRows.Row) =
            {
                Date = row.``Datum obchodu`` |> Option.defaultValue(new DateTime())
                Ticker = row.Symbol
                Count = int row.``Počet``
                Price = decimal row.``Cena``
                Fee = getFee row
                Total = decimal row.``Cena``
                Currency = row.``Měna``
            }

        let convertToBaseTransfer (row: TradeRows.Row) =
            {
                Date = row.``Datum obchodu`` |> Option.defaultValue(new DateTime())
                Amount = getAmount row
                Currency = row.``Měna``
            }

        let convertToTrade item =
            let baseTrade = convertToBaseTrade item
            let baseTradeWithTotal = { baseTrade with Total = (baseTrade.Price * decimal baseTrade.Count) + baseTrade.Fee }
            match item.``Směr`` with
                | Regex @"Nákup" [] -> Buy(baseTradeWithTotal)
                | Regex @"Prodej" [] -> Sell(baseTradeWithTotal)

        let convertToTransfer item =
            let baseTransfer = convertToBaseTransfer item
            match item.``Text FIO`` with
                | Regex @"na účet" [] -> Withdrawal(baseTransfer)
                | Regex @"z účtu" [] -> Deposit(baseTransfer)

        let mapValidTypes item =
            match item with
                | Trade row -> StockTradeType(convertToTrade row)
                | Transfer row -> TransferType(convertToTransfer row)

        let getDefaultDividend date symbol = {
            Date = date |> Option.defaultValue(new DateTime())
            Ticker = symbol
            Amount = 0m
            Fee = 0m
            Tax = 0m
            Currency = ""
        }

        let mapToStockTypes groupedByDividends defaultDividend =
            match groupedByDividends with
                    | (true, dividends) -> dividends |> Seq.fold reduceDivCollection defaultDividend |> DividendType |> Seq.singleton
                    | (false, rest) -> rest |> Seq.filter filterOther
                                            |> Seq.map mapValidTypes
                    | _ -> Seq.empty

        let grouped = data.Rows |> Seq.groupBy (fun (row: TradeRows.Row) -> (row.``Datum obchodu``, row.Symbol))
                                |> Seq.map (fun tuple -> 
                                    let ((date, symbol), data) = tuple
                                    let mapped = data |> Seq.map mapRow 
                                    let result = mapped |> Seq.groupBy groupByDividendType
                                                        |> Seq.map (fun el -> mapToStockTypes el (getDefaultDividend date symbol))
                                                        |> Seq.concat
                                    result 
                                    )
                                |> Seq.concat

        for row in grouped do
            match row with
                | TransferType transfer -> printfn "Transfer: %A" transfer
                | StockTradeType trade -> printfn "Trade %A" trade
                | DividendType dividend -> printfn "Dividend %A" dividend
                | _ -> printfn "Other"


        let classified = data.Rows |> Seq.map mapRow
        
        for row in classified do 
            match row with 
                | Trade trade -> printfn "Trade: %s" trade.``Text FIO``
                | Other data -> printfn "Other: %s" data.``Text FIO``
                | _ -> printfn "Unknown"
