namespace FioCsvParser

open FSharp.Data
open System.Text
open System.Text.RegularExpressions
open System

module Fio =
    Encoding.RegisterProvider CodePagesEncodingProvider.Instance

    let [<Literal>] csvDefinitionFile = __SOURCE_DIRECTORY__ + "/Data/Obchody.csv"

    type TradeRows = CsvProvider<csvDefinitionFile, Separators = ";", SkipRows = 3, HasHeaders = true, Encoding = "1250", Culture = "cs-CZ">
    
    type FioDividendTypes = 
        | DividendAmount of TradeRows.Row
        | DividendTax of TradeRows.Row
        | DividendFee of TradeRows.Row

    type FioData = 
        | Transfer of TradeRows.Row
        | Trade of TradeRows.Row
        | Dividend of FioDividendTypes
        | Other of TradeRows.Row

    type AmountWithCurrency = { Amount: decimal; Currency: string }

    type BaseStockTrade = { Date: DateTime; Ticker: string; Count: int; Price: decimal; Fee: decimal; Total: decimal; Currency: string; }
    type StockDividend = { Date: DateTime; Ticker: string; Amount: decimal; Fee: decimal; Currency: string; Tax: decimal; }
    
    type StockTrade = 
        | Buy of BaseStockTrade
        | Sell of BaseStockTrade

    type Transfer = 
        | Deposit of AmountWithCurrency
        | Withdrawal of AmountWithCurrency

    type SingleRowType = 
        | TransferType of Transfer
        | StockTradeType of StockTrade
        | DividendType of StockDividend

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let printHeaders headers = 
        match headers with 
            | Some headers -> for header in headers do printf "%s " header
            | None -> printf "No headers"

    let testFun =
        let data = TradeRows.Load csvDefinitionFile
        
        let mapRow (row: TradeRows.Row) = 
            match row.``Text FIO`` with
                | Regex @"Nákup" [] -> Trade row
                | Regex @"Prodej" [] -> Trade row
                | Regex @"Převod" [] -> Transfer row
                | Regex @"Dividenda" [] -> Dividend (DividendAmount row)
                | Regex @"Vloženo" [] -> Dividend (DividendAmount row)
                | Regex @"Poplatek" [] -> Dividend (DividendFee row)
                | Regex @"Daň" [] -> Dividend (DividendTax row)
                | Regex @"ADR Fee" [] -> Dividend (DividendFee row)
                | _ -> Other row


        let unwrapDividend item = match item with
                                  | Dividend div -> div

        let reduceDivCollection acc el = match unwrapDividend el with
                                        | DividendAmount amount -> { acc with Amount = decimal amount.Počet }
                                        | DividendTax tax -> { acc with Tax = decimal tax.Počet }
                                        | DividendFee fee -> { acc with Fee = decimal fee.Počet }
                                        | _ -> acc       

        let grouped = data.Rows |> Seq.groupBy (fun row -> (row.``Datum obchodu``, row.Symbol))
                                |> Seq.map (fun tuple -> 
                                    let ((date, symbol), data) = tuple
                                    let mapped = data |> Seq.map mapRow 
                                    let defaultDividend: StockDividend = {
                                        Date = date |> Option.defaultValue(new DateTime())
                                        Ticker = symbol
                                        Amount = 0m
                                        Fee = 0m
                                        Tax = 0m
                                        Currency = ""
                                    }
                                    let result = mapped |> Seq.groupBy (fun item -> match item with 
                                                                                    | Dividend _ -> true
                                                                                    | _ -> false
                                                                        )
                                                        |> Seq.map (fun grouped -> match grouped with
                                                                                    | (true, dividends) -> dividends |> Seq.fold reduceDivCollection defaultDividend 
                                                                                    | (false, rest) -> rest
                                                                    )
                                    result
                                    )
                                // )

        let classified = data.Rows |> Seq.map mapRow
        
        for row in classified do 
            match row with 
                | Trade trade -> printfn "Trade: %s" trade.``Text FIO``
                | Other data -> printfn "Other: %s" data.``Text FIO``
                | _ -> printfn "Unknown"
