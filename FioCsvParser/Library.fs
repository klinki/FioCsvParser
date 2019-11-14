namespace FioCsvParser

open FSharp.Data
open System.Text
open System.Text.RegularExpressions

module Fio =
    Encoding.RegisterProvider CodePagesEncodingProvider.Instance

    let [<Literal>] csvDefinitionFile = __SOURCE_DIRECTORY__ + "/Data/Obchody.csv"

    type TradeRows = CsvProvider<csvDefinitionFile, Separators = ";", SkipRows = 3, HasHeaders = true, Encoding = "1250", Culture = "cs-CZ">
    
    type FioData = 
        | Transfer of TradeRows.Row
        | Trade of TradeRows.Row
        | Dividend of TradeRows.Row
        | DividendTax of TradeRows.Row
        | Fee of TradeRows.Row
        | Other of TradeRows.Row

    type AmountWithCurrency = { Amount: decimal; Currency: string }

    type Transfer = 
        | Deposit of AmountWithCurrency
        | Withdrawal of AmountWithCurrency

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let data = TradeRows.Load csvDefinitionFile

    let firstRow = data.Rows |> Seq.head

    let testFun =
        let data = TradeRows.Load csvDefinitionFile
        
        match data.Headers with
            | Some headers -> for header in headers do printf "%s " header
            | None -> printf "No headers"

        for row in data.Rows do 
            printfn "HLOC: %s " row.``Text FIO``

        let mapRow (row: TradeRows.Row) = match row.``Text FIO`` with
            // | Regex @"" [] -> Transfer row
            | Regex @"Nákup" [] -> Trade row
            | Regex @"Prodej" [] -> Trade row
            | Regex @"Převod" [] -> Transfer row
            | Regex @"Dividenda" [] -> Dividend row
            | Regex @"Vloženo" [] -> Dividend row
            | Regex "@Poplatek" [] -> Fee row
            | _ -> Other row

        let classified = data.Rows |> Seq.map mapRow
        
        for row in classified do 
            match row with 
                | Trade trade -> printfn "Trade: %s" trade.``Text FIO``
                | Other data -> printfn "Other: %s" data.``Text FIO``
                | _ -> printfn "Unknown"
