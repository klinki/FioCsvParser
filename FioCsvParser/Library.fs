namespace FioCsvParser

open FSharp.Data
open System.Text
open System.Text.RegularExpressions
open System

module internal Fio =
    Encoding.RegisterProvider CodePagesEncodingProvider.Instance

    let [<Literal>] csvDefinitionFile = __SOURCE_DIRECTORY__ + "/Data/Obchody.csv"

    type TradeRows = CsvProvider<csvDefinitionFile, Separators = ";", SkipRows = 3, HasHeaders = true, Culture = "cs-CZ", IgnoreErrors = true>

    type FioData =
        | Transfer of TradeRows.Row
        | Trade of TradeRows.Row
        | MaskedDividendAmount of TradeRows.Row
        | DividendAmount of TradeRows.Row
        | DividendTax of TradeRows.Row
        | DividendFee of TradeRows.Row
        | DividendAdrFee of TradeRows.Row
        | SqueezeOut of TradeRows.Row
        | SpinOff of TradeRows.Row
        | Other of TradeRows.Row

    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    let printHeaders headers = 
        match headers with 
            | Some headers -> for header in headers do printf "%s " header
            | None -> printf "No headers"

    (* Map row to entity *)
    let mapRow (row: TradeRows.Row) =
        match row.``Text FIO`` with
            | Regex @"Nákup" [] -> Trade row
            | Regex @"Prodej" [] -> Trade row
            | Regex @"Převod" [] -> Transfer row
            | Regex @"Dividenda" [] -> DividendAmount row
            | Regex @"Vloženo" [] ->  MaskedDividendAmount row
            | Regex @"Poplatek" [] -> DividendFee row
            | Regex @"Daň" [] -> DividendTax row
            | Regex @"ADR Fee" [] -> DividendAdrFee row
            | Regex @".* - Výplata úrokového výnosu" [] -> DividendAmount row
            | Regex @"Squeeze out" [] -> SqueezeOut row
            | Regex @"Čistá cena" [] -> Trade row
            | Regex @"Spin-off" [] -> SpinOff row
            | _ -> Other row

module internal DomainMapper =
    open Fio

    let parseDecimal item = Decimal.Parse item
    let parseInt item = Decimal.Parse item |> Decimal.ToInt32

    let groupByDividendType item =
        match item with
            | MaskedDividendAmount _ | DividendAmount _ | DividendTax _ | DividendFee _ | DividendAdrFee _ -> true
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
            | "USD" -> Decimal row.``Objem v USD``
            | "CZK" -> parseDecimal row.``Objem v CZK``
            | "EUR" -> parseDecimal row.``Objem v EUR``
            | _     -> 0m

    let reduceDivCollection acc el =
        match el with
            | DividendAmount amount -> { acc with Amount = parseDecimal amount.Počet; Currency = amount.Měna }
            | DividendTax tax -> { acc with Tax = parseDecimal tax.Počet }
            | DividendAdrFee fee -> { acc with Fee = parseDecimal fee.Počet }
            | DividendFee fee -> { acc with Fee = getFee fee ; Currency = fee.Měna }
            | MaskedDividendAmount amount -> { acc with Amount = parseDecimal amount.``Objem v CZK``; Currency = amount.Měna}
            | _ -> acc

    let convertToBaseTrade (row: TradeRows.Row) =
        {
            Date = row.``Datum obchodu`` |> Option.defaultValue(new DateTime())
            Ticker = row.Symbol
            Count = parseInt row.``Počet``
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
            | Regex @"Převod" [] -> match item.``Text FIO`` with
                                    | Regex @"Nákup" [] -> Buy(baseTradeWithTotal)
                                    | Regex @"Prodej" [] -> Sell(baseTradeWithTotal)

    let parseSpinoff item =
        let baseTrade = convertToBaseTrade item
        let baseTradeWithTotal = { baseTrade with Total = 0m; Ticker = item.Měna; Price = 0m }
        Buy(baseTradeWithTotal)

    let convertToTransfer item =
        let baseTransfer = convertToBaseTransfer item
        match item.``Text FIO`` with
            | Regex @"na účet" [] -> Withdrawal(baseTransfer)
            | Regex @"z účtu" [] -> Deposit(baseTransfer)

    let mapValidTypes item =
        match item with
            | Trade row -> StockTradeType(convertToTrade row)
            | Transfer row -> TransferType(convertToTransfer row)
            | SqueezeOut row -> StockTradeType(convertToTrade row)
            | SpinOff row -> StockTradeType(parseSpinoff row)

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

    (* Group rows by date and symbol *)
    let groupRows rows = rows |> Seq.groupBy (fun (row: TradeRows.Row) -> (row.``Datum obchodu``, row.Symbol))
                              |> Seq.map (fun tuple -> 
                                let ((date, symbol), data) = tuple
                                let mapped = data   |> Seq.map mapRow 
                                let result = mapped |> Seq.groupBy groupByDividendType
                                                    |> Seq.map (fun el -> mapToStockTypes el (getDefaultDividend date symbol))
                                                    |> Seq.concat
                                result 
                                )
                              |> Seq.concat

    (* This functions groups 2 DividendTypes when one directly follows another and one has fee, other doesn't and one has ticker and other doesn't *)
    let groupRelatedDividendPairs items =
        (* Merge dividend type without fee with dividend type with fee into one dividend type with fee *)
        let addDividendFeeToDividend a b 
            = match (a, b) with
                | (DividendType divA, DividendType divB) when divA.Fee = 0m && divB.Fee > 0m -> DividendType { divA with Fee = divB.Fee }
                | (DividendType divA, DividendType divB) when divB.Fee = 0m && divA.Fee > 0m -> DividendType { divB with Fee = divA.Fee }

        (* functional approach to go through list and group together items matching the pairing condition *)
        let removePairs pairing items =
          let rec loop acc = function
            | a :: b :: t when pairing a b -> loop ((addDividendFeeToDividend a b)::acc) t
            | h :: t -> loop (h::acc) t
            | [] -> List.rev acc
          loop [] (List.ofSeq items)

        (* Identify dividend and dividend fee pairs *)
        let pairDividendTypes a b =
            match (a,b) with 
                | (DividendType divA, DividendType divB) -> (divA.Date = divB.Date && divA.Currency = divB.Currency)
                                                            && (divA.Fee = 0m  || divB.Fee = 0m) 
                                                            && (divA.Ticker.Length = 0 || divB.Ticker.Length = 0)
                | _ -> false
    
        items |> removePairs pairDividendTypes


    let parseFioFile (fileName: string) =
        Encoding.RegisterProvider CodePagesEncodingProvider.Instance
        let data = TradeRows.Load fileName
        groupRows data.Rows |> groupRelatedDividendPairs

    let parseFioStream (stream: IO.Stream) = 
        Encoding.RegisterProvider CodePagesEncodingProvider.Instance
        let data = TradeRows.Load stream
        groupRows data.Rows |> groupRelatedDividendPairs

    let parseInputString (input: string) =
        Encoding.RegisterProvider CodePagesEncodingProvider.Instance
        let data = TradeRows.Parse input
        groupRows data.Rows |> groupRelatedDividendPairs

    let testFun() =
        let data = TradeRows.Load csvDefinitionFile
        let grouped = groupRows data.Rows
        
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

type CsvParser =
    static member Parse(filename: string) = DomainMapper.parseFioFile filename
    static member Parse(input: IO.Stream) = DomainMapper.parseFioStream input
    static member ParseString(input) = DomainMapper.parseInputString input
