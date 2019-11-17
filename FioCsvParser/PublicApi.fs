namespace FioCsvParser

open System

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

type FioCsvParser =
    static member ParseFile(filename) = parseFioFile fileName
