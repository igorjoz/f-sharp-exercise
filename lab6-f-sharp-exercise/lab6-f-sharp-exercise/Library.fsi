namespace LibrarySystem

open System

[<Measure>] type USD
[<Measure>] type PLN
[<Measure>] type EUR

type ContactMechanism =
    | Phone of string
    | Address of string
    | Both of string * string

type ContactDetails = {
    FirstName: string
    LastName: string
    DateOfBirth: DateTime
    LibraryCardNumber: string
    Email: string option
    ContactInfo: ContactMechanism
}

type AccountStatus =
    | Standard
    | Premium

type Reader = {
    Id: int
    Details: ContactDetails option
    Deposit: decimal<USD>
    JoinDate: DateTime
    AccountStatus: AccountStatus
    Fines: decimal<PLN>
}

type Loan = {
    ISBN: string
    IsReturned: bool
}

type LoanHistory = {
    ReaderId: int
    Loans: Loan list
}

module ExchangeRates =
    val getRate : currencyCode:string -> Async<decimal>

type Library =
    new : readersFile:string * loansDirectory:string -> Library
    member GetReaders : unit -> Reader list
    member GetLoanHistory : readerId:int -> Loan list
    member IsPatronForLongerThan : readerId:int * days:int -> bool
    member AddFine : readerId:int * amount:decimal<PLN> -> unit
    member PromoteToPremium : minBooks:int * minDays:int -> unit
    member GetReader : readerId:int -> Reader option
