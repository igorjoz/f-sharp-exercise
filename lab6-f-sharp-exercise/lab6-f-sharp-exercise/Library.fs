namespace LibrarySystem

open System
open System.IO
open System.Net.Http
open Newtonsoft.Json
open System.Threading.Tasks

// Units of Measure
[<Measure>] type USD
[<Measure>] type PLN
[<Measure>] type EUR

// Domain Types
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

// DTOs for JSON deserialization
module DTO =
    [<AllowNullLiteral>]
    type ContactDetailsDTO() =
        [<JsonProperty("FirstName")>]
        member val FirstName = "" with get, set
        [<JsonProperty("LastName")>]
        member val LastName = "" with get, set
        [<JsonProperty("DateOfBirth")>]
        member val DateOfBirth = DateTime.MinValue with get, set
        [<JsonProperty("LibraryCardNumber")>]
        member val LibraryCardNumber = "" with get, set
        [<JsonProperty("Email")>]
        member val Email : string = null with get, set
        [<JsonProperty("PhoneNumber")>]
        member val PhoneNumber : string = null with get, set
        [<JsonProperty("Address")>]
        member val Address : string = null with get, set

    [<AllowNullLiteral>]
    type ReaderDTO() =
        [<JsonProperty("Id")>]
        member val Id = 0 with get, set
        [<JsonProperty("Details")>]
        member val Details : ContactDetailsDTO = null with get, set
        [<JsonProperty("Deposit")>]
        member val Deposit = 0.0m with get, set
        [<JsonProperty("JoinDate")>]
        member val JoinDate = DateTime.MinValue with get, set
        [<JsonProperty("AccountStatus")>]
        member val AccountStatus = "" with get, set
        [<JsonProperty("Fines")>]
        member val Fines = 0.0m with get, set

    type LoanDTO() =
        [<JsonProperty("ISBN")>]
        member val ISBN = "" with get, set
        [<JsonProperty("IsReturned")>]
        member val IsReturned = false with get, set

    type LoanHistoryDTO() =
        [<JsonProperty("ReaderId")>]
        member val ReaderId = 0 with get, set
        [<JsonProperty("Loans")>]
        member val Loans : LoanDTO list = [] with get, set

module DataAccess =
    let mapContactDetails (dto: DTO.ContactDetailsDTO) : ContactDetails =
        let contactInfo =
            match dto.PhoneNumber, dto.Address with
            | p, a when not (String.IsNullOrWhiteSpace p) && not (String.IsNullOrWhiteSpace a) -> Both (p, a)
            | p, _ when not (String.IsNullOrWhiteSpace p) -> Phone p
            | _, a when not (String.IsNullOrWhiteSpace a) -> Address a
            | _ -> failwith "Contact details must have either phone or address"
        
        {
            FirstName = dto.FirstName
            LastName = dto.LastName
            DateOfBirth = dto.DateOfBirth
            LibraryCardNumber = dto.LibraryCardNumber
            Email = if String.IsNullOrWhiteSpace dto.Email then None else Some dto.Email
            ContactInfo = contactInfo
        }

    let mapReader (dto: DTO.ReaderDTO) : Reader =
        {
            Id = dto.Id
            Details = if isNull dto.Details then None else Some (mapContactDetails dto.Details)
            Deposit = dto.Deposit * 1.0m<USD>
            JoinDate = dto.JoinDate
            AccountStatus = if dto.AccountStatus = "Premium" then Premium else Standard
            Fines = dto.Fines * 1.0m<PLN>
        }

    let mapLoan (dto: DTO.LoanDTO) : Loan =
        { ISBN = dto.ISBN; IsReturned = dto.IsReturned }

    let mapLoanHistory (dto: DTO.LoanHistoryDTO) : LoanHistory =
        { ReaderId = dto.ReaderId; Loans = dto.Loans |> List.map mapLoan }

    let loadReaders (filePath: string) : Reader list =
        let json = File.ReadAllText(filePath)
        let dtos = JsonConvert.DeserializeObject<DTO.ReaderDTO list>(json)
        dtos |> List.map mapReader

    let loadLoanHistoryForReader (readerId: int) (directoryPath: string) : LoanHistory =
        let filePath = Path.Combine(directoryPath, sprintf "%d.json" readerId)
        if File.Exists(filePath) then
            let json = File.ReadAllText(filePath)
            let dto = JsonConvert.DeserializeObject<DTO.LoanHistoryDTO>(json)
            mapLoanHistory dto
        else
            { ReaderId = readerId; Loans = [] }

module ExchangeRates =
    type Rate() =
        [<JsonProperty("mid")>]
        member val Mid = 0.0m with get, set

    type NbpResponse() =
        [<JsonProperty("rates")>]
        member val Rates : Rate[] = [||] with get, set

    let getRate (currencyCode: string) = async {
        use client = new HttpClient()
        try
            let! json = client.GetStringAsync($"http://api.nbp.pl/api/exchangerates/rates/a/{currencyCode}/?format=json") |> Async.AwaitTask
            let response = JsonConvert.DeserializeObject<NbpResponse>(json)
            if response.Rates.Length > 0 then
                return response.Rates.[0].Mid
            else
                return 4.0m
        with ex ->
            printfn "Error fetching rate for %s: %s" currencyCode ex.Message
            // Fallback if API fails
            if currencyCode = "usd" then return 4.0m
            else return 4.3m
    }

type Library(readersFile: string, loansDirectory: string) =
    let mutable readers = DataAccess.loadReaders readersFile
    
    member this.GetReaders() = readers

    member this.GetLoanHistory(readerId: int) =
        let history = DataAccess.loadLoanHistoryForReader readerId loansDirectory
        history.Loans

    member this.IsPatronForLongerThan(readerId: int, days: int) =
        readers 
        |> List.tryFind (fun r -> r.Id = readerId)
        |> Option.map (fun r -> (DateTime.Now - r.JoinDate).TotalDays > float days)
        |> Option.defaultValue false

    member this.AddFine(readerId: int, amount: decimal<PLN>) =
        readers <- readers |> List.map (fun r -> 
            if r.Id = readerId then { r with Fines = r.Fines + amount } else r)

    member this.PromoteToPremium(minBooks: int, minDays: int) =
        readers <- readers |> List.map (fun r ->
            let history = this.GetLoanHistory(r.Id)
            let bookCount = history.Length
            let isLongEnough = (DateTime.Now - r.JoinDate).TotalDays > float minDays
            if bookCount > minBooks && isLongEnough then
                { r with AccountStatus = Premium }
            else
                r
        )

    member this.GetReader(readerId: int) =
        readers |> List.tryFind (fun r -> r.Id = readerId)
