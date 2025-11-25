module Program

open System
open LibrarySystem
open LibrarySystem.ExchangeRates

[<EntryPoint>]
let main argv =
    let readersPath = "readers.json"
    let loansDir = "loans"
    
    if not (IO.File.Exists readersPath) || not (IO.Directory.Exists loansDir) then
        printfn "Error: JSON files/directory not found in current directory: %s" (IO.Directory.GetCurrentDirectory())
        1
    else
        let library = Library(readersPath, loansDir)
        
        let readers = library.GetReaders()
        printfn "Loaded %d readers." readers.Length
        readers |> List.iter (fun r -> printfn " - Reader ID: %d" r.Id)

        printfn "Fetching exchange rates..."
        let usdToPlnRate = 
            try
                getRate "usd" |> Async.RunSynchronously
            with ex ->
                printfn "Failed to fetch USD rate: %s" ex.Message
                4.0m // Fallback

        let eurToPlnRate = 
            try
                getRate "eur" |> Async.RunSynchronously
            with ex ->
                printfn "Failed to fetch EUR rate: %s" ex.Message
                4.3m // Fallback

        printfn "USD to PLN Rate: %f" usdToPlnRate
        printfn "EUR to PLN Rate: %f" eurToPlnRate

        let rec loop () =
            printfn "\n--- Library System ---"
            printfn "1. Show Reader Info"
            printfn "2. Add Fine"
            printfn "3. Check Fines vs Deposit"
            printfn "4. Promote Readers to Premium"
            printfn "5. Exit"
            printf "Select option: "
            
            match Console.ReadLine() with
            | "1" ->
                printf "Enter Reader ID: "
                match Int32.TryParse(Console.ReadLine()) with
                | true, id ->
                    match library.GetReader(id) with
                    | Some reader ->
                        match reader.Details with
                        | Some details ->
                            printfn "Name: %s %s" details.FirstName details.LastName
                            match details.Email with
                            | Some email -> printfn "Email: %s" email
                            | None -> printfn "Email: N/A"
                        | None -> printfn "Reader has no contact details."
                    | None -> printfn "Reader not found."
                | _ -> printfn "Invalid ID."
                loop()
            | "2" ->
                printf "Enter Reader ID: "
                match Int32.TryParse(Console.ReadLine()) with
                | true, id ->
                    printf "Enter Fine Amount (PLN): "
                    match Decimal.TryParse(Console.ReadLine()) with
                    | true, amount ->
                        library.AddFine(id, amount * 1.0m<PLN>)
                        printfn "Fine added."
                    | _ -> printfn "Invalid amount."
                | _ -> printfn "Invalid ID."
                loop()
            | "3" ->
                printf "Enter Reader ID: "
                match Int32.TryParse(Console.ReadLine()) with
                | true, id ->
                    match library.GetReader(id) with
                    | Some reader ->
                        let finesPLN = reader.Fines
                        let depositUSD = reader.Deposit
                        
                        let rate = usdToPlnRate * 1.0m<PLN/USD>
                        let depositPLN = depositUSD * rate
                        
                        printfn "Fines: %f PLN" (decimal finesPLN)
                        printfn "Deposit: %f USD (~%f PLN)" (decimal depositUSD) (decimal depositPLN)
                        
                        if finesPLN > depositPLN then
                            printfn "WARNING: Fines exceed deposit!"
                        else
                            printfn "Status OK: Fines within deposit limit."
                    | None -> printfn "Reader not found."
                | _ -> printfn "Invalid ID."
                loop()
            | "4" ->
                library.PromoteToPremium(2, 365) // Example: >2 books and >1 year
                printfn "Promotion process completed."
                loop()
            | "5" -> 
                0
            | _ -> 
                printfn "Invalid option."
                loop()

        loop()
