namespace MyProject

open System

// ----------------------------
// Prosty typ String50
// ----------------------------

type String50 = String50 of string

// moduł z funkcjami pomocniczymi (pliki *.fs, nie skrypt)
module String50 =

    type T = String50 of string

    let create (s:string) =
        if s <> null && s.Length <= 50 then
            Some (String50 s)
        else
            None

    /// Apply the given function to the wrapped value
    let apply f (String50 s) = f s

    /// Get the wrapped value
    let value s = apply id s


// ============================
// Bardziej ogólne opakowane stringi – WrappedString
// ============================

module WrappedString =

    type IWrappedString =
        abstract Value : string

    let create canonicalize isValid ctor (s:string) =
        if s = null then
            None
        else
            let s' = canonicalize s
            if isValid s' then
                Some (ctor s')
            else
                None

    let apply f (s:IWrappedString) =
        s.Value |> f

    let value s = apply id s

    let equals left right =
        (value left) = (value right)

    let compareTo left right =
        (value left).CompareTo (value right)

    // pomocnicze funkcje kanonizacji/walidacji
    let singleLineTrimmed (s:string) =
        System.Text.RegularExpressions.Regex.Replace(s, "\s", " ").Trim()

    let lengthValidator len (s:string) =
        s.Length <= len

    // String100
    type String100 = String100 of string with
        interface IWrappedString with
            member this.Value =
                let (String100 s) = this
                s

    let string100 =
        create singleLineTrimmed (lengthValidator 100) String100

    let convertTo100 s = apply string100 s

    // String50 (druga wersja, zgodna z interfejsem)
    type String50 = String50 of string with
        interface IWrappedString with
            member this.Value =
                let (String50 s) = this
                s

    let string50 =
        create singleLineTrimmed (lengthValidator 50) String50

    let convertTo50 s = apply string50 s


// ============================
// EmailAddress + kontakt – na bazie WrappedString
// ============================

module EmailAddress =

    open WrappedString

    type T = EmailAddress of string with
        interface IWrappedString with
            member this.Value =
                let (EmailAddress s) = this
                s

    let create =
        let canonicalize = singleLineTrimmed
        let isValid s =
            lengthValidator 100 s &&
            System.Text.RegularExpressions.Regex.IsMatch(s, @"^\S+@\S+\.\S+$")
        WrappedString.create canonicalize isValid EmailAddress

    /// Converts any wrapped string to an EmailAddress
    let convert s = WrappedString.apply create s

    // wersja z kontynuacjami (success/failure)
    let createWithCont success failure =
        let canonicalize = WrappedString.singleLineTrimmed

        let isValid success failure (s:string) =
            if (WrappedString.lengthValidator 100 s) &&
               System.Text.RegularExpressions.Regex.IsMatch(s, @"^\S+@\S+\.\S+$")
            then
                success (EmailAddress s)
                true
            else
                failure "Email address must contain an @ sign and be shorter than 100 signs"
                false

        WrappedString.create canonicalize (isValid success failure) EmailAddress


// dyskryminowana unia ContactInfo – nielegalny stan (brak kontaktu) jest niemożliwy

type ContactInfo =
    | EmailOnly   of EmailAddress.T
    | PostOnly    of string
    | EmailAndPost of EmailAddress.T * string
