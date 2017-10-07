module Exercise

(*
If you want to execute part of this file in the REPL, you must first load the following files:
#load "Blank.fs"
#load "CsvParser.fs"
#load "LazyList.fs"
#load "LazyListModule.fs"
#r "../packages/NUnit/lib/nunit.framework.dll"
#load "../paket-files/forki/FsUnit/FsUnit.fs"
*)

open CsvParser
open HomeMadeCollections
open FsUnit

let file = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/../CsvFiles/titanic.csv")
let data =
    [|
        for line in file do
        yield parseLineWithRegex line
    |]

type Passenger = string array // That's not actually what we want

let passengers: LazyList<Passenger> = __ // We want to load the list of all the passengers

passengers |> shouldHaveLength (data.Length - 1)