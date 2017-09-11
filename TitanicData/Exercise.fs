module Exercise

open CsvParser

// Here you should write your exercise logic

let file = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/../CsvFiles/titanic.csv")
let data =
    [|
        for line in file do
        yield parseLineWithRegex line
    |]
