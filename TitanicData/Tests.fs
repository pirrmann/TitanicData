module Tests

open NUnit.Framework
open FsUnit

open Exercise

// From here you should write your tests

let [<Test>] ``This is a passing test`` () =
    true |> shouldEqual true
