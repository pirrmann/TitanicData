module Main
open Expecto

[<EntryPoint>]
let main argv =
    match argv with
    | [| "tests" |] -> Tests.runTestsInAssembly defaultConfig Array.empty
    | _ -> Exercise.Run(); 0
