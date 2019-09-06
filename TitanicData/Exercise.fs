module Exercise

(*
If you want to execute part of this file in the REPL, you must first load the following files:
#load "Blank.fs"
#load "CsvParser.fs"
#load "LazyList.fs"
#load "LazyListModule.fs"
*)

open CsvParser
open HomeMadeCollections

// That's not actually what we want
// The dataset is described here:
// https://www.kaggle.com/c/titanic/data

type Gender = | Male | Female
type PassengerClass = | FirstClass | SecondClass | ThirdClass
type Passenger = {
    Name: string
    Gender: Gender
    Age: Option<decimal>
    Fare: decimal
    Cabin: string list
    PassengerClass: PassengerClass
    Survived: bool
}

let parseDecimal (s: string) =
    System.Decimal.Parse(s, System.Globalization.CultureInfo.InvariantCulture)

let Run () =
    let file = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/../CsvFiles/titanic.csv")
    let data =
        [
            for line in file.[1..] do
            yield parseLineWithRegex line
        ]

    // We want to map a line to a Passenger type
    let mapPassenger (a: string array) : Passenger = 
        {
            Name = a.[2]
            Gender = if a.[3] = "male" then Male else Female
            Age = if a.[4] = "" then None else Some (parseDecimal(a.[4]))
            Fare = parseDecimal(a.[8])
            Survived = a.[0] = "1"
            Cabin = List.ofArray(a.[9].Split([|' '|]))
            PassengerClass =
                match a.[1] with
                | "1" -> FirstClass
                | "2" -> SecondClass
                | "3" -> ThirdClass
                | _ -> invalidArg "pclass" "Invalid passenger class"
        }

    // We want to load the list of all the passengers
    let passengers: Passenger list =
        List.map mapPassenger data

    // Now we can start to answer questions!

    let rec getLenghtWithAcc list acc =
        match list with
        | [] -> acc
        | _ :: tail -> getLenghtWithAcc tail (acc+1)
    let getLength list = getLenghtWithAcc list 0

    // How many passengers are there in this dataset?
    let passengersCount : int = List.length passengers

    let rec getItemAtIndex index list =
        match list with
        | [] -> invalidArg "index" "Index out of bounds"
        | head :: tail ->
            if index = 0 then head
            else getItemAtIndex (index-1) tail

    // Who is the 247th passenger in this dataset?
    let passenger247 : Passenger = List.item 246 passengers

    let rec getPassengersBelow10YearsOldCount (list: Passenger list) countSoFar =
        match list with
        | [] -> countSoFar
        | passenger :: otherPassengers ->
            match passenger.Age with
            | Some age when age <= 10M ->
                getPassengersBelow10YearsOldCount otherPassengers (countSoFar + 1)
            | _ ->
                getPassengersBelow10YearsOldCount otherPassengers countSoFar

    // How many childen below 10 years old are there in this dataset?
    let passengersBelow10YearsOld =
        passengers
        |> List.filter (fun p -> p.Age.IsSome && p.Age.Value <= 10M)

    let passengersBelow10YearsOldCount : int =
        passengersBelow10YearsOld
        |> List.length

    let rec getPassengersBelow10YearsOldWhoSurvivedCount (list: Passenger list) countSoFar =
        match list with
        | [] -> countSoFar
        | passenger :: otherPassengers ->
            match passenger.Age with
            | Some age when age <= 10M && passenger.Survived ->
                getPassengersBelow10YearsOldWhoSurvivedCount otherPassengers (countSoFar + 1)
            | _ ->
                getPassengersBelow10YearsOldWhoSurvivedCount otherPassengers countSoFar

    // How many of them survived?
    let passengersBelow10YearsOldCountWhoSurvived : int =
        passengersBelow10YearsOld
        |> List.filter (fun p -> p.Survived)
        |> List.length

    // What is the most expensive fare paid to onboard?
    // Who paid the most expensive fare to onboard?
    // How many of them died/survived?
    let mostExpensiveFare =
        passengers
        |> List.map (fun p -> p.Fare)
        |> List.max

    let passengersWhoPaidTheMost =
        passengers
        |> List.filter (fun p -> p.Fare = mostExpensiveFare)

    let passengersWhoPaidTheMostAndDiedCount =
        passengersWhoPaidTheMost
        |> List.filter (fun p -> not p.Survived)
        |> List.length

    let passengersWhoPaidTheMostAndSurvivedCount =
           passengersWhoPaidTheMost.Length - passengersWhoPaidTheMostAndDiedCount

    // What is the least expensive fare paid to onboard?
    // Who paid the least expensive fare to onboard?
    // How many of them survived?
    let leastExpensiveFare =
           passengers
           |> List.map (fun p -> p.Fare)
           |> List.min

    let passengersWhoPaidTheLeast =
        passengers
        |> List.filter (fun p -> p.Fare = leastExpensiveFare)

    let passengersWhoPaidTheLeastAndDiedCount =
        passengersWhoPaidTheLeast
        |> List.filter (fun p -> not p.Survived)
        |> List.length

    let passengersWhoPaidTheLeastAndSurvivedCount =
        passengersWhoPaidTheLeast.Length - passengersWhoPaidTheLeastAndDiedCount

    // Was there a passenger in cabin with "42" in its name?
    let wasThereAnyoneInCabin42 : bool =
        passengers
        |> List.collect (fun p -> p.Cabin)
        |> List.exists (fun c -> c.Contains("42"))

    // How many distinct cabins are there in the dataset?
    let distinctCabinsCount : int =
        passengers
        |> List.collect (fun p -> p.Cabin)
        |> List.distinct
        |> List.length

    // Among the 100 first passengers in the file, what is the longest name?
    let longestNameAmongThe100FirstPassengers : string =
        passengers
        |> List.take 100
        |> List.map (fun p -> p.Name)
        |> List.maxBy (fun name -> name.Length)

    let ages =
        passengers
        |> List.choose (fun p -> p.Age)
        |> List.map float

    // What is the average age of passengers?
    let averageAge : float =
        List.average ages

    // and standard deviation?
    // Variance, (S2) = moyenne de l'écart au carré de valeurs par rapport à la moyenne
    // Écart-type (S) = Racine carrée de la variance
    let standardDeviation : float =
        ages
        |> List.map (fun age -> pown (age - averageAge) 2)
        |> List.average
        |> sqrt

    let rec getCountSumAndSquaresSumWithAcc (count: float, sum: float, squaresSum: float) list =
        match list with
        | [] -> (count, sum, squaresSum)
        | head :: tail ->
            let newCount = count + 1.0
            let newSum = sum + head
            let newSquaresSum = squaresSum + head * head
            getCountSumAndSquaresSumWithAcc (newCount, newSum, newSquaresSum) tail

    // Can you compute both the average and the standard deviation in one pass?
    let (averageAge' : float, standardDeviation' : float) =
        let count, sum, squaresSum = getCountSumAndSquaresSumWithAcc (0.0, 0.0, 0.0) ages
        let averageAge = sum / count
        let standardDeviation = sqrt ((squaresSum / count - averageAge * averageAge))
        averageAge, standardDeviation

    if averageAge' <> averageAge then
        failwith "averageAge' should equal averageAge"

    if standardDeviation' <> standardDeviation
        then failwith "standardDeviation' should equal standardDeviation"

    let getSurvivalRate (passengersInGroup : Passenger list) =
        passengersInGroup
        |> List.map (fun p -> if p.Survived then 1.0 else 0.0)
        |> List.average

    // What is the global survival rate?
    // What ist the survival rate for each passenger class / sex combination?
    let globalSurvivalRate : float =
        getSurvivalRate passengers

    let survivalRates =
        passengers
        |> List.groupBy (fun p -> p.Gender, p.PassengerClass)
        |> List.map (fun ((gender, passengerClass), passengersInGroup) ->
                        getSurvivalRate passengersInGroup, gender, passengerClass)
        |> List.sortDescending

    printfn "All Good!"

