module Tests

open Expecto

open HomeMadeCollections

let alwaysTrue _ = true
let alwaysFalse _ = false

let shouldHaveLength length (actual: LazyList<'a>) =
  let actualLength = actual |> LazyList.length
  Expect.equal actualLength length (sprintf "Length should be %d" length)

let shouldBeEmpty (actual: LazyList<'a>) =
  let actualLength = actual |> LazyList.length
  Expect.equal actualLength 0 (sprintf "The list should be empty")

let shouldEqual expected actual =
  Expect.equal actual expected "Values should be equal"

let shouldBeSameSequenceAs expected actual =
  Expect.sequenceEqual actual expected "Sequences should be equal"

let shouldBeSameAsList (expected: 'a list) (actual: LazyList<'a>) =
  let actualAsList = actual |> LazyList.toList
  Expect.equal actualAsList expected (sprintf "The values should be the same as te ones from list %A" expected)

[<Tests>]
let tests =
  testList "All LazyList tests" [

    test "An empty lazy list has length 0" {
      LazyList.empty |> shouldHaveLength 0
    }

    test "An empty lazy list is empty" {
      let isEmpty = LazyList.empty |> LazyList.isEmpty
      Expect.isTrue isEmpty "The list should be empty"
    }

    test "A lazy list of length 1 has length 1" {
      Cons(0, fun () -> Nil) |> shouldHaveLength 1
    }

    test "A lazy list of length 1 is not empty" {
      let isEmpty = Cons(0, fun () -> Nil) |> LazyList.isEmpty
      Expect.isFalse isEmpty "The list should not be empty"
    }

    testList "A lazy list built from an array has the length of the array" [
      test "Empty" { [||] |> LazyList.ofArray |> shouldHaveLength 0 }
      test "Size 1" { [| 1 |] |> LazyList.ofArray |> shouldHaveLength 1 }
      test "Size 2" { [| 1; 2 |] |> LazyList.ofArray |> shouldHaveLength 2 }
      test "Size 3" { [| "a"; "b"; "c" |] |> LazyList.ofArray |> shouldHaveLength 3 }
    ]

    testList "A lazy list built from a list has the length of the list" [
      test "Empty" { [] |> LazyList.ofList |> shouldHaveLength 0 }
      test "Size 1" { [ 1 ] |> LazyList.ofList |> shouldHaveLength 1 }
      test "Size 2" { [ 1; 2 ] |> LazyList.ofList |> shouldHaveLength 2 }
      test "Size 3" { [ "a"; "b"; "c" ] |> LazyList.ofList |> shouldHaveLength 3 }
    ]

    testList "A lazy list built from a seq has the length of the seq" [
      test "Empty" { Seq.empty |> LazyList.ofSeq |> shouldHaveLength 0 }
      test "Size 1" { seq { yield 1 } |> LazyList.ofSeq |> shouldHaveLength 1 }
      test "Size 2" { seq { yield 1; yield 2 } |> LazyList.ofSeq |> shouldHaveLength 2 }
      test "Size 3" { seq { yield "a"; yield "b"; yield "c" } |> LazyList.ofSeq |> shouldHaveLength 3 }
    ]

    test "An infinite lazy list can be created from an infinite seq" {
      let infiniteSequence = Seq.unfold (fun () -> Some (1, ())) ()
      let infiniteLazyList = infiniteSequence |> LazyList.ofSeq
      infiniteLazyList |> LazyList.take 10000 |> shouldHaveLength 10000
    }

    testList "Converting from an array and back to an array produces an identical array" [
      let convertAndBack l = l |> LazyList.ofArray |> LazyList.toArray
      yield test "Empty" { [||] |> convertAndBack |> shouldEqual [||] }
      yield test "Size 1" { [| 1 |] |> convertAndBack |> shouldEqual [| 1 |] }
      yield test "Size 2" { [| 1; 2 |] |> convertAndBack |> shouldEqual [| 1; 2 |] }
      yield test "Size 3" { [| "a"; "b"; "c" |] |> convertAndBack |> shouldEqual [| "a"; "b"; "c" |] }
    ]

    testList "Converting from a list and back to a list produces an identical list" [
      let convertAndBack l = l |> LazyList.ofList |> LazyList.toList
      yield test "Empty" { [] |> convertAndBack |> shouldEqual [] }
      yield test "Size 1" { [ 1 ] |> convertAndBack |> shouldEqual [ 1 ] }
      yield test "Size 2" { [ 1; 2 ] |> convertAndBack |> shouldEqual [ 1; 2 ] }
      yield test "Size 3" { [ "a"; "b"; "c" ] |> convertAndBack |> shouldEqual [ "a"; "b"; "c" ] }
    ]

    testList "Converting from a seq and back to a seq produces an identical seq" [
      let convertAndBack l = l |> LazyList.ofSeq |> LazyList.toSeq
      yield test "Empty" { Seq.empty |> convertAndBack |> shouldBeSameSequenceAs Seq.empty }
      yield test "Size 1" { seq { yield 1 } |> convertAndBack |> shouldBeSameSequenceAs (seq { yield 1 }) }
      yield test "Size 2" { seq { yield 1; yield 2 } |> convertAndBack |> shouldBeSameSequenceAs (seq { yield 1; yield 2 }) }
      yield test "Size 3" { seq { yield "a"; yield "b"; yield "c" } |> convertAndBack |> shouldBeSameSequenceAs (seq { yield "a"; yield "b"; yield "c" }) }
    ]

    test "LazyList.head throws on an empty list" {
      Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty |> LazyList.head |> ignore) "It should throw"
    }

    test "LazyList.head returns the head of a lazy list" {
      Cons(1, fun () -> LazyList.empty) |> LazyList.head |> shouldEqual 1
    }

    test "LazyList.tail throws on an empty list" {
      Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty |> LazyList.tail |> ignore) "It should throw"
    }

    test "LazyList.tail returns the evaluated tail of a lazy list" {
      [ "a"; "b"; "c" ] |> LazyList.ofList |> LazyList.tail |> shouldBeSameAsList ["b"; "c"]
    }

    test "LazyList.iter is be called exactly once for all values in correct order" {
      let mutable acc = ""
      [ "a"; "b"; "c" ] |> LazyList.ofList |> LazyList.iter (fun s -> acc <- acc + s)
      acc |> shouldEqual "abc"
    }

    testList "LazyList.item extracts the value at the given index" [
      let lazyList = [ "a"; "b"; "c" ] |> LazyList.ofList
      yield test "Index 0" { lazyList |> LazyList.item 0 |> shouldEqual "a" }
      yield test "Index 1" { lazyList |> LazyList.item 1 |> shouldEqual "b" }
      yield test "Index 2" { lazyList |> LazyList.item 2 |> shouldEqual "c" }
    ]

    testList "LazyList.item throws for invalid indexes" [
      let lazyList = [ "a"; "b"; "c" ] |> LazyList.ofList
      yield test "Index -1" { Expect.throwsT<System.IndexOutOfRangeException> (fun () -> lazyList |> LazyList.item -1 |> ignore) "It should throw" }
      yield test "Index 3" { Expect.throwsT<System.IndexOutOfRangeException> (fun () -> lazyList |> LazyList.item 3 |> ignore) "It should throw" }
    ]

    testList "LazyList.length has the same length as the sequence used to build it" [
      yield test "Empty" { Seq.empty |> LazyList.ofSeq |> shouldHaveLength 0 }
      yield test "Size 1" { seq { yield 1 } |> LazyList.ofSeq |> shouldHaveLength 1 }
      yield test "Size 2" { seq { yield 1; yield 2 } |> LazyList.ofSeq |> shouldHaveLength 2 }
      yield test "Size 3" { seq { yield "a"; yield "b"; yield "c" } |> LazyList.ofSeq |> shouldHaveLength 3 }
    ]

    testList "LazyList.take takes only the first n elements" [
      let lazyList = [ "a"; "b"; "c" ] |> LazyList.ofList
      yield test "0" { lazyList |> LazyList.take 0 |> shouldBeSameAsList [] }
      yield test "1" { lazyList |> LazyList.take 1 |> shouldBeSameAsList [ "a" ] }
      yield test "2" { lazyList |> LazyList.take 2 |> shouldBeSameAsList [ "a"; "b" ] }
      yield test "3" { lazyList |> LazyList.take 3 |> shouldBeSameAsList [ "a"; "b"; "c" ] }
      yield test "4" { lazyList |> LazyList.take 4 |> shouldBeSameAsList [ "a"; "b"; "c" ] }
    ]

    testList "Mapping the identity function on a list produces an identical list" [
      let convertMapIdentityAndConvertAndBack l = l |> LazyList.ofList |> LazyList.map id |> LazyList.toList
      yield test "Empty" { [] |> convertMapIdentityAndConvertAndBack |> shouldEqual [] }
      yield test "Size 1" { [ 1 ] |> convertMapIdentityAndConvertAndBack |> shouldEqual [ 1 ] }
      yield test "Size 2" { [ 1; 2 ] |> convertMapIdentityAndConvertAndBack |> shouldEqual [ 1; 2 ] }
      yield test "Size 3" { [ "a"; "b"; "c" ] |> convertMapIdentityAndConvertAndBack |> shouldEqual [ "a"; "b"; "c" ] }
    ]

    test "Mapping an increment on a list increments all values" {
        [ 1; 2; 3; 4; 5 ]
        |> LazyList.ofList
        |> LazyList.map (fun x -> x + 1)
        |> shouldBeSameAsList [ 2; 3; 4; 5; 6 ]
    }

    test "Distinct filters out duplicate values and keeps ordering" {
        [ 1; 5; 3; 4; 4; 5; 1; 2; 3 ]
        |> LazyList.ofList
        |> LazyList.distinct
        |> shouldBeSameAsList [ 1; 5; 3; 4; 2 ]
    }

    test "Exists is false on an empty list" {
        LazyList.empty |> LazyList.exists alwaysTrue |> shouldEqual false
    }

    test "Exists evaluates to true for an non empty list with an always true predicate" {
        [ 1 ] |> LazyList.ofList |> LazyList.exists alwaysTrue |> shouldEqual true
    }

    test "Exists evaluates to false if no value matches the predicate" {
        [ 1 ] |> LazyList.ofList |> LazyList.exists (fun x -> x > 1) |> shouldEqual false
    }

    test "Exists evaluates to true if a value matches the predicate" {
        [ 1; 2 ] |> LazyList.ofList |> LazyList.exists (fun x -> x > 1) |> shouldEqual true
    }

    test "Find throws on an empty list" {
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () -> LazyList.empty |> LazyList.find alwaysTrue |> ignore) "It should throw"
    }

    test "Find evaluates to the first value for an non empty list with an always true predicate" {
        [ 1 ] |> LazyList.ofList |> LazyList.find alwaysTrue |> shouldEqual 1
    }

    test "Find throws if no value matches the predicate" {
        Expect.throwsT<System.Collections.Generic.KeyNotFoundException> (fun () -> [ 1 ] |> LazyList.ofList |> LazyList.find (fun x -> x > 1) |> ignore) "It should throw"
    }

    test "Find evaluates to the first value which matches the predicate" {
        [ 1; 2 ] |> LazyList.ofList |> LazyList.find (fun x -> x > 1) |> shouldEqual 2
    }

    test "TryFind evaluates to None on an empty list" {
        LazyList.empty |> LazyList.tryFind alwaysTrue |> shouldEqual None
    }

    test "TryFind evaluates to the first value for an non empty list with an always true predicate" {
        [ 1 ] |> LazyList.ofList |> LazyList.tryFind alwaysTrue |> shouldEqual (Some 1)
    }

    test "TryFind evaluates to None if no value matches the predicate" {
        [ 1 ] |> LazyList.ofList |> LazyList.tryFind (fun x -> x > 1) |> shouldEqual None
    }

    test "TryFind evaluates to the first value which matches the predicate" {
        [ 1; 2 ] |> LazyList.ofList |> LazyList.tryFind (fun x -> x > 1) |> shouldEqual (Some 2)
    }

    test "Filtering an empty list returns an empty list" {
        LazyList.empty |> LazyList.filter alwaysTrue |> shouldBeEmpty
    }

    test "Filtering with an always true predicate returns an identical list" {
        [ 1; 2; 3; 4; 5 ] |> LazyList.ofList |> LazyList.filter alwaysTrue |> shouldBeSameAsList [ 1; 2; 3; 4; 5 ]
    }

    test "Filtering with an always false predicate returns an empty list" {
        [ 1; 2; 3; 4; 5 ] |> LazyList.ofList |> LazyList.filter alwaysFalse |> shouldBeEmpty
    }

    test "Filtering only even values returns the filtered list" {
        [ 1; 2; 3; 4; 5 ] |> LazyList.ofList |> LazyList.filter (fun x -> x % 2 = 0) |> shouldBeSameAsList [2; 4]
    }

    test "Choosing on an empty list returns an empty list" {
        LazyList.empty |> LazyList.choose Some |> shouldBeEmpty
    }

    test "Choosing filters out None values" {
        [ Some 1; None; Some 3; Some 4; None ] |> LazyList.ofList |> LazyList.choose id |> shouldBeSameAsList [1; 3; 4]
    }

    test "Min throws on an empty list" {
        Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty |> LazyList.min |> ignore) "It should throw"
    }

    testList "Min evaluates to the minimum value in a list" [
        yield test "Integers" { [ 4; 3; 1; 2; 5 ] |> LazyList.ofList |> LazyList.min |> shouldEqual 1 }
        yield test "Strings" { [ "ef"; "abcd"; "vwxyz" ] |> LazyList.ofList |> LazyList.min |> shouldEqual "abcd" }
    ]

    test "Max throws on an empty list" {
        Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty |> LazyList.max |> ignore) "It should throw"
    }

    testList "Max evaluates to the maximum value in a list" [
        yield test "Integers" { [ 4; 3; 1; 2; 5 ] |> LazyList.ofList |> LazyList.max |> shouldEqual 5 }
        yield test "Strings" { [ "ef"; "abcd"; "vwxyz" ] |> LazyList.ofList |> LazyList.max |> shouldEqual "vwxyz" }
    ]

    test "MinBy throws on an empty list" {
        Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty |> LazyList.minBy id |> ignore) "It should throw"
    }

    testList "MinBy evaluates to the minimum value in a list using the projection" [
        yield test "Integers" { [ 4; 3; 1; 2; 5 ] |> LazyList.ofList |> LazyList.minBy (fun x -> -x) |> shouldEqual 5 }
        yield test "Strings" { [ "ef"; "abcd"; "vwxyz" ] |> LazyList.ofList |> LazyList.minBy (fun x -> x.Length) |> shouldEqual "ef" }
    ]

    test "MaxBy throws on an empty list" {
        Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty |> LazyList.maxBy id |> ignore) "It should throw"
    }

    testList "MaxBy evaluates to the maximum value in a list using the projection" [
        yield test "Integers" { [ 4; 3; 1; 2; 5 ] |> LazyList.ofList |> LazyList.maxBy (fun x -> -x) |> shouldEqual 1 }
        yield test "Strings" { [ "ef"; "abcd"; "vwxyz" ] |> LazyList.ofList |> LazyList.maxBy (fun x -> x.Length) |> shouldEqual "vwxyz" }
    ]

    test "Average throws on an empty list" {
        Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty<float> |> LazyList.average |> ignore) "It should throw"
    }

    testList "Average evaluates to the average value in a list" [
        yield test "Floats" { [ 4.0; 3.0; 1.0; 2.0; 5.0 ] |> LazyList.ofList |> LazyList.average |> shouldEqual 3.0 }
        yield test "Decimals" { [ 4.0M; 3.0M; 1.0M; 2.0M; 5.0M ] |> LazyList.ofList |> LazyList.average |> shouldEqual 3.0M }
    ]

    test "AverageBy throws on an empty list" {
        Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty<float> |> LazyList.averageBy id |> ignore) "It should throw"
    }

    testList "MaxBy evaluates to the maximum value in a list using the projection" [
        yield test "Floats" { [ 4.0; 3.0; 1.0; 2.0; 5.0 ] |> LazyList.ofList |> LazyList.averageBy (fun x -> x * 2.0) |> shouldEqual 6.0 }
        yield test "Decimals" { [ 4.0M; 3.0M; 1.0M; 2.0M; 5.0M ] |> LazyList.ofList |> LazyList.averageBy (fun x -> x * x) |> shouldEqual 11.0M }
    ]

    test "Fold on an empty list evaluates to the initial state" {
        LazyList.empty |> LazyList.fold (fun state _ -> state) 42 |> shouldEqual 42
    }

    test "Fold on list folds items from the list using the folder" {
        [ 1; 2; 3 ] |> LazyList.ofList |> LazyList.fold (+) 0 |> shouldEqual 6
    }

    test "Fold on list folds items in the list order" {
        [ "ef"; "abcd"; "vwxyz" ] |> LazyList.ofList |> LazyList.fold (+) "" |> shouldEqual "efabcdvwxyz"
    }

    test "Reduce throws on an empty list" {
        Expect.throwsT<System.ArgumentException> (fun () -> LazyList.empty |> LazyList.reduce (fun state _ -> state) |> ignore) "It should throw"
    }

    test "Reduce on list reduces items from the list using the reduction" {
        [ 1; 2; 3 ] |> LazyList.ofList |> LazyList.reduce (+) |> shouldEqual 6
    }

    test "Reduce on list folds items in the list order" {
        [ "ef"; "abcd"; "vwxyz" ] |> LazyList.ofList |> LazyList.reduce (+) |> shouldEqual "efabcdvwxyz"
    }

    test "GroupBy on an empty list returns an empty list" {
        LazyList.empty |> LazyList.groupBy id |> shouldBeEmpty
    }

    test "GroupBy groups using the key projection" {
        [ "cat"; "turtle"; "dog"; "unicorn"; "donkey" ]
        |> LazyList.ofList
        |> LazyList.groupBy (fun s -> s.Length)
        |> LazyList.map (fun (key, values) -> key, LazyList.toList values)
        |> LazyList.toList
        |> shouldEqual [
            3, ["cat"; "dog"]
            6, ["turtle"; "donkey"]
            7, ["unicorn"]
           ]
    }

    testList "Concatenating empty lists produces an empty list" [
        yield test "No list" { LazyList.concat [] |> shouldBeEmpty }
        yield test "1 empty list" { LazyList.concat [LazyList.empty] |> shouldBeEmpty }
        yield test "2 empty lists" { LazyList.concat [LazyList.empty; LazyList.empty] |> shouldBeEmpty }
    ]

    test "Concatenating an empty list to a list produces an identical list" {
        [
            LazyList.ofList [1; 2; 3]
            LazyList.empty
        ]
        |> LazyList.concat
        |> shouldBeSameAsList [1; 2; 3]
    }

    test "Concatenating a list to an empty list produces an identical list" {
        [
            LazyList.empty
            LazyList.ofList [1; 2; 3]
        ]
        |> LazyList.concat
        |> shouldBeSameAsList [1; 2; 3]
    }

    test "Concatenating lists concatenates in correct order" {
        [
            LazyList.ofList [1; 2; 3]
            LazyList.ofList [4; 5; 6]
            LazyList.ofList [7]
        ]
        |> LazyList.concat
        |> shouldBeSameAsList [1; 2; 3; 4; 5; 6; 7]
    }

    testList "Collecting empty lists produces an empty list" [
        yield test "No list" { LazyList.empty |> LazyList.collect id |> shouldBeEmpty }
        yield test "1 empty list" { [LazyList.empty] |> LazyList.ofList |> LazyList.collect id  |> shouldBeEmpty }
        yield test "2 empty lists" { [LazyList.empty; LazyList.empty] |> LazyList.ofList |> LazyList.collect id  |> shouldBeEmpty }
    ]

    test "Collecting lists concatenates the produced lists in correct order" {
        [ 2; 5; 3 ]
        |> LazyList.ofList
        |> LazyList.collect (fun x -> LazyList.ofList [0..x..10])
        |> shouldBeSameAsList [0; 2; 4; 6; 8; 10; 0; 5; 10; 0; 3; 6; 9]
    }
  ]
