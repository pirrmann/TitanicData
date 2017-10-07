namespace HomeMadeCollections

type LazyList<'a> =
    | Nil
    | Cons of 'a * (unit -> LazyList<'a>)
    interface System.Collections.Generic.IEnumerable<'a> with
        member x.GetEnumerator() : System.Collections.Generic.IEnumerator<'a> = (new LazyListEnumerator<'a>(x)) :> System.Collections.Generic.IEnumerator<'a>
        member x.GetEnumerator() : System.Collections.IEnumerator = (new LazyListEnumerator<'a>(x)) :> System.Collections.IEnumerator
and LazyListEnumerator<'a> (l: LazyList<'a>) =
    let mutable pointedList = l
    let mutable current = Unchecked.defaultof<'a>
    interface System.Collections.Generic.IEnumerator<'a> with
        member this.Current
            with get() : 'a = current
        member this.Current
            with get() : obj = current :> obj
        member this.MoveNext () =
            match pointedList with
            | Nil -> false
            | Cons (head, tail) ->
                pointedList <- tail()
                current <- head
                true
        member this.Reset() =
            pointedList <- l
            current <- Unchecked.defaultof<'a>
        member this.Dispose() = pointedList <- Nil

