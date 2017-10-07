namespace HomeMadeCollections

type LazyList<'a> =
    | Nil
    | Cons of 'a * (unit -> LazyList<'a>)
    interface System.Collections.Generic.IEnumerable<'a>