namespace HomeMadeCollections

module LazyList =
    let empty<'a> : LazyList<'a> = Nil
    let isEmpty (l: LazyList<'a>) : bool = true

    let ofList (l: 'a list) : LazyList<'a> = Nil
    let ofArray (a: 'a array) : LazyList<'a> = Nil
    let ofSeq (s: 'a seq) : LazyList<'a> = Nil

    let toList (l: LazyList<'a>) : 'a list = List.empty
    let toArray (l: LazyList<'a>) : 'a array = Array.empty
    let toSeq (l: LazyList<'a>) : 'a seq = l :> seq<'a>

    let head (l: LazyList<'a>) : 'a = Unchecked.defaultof<'a>
    let tail (l: LazyList<'a>) : LazyList<'a> = Nil

    let iter (action  : 'a -> unit) (l: LazyList<'a>) : unit = ()
    let item (index: int) (l: LazyList<'a>) : 'a = Unchecked.defaultof<'a>
    let take (n:int) (l: LazyList<'a>) : LazyList<'a> = Nil
    let length (l: LazyList<'a>) : int = 0

    let map (mapping : 'a -> 'b) (l: LazyList<'a>) : LazyList<'b> = Nil
    let distinct (l: LazyList<'a>) : LazyList<'a> when 'a : equality = Nil

    let exists (predicate: 'a -> bool) (l: LazyList<'a>) : bool = false
    let find (predicate: 'a -> bool) (l: LazyList<'a>) : 'a = Unchecked.defaultof<'a>
    let tryFind (predicate: 'a -> bool) (l: LazyList<'a>) : 'a option = None
    let filter (predicate: 'a -> bool) (l: LazyList<'a>) : LazyList<'a> = Nil
    let choose (chooser: 'a -> 'b option) (l: LazyList<'a>) : LazyList<'b> = Nil

    let min (l: LazyList<'a>) : 'a when 'a : comparison = Unchecked.defaultof<'a>
    let max (l: LazyList<'a>) : 'a when 'a : comparison = Unchecked.defaultof<'a>

    let minBy (projection: 'a -> 'b) (l: LazyList<'a>) : 'a when 'b : comparison = Unchecked.defaultof<'a>
    let maxBy (projection: 'a -> 'b) (l: LazyList<'a>) : 'a when 'b : comparison = Unchecked.defaultof<'a>

    let inline average (l: LazyList<'a>) : 'a
        when 'a : (static member (+) : 'a * 'a -> 'a) 
        and  'a : (static member DivideByInt : 'a * int -> 'a) 
        and  'a : (static member Zero : 'a) = Unchecked.defaultof<'a>

    let inline averageBy (projection: 'a -> 'b) (l: LazyList<'a>) : 'b
        when 'b : (static member (+) : 'b * 'b -> 'b) 
        and  'b : (static member DivideByInt : 'b * int -> 'b) 
        and  'b : (static member Zero : 'b) = Unchecked.defaultof<'b>

    let fold (folder: 'state -> 'a -> 'state) (state: 'state) (l: LazyList<'a>) : 'state = Unchecked.defaultof<'state>
    let reduce (reduction: 'a -> 'a -> 'a) (l: LazyList<'a>) : 'a = Unchecked.defaultof<'a>

    let groupBy (projection: 'a -> 'key) (l: LazyList<'a>) : LazyList<'key * LazyList<'a>> when 'key : equality = Nil

    let concat (lists: seq<LazyList<'a>>) : LazyList<'a> = Nil
    let collect (mapping : 'a -> LazyList<'b>) (l: LazyList<'a>) : LazyList<'b> = Nil
