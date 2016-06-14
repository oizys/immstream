type ClosedStream = 
    { name: string;
        current: int; }

[<CustomEquality; CustomComparison>]
type Stream = 
    { name: string;
        current: int;
        previous: Stream option;
        map: (int -> int);
        filter: (int -> bool);
        subs: Stream list; }

    member this.Close() =
        { ClosedStream.name = this.name; ClosedStream.current = this.current; }

    override x.Equals(yobj) =
        match yobj with
        | :? Stream as y -> (x.name = y.name)
        | _ -> false

    override x.GetHashCode() = hash x.name
    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Stream as y -> compare x.name y.name
            | _ -> invalidArg "yobj" "cannot compare values of different types"


let rec xfrm (sel:Stream) (f: Stream -> Stream) (top:Stream) =
    match sel with
    | t when (t=top) -> f top
    | _ -> { top with subs = top.subs |> List.map (fun s -> xfrm sel f s) }

let rec find (sel:Stream -> bool) (top:Stream) =
    match sel(top) with
    | true -> top
    | false -> Seq.pick (fun x -> Some (find sel x)) top.subs

let rec xemit (v:int) (sendr:Stream) =
    let mv = sendr.map v
    { sendr with 
        current = if sendr.filter mv then mv else sendr.current; 
        previous = Some sendr;
        subs = sendr.subs 
            //|> List.filter(fun s -> s.filter mv)   unsub when
            |> List.map(fun s -> xemit mv s); }

let xsub (sendr:Stream) (rcvr:Stream) =
    { sendr with subs = (sendr.subs @ [rcvr]) }

let xunsub (sendr:Stream) (rcvr:Stream) =
    { sendr with subs = sendr.subs |> List.filter(fun s -> s = rcvr) }

//-------------------------------------

let current (sendr:Stream) (top:Stream) =
    find (fun x -> x=sendr) top

let sub (sendr:Stream) (rcvr:Stream) (top:Stream) =
    xfrm sendr (fun x -> xsub x rcvr) top

let unsub(sendr:Stream) (rcvr:Stream)  (top:Stream) =
    xfrm sendr (fun x -> xunsub x rcvr) top

let emit (sendr:Stream) (v:int) (top:Stream) =
    xfrm sendr (fun x -> xemit v x) top

let rec hist (sendr) =
    seq { 
        yield sendr.current
        match sendr.previous with 
            | None -> ()
            | Some s -> yield! hist s
    }

let rec maptree (f : (Stream * int) -> 'a) (top:Stream) (depth:int) =
    f (top, depth) :: (List.map (fun x -> maptree f x (depth + 1)) top.subs |> List.concat)
// Alternate seq form: 
//    seq {
//        yield f (top, depth)
//        yield! seq { for x in top.subs do
//                        yield! maptree f x (depth + 1) }
//    }

//-------------------------------------

let filterstream (name: string) (f: int -> bool) =
    { name = name; previous = None; current = 0; subs = []; map = id; filter = f }

let mapstream (name: string) (f: int -> int) =
    { name = name; previous = None; current = 0; subs = []; map = f; filter = fun x -> true }

let stream (name: string) =
    { name = name; previous = None; current = 0; subs = []; map = id; filter = fun x -> true }

let printstream = mapstream "print" (fun x -> printfn "printing %A" x; 0)

[<EntryPoint>]
let main argv = 
    let root = stream "root"
    let a = { stream "a" with current = 5; map = fun x -> x * 2 }

    let d =
        root 
        |> sub root a
        |> emit root 2
        |> sub a printstream
    let e = 
        d 
        |> sub (current a d) (filterstream "overten" (fun x -> x > 10))
        |> emit root 7
        |> emit root 3

    let xe = maptree (fun x -> printfn "%s%A %A" (String.replicate (snd x) "    ") (fst x).name (fst x).current) e 0
    printfn "root hist %A" (hist e)
    printfn "a hist %A" (hist (current a e))

    0 // return an integer exit code
