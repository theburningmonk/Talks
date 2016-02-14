module CloudWatchSelector

open System
open System.Collections.Generic
open System.Globalization
open System.Text.RegularExpressions

type MetricTerm =
    | Namespace
    | Name

type StatsTerm =
    | Average
    | Min
    | Max
    | Sum
    | SampleCount

type Unit      = | Unit
type Dimension = | Dimension

type TimeFrame =
    | Last              of TimeSpan
    | Since             of DateTime
    | Between           of DateTime * DateTime

type Filter =
    | MetricFilter      of MetricTerm * (string -> bool)
    | StatsFilter       of StatsTerm * (float -> bool)
    | UnitFilter        of Unit * (string -> bool)
    | DimensionFilter   of Dimension * (string * string)
    | CompositeFilter   of Filter * Filter

    static member (+) (lf : Filter, rt : Filter) = CompositeFilter (lf, rt)

type Period = | Period  of TimeSpan

type Query =
    {
        Filter      : Filter
        TimeFrame   : TimeFrame
        Period      : Period option
    }

let inline lowerCase (str : string)   = str.ToLower()
let inline isRegexMatch pattern input = Regex.IsMatch(input, pattern)

let inline eqFilter filter term lf = 
    filter <| (term, fun rt -> lowerCase lf = lowerCase rt)

let inline regexFilter filter term pattern = 
    filter <| (term, fun input -> isRegexMatch (lowerCase pattern) (lowerCase input))

let inline statsFilter term op value = StatsFilter <| (term, fun x -> op x value)

let inline minutes n = n |> TimeSpan.FromMinutes
let inline hours n   = n |> TimeSpan.FromHours
let inline days n    = n |> TimeSpan.FromDays
let timestampFormat  = "yyyy-MM-dd HH:mm:ss"

let (|StringCI|_|) (str : string) input = 
    if str.Equals(input, StringComparison.CurrentCultureIgnoreCase) 
    then Some () 
    else None

let (|EmptyString|_|) input = 
    if String.IsNullOrWhiteSpace input then Some () else None

let (|StartsWith|_|) char (input : string) = 
    if input.[0] = char then Some () else None

let (|EndsWith|_|) char (input : string) =
    if input.[input.Length-1] = char then Some () else None

let (|QuotedString|_|) (input : string) =
    match input with
    | EmptyString -> None
    | str when str.Length < 2 -> None
    | StartsWith ''' & EndsWith ''' -> 
        Some <| input.Substring(1, input.Length - 2)
    | _ -> None

let (|Float|) input = 
    match System.Double.TryParse input with
    | true, n -> n
    | _       -> failwithf "Unexpected token [%s], expecting a floating number." input

let (|Operator|) input = 
    match input with
    | "="  -> (=)
    | ">=" -> (>=)
    | ">"  -> (>)
    | "<=" -> (<=)
    | "<"  -> (<)
    | x    -> failwithf "Unexpected token [%s], expecting =, >=, >, <= or <." x

let (|Minutes|Hours|Days|) input = 
    match input with
    | StringCI "Minutes" -> Minutes minutes
    | StringCI "Hours"   -> Hours hours
    | StringCI "Days"    -> Days days
    | x                  -> failwithf "Unexpected token [%s], expecting 'minutes', 'hours' or 'days'." x

let (|TimeStamp|) input =
    match input with
    | QuotedString str ->
        match DateTime.TryParseExact(str, timestampFormat, CultureInfo.CurrentCulture, DateTimeStyles.None) with
        | true, dt -> Some dt
        | _ -> None
    | _ -> None
    |> function 
        | Some x -> x 
        | _ -> failwithf "Token [%s] is not a valid timestamp, expecting a timestamp in format '%s'." input timestampFormat

let (|And|_|) = function 
    | StringCI "and"::tl -> Some(tl)     
    | _ -> None

let (|At|_|) = function 
    | StringCI "at"::tl -> Some(tl) 
    | _ -> None

let (|NamespaceIs|_|) = function
    | StringCI "NamespaceIs"::QuotedString ns::tl
        -> Some(eqFilter MetricFilter Namespace ns, tl)
    | _ -> None

let (|NamespaceLike|_|) = function
    | StringCI "NamespaceLike"::QuotedString pattern::tl 
        -> Some(regexFilter MetricFilter Namespace pattern, tl)
    | _ -> None

let (|NameIs|_|) = function
    | StringCI "NameIs"::QuotedString n::tl 
        -> Some(eqFilter MetricFilter Name n, tl)
    | _ -> None

let (|NameLike|_|) = function
    | StringCI "NameLike"::QuotedString pattern::tl 
        -> Some(regexFilter MetricFilter Name pattern, tl)
    | _ -> None

let (|UnitIs|_|) = function
    | StringCI "UnitIs"::QuotedString unit::tl
        -> Some(eqFilter UnitFilter Unit unit, tl)
    | _ -> None

let (|DimensionContains|_|) = function
    | StringCI "DimensionContains"::QuotedString name::QuotedString value::tl
        -> Some(DimensionFilter(Dimension, (name, value)), tl)
    | _ -> None

let (|Average|_|) = function
    | StringCI "Average"::Operator op::Float value::tl -> Some(statsFilter StatsTerm.Average op value, tl)
    | _ -> None

let (|Min|_|) = function
    | StringCI "Min"::Operator op::Float value::tl -> Some(statsFilter StatsTerm.Min op value, tl)
    | _ -> None

let (|Max|_|) = function
    | StringCI "Max"::Operator op::Float value::tl -> Some(statsFilter StatsTerm.Max op value, tl)
    | _ -> None

let (|Sum|_|) = function
    | StringCI "Sum"::Operator op::Float value::tl -> Some(statsFilter StatsTerm.Sum op value, tl)
    | _ -> None

let (|SampleCount|_|) = function
    | StringCI "SampleCount"::Operator op::Float value::tl -> Some(statsFilter StatsTerm.SampleCount op value, tl)
    | _ -> None

let (|DuringLast|_|) = function
    | StringCI "DuringLast"::Float n::(Minutes unit | Hours unit | Days unit)::tl
        -> Some(unit n |> Last, tl)
    | _ -> None

let (|Since|_|) = function 
    | StringCI "Since"::TimeStamp timestamp::tl 
        -> Some(TimeFrame.Since timestamp, tl)
    | _ -> None

let (|Between|_|) = function
    | StringCI "Between"::TimeStamp startTime::TimeStamp endTime::tl 
        -> Some(TimeFrame.Between(startTime, endTime), tl)
    | _ -> None

let (|IntervalOf|_|) = function
    | StringCI "IntervalOf"::Float n::(Minutes unit | Hours unit | Days unit)::tl
        -> Some(unit n |> Period, tl)
    | _ -> None

let tokenize (str : string) = 
    let tokens = new List<string>()        
    let buffer = new List<char>()
    let enumerator = str.TrimStart(' ').GetEnumerator()
    let mutable isInQuotes = false
    let flushBuffer () = 
        new String(buffer.ToArray()) |> tokens.Add
        buffer.Clear()

    while enumerator.MoveNext() do
        match enumerator.Current, isInQuotes, buffer.Count with
        // spaces in between single quotes should be included
        | ' ', true, _  -> buffer.Add enumerator.Current
        // e.g. "mary   had a" ignore the adjacent spaces between "mary" and "had"
        | ' ', false, 0 -> ()
        // otherwise this is the end of a token
        | ' ', false, _ -> flushBuffer()
        // ' appearing at the start of a token, e.g. "date is '2014-01-14'"
        | ''', false, 0 -> 
            buffer.Add enumerator.Current
            isInQuotes <- true
        // ' appearing at the end of a token
        | ''', true, _ ->
            buffer.Add enumerator.Current
            isInQuotes <- false
            flushBuffer()                
        | x, _, _ -> buffer.Add enumerator.Current

    if buffer.Count > 0 then flushBuffer()
        
    tokens |> List.ofSeq

let parseFilter tokens = 
    let inline flatten filters = filters |> List.rev |> List.reduce (+)

    let rec loop acc = function
        | NamespaceIs   (filter, tl) | NamespaceLike (filter, tl) 
        | NameIs        (filter, tl) | NameLike      (filter, tl)
        | UnitIs        (filter, tl)
        | Average       (filter, tl) | Sum           (filter, tl)
        | Min           (filter, tl) | Max           (filter, tl)
        | SampleCount   (filter, tl)
        | DimensionContains (filter, tl)
            -> match tl with
                | And tl -> loop (filter::acc) tl
                | _ -> flatten (filter::acc), tl
        | _ -> failwith "Missing filters. You need to specify at least one filter on the metric or stats."

    loop [] tokens

let parseTimeFrame (filter, tokens) = 
    match tokens with
    | DuringLast    (timeframe, tl)
    | Since         (timeframe, tl)
    | Between       (timeframe, tl)
        -> { Filter = filter; TimeFrame = timeframe; Period = None }, tl
    | _ -> failwith "Missing timeframe. You need to specify a timeframe for the query with 'duringLast', 'since' or 'between'."

let parsePeriod (query, tokens) =
    match tokens with
    | At tl -> 
        match tl with
        | IntervalOf (period, _) -> { query with Period = Some period }
        | _ -> failwithf "Missing period. Expecting 'intervalOf' after 'at'."
    | _ -> query

let parse (input : string) =
    input
    |> tokenize
    |> parseFilter
    |> parseTimeFrame
    |> parsePeriod