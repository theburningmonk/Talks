#r @"\\mac\Home\Git\talks\Tame cloud complexity with F# DSLs\demo\FSharpDSLs\../../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r @"\\mac\Home\Git\talks\Tame cloud complexity with F# DSLs\demo\FSharpDSLs\../../packages/FParsec/lib/net40-client/FParsec.dll"

open System
open FParsec
open FParsec.Internals

type Identifier = 
    | Asterisk
    | Attribute of string

type Operant = 
    | S         of string
    | NDouble   of double
    | NBigInt   of bigint

type FilterCondition = 
    | Equal                 of Operant
    | NotEqual              of Operant
    | GreaterThan           of Operant
    | GreaterThanOrEqual    of Operant
    | LessThan              of Operant
    | LessThanOrEqual       of Operant
    | NotNull
    | Null
    | Contains              of Operant
    | NotContains           of Operant
    | BeginsWith            of Operant    
    | Between               of Operant * Operant
    | In                    of Operant list

type OrderDirection =
    | Asc
    | Desc

type Action = 
    | Select of Identifier list
    | Count

type From   = From of string
type Filter = Identifier * FilterCondition
type Where  = Where of Filter list
type Limit  = Limit of int

type QueryOption =
    | NoConsistentRead
    | QueryPageSize of int
    | Index         of index:string * allAttributes:bool
    | QueryNoReturnedCapacity

type DynamoQuery =
    {
        Action  : Action
        From    : From
        Where   : Where
        Limit   : Limit option
        Order   : OrderDirection option
        Options : QueryOption [] option
    }

let (<&&>) f g x = (f x) && (g x)
let (<||>) f g x = (f x) || (g x)


type Parser<'t> = Parser<'t, unit>

let ws = spaces // eats any whitespace

// helper functions that ignores subsequent whitespaces
let pstring s        : Parser<_> = pstring s .>> ws
let skipString s     : Parser<_> = skipStringCI s .>> ws
let stringReturn s r : Parser<_> = stringCIReturn s r .>> ws
let pfloat  : Parser<float> = pfloat .>> ws
let pint32  : Parser<int32> = pint32 .>> ws



let pasterisk = stringReturn "*" Asterisk

let isAttrName = isLetter <||> isDigit
let pattribute = 
    many1SatisfyL isAttrName "attribute name"
    .>> ws 
    |>> Attribute

let comma = pstring ","

let pattributes = choice [ pasterisk; pattribute ]
let pselect = 
    skipString "select" 
    >>. (sepBy1 pattributes comma |>> Select)

let pcount = stringReturn "count *" Count

// an action is either select or count
let paction = ws >>. choice [ pselect; pcount ] .>> ws

let isTableName = isLetter <||> isDigit <||> isAnyOf ['.'; '-'; '_']
let pfrom : Parser<_> =
    ws
    >>. skipString "from"
    >>. ((many1SatisfyL isTableName "table name") |>> From)
    .>> ws

let v1 = pipe2 paction pfrom (fun act table -> act, table)



let (|BigInt|_|) x =
    match bigint.TryParse x with
    | true, x -> Some x
    | _ -> None

// can't just use pfloat here because it loses precision over very
// large numbers e.g. 1020547106541605689
let pnumber : Parser<_> = 
    many1Satisfy2 (isDigit <||> ((=) '-')) (isDigit <||> ((=) '.'))
    |>> (function 
        | BigInt x -> NBigInt x
        | x -> Double.Parse x |> NDouble)
        
let pstringLiteral =
    between (pstring "\"") (pstring "\"") (manySatisfy isAsciiLetter)
    |>> S

let operant = 
    ws 
    >>. choiceL [ pstringLiteral; pnumber ] "String or Numeric value" 
    .>> ws


let binaryOperators = 
    choice [ 
        stringReturn "="  Equal
        stringReturn ">=" GreaterThanOrEqual
        stringReturn ">"  GreaterThan
        stringReturn "<=" LessThanOrEqual
        stringReturn "<"  LessThan
        stringReturn "begins with" BeginsWith ]
let binaryCondition = 
    pipe3 
        pattribute binaryOperators operant 
        (fun id op v -> id, op v)

let between = stringReturn "between" Between
let and'    = skipString "and"
let betweenCondition = 
    pipe5
        pattribute between operant and' operant 
        (fun id op v1 _ v2 -> id, op(v1, v2))

let pcondition = 
    choice [ 
        attempt binaryCondition; 
        attempt betweenCondition ]
let filterConditions = sepBy1 (ws >>. pcondition .>> ws) (ws >>. and')
let pwhere = 
    ws 
    >>. skipString "where" 
    >>. (filterConditions |>> Where) 
    .>> ws

let v2 = 
    pipe3 
        paction pfrom pwhere 
        (fun act from where -> 
            {
                Action  = act
                From    = from
                Where   = where
                Order   = None
                Limit   = None
                Options = None
            })



let plimit = ws >>. skipString "limit" >>. pint32 |>> Limit

let porder = 
    ws 
    >>. choice [ 
            stringReturn "order asc" Asc
            stringReturn "order desc" Desc ]
    .>> ws

let v3 = 
    pipe5 
        paction pfrom pwhere (opt porder) (opt plimit)
        (fun act from where order limit -> 
            {
                Action  = act
                From    = from
                Where   = where
                Order   = order
                Limit   = limit
                Options = None
            })




let noconsistentRead = stringReturn "noconsistentread" NoConsistentRead

let openParen  = skipString "("
let closeParen = skipString ")"

// PageSize (42)
let queryPageSize  = 
    skipString "pagesize" 
    >>. openParen 
    >>. pint32 
    .>> closeParen 
    |>> QueryPageSize

// DynamoDB allows a-z, A-Z, 0-9, _, - and . in the index name
let isValidChar c = 
    isAsciiLetter c || isDigit c || c = '_' || c = '-' || c = '.'
let pindexName = 
    ws 
    >>. identifier (IdentifierOptions(isAsciiIdStart = isValidChar, isAsciiIdContinue = isValidChar)) 
    .>> ws
    
let pTrue, pFalse = stringReturn "true" true, stringReturn "false" false

// index (MyIndex, true)
let index = 
    skipString "index" 
    >>. openParen
    >>. (tuple3 pindexName comma (choice [ pTrue; pFalse ])
        |>> (fun (idxName, _, isAll) -> idxName, isAll))
    .>> closeParen
    |>> Index

let noReturnedCapacity = stringReturn "NoReturnedCapacity" QueryNoReturnedCapacity

let queryOption  = 
    choice [ 
        noconsistentRead
        queryPageSize
        index
        noReturnedCapacity ]
let queryOptions = sepBy1 queryOption comma |>> List.toArray

// with (noreturnedcapacity, noconsistentread, pagesize (42))
let pwith = 
    ws 
    >>. skipString "with" 
    >>. openParen 
    >>. queryOptions 
    .>> closeParen




// FParsec only supports up to pipe5, extend it by piping the result of the first 5 parser through a second pipe
let pipe6 (p1: Parser<'a,'u>) (p2: Parser<'b,'u>) (p3: Parser<'c,'u>) (p4: Parser<'d,'u>) (p5: Parser<'e,'u>) (p6: Parser<'f, 'u>) g =
    pipe2 (pipe5 p1 p2 p3 p4 p5 (fun a b c d e -> a, b, c, d, e)) p6 (fun (a, b, c, d, e) f -> g a b c d e f)

let tuple6 p1 p2 p3 p4 p5 p6 = pipe6 p1 p2 p3 p4 p5 p6 (fun a b c d e f -> (a, b, c, d, e, f))


let pquery : Parser<DynamoQuery> = 
    tuple6 paction pfrom pwhere (opt porder) (opt plimit) (opt pwith)
    |>> (fun (action, from, where, order, limit, with') -> 
        { 
            Action = action
            From = from
            Where = where
            Limit = limit
            Order = order
            Options = with' 
        })