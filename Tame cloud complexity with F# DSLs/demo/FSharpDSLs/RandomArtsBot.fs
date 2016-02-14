module RandomArtsBot

open FParsec
open FParsec.Internals

type Expr =
    | VariableX
    | VariableY
    | Constant

    | Add      of Expr * Expr
    | Subtract of Expr * Expr
    | Product  of Expr * Expr
    | Divide   of Expr * Expr

    | Max      of Expr * Expr
    | Min      of Expr * Expr
    | Average  of Expr * Expr

    | Mod      of Expr * Expr
    | Well     of Expr
    | Tent     of Expr

    | Sin of Expr
    | Cos of Expr
    | Tan of Expr

    | Sqr  of Expr
    | Sqrt of Expr

    | Level of Expr * Expr * Expr
    | Mix   of Expr * Expr * Expr

    override x.ToString() =
        match x with
        | VariableX -> "x"
        | VariableY -> "y"
        | Constant  -> "const"

        | Add      (e1, e2) -> sprintf "(+ %O %O)" e1 e2
        | Subtract (e1, e2) -> sprintf "(- %O %O)" e1 e2
        | Product  (e1, e2) -> sprintf "(* %O %O)" e1 e2
        | Divide   (e1, e2) -> sprintf "(/ %O %O)" e1 e2
        | Max      (e1, e2) -> sprintf "(max %O %O)" e1 e2
        | Min      (e1, e2) -> sprintf "(min %O %O)" e1 e2
        | Average  (e1, e2) -> sprintf "(avg %O %O)" e1 e2
        | Mod      (e1, e2) -> sprintf "(mod %O %O)" e1 e2

        | Well e -> sprintf "(well %O)" e
        | Tent e -> sprintf "(tent %O)" e
        | Sin  e -> sprintf "(sin %O)" e
        | Cos  e -> sprintf "(cos %O)" e
        | Tan  e -> sprintf "(tan %O)" e
        | Sqr  e -> sprintf "(sqr %O)" e
        | Sqrt e -> sprintf "(sqrt %O)" e

        | Level (e1, e2, e3) -> sprintf "(lvl %O %O %O)" e1 e2 e3 
        | Mix   (e1, e2, e3) -> sprintf "(mix %O %O %O)" e1 e2 e3



type Parser<'t> = Parser<'t, unit>

// abbreviations
let ws = spaces     // eats any whitespace

// shadow functions to make them ignore whitespace
let skipStringCI s     = skipStringCI s .>> ws
let stringCIReturn s r = stringCIReturn s r .>> ws

let openParen  = skipStringCI "("
let closeParen = skipStringCI ")"

let (expr : Parser<Expr>), exprImpl = createParserForwardedToRef()

let variableX = stringCIReturn "x" VariableX
let variableY = stringCIReturn "y" VariableY
let constant  = stringCIReturn "const" Constant

let unaryOp symbol op =
    openParen >>. (skipStringCI symbol) >>. expr .>> closeParen |>> op

let binaryOp symbol op =
    pipe5
        openParen (skipStringCI symbol) expr expr closeParen
        (fun _ _ e1 e2 _ -> op (e1, e2))


// FParsec only supports up to pipe5, extend it by piping the result of 
// the first 5 parser through a second pipe
let pipe6 
        (p1: Parser<'a,'u>) 
        (p2: Parser<'b,'u>) 
        (p3: Parser<'c,'u>) 
        (p4: Parser<'d,'u>) 
        (p5: Parser<'e,'u>) 
        (p6: Parser<'f,'u>) 
        map =
    pipe2 
        (pipe5 p1 p2 p3 p4 p5 (fun a b c d e -> a, b, c, d, e)) 
        p6 
        (fun (a, b, c, d, e) f -> map a b c d e f)

let ternaryOp symbol op =
    pipe6
        openParen (skipStringCI symbol) expr expr expr closeParen
        (fun _ _ e1 e2 e3 _ -> op (e1, e2, e3))

let sin = unaryOp "sin" Sin
let cos = unaryOp "cos" Cos
let tan = unaryOp "tan" Tan

let sqr  = unaryOp "sqr" Sqr
let sqrt = unaryOp "sqrt" Sqrt

let mod' = binaryOp "mod" Mod
let well = unaryOp "well" Well
let tent = unaryOp "tent" Tent

let max = binaryOp "max" Max
let min = binaryOp "min" Min
let avg = binaryOp "avg" Average

let add  = binaryOp "+" Add
let sub  = binaryOp "-" Subtract
let prod = binaryOp "*" Product
let div  = binaryOp "/" Divide
        
let lvl = ternaryOp "lvl" Level
let mix = ternaryOp "mix" Mix

// NOTE : this is rather inefficient with lots of backtracking
do exprImpl := 
    choice [ 
        attempt variableX 
        attempt variableY
        attempt constant 
        attempt sin
        attempt cos
        attempt tan
        attempt sqr
        attempt sqrt
        attempt mod' 
        attempt well
        attempt tent
        attempt max
        attempt min
        attempt avg
        attempt add
        attempt sub
        attempt prod
        attempt div
        attempt lvl
        attempt mix
    ]

let parse text = 
    match run expr text with
    | Success (result, _, _) -> result