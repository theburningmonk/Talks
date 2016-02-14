// use let to bind values to names
// everything is immutable by default
let x = 42

// mutable states requires extra 'mutable' keyword
let mutable y = 42
y <- 24 
// note <- is different from assignment operator = 
// as it's a "destructive assignment"

// also use 'let' to bind functions to names
let inline multiply x y = x * y

// type inference works out types of 'x' and 'y'

// function arguments can be fixed to create new function
// aka partial application
let double = multiply 2

// lambdas
List.filter (fun n -> n % 2 = 0) [1..10]

// use pipes |> to chain nested functions calls
[1..10] |> List.filter (fun n -> n % 2 = 0)

// you can nest many function calls this way
let termFrequency words =
    words
    |> Seq.groupBy id
    |> Seq.map (fun (word, seq) -> word, Seq.length seq)
    |> Seq.sortByDescending (fun (_, n) -> n)
    |> Seq.iter (fun (word, n) -> printfn "%s - %d" word n)

// did you notice we used pattern matching against the tuple?
// no need for Tuple.Create... & Tuple.Item1, Tuple.Item2, etc.
let a, b = 42, "42"

// also whitespace matters for compilation too
// but we already use whitespace to align our code for readibility
// so why force {} & whitespaces when whitespaces can do both?


// you can have classes, & interfaces, etc. too
type IPerson = 
    abstract member Name : string

type Person (name:string) =
    interface IPerson with
        member __.Name = name


[<Measure>]
type Pence

[<Measure>]
type Pounds

// fields are immutable by default
type Employee =
    {
        Name   : string
        Salary : int<Pounds>
    }

let john = { Name = "John Doe"; Salary = 40000<Pounds> }

let promote raise employee = 
    { employee with Salary = employee.Salary + raise }

let johnAfterPromotion = promote 5000<Pence> john


// union types, far more powerful than enums
type PaymentMethod =
    | Cash
    | CreditCard of int64
    | Paypal     of string

let processPayment paymentMethod (amount : int<Pounds>) = 
    match paymentMethod with
    | Cash -> 
        printfn "paid £%d in cash" amount
    | CreditCard cardNumber -> 
        printfn "paid £%d with credit card [%d]" amount cardNumber
    | Paypal email ->
        printfn "paid £%d with PayPal [%s]" amount email

processPayment Cash 500<Pounds>
processPayment (Paypal "john.doe@gmail.com") 42<Pounds>

// so much we haven't touched on..
// type providers, async, comp expressions, etc.