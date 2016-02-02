// use let to bind values and functions to names
let x = 42

let multiply x y = x * y

// function arguments can be prefixed to create new function
let double = multiply 2

// lambdas everywhere
List.filter (fun n -> n % 2 = 0) [1..10]

// use pipes |> to chain nested functions calls
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

let johnAfterPromotion = promote 5000<Pounds> john


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