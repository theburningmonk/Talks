module DynamoDB.SQL.Tests

open FsUnit
open NUnit.Framework
open DynamoDB.SQL
open FParsec

let parseDynamoQuery str = 
    match run pquery str with
    | Success (x, _, _) -> x

[<Test>]
let ``when there is white spaces around the attribute names and table name they should be ignored`` () =
    let select = "SELECT Name,    Age,
                            Salary
                    FROM   Employees 
                    WHERE FirstName   =      \"Yan\"
                    LIMIT     5"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Attribute "Name"; Attribute "Age"; Attribute "Salary" ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Limit  |> should equal <| Some(Limit 5)

[<Test>]
let ``when the SELECT, FROM, WHERE and LIMIT keywords are not in capitals they should still be parsed correctly`` () =
    let select = "sELeCT Name, Age, Salary
                    FrOm Employees
                    where FirstName = \"Yan\"
                    liMIt 5"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Attribute "Name"; Attribute "Age"; Attribute "Salary" ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Limit  |> should equal <| Some(Limit 5)

[<Test>]
let ``when there are multiple conditions in the where clause they should all be parsed`` () =
    let select = "SELECT * FROM Employees 
                  WHERE FirstName = \"Yan\" 
                  AND LastName = \"Cui\""

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal(S "Yan")); (Attribute "LastName", Equal(S "Cui")) ]
    query.Limit  |> should equal <| None

[<Test>]
let ``when < operator is used it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age < 99"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", LessThan (NBigInt 99I)) ]
    query.Limit  |> should equal <| None

[<Test>]
let ``when <= operator is used it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age <= 99"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", LessThanOrEqual (NBigInt 99I)) ]
    query.Limit  |> should equal <| None

[<Test>]
let ``when > operator is used it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age > 99"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", GreaterThan (NBigInt 99I)) ]
    query.Limit  |> should equal <| None

[<Test>]
let ``when >= operator is used it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age >= 99"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", GreaterThanOrEqual (NBigInt 99I)) ]
    query.Limit  |> should equal <| None

[<Test>]
let ``when the Begins With operator is used it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND LastName BEGINS WITH \"Cui\""

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "LastName", BeginsWith (S "Cui")) ]
    query.Limit  |> should equal <| None
    
[<Test>]
let ``when the Between operator is used it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age BETWEEN 10 AND 30"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", Between ((NBigInt 10I), (NBigInt 30I))) ]
    query.Limit  |> should equal <| None

[<Test>]
let ``when limit clause is specified, it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age >= 30 LIMIT 10"

    let query = parseDynamoQuery select
            
    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", GreaterThanOrEqual(NBigInt 30I)) ]
    query.Limit  |> should equal <| Some(Limit 10)

[<Test>]
let ``when order asc is specified, it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age >= 30 ORDER ASC LIMIT 10"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", GreaterThanOrEqual(NBigInt 30I)) ]
    query.Limit  |> should equal <| Some(Limit 10)
    query.Order  |> should equal <| Some(Asc)

[<Test>]
let ``when order desc is specified, it should be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" AND Age >= 30 ORDER DESC LIMIT 10"

    let query = parseDynamoQuery select

    query.Action |> should equal <| Select [ Asterisk ]
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", GreaterThanOrEqual(NBigInt 30I)) ]
    query.Limit  |> should equal <| Some(Limit 10)
    query.Order  |> should equal <| Some(Desc)

[<Test>]
let ``when a count query is specified, it should be parsed correctly`` () =
    let count = "COUNT * FROM Employees WHERE FirstName = \"Yan\" AND Age >= 30 ORDER DESC LIMIT 10"

    let query = parseDynamoQuery count

    query.Action |> should equal <| Count
    query.From   |> should equal <| From "Employees"
    query.Where  |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")); (Attribute "Age", GreaterThanOrEqual(NBigInt 30I)) ]
    query.Limit  |> should equal <| Some(Limit 10)
    query.Order  |> should equal <| Some(Desc)

[<Test>]
let ``when NoConsistentRead option is specified it should be captured in the Options clause`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" WITH (  nOConsiStentRead )"
        
    let query = parseDynamoQuery select

    query.Action  |> should equal <| Select [ Asterisk ]
    query.From    |> should equal <| From "Employees"
    query.Where   |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Options |> should equal <| Some [| NoConsistentRead |]

[<Test>]
let ``when PageSize option is specified it should be captured in the Options clause`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" WITH (Pagesize(  10) )"
        
    let query = parseDynamoQuery select

    query.Action  |> should equal <| Select [ Asterisk ]
    query.From    |> should equal <| From "Employees"
    query.Where   |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Options |> should equal <| Some [| QueryPageSize 10 |]

[<Test>]
let ``when Index option is specified with AllAttributes set to true it should be captured in the Options clause`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" WITH (Index( _M-y.1nd3x ,  true) )"
        
    let query = parseDynamoQuery select

    query.Action  |> should equal <| Select [ Asterisk ]
    query.From    |> should equal <| From "Employees"
    query.Where   |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Options |> should equal <| Some [| Index("_M-y.1nd3x", true) |]

[<Test>]
let ``when Index option is specified with AllAttributes set to false it should be captured in the Options clause`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" WITH (Index( _M-y.1nd3x ,  false) )"
        
    let query = parseDynamoQuery select

    query.Action  |> should equal <| Select [ Asterisk ]
    query.From    |> should equal <| From "Employees"
    query.Where   |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Options |> should equal <| Some [| Index("_M-y.1nd3x", false) |]

[<Test>]
let ``when NoReturnedCapacity option is specified it should be captured in the Options clause`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" WITH ( NoReturnedCapacity)"
        
    let query = parseDynamoQuery select

    query.Action  |> should equal <| Select [ Asterisk ]
    query.From    |> should equal <| From "Employees"
    query.Where   |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Options |> should equal <| Some [| QueryNoReturnedCapacity |]

[<Test>]
let ``when both NoConsistentRead and PageSize options are specified they should be captured in the Options clause`` () =
    let select = "SELECT * FROM Employees WHERE FirstName = \"Yan\" WITH ( NOconsistentRead, Pagesize(  10) )"
        
    let query = parseDynamoQuery select

    query.Action  |> should equal <| Select [ Asterisk ]
    query.From    |> should equal <| From "Employees"
    query.Where   |> should equal <| Where [ (Attribute "FirstName", Equal (S "Yan")) ]
    query.Options |> should equal <| Some [| NoConsistentRead; QueryPageSize 10 |]
            
[<Test>]
let ``when the attribute value is a very long number it should be parsed without losing precision`` () =
    // from issue #29
    let select = "SELECT * FROM pd_cs_friendshiprequest WHERE ForUserId = 10205471065416239"
        
    let query = parseDynamoQuery select
    query.Where |> should equal <| Where [ (Attribute "ForUserId", Equal (NBigInt 10205471065416239I))]

    // where the attribute value is > int64 but < uint64
    let select = "SELECT * FROM pd_cs_friendshiprequest WHERE ForUserId = 9223372036854775809"

    let query = parseDynamoQuery select
    query.Where |> should equal <| Where [ (Attribute "ForUserId", Equal (NBigInt 9223372036854775809I))]

    // where the attribute value is negative
    let select = "SELECT * FROM pd_cs_friendshiprequest WHERE ForUserId = -9223372036854775808"

    let query = parseDynamoQuery select
    query.Where |> should equal <| Where [ (Attribute "ForUserId", Equal (NBigInt -9223372036854775808I))]

[<Test>]
let ``when the attribute value is a small number it should still be parsed correctly`` () =
    let select = "SELECT * FROM Employees WHERE Id = 3"

    let query = parseDynamoQuery select
    query.Where |> should equal <| Where [ (Attribute "Id", Equal (NBigInt 3I))]

    let select = "SELECT * FROM Employees WHERE Id = -3"

    let query = parseDynamoQuery select
    query.Where |> should equal <| Where [ (Attribute "Id", Equal (NBigInt -3I))]

[<Test>]
let ``when the attribute value is a decimal it should be parsed as NDouble`` () =
    let select = "SELECT * FROM Employees WHERE Id = 42.32"

    let query = parseDynamoQuery select
    query.Where |> should equal <| Where [ (Attribute "Id", Equal (NDouble 42.32))]