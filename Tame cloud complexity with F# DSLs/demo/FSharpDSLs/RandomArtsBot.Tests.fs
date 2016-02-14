module RandomArtsBotTests

open FsUnit
open NUnit.Framework
open FsCheck

open RandomArtsBot

module ``Parser tests`` =
    [<Test>]
    let ``expressions generated from Expr should be parsed to the same Expr`` () = 
        let property (expr : Expr) =
            let expr' = expr.ToString() |> parse
            expr' = expr

        Check.QuickThrowOnFailure property