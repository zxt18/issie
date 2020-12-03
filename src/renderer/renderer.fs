module Renderer


open Fable.SimpleJson



    module TestJson =

        type Component = { H : int }


    let checkJson () =
        let json = """{"H":10}"""

        Json.tryParseAs<TestJson.Component> json // error
        |> printfn "\n\nParsing Renderer.TestJson: \n%A\n"
        Json.tryParseAs<CommonTypes.TestJson.Component> json
        |> printfn "\n\nParsing CommonTypes.TestJson: \n%A\n"
 

printfn "Starting renderer..."

checkJson()

printfn "done"

