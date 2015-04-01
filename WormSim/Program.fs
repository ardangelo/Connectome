// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Connectome
open System
open System.IO
open System.Collections.Generic

[<EntryPoint>]
let main argv = 
    System.Console.CancelKeyPress.Add(fun _ -> printfn "exiting...")
    
    let file = File.ReadAllLines("../../CElegansConnectome.csv")
    let c = new Dictionary<string, Neuron list>()

    file |> Array.iter (fun line ->
        let [|pre; post; weight|] = line.Split(',')
        if not (c.ContainsKey pre) then
            c.Add(pre, List.Empty)
        c.Item(pre) <- (post, Int32.Parse(weight))::c.Item(pre))

    let agent : MailboxProcessor<string> = MailboxProcessor.Start(fun inbox ->
        let rec messageLoop = async {
            let! msg = inbox.Receive()
            printfn "%s" msg
            return! messageLoop
        }

        messageLoop
    )

    let connectome = new Connectome(15, c, agent)
    
    let rec stimulate () =
        printf "Enter neuron (or ENTER to exit): "
        let n = Console.ReadLine()
        if not (n = String.Empty) then
            if connectome.Connectome.ContainsKey n then
                connectome.stimulate n
            stimulate()
    stimulate()

    0 // return an integer exit code
