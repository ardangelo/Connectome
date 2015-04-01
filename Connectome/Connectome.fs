namespace Connectome

open System.Collections.Generic

type Neuron = (string * int)

type Connectome(threshold, connectome, client) = 
    member this.Threshold = threshold
    member this.Connectome : Dictionary<string, Neuron list> = connectome
    member this.Client : MailboxProcessor<string> = client

    member this.runConnectome postsynaptic neuron =
        let dendriteAccumulate postsynaptic neuron =
            let rec update (postsynaptic : Map<string, int>) postsynaccae =
                match postsynaccae with
                |   (n, w)::ps ->
                    if not (postsynaptic.ContainsKey n) then 
                        update (postsynaptic.Add(n, w)) ps
                    else update (postsynaptic.Add(n, postsynaptic.Item(n) + w)) ps
                |   [] -> postsynaptic
            
            this.Connectome.Item(neuron) |> update postsynaptic

        let fireNeuron postsynaptic neuron =
            let rec fireAndUpdate (postsynaptic : Map<string, int>) postsynaccae =
                match postsynaccae with
                |   (n:string, w:int)::ps ->
                    if ((n.Substring(0,2)) = "MV" || (n.Substring(0,2)) = "MD") then
                        this.Client.Post(sprintf "Muscle fired: %s %i" n w)
                        fireAndUpdate (postsynaptic.Add(n, 0)) ps
                    else 
                        fireAndUpdate ((dendriteAccumulate postsynaptic n).Add(n, 0)) ps
                |   [] -> postsynaptic

            (dendriteAccumulate postsynaptic neuron) |> Map.filter (fun n w -> (abs w) > this.Threshold) |> Map.toList |> fireAndUpdate postsynaptic
        
        let rec fireAndUpdate postsynaptic postsynaccae =
            match postsynaccae with
            |   (n, w)::ps ->
                let postsynapticafterfiring = (fireNeuron postsynaptic n).Add(n, 0)
                fireAndUpdate postsynapticafterfiring ps
            |   [] -> postsynaptic
        
        let firedNeurons = (dendriteAccumulate postsynaptic neuron) |> Map.filter (fun n w -> (abs w) > this.Threshold)
        firedNeurons |> Map.toList |> fireAndUpdate postsynaptic

    member this.mailbox = MailboxProcessor<Neuron>.Start(fun inbox -> 
        let rec loop postsynaptic = async {
            let! (n, w) = inbox.Receive()
            let postsynaptic' = this.runConnectome postsynaptic n
            return! loop postsynaptic' }

        loop Map.empty<string, int>)
    
    member this.stimulate(n) =
        this.mailbox.Post (n, 0)