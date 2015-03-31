namespace Connectome

open System.Collections.Generic

type Neuron = (string * int)

type Connectome(threshold, connectome, client) = 
    member this.Threshold = threshold
    member this.Connectome : Dictionary<string, Neuron list> = connectome
    member this.Client : MailboxProcessor<Neuron> = client

    member this.inbox = MailboxProcessor<Neuron>.Start(fun agent -> 
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
                        this.Client.Post((n, w))
                        fireAndUpdate (postsynaptic.Add(n, 0)) ps
                    else 
                        fireAndUpdate ((dendriteAccumulate postsynaptic n).Add(n, 0)) ps
                |   [] -> postsynaptic

            (dendriteAccumulate postsynaptic neuron) |> Map.filter (fun n w -> w > this.Threshold) |> Map.toList |> fireAndUpdate postsynaptic

        let runConnectome postsynaptic (neuron, weight) =
            let rec fireAndUpdate postsynaptic postsynaccae =
                match postsynaccae with
                |   (n, w)::ps ->
                    fireAndUpdate ((fireNeuron postsynaptic n).Add(n, 0)) ps
                |   [] -> postsynaptic

            (dendriteAccumulate postsynaptic neuron) |> Map.filter (fun n w -> w > this.Threshold) |> Map.toList |> fireAndUpdate postsynaptic

        let rec loop postsynaptic = async {
            let! msg = agent.Receive()
            return! loop (runConnectome postsynaptic msg) }

        loop Map.empty<string, int>)