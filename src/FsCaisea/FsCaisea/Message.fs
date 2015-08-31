module FsCaisea.Message

open System

type Message() =
    member val x = ""

[<CustomComparison; CustomEquality>]
type IMessageListener =
    {   mutable Id : string
        mutable MessageReceived : unit -> unit }

    interface IComparable<IMessageListener> with
        member ml.CompareTo { Id = id } = ml.Id.CompareTo id

    interface IComparable with
        member ml.CompareTo obj =
            match obj with
            | :? IMessageListener as other -> (ml :> IComparable<_>).CompareTo other
            | _ -> invalidArg "obj" "not a imessagelistener"
    
    interface IEquatable<IMessageListener> with
        member ml.Equals { Id = id } = ml.Id = id

    override ml.Equals obj =
        match obj with
        | :? IMessageListener as other -> (ml :> IEquatable<_>).Equals other
        | _ -> invalidArg "obj" "not a imessagelistener"

    override ml.GetHashCode() = ml.Id.GetHashCode()

type IMessageProducer =
    abstract member AddMessageListener : IMessageListener -> unit
    abstract member RemoveMessageListener : IMessageListener -> unit
    abstract member SendMessage : Message -> unit
    
type MessageHandler() =
    let mutable listeners = Set.empty<IMessageListener>
    let message = Message()

    member mh.AddListener ml = listeners <- Set.add ml listeners
    member mh.RemoveListener ml = Set.remove ml listeners

