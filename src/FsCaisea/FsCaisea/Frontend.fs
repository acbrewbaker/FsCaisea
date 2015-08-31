module FsCaisea.Frontend

open System
open System.IO
open System.Text

type ICode = int
type SymTab = int

type [<AbstractClass>] Scanner(source:Source) =
    member val CurrentToken = { new Token(source) with member t.Extract() = t.Extract() } with get, set
    member s.NextToken() = s.CurrentToken <- s.ExtractToken()
    abstract ExtractToken : unit -> Token
    member  s.CurrentChar() = source.CurrentChar
    member s.NextChar() = source.NextChar()

and [<AbstractClass>] Parser(scanner:Scanner) =
    let messageHandler = Message.MessageHandler() 
    interface Message.IMessageProducer with
        member p.AddMessageListener ml = messageHandler.
    abstract iCode : ICode
    abstract symTab : SymTab
    abstract Parse : unit -> unit
    abstract GetErrorCount : unit -> int
    member p.CurrentToken() = scanner.CurrentToken
    member p.NextToken() = scanner.NextToken()
    


and [<AbstractClass>] Token(source:Source) as t =
    let ty : TokenType option = None
    let source = source
    let lineNum = source.LineNum
    let position = source.CurrentPos
    let mutable text = ""
    let mutable value : obj option = None

    let currentChar() = source.CurrentChar()

    let nextChar() = source.NextChar()
    
    do t.Extract() |> ignore
    
    abstract Extract : unit -> char
    default t.Extract() =
        text <- currentChar() |> string
        value <- None
        nextChar()

    member t.PeekChar() = source.PeekChar()

    

and TokenType = int

and EofToken(source) =
    inherit Token(source)

    member et.Extract(source) = ()


and Source(reader:StringReader) =
    let EOL = '\n'
    let EOF = char 0
    let mutable line, lineNum, currentPos = "", 0, -2

    let readLine() = 
        line <- reader.ReadLine()
        currentPos <- -1
        if line <> "" then lineNum <- lineNum + 1
        
    member val Reader = reader
    member val LineNum = 0
    member val CurrentPos = -2 with get, set
    
    member s.CurrentChar() = 
        if s.CurrentPos = -2 
        then readLine(); s.NextChar()
        elif line = String.Empty then EOF
        elif (s.CurrentPos = -1) || (s.CurrentPos = line.Length) then EOL
        elif s.CurrentPos > line.Length then readLine(); s.NextChar()
        else line.[s.CurrentPos]

    member s.NextChar() = s.CurrentPos <- s.CurrentPos + 1; s.CurrentChar()
    member s.PeekChar() =
        s.CurrentChar() |> ignore
        match line with 
        | "" -> EOF
        | _ ->
            let nextPos = s.CurrentPos + 1
            if nextPos < line.Length then line.[nextPos] else EOL
    
    member s.Close() = reader.Close()

