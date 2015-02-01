open System.Collections.Generic;

//the trie node type which is stores a char, a bool to mark the end of the node and a list of all it's subnodes
type TrieNode = TrieNode of (char * bool * TrieNode list) with
    //just a getter the char stored
    member this.Char = match this with TrieNode(c,weh,subnodes) -> c
    //get the first node that is a subnode of the current node and stores the char passed as parameter (c)
    //this returns a list which can be either empty or have one element this is probably not the best option since there will never be
    //more than one element in the list but I am new to F#
    member this.GetChild(c:char) = match this with TrieNode(c,weh,subnodes) ->  match List.tryFind (fun (this:TrieNode) -> this.Char = c) subnodes with
                                                                                    | Some value -> [value]
                                                                                    | None -> []
    //just return true if a word ends here
    member this.AWordEndsHere = match this with TrieNode(_,weh,_) -> weh          
    //just a getter for the the subnodes
    member this.Subnodes = match this with TrieNode(_,_,subnodes) -> subnodes         

//type members cannot be recursive so I implemented this in a module
//a more functional approach was to implement all methods here I guess ... anyways ...
module TrieFunctions = 
    //this inserts a word passed as a char list
    let rec insertWord (wordChars:char list) = function
        | TrieNode(c, weh, subnodes) as node ->
            if(wordChars.Length > 0) then
                //get the first char in the word and see if there are any child nodes storing that char
                let child = node.GetChild(wordChars.Head)
                if child = [] then 
                    //if there aren't insert this node and make a new node to pass to the insertion function
                    let newnode = TrieNode(wordChars.Head,false,[])
                    TrieNode(c,weh,(insertWord wordChars.Tail newnode)::subnodes )
                else
                    //otherwise the node that has to be passed to the insertion function is the child node as all other nodes will be 
                    //added after it and the first char in the word will be added
                    TrieNode(wordChars.Head,false,(insertWord wordChars.Tail child.Head)::subnodes )
            else
                //there are no more characters in the word so we must add a node with no subnodes and mark it as a valid end of a word
                TrieNode(c,true,[])
    //turn a string into a char list - ofSeq turns any sequence into a list of it's composing elements 
    //and a string is made up of chars
    let stringToCharList(s:string) = List.ofSeq s 
    //print the trie - use an accumulator(acc)
    let rec print acc = function
    //if there are no subnodes 
    | TrieNode(c, weh, []) -> 
        //add the current char node to the accumulator, print if it the end of a word and return unit
        let str = acc + c.ToString()
        if weh then printfn "%s" str
        ()
    | TrieNode(c, weh, subnodes) -> 
        //add the current char node to the accumulator, print if it the end of a word
        let str = acc + (if c.Equals(' ') then "" else c.ToString())
        if weh then printfn "%s" str
        //iterate through the list of subnodes and call print function for each of them
        List.iter (fun (node : TrieNode) -> print str node) subnodes
            
//unlike in C# in F# the type has a default constructor containing the maximum number of parameters
type Trie(inner : TrieNode) =
    member this.InsertWord(wordChars:char list) = Trie(TrieFunctions.insertWord wordChars inner)
    member this.InsertWord(str:string) = Trie(TrieFunctions.insertWord (TrieFunctions.stringToCharList str) inner)
    member this.Root() = inner
    //and an empty constructor must be declared if you want to have it
    new() = Trie(TrieNode(' ',false,List.empty))
 
let trie = Trie()
                .InsertWord("abc")
                .InsertWord("abcd")
                .InsertWord("abcd")
                .InsertWord("abcde")
                .InsertWord("abcdef")
                .InsertWord("ab123cd")
                .InsertWord("abc123d")
                .InsertWord("abc132d")

TrieFunctions.print "" (trie.Root())