let commonPrefix (strs: string list) =
    let dtorStr (str: string) =
        let chars = str |> Seq.toList
        chars.Head, str.Substring 1

    let rec aux (prefix: char list) strs =
        let nextStrs = strs |> List.map dtorStr

        let pick = fst nextStrs.Head
        if List.forall (fst >> (=) pick) nextStrs then
            if List.exists (snd >> String.length >> (=) 0) nextStrs then
                pick :: prefix
            else
                aux (pick :: prefix) (nextStrs |> List.map snd)
        else prefix
    
    aux [] strs 
    |> Seq.ofList
    |> Seq.map string
    |> Seq.rev
    |> String.concat ""