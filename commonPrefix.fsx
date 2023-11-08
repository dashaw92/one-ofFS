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

let commonPrefix2 (strs: string list) =
    let rec aux prefix strs =
        if List.length strs = 0 || List.exists (List.length >> (=) 0) strs then prefix
        else
            let pick = List.head <| List.head strs
            if List.forall (List.head >> (=) pick) strs then
                aux (pick :: prefix) (strs |> List.map List.tail)
            else prefix

    let strs = List.map Seq.toList strs
    aux [] strs 
    |> List.rev
    |> Array.ofList
    |> System.String.Concat