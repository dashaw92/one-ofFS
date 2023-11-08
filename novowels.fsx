let flip f a b = f b a

let filterVowels = 
    let vowels = ['a'; 'A'; 'e'; 'E'; 'i'; 'I'; 'o'; 'O'; 'u'; 'U']
    flip List.contains vowels >> not

let noVowels = String.filter filterVowels