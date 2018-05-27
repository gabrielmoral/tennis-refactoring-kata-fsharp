module Tests

open System
open Xunit



[<Fact>]
let ``My test`` () =
    Assert.True(true)

type Difference = int

type PlayerScore = Love | Fifteen | Thirty | Forty | Advantage | Winner

type Draws = 
    | LoveAll
    | FifteenAll
    | ThirtyAll
    | Deuce

type Score = 
    | Draw of Draws
    | Player1Wins
    | Player2Wins
    | Other of PlayerScore * PlayerScore 

let normalizePlayerScore p1 p2 = 
    match p1, p2 with
    | 0,_ -> Love
    | 1,_ -> Fifteen
    | 2,_ -> Thirty
    | 3,_ -> Forty
    | x, y when x <= y -> Forty
    | x, y when x > y -> Advantage
    | x, y when (x - y) >= 2 -> Winner

let calculateScore a b : Score =
    match a, b with
    | Love, Love        -> Draw LoveAll
    | Fifteen, Fifteen  -> Draw FifteenAll
    | Thirty, Thirty    -> Draw ThirtyAll
    | Forty, Forty      -> Draw Deuce
    | Winner, _         -> Player1Wins
    | _, Winner         -> Player2Wins
    | _                 -> Other (a , b)

let score a b =
    let p1Score = (normalizePlayerScore a b) 
    let p2Score = (normalizePlayerScore b a)

    calculateScore p1Score p2Score