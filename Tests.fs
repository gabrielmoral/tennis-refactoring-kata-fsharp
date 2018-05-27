module Tests

open System
open Xunit



[<Fact>]
let ``My test`` () =
    Assert.True(true)

type Difference = int

type PlayerScore = Love | Fifteen | Thirty | Forty | Advantage of Difference

type Score = 
    | LoveAll
    | FifteenAll
    | ThirtyAll
    | Deuce
    | Player1Wins
    | Player2Wins
    | Combined of PlayerScore * PlayerScore

type ScoreCalculation = 
    | Empate of Score
    | Other of PlayerScore * PlayerScore

let normalizePlayerScore p1 p2 = 
    match p1, p2 with
    | 0,_ -> Love
    | 1,_ -> Fifteen
    | 2,_ -> Thirty
    | 3,_ -> Forty
    | x, y when x <= y -> Forty
    | x, y when x > y -> Advantage (x - y)

let calculateScore a b : ScoreCalculation =
    match a, b with
    | Love, Love -> Empate LoveAll
    | Fifteen, Fifteen -> Empate FifteenAll
    | Thirty, Thirty -> Empate ThirtyAll
    | Forty, Forty -> Empate Deuce
    | _ -> Other (a , b)

let score a b =
    let p1Score = (normalizePlayerScore a b) 
    let p2Score = (normalizePlayerScore b a)

    let normalizedScore = calculateScore p1Score p2Score

    match normalizedScore with
    | Empate x -> x
    | Other (p1, p2)-> 
        match p1, p2 with
        | Advantage difference, _ when difference >= 2 -> Player1Wins
        | _, Advantage difference when difference >= 2 -> Player2Wins
        | _ -> Combined (p1, p2)