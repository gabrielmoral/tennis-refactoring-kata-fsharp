module Tests

type PlayerScore = Love | Fifteen | Thirty | Forty | Advantage | Winner

type Draws =  LoveAll | FifteenAll | ThirtyAll | Deuce

type Score = 
    | Draw of Draws
    | Player1Wins
    | Player2Wins
    | Other of PlayerScore * PlayerScore 

let (|Even|Odd|) input input2 = if input % 2 = 0 then Even else Odd

let normalizePlayerScore p1Points p2Points = 

    let calculate p1 p2 = 
        match p1 - p2 with
        | x when x <= 0 -> Forty
        | x when x = 1 -> Advantage
        | x when x >= 2 -> Winner
        
    match p1Points with
    | 0 -> Love
    | 1 -> Fifteen
    | 2 -> Thirty
    | 3 -> Forty
    | _ -> calculate p1Points p2Points

let normalizeMatchScore p1Score p2Score = (normalizePlayerScore p1Score p2Score) , (normalizePlayerScore p2Score p1Score)

let calculateScore matchScore : Score =
    match matchScore with
    | Love, Love        -> Draw LoveAll
    | Fifteen, Fifteen  -> Draw FifteenAll
    | Thirty, Thirty    -> Draw ThirtyAll
    | Forty, Forty      -> Draw Deuce
    | Winner, _         -> Player1Wins
    | _, Winner         -> Player2Wins
    | _                 -> Other matchScore

let score p1Points p2Points = normalizeMatchScore p1Points p2Points |> calculateScore