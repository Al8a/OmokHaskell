module Main where 
    import Game
    import System.IO
    import System.Random 
    
    {-Main function to play an omok game between two human players.
    It returns an IO() value. 
    The dimension of the board is 15x15, and user inputs are read from the standard input 
    (see the readXY function below) and outputs like the board state and the game
    outcome are printed on the standard output.-}
    main :: IO ()
    main = do
        let bd = mkBoard 15
        putStrLn (boardToStr playerToChar bd)
        mainHelper bd

   
    mainHelper :: [[Int]] -> IO()
    mainHelper bd = do 
        playerCoordinates <- readXY bd 1
        let bdPlayerMove = mark (fst playerCoordinates) (snd playerCoordinates) bd 1
        putStrLn (boardToStr playerToChar bdPlayerMove)
        if isWonBy bdPlayerMove 1 then do
            putStrLn "Player 1 is the winner!"
            return ()
        else do
            opponentCoordinates <- readXY bdPlayerMove 2
            let bdOpponentMove = mark (fst opponentCoordinates) (snd opponentCoordinates) bdPlayerMove 2
            putStrLn (boardToStr playerToChar bdOpponentMove)
            if isWonBy bdOpponentMove 2 then do
                putStrLn "Player 2 is the winner!"
                return ()
            else if isDraw bdOpponentMove then do
                putStrLn "Game is a draw!"
                return ()
            else do
                mainHelper  bdOpponentMove
    
    {-Return a character representation of a player p. It returns a Char
    value. This function may be used to print the current state of
    a board (see the boardToStr function in Part I). -}
    playerToChar :: Int -> Char
    playerToChar p
        | p == 1 = 'O'      -- Player
        | p == 2 = 'X'      -- Computer
        | otherwise = '.'   -- Empty space

    {-Read a 1-based pair of indices (x, y) for player p, denoting an 
    unmarked place in a board bd. The function reads inputs from the
    standard input (stdin) and returns an IO value such as IO(Int,Int)
    or IO(Integer,Integer). -}
    readXY :: [[Int]] -> Int -> IO(Int, Int)
    readXY bd p = do
        putStrLn ("Player " ++ show p ++ ", enter x and y coordinates separated by spaces (must be >= 1 and <= " ++ show(size bd) ++ "): ")
        line <- getLine
        let enteredValues = map read (words line)
        if length enteredValues /= 2 || enteredValues !! 0 < 1 || enteredValues!!0 > (size bd) || enteredValues!!1 < 0 || enteredValues!!1 > (size bd) then do
            putStrLn "Invalid input!"
            readXY bd p
        else if isMarked (enteredValues !! 0) (enteredValues !! 1) bd then do
            putStrLn "Place is not empty!"
            readXY bd p
        else
            return (enteredValues !! 0, enteredValues !! 1)
    


    