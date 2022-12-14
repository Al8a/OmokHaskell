module Main where 
    import System.IO 
    import Board
    
    {- 
    Alan Ochoa  
    Diana Garcia 
    PROJECT 3: FUNCTIONAL PROGRAMMING WITH HASKELL
    CS-3360 Design/Implementation Prog Lan -} 
    ------------------------------------------------------------------------------------
    -- Part 1: Reading user inputs and printing outputs 
    
    {-
    Return a character representation of a player p. It returns a Char value. 
    This function may be used to print the current state of a board. -}
    playerToChar :: Int -> Char
    playerToChar p
        | p == 1 = 'O'      -- Player
        | p == 2 = 'X'      -- Player 2 / Opponent / [Computer]
        | otherwise = '.'   -- Empty space


    {-
    Read a 1-based pair of indices (x, y) for player p, denoting an unmarked place in a board bd. 
    The function reads inputs from the standard input (stdin) 
    returns an IO value such as IO(Int,Int) or IO(Integer,Integer). -}
    readXY :: [[Int]] -> Int -> IO(Int, Int)
    readXY bd p = do
        putStrLn (boardToStr playerToChar bd)
        putStr "Enter x and y (1-15, e.g., 8 10 | Enter -1 to quit)? " 
        line <- getLine 

        let parsed = reads line :: [(Int, String)] in 
            if null parsed then helperreadXY                        -- empty input detected
            else let (x,_) = head parsed in
                if x == -1 then error "Quiting game..."             -- quit
                else if x > 0 && x <= size bd then                  -- bound check 
                    let parsed = reads (tail(tail line)) :: [(Int, String)] in     
                    if null parsed then helperreadXY                -- empty input detected
                    else let (y,_) = head parsed in
                        if y == -1 then error "Quiting game..."     -- quit
                        else if y > 0 && y <= size bd && isEmpty x y bd then return (x,y)
                        else helperreadXY
                else helperreadXY                                   -- invaild input
        where 
            helperreadXY = do
                putStrLn "Invalid input!"
                readXY bd p
    

    ------------------------------------------------------------------------------------
    -- Part 2 Playing the game 

    {-
    Main function to play an omok game between two human players.
    It returns an IO() value. 
    The dimension of the board is 15x15
    User inputs are read from the standard input
    Outputs the board, state the game outcome are printed on standard output. -}
    main :: IO ()
    main =  do
        let bd = mkBoard 15
        mainHelper bd
        where
            mainHelper :: [[Int]] -> IO()
            mainHelper bd = do 
                playerCoordinates <- readXY bd 1
                let playerMove = uncurry mark playerCoordinates bd 1 
                putStrLn (boardToStr playerToChar playerMove)

                if isWonBy playerMove 1 then do
                    putStrLn "Player 1 - 'O' | Wins!"
                    return ()

                else do
                    opponentCoordinates <- readXY playerMove 2
                    let opponentMove = uncurry mark opponentCoordinates playerMove 2
                    putStrLn (boardToStr playerToChar opponentMove)

                    if isWonBy opponentMove 2 then do
                        putStrLn "Player 2 - 'X' | Wins!"
                        return ()
                    
                    else if isDraw opponentMove then do
                        putStrLn "Game Draw!"
                        return ()
                    
                    else do
                        mainHelper opponentMove