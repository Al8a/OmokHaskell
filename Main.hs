module Main where 
    import System.IO 
    import Board

    ------------------------------------------------------------------------------------
    -- Part 1: Reading user inputs and printing outputs 
    
    {-
    Return a character representation of a player p. It returns a Char value. 
    This function may be used to print the current state of a board. -}
    playerToChar :: Int -> Char
    playerToChar p
        | p == 1 = 'O'      -- Player
        | p == 2 = 'X'      -- Computer
        | otherwise = '.'   -- Empty space


    {-
    Read a 1-based pair of indices (x, y) for player p, denoting an unmarked place in a board bd. 
    The function reads inputs from the standard input (stdin) 
    returns an IO value such as IO(Int,Int) or IO(Integer,Integer). -}
    readXY :: [[Int]] -> Int -> IO(Int, Int)
    readXY bd p = do
        print (size bd)
        putStrLn ("Player " ++ show p ++ " turn: enter x y coordinates (1-15 or -1 to quit)?") 
        line <- getLine

        let enteredCoordinates = map read (words line)
        
        if length enteredCoordinates /= 2 || head enteredCoordinates  < 1 || head enteredCoordinates > size bd || enteredCoordinates !! 1 < 0 || enteredCoordinates !! 1 > size bd then 
            do
                putStrLn "Invalid input!"
                readXY bd p
        
        else if isMarked (head enteredCoordinates) (tail enteredCoordinates !! 1) bd then do
            putStrLn "Place is not empty!"
            readXY bd p
        
        else
            return (head enteredCoordinates, enteredCoordinates !! 1)
    

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

    
    mainHelper :: [[Int]] -> IO()
    mainHelper bd = do 
                    playerCoordinates <- readXY bd 1
                    let bdPlayerMove = uncurry mark playerCoordinates bd 1
                    putStrLn (boardToStr playerToChar bdPlayerMove)

                    if isWonBy bdPlayerMove 1 then do
                        putStrLn "Player 1 is the winner!"
                        return ()
        
                    else do
                        opponentCoordinates <- readXY bdPlayerMove 2
                        let bdOpponentMove = uncurry mark opponentCoordinates bdPlayerMove 2
                        putStrLn (boardToStr playerToChar bdOpponentMove)
                        
                        if isWonBy bdOpponentMove 2 then do
                            putStrLn "Player 2 is the winner!"
                            return ()
                        
                        else if isDraw bdOpponentMove then do
                            putStrLn "Game is a draw!"
                            return ()
                        
                        else do
                            mainHelper  bdOpponentMove

            