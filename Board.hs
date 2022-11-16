module Board where 
    -- Part 1: Creating a board and accessing its elements
    
    {-
    Return an empty nxn board, where n is a positive number. 
    A 1-based pair of indices (x,y) will be used to access a specific place of the board
    x and y are column and row indices -}

    mkBoard :: Int -> [[Int]]                                           
    mkBoard n = [[0 | x <- [1 .. n]] | y <- [1 .. n]]  


    {-
    Create and return the first player. 
    Player = 1. 
    Refer to the first player -}

    mkPlayer :: Int
    mkPlayer = 1


    {-
    Create and return the second player/rival 
    Opponent = 2 -}

    mkOpponent :: Int
    mkOpponent = 2
  

    -- Return the size of a board bd, n for an nxn board.
    size :: [[Int]] -> Int
    size bd = length bd 


    {- 
    Return a row y of a board bd, where y is a 1-based index. 
    It returns a list of size n, where n is the size of bd. -}

    row :: Int -> [[Int]] -> [Int]
    row _ [[]] = []                                                             
    row y (h : t)                                                               
        | y == 1 = h                                                            
        | otherwise = row (y - 1) t     


    {-
    Return a column x of a board bd, where x is a 1-based index.
    It returns a list of size n, where n is the size of bd. -}
                                                                                                   
    col :: Int -> [[Int]] -> [Int]                                              
    col x bd = [row n bd !! (x - 1) | n <- [1 .. size bd]]                    


    -- Return diagonal at indicated (x, y) place.                               
    diagonal :: Int -> Int -> [[Int]] -> [Int]                                  
    diagonal x y bd                                                             
        | (y - 1) < size bd && (x - 1) < size bd = row y bd !! (x - 1) : diagonal (x + 1) (y + 1) bd                                                         
        | otherwise = []                   
    
    ------------------------------------------------------------------------------------
    -- Part 2: Checking places and placing stones     

    {-
    Mark a place (x,y) in a board bd by a player p 
    Where x and y are 1-based column and row indices.
    The specified place is assumed to be empty. -}

    mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
    mark x y (h:t) p
        | y == 1 = markRow x h p : t
        | otherwise = h : mark x (y-1) t p
    
    markRow :: Int -> [Int] -> Int -> [Int]                                     
    markRow n (h : t) p                                                         
        | n == 1 = p : t                                                        
        | otherwise = h : markRow (n - 1) t p  
         
    {-
    Is a place (x,y) of a board bd unmarked or a stone not placed? 
    The x and y are 1-based column and row indices. -}   

    isEmpty :: Int -> Int -> [[Int]] -> Bool
    isEmpty x y bd = place == 0
        where place = row y bd !! (x-1)

    {-
    Does a place (x,y) of a board bd have a stone placed by a player p?
    The x and y are 1-based column and row indices.-} 

    isMarked :: Int -> Int -> [[Int]] -> Bool
    isMarked x y bd = not (isEmpty x y bd)
   
    {-Return the player of the stone placed on a place (x,y) of a board bd.
    The x and y are 1-based column and row indices. -}
    
    isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
    isMarkedBy x y bd p
        | isEmpty x y bd = False
        | otherwise = row y bd !! (x-1) == p
   
     -- Return the player of the stone placed on a place (x, y) of a board bd. (\Assuming not empty)                                                             
    marker :: Int -> Int -> [[Int]] -> Int                                      
    marker x y bd = row y bd !! (x - 1)       

    {-Are all places of board bd marked, i.e., there is no empty place? -}
    -- Check if board fully marked by stones or not.                            
    isFull :: [[Int]] -> Bool                                                   
    isFull bd =  length (filter (\x -> x == 0) (concat bd)) == 0     

    ------------------------------------------------------------------------------------
    -- Part 3: Determining the game outcome 

    {-Is the game played on a board bd won by a player p? That is, does 
    the board bd has a winning row for the player p? -}
    isWonBy :: [[Int]] -> Int -> Bool
    isWonBy bd p                                                                
        | elem True ([hasWinSeq (row n bd) p | n <- [1 .. size bd]]) = True -- \Horizontal                                                                      
        | elem True ([hasWinSeq (col n bd) p | n <- [1 .. size bd]]) = True -- \Vertical      

        | elem True ([hasWinSeq (diagonal n 1 bd) p | n <- [1 .. size bd]]) = True -- Top Diagonal                                                             
        | elem True ([hasWinSeq (diagonal 1 n bd) p | n <- [1 .. size bd]]) = True -- Bottom Diagonal     

        | elem True ([hasWinSeq (diagonal n 1 (reverse bd)) p | n <- [1 .. size bd]]) = True -- Reverse Diagonal                                               
        | elem True ([hasWinSeq (diagonal 1 n (reverse bd)) p | n <- [1 .. size bd]]) = True -- Reverse Diagonal                                               
        | otherwise = False                     
    
    
    -- Check if given row/col has a win sequence.                               
    hasWinSeq :: [Int] -> Int -> Bool                                           
    hasWinSeq [] _ = False                                                      
    hasWinSeq (h : t) p                                                         
        | h == p && length t >= 4 && length ([n | n <- take 4 t, n == p]) == 4 = True                                                                          
        | otherwise = hasWinSeq t p                                             
                                       

    {-Is the game played on a board bd ended in a draw? -}
    isDraw :: [[Int]] -> Bool
    isDraw bd = isFull bd


    {-Is the game played on a board bd over? -}
    isGameOver :: [[Int]] -> Bool
    isGameOver bd 
        | isWonBy bd mkPlayer = True
        | isWonBy bd mkOpponent = True
        | isDraw bd = True
        | otherwise = False

    ------------------------------------------------------------------------------------
    -- Part 4 : Converting to a string for printing 

    {- Return a string representation of a board bd. This is a
    higher-order function, and playerToChar is a function that
    converts a player to a character representation, e.g., 'O' and
    'X' (see Part II below). A formatted sample return value is
    shown below; it is produced with the playerToChar function that
    maps the first player to 'O', the second player to 'X', and
    others to '.'. -}


    --  Return a string representation of a board bd
    boardToStr :: (Int -> Char) -> [[Int]] -> String
    boardToStr playerToChar bd = h1 ++ h2 ++ rows
        where 
            h1 = "x " ++ concat([show (mod i 10 ) ++ " "| i <- [1 .. size bd]]) ++ "\n"
            h2 = "y " ++ concat(["-" | i <- [1 .. ((size bd) * 2)]]) ++ "\n"
            rows = concat([show(mod i 10) ++ "|" ++ rowToStr playerToChar (row i bd) | i <- [1 .. size bd]])


    -- Return a string representation of a row.
    rowToStr :: (Int -> Char) -> [Int] -> String
    rowToStr _ [] = "\n"
    rowToStr f (h : t) = [f h] ++ " " ++ rowToStr f t




    {- Sample output provided 
     " x 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5\n
      y ------------------------------\n
      1| . . . . . . . . . . . . . . .\n
      2| . . . . . . . . . . . . . . .\n
      3| . . . . . . . . . . . . . . .\n
      4| . . . . . . . . . . . . . . .\n
      5| . . . . . . . . . . . . . . .\n
      6| . . . . . . . . O . . . . . .\n
      7| . . . . . . . X X . . . . . .\n
      8| . . . . . . X O O . . . . . .\n
      9| . . . . . . . . . . . . . . .\n
      0| . . . . . . . . . . . . . . .\n
      1| . . . . . . . . . . . . . . .\n
      2| . . . . . . . . . . . . . . .\n
      3| . . . . . . . . . . . . . . .\n
      4| . . . . . . . . . . . . . . .\n
      5| . . . . . . . . . . . . . . .\n" 
      
      O's turn: enter x y (1-15 or -1 to quit)? 7 9 -}


  