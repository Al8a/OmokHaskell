module Game where 
    -- Board creation

    {-Return an empty nxn board, where n is a positive number. 
    A 1-based pair of indices (x,y) will be used to access a specific place of
    the board, where x and y are column and row indices, respectively.
    However, it's up to you how to represent an omok board concretely.  -}
    mkBoard :: Int -> [[Int]]
    mkBoard 0 = [[]]
    mkBoard n = mkBoardHelper n n
    
    mkBoardHelper :: Int -> Int -> [[Int]]
    mkBoardHelper 1 n = [mkRow n]
    mkBoardHelper m n = mkRow n : mkBoardHelper (m - 1) n

    mkRow :: Int -> [Int]
    mkRow 0 = []
    mkRow n = 0 : mkRow (n-1)

    {-Create and return the first player. 
    It's up to you how to represent an omok player concretely.
    E.g., mkPlayer = 1. 
    The idea is to call this function whenever you need to refer to the
    first play, rather than using a magic number like 1. -}
    mkPlayer :: Int
    mkPlayer = 1

  
    {-Create and return the second player, i.e., the opponent. E.g.,mkOpponent = 2 -}
    mkOpponent :: Int
    mkOpponent = 2
  
    {-Return the size of a board bd, n for an nxn board. -}
    size :: [[Int]] -> Int
    size bd = length bd

    {- Return a row y of a board bd, where y is a 1-based index. It returns
     a list of size n, where n is the size of bd. -}
    row :: Int -> [[Int]] -> [Int]
    row y bd = bd !! (y - 1)

    {-Return a column x of a board bd, where x is a 1-based index.
    It returns a list of size n, where n is the size of bd. -}
    column :: Int -> [[Int]] -> [Int]
    column x bd = columnHelper x bd (size(bd))

    columnHelper :: Int -> [[Int]] -> Int -> [Int]
    columnHelper x bd y
        | y == 1 = [row y bd !! (x-1)]
        | otherwise = columnHelper x bd (y-1) ++ [row (y-1) bd !! (x-1)]
    
    
    {-Mark a place (x,y) in a board bd by a player p, where x and y 
    are 1-based column and row indices. The specified place is assumed
    to be empty (see below). -}
    mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
    mark x y (firstRow : rest) p
        | y == 1 = (markRow x firstRow p) : rest
        | otherwise = firstRow : mark x (y-1) rest p

    {-Is a place (x,y) of a board bd unmarked or a stone not placed? 
    The x and y are 1-based column and row indices.  -}    
    isEmpty :: Int -> Int -> [[Int]] -> Bool
    isEmpty x y bd = place == 0
        where place = (row y bd) !! (x-1)

    {-Does a place (x,y) of a board bd have a stone placed? The x and y 
    are 1-based column and row indices.      -}
    isMarked :: Int -> Int -> [[Int]] -> Bool
    isMarked x y bd = not (isEmpty x y bd)

    {-Does a place (x,y) of a board bd have a stone placed by a player p?
    The x and y are 1-based column and row indices.-} 
    isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
    isMarkedBy x y bd p
        | isEmpty x y bd = False
        | otherwise = (row y bd) !! (x-1) == p
   
   
    {-Return the player of the stone placed on a place (x,y) of a board bd.
    The x and y are 1-based column and row indices. -}
    isFull :: [[Int]] -> Bool
    isFull [[]] = True
    isFull [row] = isRowFull row
    isFull (head : tail)
        | isRowFull head = isFull tail
        | otherwise = False
    
    {-Are all places of board bd marked, i.e., there is no empty place? -}

    -- Game Check logic 


    {-Is the game played on a board bd won by a player p? That is, does 
    the board bd has a winning row for the player p? -}
    isWonBy :: [[Int]] -> Int -> Bool
    isWonBy bd p
        | elem True [ isWonByRow (row n bd) p 0 | n <- [1..(size bd)] ] = True
        | elem True [ isWonByRow (column n bd) p 0 | n <- [1..(size bd)] ] = True
        | elem True [ isWonByRow (row n (diagonals bd)) p 0 | n <- [1..(size(diagonals bd))] ] = True
        | elem True [ isWonByRow (row n (diagonals (reverse (bd)))) p 0 | n <- [1..(size(diagonals bd))] ] = True
        | otherwise = False
  
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




    {- Return a string representation of a board bd. This is a
    higher-order function, and playerToChar is a function that
    converts a player to a character representation, e.g., 'O' and
    'X' (see Part II below). A formatted sample return value is
    shown below; it is produced with the playerToChar function that
    maps the first player to 'O', the second player to 'X', and
    others to '.'. -}
    
    boardToStr playerToChar bd
    









    {-
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
      5| . . . . . . . . . . . . . . ." -}


  