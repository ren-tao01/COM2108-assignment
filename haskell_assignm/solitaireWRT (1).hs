 
import Data.List(sortBy)
import System.Random
import Data.Maybe
import Prelude

data Suit = Diamonds | Clubs | Hearts | Spades
    deriving
        (Eq, Ord, Show, Enum)
data Pip = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
    deriving
        (Eq, Ord, Show, Enum)
type Card = (Pip, Suit)
type Deck = [Card]

-- Stock is not visible 
type Stock = [Card]

pack :: [Card]
pack = [(p, s) | p <- [Ace .. King], s <- [Diamonds .. Spades]]

-- can use succ and pred
sCard :: Card -> Card
sCard (King, s) = (Ace, s)
sCard (p, s) = (head (tail (dropWhile(/= p) [Ace .. King])), s)

pCard :: Card -> Card
pCard (Ace, s) = (King, s)
pCard (p, s) = (last (takeWhile(/= p) [Ace .. King]), s)

isAce :: Card -> Bool 
isAce (p, s)
    | p == Ace = True 
    | otherwise = False

isKing :: Card -> Bool 
isKing (p, s)
    | p == King = True 
    | otherwise = False 

shuffle :: Int -> Deck -> Deck
cmp (x1,y1) (x2,y2) = compare y1 y2
shuffle rng cs = [ c | (c, n) <- sortBy cmp
                          (zip cs ((randoms (mkStdGen rng)) :: [Int]))]

type Foundation = [Card]
type Column = [Card]
-- Bool for visibility
type SColumn = [(Card, Bool)]
type Reserve = [Card]                         
data Board = EOBoard Foundation [Column] Reserve |
             SBoard Foundation [SColumn] Stock
                deriving
                    (Eq)
    
instance Show Board where
    show b  
        | isEOBoard b = "EOBoard"  ++ (boardshowEO "  " b)
        | isSBoard b = "SBoard" ++ (boardshowS "  " b)
        where
            -- isEOBoard show functions
            boardshowEO space (EOBoard f c r) = "\n" ++ (fshow space f) ++ (cshow1 space c) 
                ++ (rshow space r)

            boardshowS space (SBoard f c s) = "\n" ++ (fshow space f) ++ (cshow2 space c)
                ++ (sshow space s)
            --
            fshow space foundations = "Foundations" ++ space ++ show foundations ++ "\n"
            --
            cshow1 space columns = "Columns" ++ "\n" ++ (allcshow1 space columns)

            allcshow1 space [] = []
            allcshow1 space (col:columns) = space ++ show col ++ "\n" ++ (allcshow1 space columns)

            cshow2 space columns = "Columns" ++ "\n" ++ (allcshow2 space columns)

            allcshow2 space [] = []
            allcshow2 space (col:columns) = space ++ "[" ++ cardInCol space col ++ "]" ++ "\n" 
                ++ (allcshow2 space columns)

            cardInCol space [] = []
            cardInCol space (c:cs) 
                | null cs = showSCard space c
                | otherwise = showSCard space c ++ "," ++ cardInCol space cs

            showSCard space (c, visibility) 
                | visibility = show c
                | otherwise = "<unknown>"

            --
            sshow space stock = "Stock" ++ space ++ show (length stock `div` 10) ++ " Deals remaining"

            rshow space reserve = "Reserve" ++ space ++ show reserve

-- check if it is Eight-Off Board
isEOBoard:: Board -> Bool 
isEOBoard (EOBoard _ _ _) = True
isEOBoard _ = False

-- check if it is Spider Board
isSBoard :: Board -> Bool 
isSBoard (SBoard _ _ _) = True 
isSBoard _ = False

makeColumns :: Int -> Deck -> [Column]
makeColumns 0 _ = []
makeColumns rows deck = (take 6 deck) : makeColumns (rows - 1) (drop 6 deck)

-- eODeal takes in the seed for rng
eODeal :: Int -> Board
eODeal rng = EOBoard [] columns reserve
    where 
        cards = splitAt 48 (shuffle rng pack)
        columns = makeColumns 8 (fst cards)
        reserve = snd cards

toFoundations:: Board -> Board
toFoundations board@(EOBoard f cs rs) 
    | length f == 0 = findAcesFromAll board
    | board' == board = board
    | board'' == board' = checkTableau board
    | otherwise = checkTableau board'
        where
            board' = checkTableau board
            board'' = checkTableau board'

-- putting some/all the available Aces to the Foundation
findAcesFromAll :: Board -> Board
findAcesFromAll board@(EOBoard f cs rs)
    | any isAce (everyFirstCardFromCol cs) = toFoundations(EOBoard newF newCs rs)
    | any isAce rs = toFoundations(EOBoard newF1 cs newR)
    | otherwise = board
        where
            newF = filter (isAce) (everyFirstCardFromCol cs) ++ f
            newCs = updateColumns newF cs
            newF1 = filter (isAce) rs ++ f
            newR = updateReserve newF1 rs

-- updates the Tableau after moving cards to the Foundation
checkTableau :: Board -> Board
checkTableau (EOBoard foundation columns reserve) = EOBoard res_f res_c res_r 
    where 
        res_f = foldr (\ r f -> updateFoundationFromReserve r f)
            (foldr (\ c f -> updateFoundationFromColumns c f) foundation firstCards) reserve
            where
                firstCards = everyFirstCardFromCol columns
        res_c = updateColumns res_f columns
        res_r = updateReserve res_f reserve

-- Reserve and Foundation is [Card]
updateFoundationFromReserve :: Card -> Foundation -> Foundation
updateFoundationFromReserve _ [] = []
updateFoundationFromReserve card foundation@(f:fs) 
    | isAce card = card : foundation
    | pCard card == f = card : fs 
    | otherwise = f : (updateFoundationFromReserve card fs)

-- find matching place in foundation then do the 1 change only
updateFoundationFromColumns :: Card -> Foundation -> Foundation
updateFoundationFromColumns _ [] = []
updateFoundationFromColumns card foundation@(f:fs)
    | isAce card = card : foundation
    | pCard card == f = card : fs 
    | otherwise = f : (updateFoundationFromColumns card fs)

everyFirstCardFromCol:: [Column] -> [Card]
everyFirstCardFromCol [] = []
everyFirstCardFromCol (col:cols)
    | not (null col) = head col : everyFirstCardFromCol cols
    | otherwise = everyFirstCardFromCol cols

updateColumns:: Foundation -> [Column] -> [Column]
updateColumns f1 col = foldr (\ card col -> removeCardFromColumns card col) col f1

-- checking the cards at foundation if they match to the unchanged columns
removeCardFromColumns :: Card -> [Column] -> [Column]
removeCardFromColumns _ [] = []
removeCardFromColumns card col@(c:cs)
    | not (null c) && head c <= card && sameSuit card (head c) = drop 1 c : removeCardFromColumns card cs
    | otherwise = c : removeCardFromColumns card cs

updateReserve :: Foundation -> Reserve -> Reserve
updateReserve f1 res = foldr (\ card res -> removeCardFromReserve card res) res f1

-- Reserve :: [Card] the card is from foundation
removeCardFromReserve :: Card -> Reserve -> Reserve
removeCardFromReserve _ [] = []
removeCardFromReserve card reserve@(r:rs)
    | r <= card && sameSuit card r = removeCardFromReserve card rs
    | otherwise = r : removeCardFromReserve card rs

sameSuit :: Card -> Card -> Bool
sameSuit (_,s1) (_,s2)
    | s1 == s2 = True 
    | otherwise = False

constantEO :: Board
constantEO = EOBoard f cols res
    where
        f = []
        cols = [[(Ace,Clubs),(Seven,Diamonds),(Ace,Hearts),(Queen,Hearts),(King,Clubs),(Four,Spades)],
                [(Five,Diamonds),(Queen,Spades),(Three,Diamonds),(Five,Spades),(Six,Spades),(Seven,Hearts)],
                [(King,Hearts),(Ten,Diamonds),(Seven,Spades),(Queen,Diamonds),(Five,Hearts),(Eight,Diamonds)],
                [(Jack,Spades),(Six,Hearts),(Seven,Clubs),(Eight,Spades),(Ten,Clubs),(Queen,Clubs)],
                [(Ace,Spades),(Eight,Clubs),(Ace,Diamonds),(King,Diamonds),(Jack,Hearts),(Four,Clubs)],
                [(Two,Diamonds),(Three,Hearts),(Three,Clubs),(Ten,Hearts),(Six,Diamonds),(Jack,Clubs)],
                [(Nine,Spades),(Four,Diamonds),(Nine,Clubs),(Nine,Hearts),(Three,Spades),(Ten,Spades)],
                [(Two,Clubs),(Two,Spades),(Four,Hearts),(Nine,Diamonds),(King,Spades),(Eight,Hearts)]]
        res = [(Two,Hearts),(Six,Clubs),(Five,Clubs),(Jack,Diamonds)]

-- SPIDER SOLITAIRE

sDeal :: Int -> Board
sDeal rng = SBoard [] res_c res_s
    where
        pileOfCards = shuffle rng (pack ++ pack)
        cardForCols = take 54 pileOfCards
        res_c = dealCards 6 4 (take 24 cardForCols) ++ dealCards 5 6 (drop 24 cardForCols)
            where
                -- nC = number of cards; nP = number of piles
                dealCards:: Int -> Int -> [Card] -> [SColumn]
                dealCards _ 0 _ = []
                dealCards nC nP allC@(c:cs) = [zip (take nC allC) (True : replicate (nC -1) False)] ++ dealCards nC (nP -1) cs
        res_s = drop 54 pileOfCards
            

constantS = SBoard f sc s
    where
        f = [(King,Hearts)]
    
        sc = [
                [((Eight,Diamonds),True), ((Nine,Hearts),True)],
                [((Two,Diamonds),True)],
                [((Ace,Spades),True),((Two,Spades),True),((Three,Spades),True),((Four,Spades),True),
                    ((Five,Spades),True),((Six,Clubs),True),((Seven,Clubs),True),((Eight,Clubs),True),
                    ((Nine,Clubs),True),((Ten,Diamonds),True),((Jack,Diamonds),True),((Queen,Diamonds),True),
                    ((King,Diamonds),True),((Ace,Diamonds),False),((Ace,Diamonds),False)],
                [((Seven,Clubs),True),((Eight,Diamonds),True),((Nine,Diamonds),True),((Ten,Diamonds),True),
                ((Jack,Diamonds),True),((Queen,Diamonds),True),((King,Diamonds),True),((Nine,Clubs),True),
                ((Ten,Hearts),True),((Jack,Clubs),True)],
                [((Ace,Hearts),True),((Two,Hearts),True),((Three,Hearts),True),((Four,Hearts),True),
                    ((Five,Hearts),True),((Six,Diamonds),True),((Seven,Diamonds),True),((Queen,Clubs),True),
                    ((King,Hearts),True)],
                [((Two,Diamonds),True),((Three,Diamonds),True),((Four,Diamonds),True)],
                [((Jack,Clubs),True),((Queen,Clubs),True),((King,Clubs),True),((Two,Spades),True),
                    ((Three,Spades),True),((Four,Diamonds),True),((Five,Diamonds),True),((Six,Diamonds),True),
                    ((Seven,Hearts),True),((Eight,Clubs),True),((Nine,Spades),True),((Ten,Clubs),True), 
                    ((Ace,Clubs),True),((Two,Clubs),True),((Three,Clubs),True),((Four,Clubs),True),
                    ((Five,Spades),True)],
                [((Seven,Spades), True),((Eight,Spades), True),((Nine,Spades), True),((Ten,Spades), True),
                    ((Jack,Spades), True),((Queen,Spades), True),((King,Spades), True),((Ace,Diamonds),False),
                    ((Ace,Diamonds),False),((Ace,Diamonds),False)],
                [((Jack,Hearts), True),((Queen,Hearts), True)],
                [((Ace,Clubs), True),((Two,Clubs), True)]
            ]
        s = take 20 (repeat (Ace,Diamonds)) -- not known from the brief hence (Ace,Diamonds) is used as a dummy card

-- getting Reserve of the board
getRes :: Board -> Reserve
getRes (EOBoard f col res) = res

-- getting [Column] of the board
getCols :: Board -> [Column]
getCols (EOBoard f col res) = col

findMoves :: Board -> [Board]
findMoves board@(EOBoard f cols res) =  moveKing board ++
                                        reserveToColumns res board ++
                                        columnsToColumns cols board ++
                                        columnsToReserve (everyFirstCardFromCol cols) board

-- moving available King cards from Reserve and [Column]
moveKing :: Board -> [Board]
moveKing bb@(EOBoard f columns@(col:cols) res)
    | any null columns && any isKing res 
        = moveKingFromReserve (filter isKing res) bb
    | any null columns && any isKing (everyFirstCardFromCol columns)
        = moveKingFromCols (filter isKing (everyFirstCardFromCol columns)) bb
    | otherwise = []
        where
            firstKingRes = take 1 (filter (isKing) res)
            newC =  firstKingRes : filter (not.null) columns
            newR = remove1CardFromReserve (firstKingRes !! 1) res

-- takes a list of Kings that are available in the Reserve
moveKingFromReserve :: [Card] -> Board -> [Board]
moveKingFromReserve [] bb = []
moveKingFromReserve (c:cs) bb@(EOBoard f cols res)
    | any null cols = EOBoard f newC newR : moveKingFromReserve cs bb
    | otherwise = []
        where
            newC = [c] : filter (not.null) cols
            newR = remove1CardFromReserve c res

moveKingFromCols :: [Card] -> Board -> [Board]
moveKingFromCols [] bb = []
moveKingFromCols (c:cs) bb@(EOBoard f cols res)
    | any null cols = EOBoard f newC2 res : moveKingFromCols cs bb
    | otherwise = []
        where
            newC = remove1CardFromColumns c cols
            newC2 = [c] : filter (not.null) cols

-- moving cards within the Column in [Column]
columnsToColumns :: [Column] -> Board -> [Board]
columnsToColumns [] _ = []
columnsToColumns (col:cols) bb@(EOBoard f col2 res) 
    | not (null col) && any (matchingCard (sCard (last stack))) (everyFirstCardFromCol col2) = newB : columnsToColumns cols bb
    | otherwise = columnsToColumns cols bb
    where
        stack = sBehind col
        newB = moveCards stack bb

-- check if the cards match to each other
matchingCard :: Card -> Card -> Bool
matchingCard c1 c2 
    | c1 == c2 = True 
    | otherwise = False

-- grouping the successor behind the top card with the top card
sBehind :: Column -> [Card]
sBehind [] = []
sBehind (c:[]) = [c]
sBehind (c1:c2:col)
    | c2 == sCard c1 = c1 : sBehind (c2:col)
    | otherwise = [c1]

-- moving a group of cards or just a card
moveCards :: [Card]-> Board -> Board
moveCard [] b = b
moveCards cs (EOBoard f columns@(col:cols) res)
    | not (null col) && sCard (last cs) == head col = EOBoard f newCol res
    | otherwise = EOBoard f newCol res
        where
            tempCol = removeStackFromCol cs columns
            newCol = addStackToCol cs tempCol

-- removing the group of cards or just a card from the [Column]
removeStackFromCol :: [Card] -> [Column] -> [Column]
removeStackFromCol [] [] = []
removeStackFromCol cs [] = []
removeStackFromCol [] cols = cols
removeStackFromCol cs (col:cols) 
    | not (null col) && head cs == head col = drop (length cs) col : cols
    | otherwise = col : removeStackFromCol cs cols

-- adding the group of cards or just a card to the [Column]
addStackToCol :: [Card] -> [Column] -> [Column]
addStackToCol _ [] = []
addStackToCol cs (col:cols) 
    | not (null col) && sCard (last cs) == head col = (cs ++ col) : cols
    | otherwise = col : addStackToCol cs cols

-- moving top card from [Column] to Reserve
columnsToReserve :: [Card] -> Board -> [Board]
columnsToReserve [] b = []
columnsToReserve (c:cs) bb@(EOBoard f cols res)
    | reserveHasSpace res = newReserve c newB : columnsToReserve cs bb
    | otherwise = []
        where
            newB = EOBoard f (remove1CardFromColumns c cols) res

-- checking if reserve has the space 
reserveHasSpace :: Reserve -> Bool 
reserveHasSpace r
    | length r < 8 = True
    | otherwise = False

newReserve :: Card -> Board -> Board
newReserve c (EOBoard f cols res) = EOBoard f cols (c:res)

remove1CardFromColumns :: Card -> [Column] -> [Column]
remove1CardFromColumns _ [] = []
remove1CardFromColumns  card column@(col:cols)
    | not (null col) && head col == card = drop 1 col : remove1CardFromColumns card cols
    | otherwise = col : remove1CardFromColumns card cols

-- moving possible card(s) from Reserve that satisfies the condition
reserveToColumns :: Reserve -> Board -> [Board]
reserveToColumns [] _ = []
reserveToColumns (r:rs) bb@(EOBoard f cols res)
    | any (matchingCard (sCard r)) (everyFirstCardFromCol cols) 
        = newB : reserveToColumns rs bb
    | otherwise = reserveToColumns rs bb
        where 
            newB = EOBoard f (newColumns r cols) (remove1CardFromReserve r res)

newColumns :: Card -> [Column]-> [Column]
newColumns _ [] = []
newColumns card (col:cols)
    | not (null col) && sCard card == head col = ((card:col):cols)
    | otherwise = col : newColumns card cols

remove1CardFromReserve :: Card -> Reserve -> Reserve
remove1CardFromReserve _ [] = []
remove1CardFromReserve card reserve@(r:rs)
    | r == card =  remove1CardFromReserve card rs
    | otherwise = r : remove1CardFromReserve card rs

              
playSolitaire :: Board -> Int
playSolitaire board 
    | chooseMove board == Nothing = checkScore board
    | otherwise = playSolitaire (convertMaybe board')
        where
            board' = chooseMove board

convertMaybe :: Maybe Board -> Board
convertMaybe bb = case bb of
                    Just bb -> bb

chooseMove :: Board -> Maybe Board
chooseMove board
    | board == board' = Nothing
    | board == board'' = Nothing
    | otherwise = Just board'
        where
            toF = toFoundations board
            board'
                | null (findMoves toF) = toF
                | otherwise = checkAndChoose (findMoves toF)
            toF' = toFoundations board'
            board'' 
                | null (findMoves toF') = toF'
                | otherwise = checkAndChoose (findMoves toF')

-- choosing the move that it will move some cards to the foundation, else the last move in the list will be chosen
checkAndChoose :: [Board] -> Board
checkAndChoose [bb] = bb
checkAndChoose list@(bb:bbs)
    | toFoundations bb /= bb = bb
    | toFoundations bb == bb = checkAndChoose bbs

checkScore :: Board -> Int 
checkScore (EOBoard [] cols res)  = 0
checkScore bb@(EOBoard ((p,s):fs) cols res)
    | haveWon bb = 52
    | otherwise = head [n | (pip, n) <- zip [Ace .. King] [1..], pip == p] 
                    + checkScore (EOBoard fs cols res)

haveWon :: Board -> Bool
haveWon (EOBoard f cols res)
    | null cols && null res = True 
    | otherwise = False    

haveWon (SBoard f cols stock)
    | null cols && null stock = True 
    | otherwise = False

-- input = seed for rng, number of games; return = num of wins, avg score
analyseEO :: Int -> Int -> (Int, Float) 
analyseEO _ 0 = (0, 0)
analyseEO seed num = (wins, avg)
    where 
        boards = listOfEOGames seed num
        wins = numOfWins num boards
        total = totalScore num boards
        avg = avgScore total num

numOfWins:: Int -> [Board] -> Int
numOfWins 0 bs = 0
numOfWins num (board:boards)
    | playSolitaire board == 52 = 1 + numOfWins (num - 1) boards
    | otherwise = numOfWins (num - 1) boards

totalScore:: Int -> [Board] -> Int
totalScore _ [] = 0
totalScore num (board:boards) = playSolitaire board  + totalScore (num - 1) boards

-- takes in totalScore, num of games
avgScore:: Int -> Int -> Float
avgScore _ 0 = 0
avgScore ts n = (/) (fromIntegral ts) (fromIntegral n)

-- making a number of eight-off boards 
listOfEOGames :: Int -> Int -> [Board]
listOfEOGames _ 0 = []
listOfEOGames seed num = eODeal seed : listOfEOGames (seed + 1) (num - 1)




  {- Paste the contents of this file, including this comment, into your source file, below all
     of your code. You can change the indentation to align with your own, but other than this,
     ONLY make changes as instructed in the comments.
   -}
  -- Constants that YOU must set:
studentName = "Ren Tao Wan"
studentNumber = "200181101"
studentUsername = "aca20rtw"

initialBoardDefined = constantEO {- replace XXX with the name of the constant that you defined
                               in step 3 of part 1 -}
secondBoardDefined = constantS {- replace YYY with the constant defined in step 5 of part 1,
                              or if you have chosen to demonstrate play in a different game
                              of solitaire for part 2, a suitable contstant that will show
                              your play to good effect for that game -}

  {- Beyond this point, the ONLY change you should make is to change the comments so that the
     work you have completed is tested. DO NOT change anything other than comments (and indentation
     if needed). The comments in the template file are set up so that only the constant eight-off
     board from part 1 and the toFoundations function from part 1 are tested. You will probably
     want more than this tested.

     CHECK with Emma or one of the demonstrators if you are unsure how to change this.

     If you mess this up, your code will not compile, which will lead to being awarded 0 marks
     for functionality and style.
  -}


main :: IO()
main =
    do
      putStrLn $ "Output for " ++ studentName ++ " (" ++ studentNumber ++ ", " ++ studentUsername ++ ")"

      putStrLn "***The eight-off initial board constant from part 1:"
      print initialBoardDefined

      let board = toFoundations initialBoardDefined
      putStrLn "***The result of calling toFoundations on that board:"
      print board

      {- Move the start comment marker below to the appropriate position.
        If you have completed ALL the tasks for the assignment, you can
        remove the comments from the main function entirely.
        DO NOT try to submit/run non-functional code - you will receive 0 marks
        for ALL your code if you do, even if *some* of your code is correct.
      -}

    --   start comment marker - move this if appropriate

      let boards = findMoves board      -- show that findMoves is working
      putStrLn "***The possible next moves after that:"
      print boards

      let chosen = chooseMove board     -- show that chooseMove is working
      putStrLn "***The chosen move from that set:"
      print chosen

      putStrLn "***Now showing a full game"     -- display a full game
      score <- displayGame initialBoardDefined 0
      putStrLn $ "Score: " ++ score
      putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire initialBoardDefined)


      putStrLn "\n\n\n************\nNow looking at the alternative game:"

      putStrLn "***The spider initial board constant from part 1 (or equivalent if playing a different game of solitaire):"
      print secondBoardDefined          -- show the suitable constant. For spider solitaire this
                                        -- is not an initial game, but a point from which the game
                                        -- can be won

      putStrLn "***Now showing a full game for alternative solitaire"
      score <- displayGame secondBoardDefined 0 -- see what happens when we play that game (assumes chooseMove
                                                -- works correctly)
      putStrLn $ "Score: " ++ score
      putStrLn $ "and if I'd used playSolitaire, I would get score: " ++ show (playSolitaire secondBoardDefined)

      

  {- displayGame takes a Board and move number (should initially be 0) and
     displays the game step-by-step (board-by-board). The result *should* be
     the same as performing playSolitaire on the initial board, if it has been
     implemented correctly.
     DO NOT CHANGE THIS CODE other than aligning indentation with your own.
  -}
displayGame :: Board -> Int ->IO String
displayGame board n =
    if haveWon board
        then return "A WIN"
    else
        do
            putStr ("Move " ++ show n ++ ": " ++ show board)
            let maybeBoard = chooseMove board
            if isJust maybeBoard then
                do
                    let (Just newBoard) = maybeBoard
                    displayGame newBoard (n+1)
            else
                do
                    let score = show (playSolitaire board)
                    return score
