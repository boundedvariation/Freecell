{-|
Module      : FreeCell
Description : A library for Freecell
Copyright   : (c) Timothy Dees, 2014
License     : MIT
Maintainer  : timothy.dees@gmail.com
Stability   : experimental

This lets you play FreeCell.  It's fun, I think.
-}

module FreeCell (
	-- * Types to work with playing cards
	Rank, Suit, Card, Stack, CardSet, Board,
	GameState, FCTree, Location, Move, Solution,

	-- * Utility functions to work with playing cards
	red, black, deck,

	-- * Functions to make, play, and solve games
	makeGame, playGame, solveRandomGame, treeSolverPruned,
	treeSolverDF, deckShuffle, allPermissable,
	solvedBoard,

	-- * I/O with games
	loadFile, loadBoardFromText,

	-- * Accessor functions for the card types 
	rank, suit, cascades, foundations, freecells, 
	gameBoard, sourceMove


	) where

import Data.List
import Data.Maybe
import System.Random
import Data.Tree
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State.Lazy
import Data.Function (on)

-- |Type to represent the rank of a card, from Ace to King.
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight |
	Nine | Ten | Jack | Queen | King  deriving (Show, Eq, Enum, Ord)

-- |Type to represent suit of a card.
data Suit = Heart | Diamond | Club | Spade deriving (Show, Eq, Enum, Ord)

-- |Type to represent a playing card.  Has a suit and a rank.
data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq, Ord)

-- |Type alias to represent a stack of cards.
type Stack = [Card]

-- |Type alias to represent a set of cards where order doesn't matter (i.e. freecells).
type CardSet = Set Card

-- |Type to represent a game board: 8 cascades, 4 foundations, 4 freecells.
data Board = Board {
		cascades :: [Stack],
		foundations :: [Stack],
		freecells :: CardSet} deriving Ord

instance Eq Board where
	Board cs fd fc == Board cs' fd' fc' = (fd == fd') && (fc == fc') && (S.fromList cs == S.fromList cs')

instance Show Board where
	show (Board cs fd fc) = csstring ++ "\n" ++ fdstring ++ "\n" ++ fcstring ++ "\n"
		where
			csstring = intercalate "\n" $ map (\x -> "C " ++ unwords (map cardstring x)) cs
			fdstring = intercalate "\n" $ map (\x -> "FD " ++ unwords (map cardstring x)) fd
			fcstring = "FC " ++ unwords (map cardstring $ S.elems fc)

-- |Type to hold the location of a card.
data Location = Cascades Int | CascadesSource | Foundations | FreeCells deriving (Show, Eq)


-- |Type holds a card, it's prior location, and it's eventual location.  Alternately,
-- it can hold BeginGame, which just represents that this was the initial board.
data Move = Move Card Location Location | BeginGame deriving Eq

-- |Type to hold a board and the move that resulted in that board.
data GameState = GameState { gameBoard :: Board, sourceMove :: Move } deriving (Show, Eq)

-- | Type alias to use for tree constructed for the solver.
type FCTree = Tree [GameState]

-- |Just a list of moves.
data Solution = Solution [Move]

instance Show Solution where
	show (Solution (BeginGame:xs)) = show (Solution xs)
	show (Solution (x:xs)) = show x ++ show (Solution xs)
	show _ = ""

instance Show Move where
	show (Move (Card rk st) l1 l2) = show rk ++ " " ++ show st ++ ": " ++ show l1 ++ " -> " ++ show l2 ++ "\n"
	show BeginGame = ""

-- |Returns whether a card is red.
red :: Card -> Bool
red (Card _ Heart) = True
red (Card _ Diamond) = True
red (Card _ _) = False

-- |Returns whether a card is black.
black :: Card -> Bool
black = not . red

-- |Associative list for the suits.
suitAssoc :: [(Int, Suit)]
suitAssoc = zip [0..3] [Heart .. Spade]

-- |Push a card into a cascade.
pushCascade :: Board -> Card -> Int -> Board
pushCascade (Board cs fd fc) cd num = Board cs' fd fc
	where cs' = applyAt cs num (\x->cd : x)

-- |Pop a card out of a cascade.
popCascade :: Board -> Card -> Board
popCascade (Board cs fd fc) cd = Board cs' fd fc
	where
		cs' = stackf cs
		stackf [] = []
		stackf ([]:xs) = []:stackf xs
		stackf (x:xs) | head x == cd = tail x : xs
					  | otherwise = x : stackf xs

-- |Push a card into a foundation stack.
pushFoundation :: Board -> Card -> Board
pushFoundation (Board cs fd fc) (Card rk st) = Board cs fd' fc
	where 
		fd' = applyAt fd num (\x -> Card rk st : x)
		num = fromJust $ elemIndex st [Heart .. Spade]

-- |Push a card into a freecell.
pushFreeCell :: Board -> Card -> Board
pushFreeCell (Board cs fd fc) cd = Board cs fd $ S.insert cd fc

-- |Pop a card out of a freecell.
popFreeCell :: Board -> Card -> Board
popFreeCell (Board cs fd fc) card = Board cs fd fc'
	where fc' = S.delete card fc

-- |Just a dumb function to attempt to identify to score moves.  Needs work, clearly.
entropyScore :: Board -> Int
entropyScore (Board cs fd fc) = nullPoints + buriedFDs + runs
	where
		nullPoints = 6 * (S.size fc - length (filter null cs))
		runs = sum $ map runlength cs
		runlength (Card King _:_) = -1
		runlength (x1:x2:xs) | (succ (rank x1) == rank x2) && 
							   (red x1 == black x2) = -1 + runlength (x2:xs)
							 | otherwise = 0
		runlength (_:_) = -1
		runlength [] = 0
		nextCard [] = Card Ace
		nextCard (Card x _:_) = Card $ safesucc x
		nextCards = map (\(x,y) -> nextCard (fd !! x) y) suitAssoc
		buriedFDs = (*3) $ sum $ concatMap (findIndices (`elem` nextCards)) cs


-- |Determines which cascades a card can be played on.
playableCascades :: Board -> Card -> [Int]
playableCascades (Board stacks _ _) cd = findIndices playableCascade stacks
	where
		playableCascade [] = True
		playableCascade (Card Ace _:_) = False
		playableCascade (st:_) = (black cd == red st) && (pred (rank st) == rank cd)

-- |Determines if a card can be played on the foundation stacks.
playableFoundation :: Board -> Card -> Bool
playableFoundation (Board _ xs _) (Card rk st) = playableFoundation' (xs !! num)
	where 
		num = fromJust $ elemIndex st [Heart .. Spade]
		playableFoundation' [] = rk == Ace
		playableFoundation' (y:_) = succ (rank y) == rk

-- |Determines if a board has available freecells.
playableFreeCell :: Board -> Bool
playableFreeCell (Board _ _ fc) = S.size fc < 4

-- |Determines all legal plays for a given Board and Card.
allCardPlays :: Board -> Card -> Location -> [GameState]
allCardPlays bd card source = allCardPlaysNoFC bd card source ++ fcplays
	where
		fcplays = [GameState (pushFreeCell bd card) (Move card source FreeCells) | playableFreeCell bd]

-- |Determines all legal plays excluding freecells.  Not sure this is necessary...
allCardPlaysNoFC :: Board -> Card -> Location -> [GameState]
allCardPlaysNoFC bd card source = pf ++ stackplays
	where
		pf = [GameState (pushFoundation bd card) (Move card source Foundations) | playableFoundation bd card]
		cascadeInts = playableCascades bd card
		cascadeBoards = map (pushCascade bd card) cascadeInts
		stackplays = map (\(x,y) -> GameState x $ Move card source (Cascades y)) $ zip cascadeBoards cascadeInts

-- |Determines which cards are available to be played from the cascades.
availableCascadeCards :: Board -> [Card]
availableCascadeCards (Board cs _ _) = map head $ filter (not . null) cs

-- |Determines which cards are in the freecells.
availableFreeCellCards :: Board -> Stack
availableFreeCellCards = S.elems . freecells

-- |Utility function to succ the rank of a card without throwing an error if you succ a King.
safesucc :: Rank -> Rank
safesucc King = King
safesucc x = succ x

-- |Determines which card is the highest rank of card of a given color that can be forced into a move.
highestForceable :: [[Card]] -> Bool -> Rank
highestForceable [[],[],_,_] False = Two
highestForceable [_,_,[],[]] True = Two
highestForceable [he,di,cl,sp] rd | null stack1 || null stack2 = Two
                                  | otherwise = lesser
	where 
		(stack1,stack2) = if not rd then (he, di) else (cl, sp)
		lesser = safesucc $ rank $ head $ if rank (head stack1) > rank (head stack2) then stack2 else stack1
highestForceable _ _ = Two

-- |Determines which moves to the foundations should be forced 
-- (i.e. an Ace is played automatically to the foundations.)
forcedMove :: GameState -> Bool
forcedMove (GameState (Board _ fd _) (Move cd _ Foundations)) = rank cd <= highestForceable fd (red cd)
forcedMove _ = False

-- |Determines all of the permissable moves for a given board.
allPermissable :: Board -> [GameState]
allPermissable bd = filter (\y->gameBoard y /= bd) $ 
					if any forcedMove moves then [head (filter forcedMove moves)] else moves
	where
		fccards = availableFreeCellCards bd
		fcboards = map (popFreeCell bd) fccards
		cscards = availableCascadeCards bd
		csboards = map (popCascade bd) cscards
		cards = fccards ++ cscards
		boards = fcboards ++ csboards
		sources = replicate (length fccards) FreeCells ++ replicate (length cscards) CascadesSource
		moves = concatMap (\(a,b,c)->allCardPlays a b c) (zip3 boards cards sources)

-- |Checks if a board is solved.
solvedBoard :: Board -> Bool
solvedBoard (Board cs _ fc) = all null cs && S.null fc

-- |Builds the lazy tree to hold all board moves.
buildTree :: Board -> FCTree
buildTree bd = unfoldTree f [GameState bd BeginGame]
	where 
		f b = (b, moves)
			where moves = if null val then [] else map (:b) val
				where val = filter (not . (`elem` map gameBoard b) . gameBoard) $
							sortBy (compare `on` (entropyScore . gameBoard)) $
							allPermissable $ gameBoard $ head b

-- |Checks the depth-first flattened structure of the board for a solution.
-- Naive and slow.
treeSolverDF :: Board -> Solution
treeSolverDF = Solution . map sourceMove . reverse . head . filter (solvedBoard . gameBoard . head) . flatten . buildTree

-- |Prunes the tree of any already-check boards and uses this tree to solve
-- the puzzle.  Has an annoying habit of eating up lots of memory and dying.  Needs
-- work.
treeSolverPruned :: Board -> Solution
treeSolverPruned = Solution . map sourceMove . reverse . head . check . buildTree

-- |Prunes the tree and solves the game (in theory!).
check :: FCTree -> [[GameState]]
check tr = evalState (check' tr) S.empty
	where check' (Node s forests) = do
		bdset <- get
		let bd = gameBoard $ head s
		if solvedBoard bd then return [s] else
			if S.member bd bdset then return [] else do
				modify (S.insert bd) 
				result <- mapM check' forests
				return $ concat result

-- |Loads a game in the appropriate format.
-- Format is:
--     ranks -> A, 2, 3, 4, 5, 6, 7, 8, 9, T, J, Q, K
--     suits -> H, D, S, C
--     cards -> i.e. AH, 4D, etc.
--     cascades -> C 4H 2S ...
--     freecells -> FC 2H 3S ...
--     foundations -> FD 2H AH
-- Note that the left-most card is the bottom card on a cascade
-- and the top card on a foundation.
loadFile :: FilePath -> IO Board
loadFile x = loadBoardFromText <$> readFile x

-- |Loads a board from a string.  The foundations part is implemented wrong, 
-- and I'll probably fix it or something.
loadBoardFromText :: String -> Board
loadBoardFromText rawtext = loadBoard (lines rawtext) (Board [] [[],[],[],[]] S.empty)
	where 
		loadBoard (('C':' ':s):ss) bd = loadBoard ss (bd { cascades = cascades bd ++ [map parser (words s)] })
		loadBoard (('F':'C':' ':s):ss) bd = loadBoard ss (bd { freecells = S.fromList $ map parser (words s) })
		loadBoard (('F':' ':s):ss) bd = loadBoard ss (bd { foundations = map parser (words s) : foundations bd })
		loadBoard _ bd = bd

-- |Parses a two-character string into a card.
parser :: String -> Card
parser ('2' : ks) = Card Two $ suitParser ks
parser ('3' : ks) = Card Three $ suitParser ks
parser ('4' : ks) = Card Four $ suitParser ks
parser ('5' : ks) = Card Five $ suitParser ks
parser ('6' : ks) = Card Six $ suitParser ks
parser ('7' : ks) = Card Seven $ suitParser ks
parser ('8' : ks) = Card Eight $ suitParser ks
parser ('9' : ks) = Card Nine $ suitParser ks
parser ('T' : ks) = Card Ten $ suitParser ks
parser ('J' : ks) = Card Jack $ suitParser ks
parser ('Q' : ks) = Card Queen $ suitParser ks
parser ('K' : ks) = Card King $ suitParser ks
parser ('A' : ks) = Card Ace $ suitParser ks
parser x = error $ "Bad parse string: " ++ x

-- |Returns a single character for each rank.
cardchar :: Rank -> Char
cardchar Ace = 'A'
cardchar King = 'K'
cardchar Queen = 'Q'
cardchar Jack = 'J'
cardchar Ten = 'T'
cardchar Nine = '9'
cardchar Eight = '8'
cardchar Seven = '7'
cardchar Six = '6'
cardchar Five = '5'
cardchar Four = '4'
cardchar Three = '3'
cardchar Two = '2'

-- |Returns a character for each suit.
suitchar :: Suit -> Char
suitchar Heart = 'H'
suitchar Club = 'C'
suitchar Diamond = 'D'
suitchar Spade = 'S'

-- |Returns a string for each card.
cardstring :: Card -> String
cardstring (Card rk st) = [cardchar rk, suitchar st]

-- |Parses a string into a suit
suitParser :: String -> Suit
suitParser "H" = Heart
suitParser "C" = Club
suitParser "D" = Diamond
suitParser "S" = Spade
suitParser x = error $ "Unrecognized suit: " ++ x

-- |A list of all 52 cards.
deck :: [Card]
deck = [Card x y | x <- [Ace ..], y <- [Heart ..]]

-- |Shuffles a deck using the IO random generator.
deckShuffle :: Eq a => [a] -> IO [a]
deckShuffle [] = return []
deckShuffle xs = do
	x <- randomRIO (0, length xs-1) :: IO Int
	let val = xs !! x
	y <- deckShuffle (filter (/=val) xs)
	return $ val : y

-- |Makes a game for you to play!
makeGame :: IO Board
makeGame = do
	s <- deckShuffle deck
	let
		(s0, l1) = splitAt 7 s
		(s1, l2) = splitAt 7 l1
		(s2, l3) = splitAt 7 l2
		(s3, l4) = splitAt 7 l3
		(s4, l5) = splitAt 6 l4
		(s5, l6) = splitAt 6 l5
		(s6, l7) = splitAt 6 l6
		s7 = l7
		cs = [s0,s1,s2,s3,s4,s5,s6,s7]
	return $ Board cs [[],[],[],[]] S.empty

-- |Text based Freecell game.
playGame :: IO ()
playGame = do
	gm <- makeGame
	let playloop g = do
		print g
		putStrLn ("Entropy: " ++ show (entropyScore g))
		if solvedBoard g then putStrLn "You win!" else do
			let 
				states = allPermissable g
				moves = map sourceMove states
				boards = map gameBoard states
				movenums = [0..length moves]
				selMove = do
						selectedMove <- read <$> getLine :: IO Int
						if (selectedMove >= length moves) || (selectedMove < 0) then
							putStrLn "Invalid move, select again." >> selMove 
								else return selectedMove
			if null moves then putStrLn "No possible moves, you lose." else 
				if length moves == 1 then putStrLn "Move forced .... " >> playloop (head boards) else do
					putStrLn "Select next move from list: "
					putStrLn $ concatMap (\(x,y) -> show x ++ ": " ++ show y) $ zip movenums moves
					mv <- selMove
					playloop $ boards !! mv
	playloop gm

-- |Creates a game and attempts to solve it.  The solver is rudimentary.
solveRandomGame :: IO ()
solveRandomGame = do
	x <- makeGame
	print x
	let j = treeSolverPruned x
	writeFile "out.txt" (show x ++ show j)
	print j

-- |A generic list function that was necessary.
applyAt :: [a] -> Int -> (a->a) -> [a]
applyAt list num f = applyAt' list 0
	where
		applyAt' [] _ = []
		applyAt' (v:vs) counter | counter == num = f v : vs
								| otherwise = v : applyAt' vs (counter + 1)