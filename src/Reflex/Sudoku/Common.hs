module Reflex.Sudoku.Common where

data Digit = Guess Int | Free Int deriving (Show, Read, Eq) -- a digit is either visible to
                           -- the user (=Free) oder has to be guessed
                           -- (= Guess)
isCorrect Correct = True
isCorrect NotCorrect = False
isCorrect IsFree = False
isCorrect NotADigit = False

isNotCorrect Correct = False
isNotCorrect NotCorrect = True
isNotCorrect IsFree = False
isNotCorrect NotADigit = False

isAFree Correct = False
isAFree NotCorrect = False
isAFree IsFree = True
isAFree NotADigit = False

isNotADigit Correct = False
isNotADigit NotCorrect = False
isNotADigit IsFree = False
isNotADigit NotADigit = True

data Correctness = Correct | NotCorrect | NotADigit | IsFree deriving (Show, Read, Eq)
type SudokuId = String

unDigit (Free x) = x
unDigit (Guess x) = x

isFree (Free _) = True
isFree (Guess _) = False

fromEither :: Either Int Int -> Digit
fromEither x = case x of
                 (Left y) -> Guess y
                 (Right y) -> Free y

toEither :: Digit -> Either Int Int
toEither digit = case digit of
                  (Guess y) -> (Left y)
                  (Free y) -> (Right y)

type Sudoku = [Digit] -- a Sudoku consists of rows of digits
data SudokuWithId = Sudoku Integer LevelDescription [Digit] deriving (Show, Read, Eq) 

extractDigits (Sudoku _ _ digits) = digits

instance Ord SudokuWithId where
  compare (Sudoku i1 l1 _) (Sudoku i2 l2 _) = case compare l1 l2 of
    EQ -> compare  i1 i2
    LT -> LT
    GT -> GT

data SudokuWithLevel = SudokuL LevelDescription [Digit] deriving (Show, Read, Eq) 
data Level = Level Integer Int LevelDescription 
data LevelDescription = Easy | Medium | Hard | Insane deriving (Show, Read, Eq, Enum, Ord)
