{-# LANGUAGE MultiWayIf,RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell #-}

module Reflex.Sudoku.Main where

import Control.Monad 
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.FileEmbed
import Data.List.Extra hiding (list)
import qualified Data.Monoid as Monoid
import Data.Maybe
import Text.Read (readMaybe)

import Reflex
import Reflex.Dom

import Reflex.Sudoku.Common
import qualified Reflex.Sudoku.Store as Store

main :: IO ()
main = do
  let sudokusToMap = Map.fromList . map (\(Sudoku i _ ds) -> (i,ds)) 
      easy =  sudokusToMap $ case Store.easy of
        (x:xs) -> makeEasy x : xs
        xs      -> xs
      medium = sudokusToMap Store.medium
      hard = sudokusToMap Store.hard
      insane = sudokusToMap Store.insane
      makeEasy (Sudoku sId l (d:digits)) = Sudoku sId l (d:map makeFree digits)
         where
           makeFree (Guess x) = Free x
           makeFree (Free x) = Free x
  mainWidgetWithCss $(embedFile "sudoku.css") $ sudokuApp (easy, medium, hard, insane)

type SudokusById = Map.Map Integer [Digit]

sudokuApp (easy,medium,hard,insane) = do
  rec let dropdowns = do 
            let caption = Map.mapWithKey (\i _ -> "Sudoku " <> show i)

            rec let sudokuDropdown sudokus header = do
                    let keys = Map.keys sudokus
                    dropdownWrapperAttrs <- forDyn selectedSudoku $
                      \sId -> ("class" =: ("selectSudokus " <> if sId `elem` keys then "highlighted" else mempty))
                    elDynAttr "div" dropdownWrapperAttrs $ do
                        el "h2" $ text header
                        dropdown (head $ Map.keys sudokus) (constDyn $ caption sudokus) def

                easyDropdown <- sudokuDropdown easy "Easy"
                mediumDropdown <- sudokuDropdown medium "Medium"
                hardDropdown <- sudokuDropdown hard "Hard"
                insaneDropdown <- sudokuDropdown insane "Insane"
                let eSudokuChange = mergeWith const $ map _dropdown_change [ easyDropdown
                                                                            , mediumDropdown
                                                                            , hardDropdown
                                                                            , insaneDropdown ]

                selectedSudoku <- holdDyn (1 :: Integer) eSudokuChange 

            return selectedSudoku

      (eCorrectness :: [Event t Correctness], dDigits :: [Dynamic t Digit]) <- fmap unzip $ elClass "div" "sudoku" $ do
        let allSdks :: Map.Map Integer [Digit] = foldl1' Map.union [easy,medium,hard,insane]
        chosenSudoku :: Dynamic t [Digit] <- forDyn selectedSudokuId $ \sId -> fromJust $ Map.lookup sId allSdks
        let digitOf (row,col) = forDyn chosenSudoku $ (!!col) . (!!row) . chunksOf 9 
        dDigits :: [Dynamic t Digit] <- sequence [digitOf (row,col) | row <- [0..8], col <- [0..8]]

        forM dDigits $ \dDigit -> do
           rec dCellContent <- holdDyn NotADigit (leftmost [NotADigit <$ (updated selectedSudokuId), eCorrectness])
               cellClass <- combineDyn (\input digit ->
                  ("class" =: ("cell " <> if | isFree digit || isNotADigit input -> mempty
                                             | isCorrect input -> "right"
                                             | otherwise -> "wrong"))) dCellContent dDigit
               eCorrectness <- elDynAttr "div" cellClass $ do 
                 let showDigit digit = if isFree digit then show $ unDigit digit else mempty
                     digitDisplay = fmap showDigit (updated dDigit)
                 cellAttr <- forDyn dDigit $ \digit ->
                    ("maxlength" =: "1") `Map.union` if isFree digit then ("disabled" =: "disabled") else Map.empty
                 pbd <- getPostBuild

                 input <- textInput (def & setValue .~ leftmost [digitDisplay, tag (fmap showDigit $ current dDigit) pbd]
                                         & attributes .~ cellAttr)
                 let newValue = _textInput_input input
                     eCorrectness = (\(digit, userInput) -> case digit of
                         Guess x -> case readMaybe userInput of
                                     Nothing -> NotADigit
                                     Just y  -> if x == y then Correct else NotCorrect
                         Free  _ -> IsFree) <$> attachDyn dDigit newValue
                 return eCorrectness
           return (eCorrectness, dDigit)

      selectedSudokuId :: Dynamic t Integer <- elAttr "div" ("class" =: "sidebar") $ do
        dSelectedSudoku <- dropdowns

        let errorCount e = foldDyn ($) (0 :: Int) $ mergeWith (.)
                    [
                      const 0 <$ updated dSelectedSudoku
                    , (+1) <$ ffilter isNotCorrect e
                    ]
                    
             
        bErrors  <- mapM (mapDyn Monoid.Sum <=< errorCount) eCorrectness
        bErrorSum :: Dynamic t Int <- mapDyn Monoid.getSum =<< mconcatDyn bErrors
        el "h3" $ dynText =<< mapDyn ((<> " Errors") . show) bErrorSum

        let isCellInputCorrect (eInput, dDigit) = do
                pbd <- getPostBuild
                foldDyn ($) False $ mergeWith (.)
                    [
                      -- const True <$ ffilter isFree (updated dDigit)
                      const True <$ ffilter isFree (leftmost [updated dDigit, tag (current dDigit) pbd])
                    , const True <$ ffilter isCorrect eInput
                    , const False <$ ffilter isNotADigit eInput
                    , const False <$ updated dSelectedSudoku
                    , const False <$ ffilter isNotCorrect eInput
                    ]

        dCorrectCells :: [Dynamic t Monoid.All] <- mapM (mapDyn Monoid.All <=< isCellInputCorrect) (zip eCorrectness dDigits)
        dSudokuSolved :: Dynamic t Bool <- mapDyn Monoid.getAll =<< mconcatDyn dCorrectCells
        elClass "h3" "solved" $ dynText =<< forDyn dSudokuSolved (\isSolved -> if isSolved then "Solved!" else mempty)


        return dSelectedSudoku
      
  return ()
