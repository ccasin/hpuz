{-# LANGUAGE NamedFieldPuns #-}
module Codec.Game.Puz 
       (Style (Plain,Circle), Square (Black,Letter,Rebus), 
        Dir (Across,Down), Puzzle (Puzzle),
        width,height,grid,solution,title,author,notes,copyright,clues,
        loadPuzzle)
where

import Codec.Game.Puz.Internal

import System.IO hiding (hGetContents)

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array

import Data.ByteString hiding (map,foldl',zipWith)
import Data.Array
import Data.List
import Data.Char

import Control.Monad

{- ------ Types ------- -}

data Style = Plain | Circle
  deriving (Eq, Show)

data Square = Black
            | Letter (Maybe Char) Style
            | Rebus (Maybe String) Style
  deriving (Eq, Show)

data Dir = Across | Down
  deriving (Eq, Show)


-- The board arrays are row-major and numbered from (0,0) to
-- (width-1,height-1)
data Puzzle =
  Puzzle {width,height :: Int,
          grid      :: Array (Int,Int) Square,
          solution  :: Array (Int,Int) Square,
          title     :: String,
          author    :: String,
          notes     :: String,
          copyright :: String,
          clues     :: [(Int,Dir,String)]}
  deriving (Show)

{- how to order clues -}
orderClues :: (Int,Dir,String) -> (Int,Dir,String) -> Ordering
orderClues (i1,d1,_) (i2,d2,_) = 
  case compare i1 i2 of
    EQ -> case (d1,d2) of
            (Across,Down) -> LT
            (Down,Across) -> GT
            _ -> EQ
    c -> c

{- ---- Internal marshalling stuff ---- -}

-- The bool is true of this is a game board and false if it is a solution
-- board
charToSquare :: Bool -> [(Int,String)] -> Char -> Char -> Char -> Square
charToSquare isGame rtbl sq rbs ext =
    if sq == blackChar then Black 
    else
      case rebus of
        Just str -> if isGame then 
                        let str' = if sq == blankChar then Nothing
                                                      else Just [sq]
                        in Rebus str' style
                    else Rebus (Just str) style
        Nothing -> if sq == blankChar then Letter Nothing style
                     else Letter (Just sq) Plain
  where
    blackChar,blankChar :: Char
    blackChar = '.'
    blankChar = '-'

    style = case ord ext of
              0   -> Plain
              128 -> Circle
              _   -> Plain   --XXX maybe I should issue some kind of warning
    
    rebus = case ord rbs of
              0 -> Nothing
              n -> case lookup n rtbl of
                     Nothing -> error ("Puzzle file contains ill-formed " ++
                                       "rebus section")
                     Just str -> Just str
    

-- The first string is the board.  The second string is the rebus board
-- (or all 0s if none exists).  The [(Int,String)] is the rebus table,
-- (or an empty list of there aren't any to lookup).  The third String
-- is the extras board.
--
-- The bool should be True if this is a game board and false if it is a
-- solution board. 
readBoard :: Bool -> String -> String -> [(Int,String)] -> String -> [Square]
readBoard isGame bd rbs rtbl ext = 
    zipWith3 (charToSquare isGame rtbl) bd rbs ext

boardStringOut :: Int -> Ptr CUChar -> IO String
boardStringOut sz ptr =
  do cuchars <- peekArray sz ptr
     return $ map (toEnum . (fromIntegral :: CUChar -> Int)) cuchars


numberClues :: [String] -> Array (Int,Int) Square -> [(Int,Dir,String)]
numberClues cls bd =
  zipWith (\(a,b) c -> (a,b,c)) (findclues 1 (0,0)) cls
  where
    (_,(xmax,ymax)) = bounds bd

    -- sq number -> position -> list of places clues are needed
    findclues :: Int -> (Int,Int) -> [(Int,Dir)]
    findclues n (x,y) =
        if black then rec else
          case (asq,bsq) of
            (True,True) -> (n,Across) : (n,Down) : rec
            (True,False) -> (n,Across) : rec
            (False,True) -> (n,Down) : rec
            (False,False) -> rec

      where
        black = bd ! (x,y) == Black
        asq = x == 0 || bd ! (x-1,y) == Black 
        bsq = y == 0 || bd ! (x,y-1) == Black

        nextind = if x == xmax then
                    if y == ymax then Nothing else Just (0,y+1)
                  else Just (x+1,y)

        nextnum = if (not black) && (asq || bsq) then n+1 else n

        rec = case nextind of Nothing -> []
                              Just ind -> findclues nextnum ind
    

{- ---- Exposed library ---- -}

-- XXX this should verify the checksums

loadPuzzle :: String -> IO Puzzle
loadPuzzle fname =
  do --- Start by getting internal puz representation
     handle <- openFile fname ReadMode
     size <- liftM fromIntegral $ hFileSize handle
     bytestring <- hGetContents handle
     let cchars :: [CUChar]
         cchars = foldr' (\w cs -> (fromIntegral w) : cs) [] bytestring
     puz <- withArray cchars (\ar -> puzLoad ar size)

     width  <- puzGetWidth puz
     height <- puzGetHeight puz
     let bdStr = boardStringOut $ width * height

     --- Now get all the raw strings we need from the internal puz structure
     gridStr   <- puzGetGrid puz >>= bdStr
     solStr    <- puzGetSolution puz >>= bdStr

     title     <- puzGetTitle puz
     author    <- puzGetAuthor puz
     copyright <- puzGetCopyright puz
     notes     <- puzGetNotes puz

     hasRebus  <- puzHasRebus puz
     rebusStr  <- if hasRebus then puzGetRebus puz >>= bdStr
                              else return $ repeat (chr 0)
     rebusTbl  <- if hasRebus then puzGetRtbl puz
                              else return $ []

     hasExtras <- puzHasExtras puz
     extrasStr <- if hasExtras then puzGetExtras puz >>= bdStr
                               else return $ repeat (chr 0)

     clueCount <- puzGetClueCount puz
     clueStrs  <- mapM (puzGetClue puz) [0..(clueCount-1)]

     --- we use these strings and the puz data to get everything we need 
     --- to build a Puzzle
     let gridsqs, solutionsqs :: [Square]
         gridsqs     = readBoard True gridStr rebusStr rebusTbl extrasStr
         solutionsqs = readBoard False solStr rebusStr rebusTbl extrasStr

         -- these guys are in row-major order, so we need to flip
         numberFold :: (Int,Int,[((Int,Int),Square)]) -> Square ->
                       (Int,Int,[((Int,Int),Square)])
         numberFold (x,y,l) sq = 
           let (x',y') = if x+1 == width then (0,y+1) else (x+1,y) in
           (x',y',(((x,y),sq):l))

         grid, solution :: Array (Int,Int) Square
         grid = array ((0,0),(width-1,height-1)) $
                  (\(_,_,l) -> l) $ foldl' numberFold (0,0,[]) gridsqs
         solution = array ((0,0),(width-1,height-1)) $
                      (\(_,_,l) -> l) $ foldl' numberFold (0,0,[]) solutionsqs

         clues :: [(Int,Dir,String)]
         clues = numberClues clueStrs grid
     return $
       Puzzle {width, height, grid, solution,
               title, author, copyright, notes,
               clues}

