{-# LANGUAGE NamedFieldPuns #-}
module Codec.Game.Puz 
       (Square (Black,Letter), Dir (Across,Down), 
        Puzzle (Puzzle),
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

import Control.Monad

{- ------ Types ------- -}

data Square = Black
            | Letter (Maybe Char)
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

cucharToSquare :: CUChar -> Square
cucharToSquare sq = 
    if sq == blackChar then Black else
      if sq == blankChar then Letter Nothing else
        Letter $ Just $ toEnum $ fromIntegral sq
  where
    blackChar,blankChar :: CUChar
    blackChar = fromIntegral $ fromEnum '.'
    blankChar = fromIntegral $ fromEnum '-'



readBoard :: Int -> Ptr CUChar -> IO [Square]
readBoard sz ptr =
  do chrs <- peekArray sz ptr
     return $ map cucharToSquare chrs

readString :: Ptr CUChar -> IO String
readString ptr =
  do cuchars <- peekArray0 (0 :: CUChar) ptr
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

     --- Now get all the pointers we need into the internal puz structure
     gridPtr <- puzGetGrid puz
     solPtr <- puzGetSolution puz
     titlePtr <- puzGetTitle puz
     authPtr <- puzGetAuthor puz
     copyPtr <- puzGetCopyright puz
     notePtr <- puzGetNotes puz

     clueCount <- puzGetClueCount puz
     cluePtrs <- mapM (puzGetClue puz) [0..(clueCount-1)]

     --- we use the pointers and the puz data to get everything we need 
     --- to build a Puzzle
     width  <- puzGetWidth puz
     height <- puzGetHeight puz

     let sz = width*height
     gridsqs <- readBoard sz gridPtr
     solutionsqs <- readBoard sz solPtr
     -- these guys are in row-major order, so we need to flip
     let numberFold :: (Int,Int,[((Int,Int),Square)]) -> Square ->
                       (Int,Int,[((Int,Int),Square)])
         numberFold (x,y,l) sq = 
           let (x',y') = if x+1 == width then (0,y+1) else (x+1,y) in
           (x',y',(((x,y),sq):l))
     let grid = array ((0,0),(width-1,height-1)) $
                  (\(_,_,l) -> l) $ foldl' numberFold (0,0,[]) gridsqs
     let solution = array ((0,0),(width-1,height-1)) $
                      (\(_,_,l) -> l) $ foldl' numberFold (0,0,[]) solutionsqs
     

     title <- readString titlePtr
     author <- readString authPtr
     copyright <- readString copyPtr
     notes <- readString notePtr

     clueStrs <- mapM readString cluePtrs
     let clues = numberClues clueStrs grid

     return $
       Puzzle {width, height, grid, solution,
               title, author, copyright, notes,
               clues}

