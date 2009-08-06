{-# LANGUAGE NamedFieldPuns #-}
module Codec.Game.Puz 
       (Square, Dir, Puzzle,
        loadPuzzle)
where

import Codec.Game.Puz.Internal

import System.IO hiding (hGetContents)

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array

import Data.ByteString hiding (map)

import Control.Monad

{- ------ Types ------- -}

data Square = Black
            | Letter (Maybe Char)
  deriving (Eq, Show)

data Dir = Across | Down
  deriving (Eq, Show)

data Puzzle =
  Puzzle {width,height :: Int,
          grid      :: [Square],
          solution  :: [Square],
          title     :: String,
          author    :: String,
          notes     :: String,
          copyright :: String,
          clueCount :: Int,
          clues     :: [(Int,Dir,String)]}
  deriving (Show)


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



{- ---- Exposed library ---- -}

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
     width <- puzGetWidth puz
     height <- puzGetHeight puz

     let sz = width*height
     grid <- readBoard sz gridPtr
     solution <- readBoard sz solPtr

     title <- readString titlePtr
     author <- readString authPtr
     copyright <- readString copyPtr
     notes <- readString notePtr

     clueStrs <- mapM readString cluePtrs
     let clues = map (\s -> (0,Across,s)) clueStrs

     return $
       Puzzle {width, height, grid, solution,
               title, author, copyright, notes,
               clueCount, clues}
