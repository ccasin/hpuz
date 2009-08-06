
module Codec.Game.Puz where

import Codec.Game.Puz.Internal

import System.IO hiding (hGetContents)

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array

import Data.ByteString hiding (map)

import Control.Monad

data Square = Black
            | Letter (Maybe Char)

data Puzzle =
  Puzzle {width,height :: Int,
          grid      :: [Square],
          solution  :: [Square],
          title     :: String,
          author    :: String,
          notes     :: String,
          copyright :: String,

marshallGrid :: Int -> Ptr CUChar -> IO [Square]
marshallGrid sz ptr =
  do chrs <- peekArray sz ptr
     return $ map charToSquare chrs
  where
    blackChar,blankChar :: CUChar
    blackChar = fromIntegral $ fromEnum '.'
    blankChar = fromIntegral $ fromEnum '-'

    charToSquare :: CUChar -> Square
    charToSquare sq = 
      if sq == blackChar then Black else
        if sq == blankChar then Letter Nothing else
          Letter $ Just $ toEnum $ fromIntegral sq
           


-- Here we get the string as a bytestring, then create a cstring from
-- it.  I think it would be faster if the C code did this for us (or
-- if haskell had a function of type Handle -> IO CString), but no
-- such luck.
-- Also, I'm not really sure this is right, since CString is chars, not
-- unsigned chars.
loadPuzzle :: String -> IO Puz
loadPuzzle s =
  do handle <- openFile s ReadMode
     size <- liftM fromIntegral $ hFileSize handle
     bytestring <- hGetContents handle
     let cchars :: [CUChar]
         cchars = foldr' (\w cs -> (fromIntegral w) : cs) [] bytestring
     withArray cchars (\ar -> puzLoad ar size)
         


