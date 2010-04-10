{-# LANGUAGE NamedFieldPuns #-}
module Codec.Game.Puz 
       (Style (Plain,Circle), Square (Black,Letter,Rebus), 
        Dir (Across,Down), Puzzle (Puzzle), Index,
        width,height,grid,solution,title,author,notes,
        copyright,timer,clues,locked,
        numberGrid,loadPuzzle,savePuzzle,stringCksum)
where

import Codec.Game.Puz.Internal

import System.IO hiding (hGetContents)
import System.IO.Error

import Foreign.Ptr
import Foreign.C
import Foreign.Marshal.Array

import Data.ByteString hiding (map,foldl,foldl',zip,zipWith,length,find,all,
                               reverse)
import Data.Array
import Data.List
import Data.Maybe

import Control.Monad

{- ------ Types ------- -}

{-| The 'Style' type enumerates the possible styles of fillable squares.
    Currently, there are only two: plain squares and circled squares. -}
data Style = Plain | Circle
  deriving (Eq, Show)

{-| The 'Square' type represents a square in a puzzle. -}
data Square
    -- | Black squares
    = Black
    -- | Standard letter squares, optionally filled in
    | Letter (Maybe Char) Style
    -- | Rebus squares, optionally filled in
    | Rebus String Style
  deriving (Eq, Show)

data Dir = Across | Down
  deriving (Eq, Show)

type Index = (Int,Int)

{-| The 'Puzzle' type represents a particular crossword.  The
    crossword's dimensions are specified by 'width' and 'height'.

    The contents of the puzzle are given by two arrays of 'Square's -
    'grid' and 'solution'.  The board arrays are in row-major order
    and are numbered from (0,0) to (width-1,height-1).  The 'grid'
    board represents the current state of play, and as such its
    squares may be partially or entirely filled in, correctly or
    incorrectly.  The 'solution' board should have the same basic
    layout as the 'grid' board (in terms of black vs letter squares),
    and should be entirely filled in.

    Various other pieces of data about the puzzle are given bu
    'title', 'author', 'notes' and 'copyright', all 'String's.

    There is an optional "timer", which is a number of seconds
    elapsed and a bool that is true if the timer is stopped and
    false otherwise.

    The field 'clues' gives the puzzle's clues.  The numbers in this
    array correspond to the numbering that would appear on the grid.
    To reconstruct this information, see the 'numberGrid' function.
 -}
data Puzzle =
  Puzzle { width,height                  :: Int,
           grid,solution                 :: Array Index Square,
           title,author,notes,copyright  :: String,
           timer                         :: Maybe (Int,Bool),
           clues                         :: [(Int,Dir,String)],
           locked                        :: Maybe CUShort
          }
  deriving (Show)

type ErrMsg = String

{- ------- Constants ------- -}
blankChar,blackChar,extrasBlankChar :: CUChar
blackChar = fromIntegral (fromEnum '.')
blankChar = fromIntegral (fromEnum '-')
extrasBlankChar = toEnum 0

styleMap :: [(CUChar,Style)]
styleMap  = [(0,Plain),(128,Circle)]
styleMap' :: [(Style,CUChar)]
styleMap' = map (\(a,b) -> (b,a)) styleMap

charToStyle :: CUChar -> Maybe Style
charToStyle i = lookup i styleMap

styleToChar :: Style -> CUChar
styleToChar s = fromJust $ lookup s styleMap'


-- how to order clues 
orderClues :: (Int,Dir,String) -> (Int,Dir,String) -> Ordering
orderClues (i1,d1,_) (i2,d2,_) = 
  case compare i1 i2 of
    EQ -> case (d1,d2) of
            (Across,Down) -> LT
            (Down,Across) -> GT
            _ -> EQ
    c -> c

{- ---- Internal marshalling stuff ---- -}

cucharToChar :: CUChar -> Char
cucharToChar = toEnum . fromEnum

charToCUChar :: Char -> CUChar
charToCUChar = toEnum . fromEnum

-- The bool is true of this is a game board and false if it is a solution
-- board
charToSquare :: Bool -> [(Int,String)] -> CUChar -> CUChar -> CUChar ->
                Square
charToSquare isGame rtbl sq rbs ext =
    if sq == blackChar then Black 
    else
      case rebus of
        Just str -> if isGame then 
                        let str' = if sq == blankChar then []
                                   else [cucharToChar sq]
                        in Rebus str' style
                    else Rebus str style
        Nothing -> if sq == blankChar then Letter Nothing style
                     else Letter (Just $ cucharToChar sq) style
  where
    style = case charToStyle ext of
              Just s  -> s
              Nothing -> Plain --XXX maybe I should issue some kind of warning
    
    rebus = if rbs == 0 then Nothing else
              case lookup (fromIntegral rbs) rtbl of
                     Nothing -> error ("Puzzle file contains ill-formed " ++
                                       "rebus section")
                     Just str -> Just str
    

squareToBoardChar :: Square -> CUChar
squareToBoardChar Black        = blackChar
squareToBoardChar (Letter m _) = case m of
                                   Nothing -> blankChar
                                   Just c  -> charToCUChar c
squareToBoardChar (Rebus m _)  = case m of
                                   []    -> blankChar
                                   (c:_) -> charToCUChar c

squareToExtrasChar :: Square -> CUChar
squareToExtrasChar Black        = styleToChar Plain
squareToExtrasChar (Letter _ s) = styleToChar s
squareToExtrasChar (Rebus _ s)  = styleToChar s

gridToExtras :: [Square] -> Maybe [CUChar]
gridToExtras sqs = 
    let es = map squareToExtrasChar sqs
        ps = styleToChar Plain
    in if all (ps==) es then Nothing else Just es

gridToRebus :: [Square] -> Maybe ([(String,Int)],[CUChar])
gridToRebus sqs = 
    case foldl folder (0,[],[]) sqs of
      (0,_,_)     -> Nothing
      (_,rtbl,is) -> Just (reverse rtbl, reverse is)
    where
      folder :: (Int,[(String,Int)],[CUChar]) -> Square ->
                (Int,[(String,Int)],[CUChar])
      folder (n,rtbl,is) sq = 
          case sq of
            Black      -> (n,rtbl,extrasBlankChar:is)
            Letter _ _ -> (n,rtbl,extrasBlankChar:is)
            Rebus s _  -> case lookup s rtbl of
                            Nothing -> (n+1, (s,n):rtbl, (toEnum (n+1)):is)
                            Just n' -> (n, rtbl, (toEnum (n'+1)):is)
            
        

-- The first string is the board.  The second string is the rebus board
-- (or all 0s if none exists).  The [(Int,String)] is the rebus table,
-- (or an empty list of there aren't any to lookup).  The third String
-- is the extras board.
--
-- The bool should be True if this is a game board and false if it is a
-- solution board. 
readBoard :: Bool -> Array Index CUChar -> Array Index CUChar 
          -> [(Int,String)] -> Array Index CUChar 
          -> Array Index Square
readBoard isGame bd rbs rtbl ext = 
    let convChar = charToSquare isGame rtbl in
      array (bounds bd) 
            (map (\(i,c) -> (i,convChar c (rbs ! i) (ext ! i)))
                 (assocs bd))

boardCharsOut :: Int -> Int -> Ptr CUChar -> IO (Array Index CUChar)
boardCharsOut width height ptr =
  let -- these guys are in row-major order, so we need to flip
      numberFold :: (Int,Int,[(Index,a)]) -> a->
                    (Int,Int,[(Index,a)])
      numberFold (x,y,l) sq = 
        let (x',y') = if x+1 == width then (0,y+1) else (x+1,y) in
          (x',y',(((x,y),sq):l))
  in
  do cuchars <- peekArray (width*height) ptr

     return $ array ((0,0),(width-1,height-1)) $
                (\(_,_,l) -> l) $ foldl' numberFold (0,0,[]) cuchars

numberClues :: [String] -> Array Index Square -> [(Int,Dir,String)]
numberClues cls bd =
  zipWith (\(a,b) c -> (a,b,c)) (findclues 1 (0,0)) cls
  where
    (_,(xmax,ymax)) = bounds bd

    -- sq number -> position -> list of places clues are needed
    findclues :: Int -> Index -> [(Int,Dir)]
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

numberGrid :: Array Index Square -> Array Index (Maybe Int)
numberGrid grid = 
  array (bounds grid) bd_ass
  where
    indexCompare :: Index -> Index -> Ordering
    indexCompare (i1,i2) (j1,j2) = case compare i2 j2 of
                                     LT -> LT
                                     GT -> GT
                                     EQ -> compare i1 j1

    ass :: [(Index,Square)]
    ass = sortBy (\(i,_) (j,_) -> indexCompare i j) $ assocs grid

    isEmpty :: Index -> Bool
    isEmpty i = case lookup i ass of
                  Nothing           -> True
                  Just Black        -> True
                  Just (Letter _ _) -> False
                  Just (Rebus _ _)  -> False

    folder :: (Int, [(Index, Maybe Int)]) -> 
              (Index,Square) -> 
              (Int, [(Index, Maybe Int)])
    folder (ct,ns) (i@(ix,iy),sq) =
      let up_e, left_e :: Bool
          up_e   = isEmpty (ix,iy-1)
          left_e = isEmpty (ix-1,iy)
      in
      case sq of 
        Black -> (ct, (i,Nothing) : ns)
        Letter _ _ -> 
            if up_e || left_e 
              then (ct+1, (i, Just ct) : ns)
              else (ct  , (i, Nothing) : ns)
        Rebus _ _  -> 
            if up_e || left_e 
              then (ct+1, (i, Just ct) : ns)
              else (ct  , (i, Nothing) : ns)

    bd_ass :: [(Index, Maybe Int)]
    (_,bd_ass) = foldl folder (1,[]) ass


loadPuzzle :: String -> IO (Either Puzzle ErrMsg)
loadPuzzle fname =
  do --- Start by getting internal puz representation
     ehandle <- try (openFile fname ReadMode)
     case ehandle of
       Left err -> 
         if isDoesNotExistError err
           then return $ Right $ "File " ++ fname ++ " does not exist."
           else 
             if isPermissionError err
               then
                 return $ Right $ "Cannot access file " ++ fname ++ 
                                  ". (permissions error)"
               else return $ Right $ "Cannot open " ++ fname
       Right handle -> do 
         size <- liftM fromIntegral $ hFileSize handle
         bytestring <- hGetContents handle
         hClose handle
         
         let cchars :: [CUChar]
             cchars = foldr' (\w cs -> (fromIntegral w) : cs) [] bytestring
         mpuz <- withArray cchars (\ar -> puzLoad ar size)
         case mpuz of
           Nothing -> return $ Right "Ill-formed puzzle"
           Just puz -> 
             puzCksumsCheck puz >>= 
               \v -> if not v 
                       then return $ 
                               Right "Ill-formed puzzle: bad checksums"
                       else do
             width  <- puzGetWidth puz
             height <- puzGetHeight puz
             let bdChrs = boardCharsOut width height
                 emptyBd = listArray ((0,0),(width-1,height-1)) 
                                     (repeat $ toEnum 0)
             
             -- Now get all the raw strings we need from the internal 
             -- puz structure
             gridChrs  <- puzGetGrid puz >>= bdChrs
             solChrs   <- puzGetSolution puz >>= bdChrs
             
             title     <- puzGetTitle puz
             author    <- puzGetAuthor puz
             copyright <- puzGetCopyright puz
             notes     <- puzGetNotes puz

             hasTimer  <- puzHasTimer puz
             timer    <- if hasTimer 
                           then liftM2 (\x y -> Just (x,y)) 
                                  (puzGetTimerElapsed puz) 
                                  (puzGetTimerStopped puz)
                           else return Nothing
             
             hasRebus  <- puzHasRebus puz
             rebusChrs <- if hasRebus then puzGetRebus puz >>= bdChrs
                                      else return emptyBd
             rebusTbl  <- if hasRebus then puzGetRtbl puz
                                      else return []
             
             hasExtras <- puzHasExtras puz
             extraChrs <- if hasExtras then puzGetExtras puz >>= bdChrs
                                       else return emptyBd
             
             clueCount <- puzGetClueCount puz
             clueStrs  <- mapM (puzGetClue puz) [0..(clueCount-1)]

             isScrambled <- puzIsLockedGet puz
             locked <- if isScrambled then liftM Just $ puzLockedCksumGet puz
                                      else return Nothing

             -- we use these strings and the puz data to get everything we
             -- need to build a Puzzle
             let grid, solution :: Array Index Square
                 grid     = readBoard True gridChrs rebusChrs rebusTbl 
                                      extraChrs
                 solution = readBoard False solChrs rebusChrs rebusTbl 
                                      extraChrs
             
                 clues :: [(Int,Dir,String)]
                 clues = numberClues clueStrs grid
             
             return $ Left $
               Puzzle {width, height, grid, solution,
                       title, author, copyright, notes, timer,
                       clues, locked}

savePuzzle :: String -> Puzzle -> IO (Maybe ErrMsg)
savePuzzle fname (Puzzle {width, height, grid, solution,
                          title, author, notes, copyright, timer,
                          clues, locked}) =
  let clueCount = length clues
      clueStrs  = map (\(_,_,s) -> s) (sortBy orderClues clues)

      -- since these arrays are row-major but that's the wrong order for
      -- libpuz, we flip the indices first, which gets us a list in the
      -- right order
      gridSqs,solSqs :: [Square]
      gridSqs = elems (ixmap ((0,0),(height-1,width-1)) 
                             (\(a,b) -> (b,a)) grid)
      solSqs  = elems (ixmap ((0,0),(height-1,width-1)) 
                             (\(a,b) -> (b,a)) solution)

      userBoard,solBoard :: [CUChar]
      userBoard   = map squareToBoardChar gridSqs
      solBoard    = map squareToBoardChar solSqs

      extrasBoard :: Maybe [CUChar]
      extrasBoard = gridToExtras solSqs

      rebusInfo :: Maybe ([(String,Int)],[CUChar])
      rebusInfo = gridToRebus solSqs
  in
  do puz <- puzCreate
            
     -- set the easy stuff that is marshalled by Internal
     puzSetWidth     puz width
     puzSetHeight    puz height

     puzSetTitle     puz title
     puzSetAuthor    puz author
     puzSetNotes     puz notes
     puzSetCopyright puz copyright

     case timer of
       Nothing -> return ()
       Just (e,s) -> puzSetTimer puz e s

     puzSetClueCount puz clueCount
     mapM_ (\(n,c) -> puzSetClue puz n c) (zip [0..] clueStrs)

     withArray userBoard (puzSetGrid puz)
     withArray solBoard (puzSetSolution puz)

     case extrasBoard of
       Nothing -> return ()
       Just b -> withArray b (puzSetExtras puz)
       
     case rebusInfo of
       Nothing -> return ()
       Just (rtbl,rbd) -> do withArray rbd (puzSetRebus puz)
                             puzSetRtbl puz rtbl

     case locked of
       Nothing -> return ()
       Just cksum -> puzLockSet puz cksum
                             
     puzCksumsCalc puz
     puzCksumsCommit puz
     cksumChk <- puzCksumsCheck puz

     if not cksumChk 
       then return $ Just "Internal Error: Checksum calculation failed." 
       else
         do sz <- puzSize puz
            allocaArray sz
              (\ ptr -> 
                  do saveChk <- puzSave puz ptr sz
                     if not saveChk 
                       then return $ Just "Internal Error: puzSave failed."
                       else catch 
                              (do handle <- openFile fname WriteMode
                                  hPutBuf handle ptr sz
                                  hClose handle
                                  return Nothing)
                              (\err -> return $ Just $ show err))


stringCksum :: String -> IO CUShort
stringCksum s = puzCksumString s (length s)
