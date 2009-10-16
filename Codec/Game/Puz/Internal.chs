{-# LANGUAGE ForeignFunctionInterface #-}

#include "puz.h"

module Codec.Game.Puz.Internal where

import Foreign
import Foreign.Ptr
import Foreign.C

import Text.ParserCombinators.Parsec

{# pointer *puz_head_t as PuzHead foreign newtype #}
{# pointer *puzzle_t as Puz foreign newtype #}

{# enum define PuzType
   { PUZ_FILE_BINARY as PuzTypeBinary
   , PUZ_FILE_TEXT as PuzTypeText
   , PUZ_FILE_UNKNOWN as PuzTypeUnknown
   }
   deriving (Eq,Show)
 #}


-- XXX is this really freeing the right thing?  How could I tell?
marshallPuz :: Ptr Puz -> IO Puz
marshallPuz pp = do fp <- newForeignPtr finalizerFree pp
                    return $ Puz fp

-- custom marshallers
-- IN
alwaysUseIn :: a -> (a -> b) -> b
alwaysUseIn a f = f a

nullIn :: (Ptr a -> IO b) -> IO b
nullIn = alwaysUseIn nullPtr

puzTypeIn :: (CInt -> IO b) -> IO b
puzTypeIn = alwaysUseIn $ cIntConv $ fromEnum PuzTypeUnknown

puzIn :: Puz -> (Ptr Puz -> IO b) -> IO b
puzIn (Puz fp) = withForeignPtr fp

-- OUT
cerrToBool :: CInt -> Bool
cerrToBool = (0 ==)

cintToBool :: CInt -> Bool
cintToBool = (1 ==)

stringOut :: Ptr CUChar -> IO String
stringOut ptr =
  do cuchars <- peekArray0 (0 :: CUChar) ptr
     return $ map (toEnum . (fromIntegral :: CUChar -> Int)) cuchars

rtblParser :: Parser [(Int,String)]
rtblParser =
  sepEndBy (do spaces
               ds <- many1 digit
               char ':'
               reb <- many1 alphaNum
               return (read ds, reb)) 
           (char ';')

rtblOut :: Ptr CUChar -> IO [(Int,String)]
rtblOut ptr =
    do str <- stringOut ptr
       case parse rtblParser "rebus table" str of
         Left err -> error ("Ill-formed puzzle file: " ++ show err)
         Right tbl -> return tbl
    


{- puz struct creation, initialization -}

-- XXX I think we don't actually need puz_init because puz_load also mallocs
-- and accepts null
{# fun puz_init as puzCreate
   {nullIn- `Ptr Puz'} -> `Puz' marshallPuz* #}

{# fun puz_load as puzLoad
   { nullIn- `Ptr Puz'
   , puzTypeIn- `PuzType'
   , id `Ptr CUChar'
   , `Int'
   } ->
   `Puz' marshallPuz* 
 #}

{- check sum checking, generation -}
{# fun puz_cksums_calc as puzCksumsCalc
   { id `Ptr Puz' } -> `()' 
 #}

{# fun puz_cksums_check as puzCksumsCheck
   { id `Ptr Puz' } -> `Bool' cerrToBool
 #}

{# fun puz_cksums_commit as puzCksumsCommit
   { id `Ptr Puz' } -> `()'
 #}

{- accessors -}
-- XXX actually the return guys on the sets check errors

{# fun puz_width_get as puzGetWidth
   { puzIn* `Puz' } -> `Int'
 #}

{# fun puz_width_set as puzSetWidth
   { puzIn* `Puz'
   , `Int'
   } -> 
   `()'
 #}


{# fun puz_height_get as puzGetHeight
   { puzIn* `Puz' } -> `Int'
 #}

{# fun puz_height_set as puzSetHeight
   { puzIn* `Puz'
   , `Int'
   } -> 
   `()'
 #}


{# fun puz_solution_get as puzGetSolution
   { puzIn* `Puz' } -> `Ptr CUChar' id
 #}

{# fun puz_solution_set as puzSetSolution
   { puzIn* `Puz' 
   , id `Ptr CUChar'
   } -> 
   `()'
 #}


{# fun puz_grid_get as puzGetGrid
   { puzIn* `Puz' } -> `Ptr CUChar' id
 #}

{# fun puz_grid_set as puzSetGrid
   { puzIn* `Puz' 
   , id `Ptr CUChar' 
   } -> 
   `()'
 #}


{# fun puz_title_get as puzGetTitle
   { puzIn* `Puz' } -> `String' stringOut*
 #}

{# fun puz_title_set as puzSetTitle
   { puzIn* `Puz'
   , id `Ptr CUChar'
   } -> 
   `()'
 #}


{# fun puz_author_get as puzGetAuthor
   { puzIn* `Puz' } -> `String' stringOut*
 #}

{# fun puz_author_set as puzSetAuthor
   { puzIn* `Puz'
   , id `Ptr CUChar'
   } -> 
   `()'
 #}


{# fun puz_copyright_get as puzGetCopyright
   { puzIn* `Puz'} -> `String' stringOut*
 #}

{# fun puz_copyright_set as puzSetCopyright
   { puzIn* `Puz'
   , id `Ptr CUChar' 
   } -> 
   `()'
 #}


{# fun puz_clue_count_get as puzGetClueCount
   { puzIn* `Puz' } -> `Int'
 #}

{# fun puz_clue_count_set as puzSetClueCount
   { puzIn* `Puz'
   , `Int'
   } -> 
   `()'
 #}


{# fun puz_clue_get as puzGetClue
   { puzIn* `Puz'
   , `Int' 
   } -> 
   `String' stringOut*
 #}

{# fun puz_clue_set as puzSetClue
   { puzIn* `Puz'
   , `Int' 
   , id `Ptr CUChar'
   } -> 
   `()'
 #}


{# fun puz_notes_get as puzGetNotes
   { puzIn* `Puz' } -> `String' stringOut*
 #}

{# fun puz_notes_set as puzSetNotes
   { puzIn* `Puz'
   , id `Ptr CUChar' } 
   -> 
   `()'
 #}


{# fun puz_has_rebus as puzHasRebus
   { puzIn* `Puz' } -> `Bool' cintToBool
 #}

{# fun puz_rebus_get as puzGetRebus
   { puzIn* `Puz' } -> `Ptr CUChar' id
 #}

{# fun puz_rebus_set as puzSetRebus
   { puzIn* `Puz'
   , id `Ptr CUChar' }
   ->
   `()'
 #}

{# fun puz_rebus_count_get as puzGetRebusCount
   { puzIn* `Puz' } -> `Int'
 #}

{# fun puz_rebus_count_set as puzSetRebusCount
   { puzIn* `Puz'
   , `Int' } 
   ->
   `()'
 #}


{# fun puz_rtblstr_get as puzGetRtbl
   { puzIn* `Puz' } -> `[(Int,String)]' rtblOut*
 #}
     
{# fun puz_rtblstr_set as puzSetRtbl
   { puzIn* `Puz'
   , `Int'
   , id `Ptr CUChar' }
   ->
   `()'
 #}



{# fun puz_has_extras as puzHasExtras
   { puzIn* `Puz' } -> `Bool' cintToBool
 #}

{# fun puz_extras_get as puzGetExtras
   { puzIn* `Puz' } -> `Ptr CUChar' id
 #}

{# fun puz_extras_set as puzSetExtras
   { puzIn* `Puz'
   , id `Ptr CUChar' }
   ->
   `()'
 #}


------
------ C2HS stuff - why isn't there a C2HS module
------

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral
