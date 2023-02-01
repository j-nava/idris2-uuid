module Data.UUID

import System
import System.Random
import Data.Bits

export
data UUID = MkUUID String

export
Eq UUID where
  (==) (MkUUID x) (MkUUID y) = x == y

export
Show UUID where
  show (MkUUID x) = x

export
FromString UUID where
  fromString = MkUUID

||| Generates a version 4 UUID given a randomization function
export
generateUUID : Applicative m => m (Fin 16) -> m UUID
generateUUID randomFn =
  MkUUID . pack <$> traverse replace (unpack "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx")

  where
    hexToChar : Int -> Char
    hexToChar i = chr (if i < 10 then i + ord '0' else (i - 10) + ord 'a')

    replace : Char -> m Char
    replace c = 
      let 
        r = the Int . cast . finToInteger <$> randomFn
      in 
      case c of
        'x' => hexToChar <$> r
        'y' => (\ry => hexToChar (ry .&. 0x3 .|. 0x8)) <$> r
        _ => pure c


||| Generates a version 4 UUID using rndFin as the randomization function
export
generateRandomUUID : HasIO io => io UUID
generateRandomUUID = generateUUID (rndFin 15)

