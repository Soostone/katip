-- | Time and memory efficient time encoding helper functions.
module Katip.Format.Time
  ( formatAsLogTime,
    formatAsIso8601,
    formatAsTime
  )
where

import Control.Monad.ST (ST)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Array as TA
import Data.Text.Internal (Text (..))
import Data.Time (Day, DiffTime, UTCTime (..), toGregorian)
import Data.Word (Word16)
import Unsafe.Coerce (unsafeCoerce)

-- Note: All functions here are optimized to never allocate anything
-- on heap. At least on ghc 8.0.1 no extra strictness annotations are
-- seem to be needed.
--
-- Exported functions are INLINEABLE

-- | Format 'UTCTime' into a short human readable format.
--
-- >>> formatAsLogTime $ UTCTime (fromGregorian 2016 1 23) 5025.123456789012
-- "2016-01-23 01:23:45"
formatAsLogTime :: UTCTime -> Text
formatAsLogTime (UTCTime day time) = toText $
  TA.run2 $ do
    buf <- TA.new 19 -- length "2016-10-20 12:34:56"
    _ <- writeDay buf 0 day
    TA.unsafeWrite buf 10 0x20 -- space
    _ <- writeTimeOfDay False buf 11 (diffTimeOfDay64 time)
    return (buf, 19)
  where
    toText (arr, len) = Text arr 0 len
{-# INLINEABLE formatAsLogTime #-}

formatAsTime :: DiffTime -> Text
formatAsTime time = toText $
  TA.run2 $ do
    buf <- TA.new 8 -- length "12:34:56"
    _ <- writeTimeOfDay False buf 0 (diffTimeOfDay64 time)
    return (buf, 8)
  where
    toText (arr, len) = Text arr 0 len
{-# INLINEABLE formatAsTime #-}

-- | Format 'UTCTime' into a Iso8601 format.
--
--  Note that this function may overcommit up to 12*2 bytes, depending
--  on sub-second precision. If this is an issue, make a copy with a
--  'Data.Text.copy'.
--
-- >>> formatAsIso8601 $ UTCTime (fromGregorian 2016 1 23) 5025.123456789012
-- "2016-11-23T01:23:45.123456789012Z"
-- >>> formatAsIso8601 $ UTCTime (fromGregorian 2016 1 23) 5025.123
-- "2016-01-23T01:23:45.123Z"
-- >>> formatAsIso8601 $ UTCTime (fromGregorian 2016 1 23) 5025
-- "2016-01-23T01:23:45Z"

--
formatAsIso8601 :: UTCTime -> Text
formatAsIso8601 (UTCTime day time) = toText $
  TA.run2 $ do
    buf <- TA.new 33 -- length "2016-10-20 12:34:56.123456789012Z"
    _ <- writeDay buf 0 day
    TA.unsafeWrite buf 10 0x54 -- T
    next <- writeTimeOfDay True buf 11 (diffTimeOfDay64 time)
    TA.unsafeWrite buf next 0x5A -- Z
    return (buf, next + 1)
  where
    toText (arr, len) = Text arr 0 len
{-# INLINEABLE formatAsIso8601 #-}

-- | Writes the @YYYY-MM-DD@ part of timestamp
writeDay :: TA.MArray s -> Int -> Day -> ST s Int
writeDay buf off day =
  do
    TA.unsafeWrite buf (off + 0) (digit y1)
    TA.unsafeWrite buf (off + 1) (digit y2)
    TA.unsafeWrite buf (off + 2) (digit y3)
    TA.unsafeWrite buf (off + 3) (digit y4)
    TA.unsafeWrite buf (off + 4) 0x2d -- dash
    TA.unsafeWrite buf (off + 5) m1
    TA.unsafeWrite buf (off + 6) m2
    TA.unsafeWrite buf (off + 7) 0x2d -- dash
    TA.unsafeWrite buf (off + 8) d1
    TA.unsafeWrite buf (off + 9) d2
    return (off + 10)
  where
    (yr, m, d) = toGregorian day
    (y1, ya) = fromIntegral (abs yr) `quotRem` 1000
    (y2, yb) = ya `quotRem` 100
    (y3, y4) = yb `quotRem` 10
    T m1 m2 = twoDigits m
    T d1 d2 = twoDigits d
{-# INLINE writeDay #-}

-- | Write time of day, optionally with sub seconds
writeTimeOfDay :: Bool -> TA.MArray s -> Int -> TimeOfDay64 -> ST s Int
writeTimeOfDay doSubSeconds buf off (TOD hh mm ss) =
  do
    TA.unsafeWrite buf off h1
    TA.unsafeWrite buf (off + 1) h2
    TA.unsafeWrite buf (off + 2) 0x3A -- colon
    TA.unsafeWrite buf (off + 3) m1
    TA.unsafeWrite buf (off + 4) m2
    TA.unsafeWrite buf (off + 5) 0x3A -- colon
    TA.unsafeWrite buf (off + 6) s1
    TA.unsafeWrite buf (off + 7) s2
    if doSubSeconds && frac /= 0
      then writeFracSeconds buf (off + 8) frac
      else return (off + 8)
  where
    T h1 h2 = twoDigits hh
    T m1 m2 = twoDigits mm
    T s1 s2 = twoDigits (fromIntegral real)
    (real, frac) = ss `quotRem` pico
    pico = 1000000000000 -- number of picoseconds  in 1 second

writeFracSeconds :: TA.MArray s -> Int -> Int64 -> ST s Int
writeFracSeconds buf off frac =
  do
    TA.unsafeWrite buf off 0x2e -- period
    if mills == 0
      then do
        writeTrunc6 buf (off + 1) (fromIntegral mics)
      else do
        writeDigit6 buf (off + 1) (fromIntegral mics)
        writeTrunc6 buf (off + 7) (fromIntegral mills)
  where
    (mics, mills) = frac `quotRem` micro
    micro = 1000000 -- number of microseconds in 1 second

writeDigit6 :: TA.MArray s -> Int -> Int -> ST s ()
writeDigit6 buf off i =
  do
    writeDigit3 buf off f1
    writeDigit3 buf (off + 3) f2
  where
    (f1, f2) = i `quotRem` 1000
{-# INLINE writeDigit6 #-}

writeDigit3 :: TA.MArray s -> Int -> Int -> ST s ()
writeDigit3 buf off i =
  do
    TA.unsafeWrite buf off (digit d1)
    TA.unsafeWrite buf (off + 1) (digit d2)
    TA.unsafeWrite buf (off + 2) (digit d3)
  where
    (d1, d) = i `quotRem` 100
    (d2, d3) = d `quotRem` 10
{-# INLINE writeDigit3 #-}

writeTrunc6 :: TA.MArray s -> Int -> Int -> ST s Int
writeTrunc6 buf off i =
  if f2 == 0
    then writeTrunc3 buf off f1
    else do
      writeDigit3 buf off f1
      writeTrunc3 buf (off + 3) f2
  where
    (f1, f2) = i `quotRem` 1000
{-# INLINE writeTrunc6 #-}

writeTrunc3 :: TA.MArray s -> Int -> Int -> ST s Int
writeTrunc3 buf off i
  | d == 0 = do
    TA.unsafeWrite buf off (digit d1)
    return (off + 1)
  | d3 == 0 = do
    TA.unsafeWrite buf off (digit d1)
    TA.unsafeWrite buf (off + 1) (digit d2)
    return (off + 2)
  | otherwise = do
    TA.unsafeWrite buf off (digit d1)
    TA.unsafeWrite buf (off + 1) (digit d2)
    TA.unsafeWrite buf (off + 2) (digit d3)
    return (off + 3)
  where
    (d1, d) = i `quotRem` 100
    (d2, d3) = d `quotRem` 10
{-# INLINE writeTrunc3 #-}

-- Following code was adapted from aeson package.
--
-- Copyright:   (c) 2015-2016 Bryan O'Sullivan
-- License:     BSD3

data T = T {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16

twoDigits :: Int -> T
twoDigits a = T (digit hi) (digit lo)
  where
    (hi, lo) = a `quotRem` 10

digit :: Int -> Word16
digit x = fromIntegral (x + 48)

data TimeOfDay64
  = TOD
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int64

diffTimeOfDay64 :: DiffTime -> TimeOfDay64
diffTimeOfDay64 t = TOD (fromIntegral h) (fromIntegral m) s
  where
    (h, mp) = fromIntegral pico `quotRem` 3600000000000000
    (m, s) = mp `quotRem` 60000000000000
    pico = unsafeCoerce t :: Integer
