{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           ClassyPrelude            hiding (stdin)
import           Conduit
import           Data.Conduit.Combinators
import qualified Data.Conduit.List        as CL
import           Data.Metrics
import           Data.Time.Clock

data Stats = Stats { statsCounter   :: TVar Int
                   , statsMeter     :: Meter IO
                   , statsStartTime :: UTCTime }


main :: IO ()
main = do
  now <- getCurrentTime
  m <- meter
  c <- atomically . newTVar $ 0
  let stats = Stats { statsCounter = c
                    , statsMeter = m
                    , statsStartTime = now }
  void . async $ printStats stats
  runConduit $
    stdin
    .| linesUnboundedAscii
    .| CL.mapM_ (processBatch stats)

processBatch :: Stats -> ByteString -> IO ()
processBatch stats _ = do
  mark (statsMeter stats)
  atomically $ modifyTVar (statsCounter stats) (+ 1)

printStats :: Stats -> IO ()
printStats stats = forever $ do
  c <- atomically . readTVar . statsCounter $ stats
  now <- getCurrentTime
  let durationSeconds = realToFrac $ now `diffUTCTime` statsStartTime stats
      nMarks = fromIntegral c
      rate = if durationSeconds == 0
             then 0
             else nMarks / durationSeconds
  rate' <- meanRate (statsMeter stats)
  rate'' <- oneMinuteRate (statsMeter stats)
  say $
    "Events = " ++ tshow nMarks
    ++ "; Seconds = " ++ tshow (round' 2 durationSeconds)
    ++ "; E/s = " ++ tshow (round' 2 rate)
    ++ "; Metrics mean E/s = " ++ tshow (round' 2 rate')
    ++ "; oneMinuteRate E/s = " ++ tshow (round' 2 rate'')
  threadDelay (1 * 10^(6 :: Int))

round' :: Int -> Double -> Double
round' n = (/ 10^n) . (fromIntegral :: Int -> Double) . round . (* 10^n)
