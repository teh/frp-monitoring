module Main where

import           BasicPrelude

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Event.Handler (AddHandler, newAddHandler, register)
import           Control.Monad (forever)
import           Data.HashMap.Strict (lookupDefault)
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), DiffTime, NominalDiffTime, secondsToDiffTime, addUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Pipes ((>->), runEffect, await, Consumer)
import           Pipes.Safe (runSafeT)
import           Reactive.Banana (Moment, Event, Behavior, filterE, accumB, mapAccum)
import           Reactive.Banana.Frameworks (compile, fromAddHandler, reactimate, actuate, Frameworks, changes, reactimate')
import qualified Systemd.Journal as J
import qualified Data.Foldable as Foldable


class HasTimestamp a where
    getTimestamp :: a -> UTCTime

instance HasTimestamp J.JournalEntry where
    getTimestamp entry =
        posixSecondsToUTCTime (fromIntegral timeAsInt / 10^6)
      where
        timeAsInt = read (decodeUtf8 lookupTime) :: Int
        lookupTime =
            lookupDefault "0" (J.mkJournalField "_SOURCE_REALTIME_TIMESTAMP") (J.journalEntryFields entry)

windowFixed :: Int -> a -> [a] -> [a]
windowFixed len new acc =
    if length acc < len
    then acc ++ [new]
    else tail acc ++ [new]

data TimeAccumulator a = TimeAccumulator
                         { timeAccumulatorRecords :: [a]
                         , timeAccumulatorLastSeen :: UTCTime
                         }

instance Foldable TimeAccumulator where
    length ta = length (timeAccumulatorRecords ta)

instance Monoid (TimeAccumulator a) where
    mempty = emptyTimeAccumulator
    mappend a b = TimeAccumulator ((timeAccumulatorRecords a) ++ (timeAccumulatorRecords b)) (timeAccumulatorLastSeen b)

emptyTimeAccumulator = TimeAccumulator [] (UTCTime (ModifiedJulianDay 0) 0)

windowTime :: (HasTimestamp hasTimestamp) => NominalDiffTime -> hasTimestamp -> TimeAccumulator hasTimestamp -> TimeAccumulator hasTimestamp
windowTime window newEntry acc =
    TimeAccumulator
    { timeAccumulatorLastSeen = getTimestamp newEntry
    , timeAccumulatorRecords = newEntry : toKeep
    }
  where
    toKeep = filter (\x -> getTimestamp x > cutoff) (timeAccumulatorRecords acc)
    cutoff = addUTCTime (-window) (getTimestamp newEntry)

mean :: (Foldable f, Integral a) => a -> f a -> Double
mean new acc = fromIntegral (Foldable.sum acc) / (fromIntegral ((length acc) + 1))

count :: Foldable f => a -> f a -> Double
count new acc = fromIntegral $ (length acc) + 1

network :: Frameworks fw => AddHandler J.JournalEntry -> Moment fw ()
network h = do
    logline <- fromAddHandler h
    let (ev1, bh1) = mapAccum [] (fmap (\x acc -> (count x acc, (windowFixed 4) x acc)) logline)
    let (ev2, bh2) = mapAccum emptyTimeAccumulator (fmap (\x acc -> (count x acc, (windowTime 120) x acc)) logline)

    reactimate $ fmap (\x -> print ("count", x)) ev1
    reactimate $ fmap (\x -> print ("window", x)) ev2
    reactimate $ fmap print (fmap getTimestamp logline)

main = do
    (addHandler, fire) <- newAddHandler
    net <- compile (network addHandler)
    actuate net

    runSafeT $ do
        let journal = J.openJournal [] J.FromEnd Nothing Nothing
        runEffect (journal >-> (fireConsumer fire))

  where
    fireConsumer :: (Monad m, MonadIO m) => (J.JournalEntry -> IO ()) -> Consumer J.JournalEntry m ()
    fireConsumer fire = do
        entry <- await
        liftIO $ fire entry
        fireConsumer fire
