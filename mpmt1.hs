--
--  mpmt1.hs: A stupid simple example of Haskell threading (forkIO)
--
-- License:
--   Apache License, Version 2.0
-- History:
--   * 2024/05/12 v0.1 Initial version
-- Author:
--   Masanori Itoh <masanori.itoh@gmail.com>
-- Usage:
---  $ ghc -threaded  -rtsopts mpmt1.hs -o mpmt1hs
---  $ ./mpmt1hs NUM_CONTEXT DURATION(in sec.) +RTS -Nn (n: number of threads)
---  Note that n of -Nn should be greater than equal NUM_CONTEXT in order to
---  use requested multi cores fully.
-- TODO:
--   * Use Getopt
--   * Implement Process model
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan (newChan, writeChan, readChan)
import Text.Printf

t mul = round . (mul *) <$> getPOSIXTime

busyLoop current time_left = do
  when (time_left > 0)
    $ do
      now  <- t 1000
      let elapse = now - current
      busyLoop now  (time_left - elapse)

busyWorker idx duration worker_chan = do
  printf "busyWorker: starting. idx: %d\n"  idx
  now  <- t 1000
  busyLoop now duration
  writeChan worker_chan idx
  printf "busyWorker: expired. idx: %d\n"  idx

main = do

  args <- getArgs
 
  let num_context = if length args >= 1
                    then (read  (args !! 0) :: Int) else 4
  let duration = if length args >= 2
                 then (read  (args !! 1)  :: Int) else 5

  printf "num_context: %d duration: %d (s)\n"  num_context duration

  worker_chan <- newChan

  forM_ [1..num_context] $ \i -> do
    forkIO $ busyWorker i (duration * 1000) worker_chan

  forM_ [1..num_context] $ \i -> do
    ret <- readChan worker_chan
    printf "main: worker exit. idx: %d\n" ret
