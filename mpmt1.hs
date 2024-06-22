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
--   * Implement main and busy_workers synchronization using channel
--     * https://github.com/crabmusket/haskell-simple-concurrency
import Data.Time.Clock.POSIX (getPOSIXTime, getCurrentTime)
import System.Environment
import Control.Monad
import Control.Concurrent
import Text.Printf

t mul = round . (mul *) <$> getPOSIXTime

busyLoop current time_left = do
  if time_left > 0 then
    do
      now  <- t 1000
      let elapse = now - current
      busyLoop now  (time_left - elapse)
  else print "Expired."

busyWorker idx duration = do
  printf "busyWorker: idx: %d\n"  idx
  now  <- t 1000
  busyLoop now duration

main = do

  args <- getArgs
 
  let num_context = if length args >= 2
                    then (read  (head args) :: Int) else 4
  let duration = if not (null args)
                 then (read  (args !! 1)  :: Int) * 1000 else 5000

  printf "num_context: %d duration: %d (ms)\n"  num_context duration

  forM_ [1..num_context] $ \i -> do
    forkIO $ busyWorker i duration

  threadDelay $ duration * 1000
