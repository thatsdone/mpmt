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
--
import Data.Time.Clock.POSIX (getPOSIXTime, getCurrentTime)
import System.Environment
import Control.Concurrent
import Text.Printf

t mul = round . (mul *) <$> getPOSIXTime

busy_loop current time_left = do
  if time_left > 0 then
    do
      now  <- t 1000
      let elapse = now - current
      busy_loop now  (time_left - elapse)
  else print "Expired."

busy_worker duration = do
  now  <- t 1000
  busy_loop now duration

create_busy_worker 0 duration = return ()
create_busy_worker n duration =
  do
    forkIO $ busy_worker duration
    create_busy_worker (n - 1) duration

main = do

  args <- getArgs
 
  let num_context = if length args >= 2
                    then (read  (head args) :: Int) else 4
  let duration = if not (null args)
                 then (read  (args !! 1)  :: Int) * 1000 else 5000

  printf "duration: %d (ms) num_context: %d\n"  duration num_context

  create_busy_worker  num_context duration

  threadDelay $ duration * 1000
