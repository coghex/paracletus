module Artos.Thread where
-- some helper threading functions can be found
import qualified Control.Concurrent as CC

threadDelay  :: Int -> IO ()
threadDelay  = CC.threadDelay
