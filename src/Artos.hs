{-# LANGUAGE Strict #-}
module Artos where
-- overarching error handling
import System.Exit ( exitFailure )
import Artos.Except ( displayException, AExcept )
-- fails IO, thus everything,
-- when an error is returned
-- by any of the submodules.
checkStatus ∷ Either AExcept () → IO ()
checkStatus (Right ()) = pure ()
checkStatus (Left err) = do
  putStrLn $ displayException err
  exitFailure
