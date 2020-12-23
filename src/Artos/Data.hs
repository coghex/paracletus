module Artos.Data where
-- various data structures for threads
import Prelude()
import UPrelude

-- events processed by the main thread
data Event = EventError !String
           | EventLogDebug !String
           deriving (Show, Eq)

-- cmds that can be asked of
-- the loading thread
data LoadCmd = LoadCmdNULL
