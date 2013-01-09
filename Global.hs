module Global where

import Object

type Global = MVar (Map Value Object)

emptyGlobal :: IO Global
emptyGlobal = newMVar M.empty

