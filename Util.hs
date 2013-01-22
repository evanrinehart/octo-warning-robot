module Util where

whenJust :: Maybe a -> (a -> IO ()) -> IO ()
whenJust Nothing _ = return ()
whenJust (Just x) f = f x

