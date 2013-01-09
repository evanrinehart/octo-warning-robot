
newObject :: Global -> String -> Env -> Case -> IO Object
newObject g str env body = do
  msgQ <- mkChan
  port <- newEmptyMVar
  mem <- newMVar M.empty
  tidmv <- newEmptyMVar
  let obj = Object {
    name = Symbol str,
    response = port,
    fifo = msgQ,
    storage = mem,
    tid = tidmv
  }
  myTid <- forkIO . forever $ do
    (arg, out) <- readChan msgQ
    case apply body env arg of
      Just (env', expr) -> do
        resp <- eval g obj env' expr -- catch here
        writeMVar out (Right resp)
      Nothing -> do
        writeMVar out (Left PatternMatchError)
  writeMVar tidmv myTid
  return obj
