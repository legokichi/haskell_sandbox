module Main where

import Network.Remote.RPC

$(makeHost "Client" "localhost" 9000)
$(makeHost "Server" "localhost" 9001)

calcServer :: Integer -> WIO Server IO Integer
calcServer t = return $ (*) 3.8 $ (*) x $ (-) 1 x

client :: WIO Client IO ()
client = do
  onHost Client
  double <- $(rpcCall 'calcServer) 3
  liftIO $ putStrLn $ "3: " ++ show double

main = do
  runServerBG $(autoService 'Server)
  runServer client
