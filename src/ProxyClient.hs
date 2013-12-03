module ProxyClient where

import Control.Concurrent.HEP

proxyLogin:: String -- login
          -> String -- password
          -> ServerAddr -- server
          -> ServerPort -- port
          -> HEP (Either String Port)
proxyLogin login password server port = do
    



