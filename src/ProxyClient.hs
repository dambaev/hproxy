{-# LANGUAGE BangPatterns #-}
module ProxyClient where

import SSHClient
import Control.Concurrent.HEP
import Data.HProxy.Rules
import Data.ByteString.Char8 as C
import Control.Monad
import Control.Monad.Trans
import Text.ParserCombinators.Parsec


data ProxyMessage = ProxyRead ByteString
                  | ProxyReadError ByteString
                  | ProxyExit

proxyLogin:: String -- login
          -> Destination -- server
          -> Destination -- destination
          -> Int
          -> HEP (Either String PortT)
proxyLogin login 
           server 
           dest@(DestinationAddrPort (IPAddress ip) port) 
           connectionsCount = do
    inbox <- liftIO $! newMBox
    ssh <- startSSHClient server login
            ("hproxyserver -d " ++ ip ++ ":" ++ show port ++ " -c " ++ 
                show connectionsCount )
        (\x->  liftIO $! sendMBox inbox $! ProxyRead x)
        (\x->  return () ) -- liftIO $! sendMBox inbox $! ProxyReadError x)
        (liftIO $! sendMBox inbox $! ProxyExit)
    msg <- liftIO $! receiveMBox inbox
    case msg of
        ProxyReadError !some -> return $! Left $! unpack some
        ProxyExit -> return $! Left "ssh client exited"
        ProxyRead !some -> do
            case parse parseOkPort "hproxyserver answer" (unpack some) of
                Right port -> return $! Right port
                Left _ -> return $! Left $! unpack some
            
    
parseOkPort:: GenParser Char st PortT
parseOkPort = do
    string "OK"
    spaces
    port <- many1 digit
    return $! read port
    


