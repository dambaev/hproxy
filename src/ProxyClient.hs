{-# LANGUAGE BangPatterns #-}
module ProxyClient where

import SSHClient
import Control.Concurrent.HEP
import Data.HProxy.Rules
import Data.ByteString.Char8 as C
import Control.Monad
import Control.Monad.Trans
import Text.ParserCombinators.Parsec
import Prelude as P

data ProxyMessage = ProxyRead ByteString
                  | ProxyReadError ByteString
                  | ProxyExit

proxyLogin:: String -- login
          -> Destination -- server
          -> Destination -- destination
          -> Int
          -> HEP (Either String (PortT, Pid))
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
        (liftIO $! do
            P.putStrLn $! "ssh client exited"
            sendMBox inbox $! ProxyExit)
    mmsg <- liftIO $! receiveMBoxAfter 60000 inbox
    stopSSHClient ssh
    case mmsg of
        Nothing -> return $! Left "ERROR: ssh client timeouted"
        Just msg -> case msg of
            ProxyReadError !some -> do
                return $! Left $! unpack some
            ProxyExit -> do
                return $! Left "ERROR: ssh exited unexpectedly"
            ProxyRead !some -> do
                case parse parseOkPort "hproxyserver answer" (unpack some) of
                    Right port -> return $! Right (port,ssh)
                    Left _ -> return $! Left $! unpack some
            
    
parseOkPort:: GenParser Char st PortT
parseOkPort = do
    string "OK"
    spaces
    port <- many1 digit
    return $! read port
    


