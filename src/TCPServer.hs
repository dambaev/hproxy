{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module TCPServer where

import Control.Concurrent
import Control.Exception
import GHC.IO.Exception
import Control.Concurrent.HEP as H
--import Control.Concurrent.HEP.Syslog
import Network as N
import Network.Socket as NS
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C.Types
import Network.Socket as S
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import GHC.IO.Handle
import System.IO
import Data.Typeable
import System.Timeout
import System.IO.Error
import Data.Maybe

data SupervisorState = SupervisorState
    { serverPort:: PortID
    , supervisorStarter:: MBox SupervisorAnswer
    , supervisorWorker:: Pid
    , supervisorConnectionsCount:: Int
    }
    deriving Typeable
instance HEPLocalState SupervisorState

data WorkerState = WorkerState
    { workerHandle :: Maybe Handle
    , workerBuffer :: Ptr CChar
    , workerConsumer:: Maybe Handle
    , workerSocket :: Socket
    }
    deriving Typeable
instance HEPLocalState WorkerState

data ClientState = ClientState
    { clientHandle :: Handle
    , clientBuffer :: Ptr CChar
    , clientConsumer:: Handle
    }
    deriving Typeable
instance HEPLocalState ClientState


data WorkerMessage = WorkerSetConsumer !Handle
    deriving Typeable
instance Message WorkerMessage

data WorkerFeedback = WorkerStarted !PortID
    deriving Typeable
instance Message WorkerFeedback

data ClientFeedback = ClientStarted !Handle
    deriving Typeable
instance Message ClientFeedback


data SupervisorCommand = SetWorkerConsumer !Handle
                       | StopServer
    deriving Typeable
instance Message SupervisorCommand

data SupervisorAnswer = ServerStarted !PortID

data ClientSupervisorCommand = StopClient
    deriving Typeable
instance Message ClientSupervisorCommand

{--
 - size for buffer for client/server connections
 -}
bufferSize = 64*1024

startTCPServerBasePort:: PortID
                      -> Int
                      -> (Int-> HEP ()) 
                      -> (Pid-> Handle-> HEP ()) 
                      -> HEP (Pid, PortID)
startTCPServerBasePort base connectionsCount receiveAction onOpen = do
    !input <- liftIO newMBox
    sv <- spawn $! procWithBracket (serverSupInit base connectionsCount input receiveAction onOpen) 
        serverSupShutdown $! proc $! serverSupervisor receiveAction onOpen 
    ServerStarted !port <- liftIO $! receiveMBox input
    return (sv, port)

setConsumer:: Pid-> Handle-> HEP ()
setConsumer pid !h = do
    H.send pid $! SetWorkerConsumer h
    return ()


serverSupInit:: PortID
             -> Int
             -> MBox SupervisorAnswer
             -> (Int-> HEP ()) 
             -> (Pid-> Handle-> HEP ()) 
             -> HEPProc
serverSupInit port connectionsCount starter receiveAction onOpen = do
    me <- self
    pid <- spawn $! procWithSubscriber me $! 
        procWithBracket (serverInit port me) serverShutdown $! 
        proc $! serverWorker receiveAction (onOpen me)
    addSubscribe pid
    setLocalState $! Just $! SupervisorState
        { serverPort = port
        , supervisorStarter = starter
        , supervisorWorker = pid
        , supervisorConnectionsCount = connectionsCount
        }
    procRunning
    
    
serverInit:: PortID
          -> Pid
          -> HEPProc
serverInit port@(PortNumber (PortNum wport)) svpid = do
    liftIO $! putStrLn  "worker started"
    lsocket <- liftIO $! socket AF_INET Stream defaultProtocol 
    liftIO $! setSocketOption lsocket ReuseAddr 1
    addr <- liftIO $! inet_addr "127.0.0.1"
    liftIO $! bind lsocket (SockAddrInet (PortNum wport) addr)
    liftIO $! listen lsocket maxListenQueue
    buff <- liftIO $! mallocBytes bufferSize
    setLocalState $! Just $! WorkerState
        { workerHandle = Nothing -- h
        , workerBuffer = buff
        , workerConsumer = Nothing
        , workerSocket = lsocket
        }
    H.send svpid $! WorkerStarted port
    procRunning
    
    

serverShutdown :: HEPProc
serverShutdown = do
    ls <- localState
    case ls of
        Nothing -> procFinished
        Just some -> do
            _ <- case (workerHandle some) of
                Nothing -> return ()
                Just wh -> liftIO $! hClose wh
            liftIO $! free (workerBuffer some)
            liftIO $! sClose $! workerSocket some

            _ <- case workerConsumer some of
                Nothing-> return ()
                Just h-> liftIO $! hClose h
            procFinished


serverWorker:: (Int-> HEP ()) -> (Handle-> HEP ()) ->  HEPProc
serverWorker receiveAction onOpen = do
    !mmsg <- receiveMaybe
    case mmsg of
        Just msg -> do
            case fromMessage msg of
                Just (WorkerSetConsumer !hout) -> do
                    Just ls <- localState
                    setLocalState $! Just $! ls { workerConsumer = Just hout}
                    procRunning
        Nothing-> serverIterate receiveAction onOpen

serverIterate receiveAction onOpen = do
    Just ls <- localState
    let !ptr = workerBuffer ls
        !consumer = workerConsumer ls
        !lsocket = workerSocket ls
    case workerHandle ls of
        Nothing -> do
            case consumer of
                Just hcons -> do
                    liftIO $! hClose hcons
                    setLocalState $! Just $! ls
                        { workerConsumer = Nothing
                        }
                    procRunning
                Nothing-> do
                    (h, host, _) <- liftIO $! N.accept lsocket
                    liftIO $! hSetBuffering h NoBuffering
                    liftIO $! hSetBinaryMode h True
                    liftIO $! putStrLn  $! "accepted connection from " ++ show host
                    setLocalState $! Just $! ls 
                        { workerHandle = Just h
                        }
                    onOpen h
                    procRunning
        Just h -> do
            case consumer of
                Nothing-> do
                    liftIO $! yield >> threadDelay 500000
                    procRunning
                Just hout -> do
                    !mread <- liftIO $! timeout 1000000 $! hGetBufSome h ptr bufferSize
                    case mread of
                        Nothing-> procRunning
                        Just 0 -> liftIO $! ioError $! mkIOError eofErrorType "no data received" Nothing Nothing
                        Just !read -> do
                            liftIO $! hPutBuf hout ptr read
                            receiveAction read
                            procRunning


serverSupShutdown = procFinished

serverSupervisor:: (Int-> HEP()) 
                -> (Pid-> Handle-> HEP())
                -> HEPProc
serverSupervisor receiveAction onOpen = do
    Just ls <- localState
    msg <- receive
    let handleChildLinkMessage:: Maybe LinkedMessage -> EitherT HEPProcState HEP HEPProcState
        handleChildLinkMessage Nothing = lift procRunning >>= right
        handleChildLinkMessage (Just (ProcessFinished pid)) = do
            lift $! liftIO $! putStrLn  $! "supervisor: server thread exited"
            subscribed <- lift getSubscribed
            case subscribed of
                [] -> lift procFinished >>= left
                _ -> lift procRunning >>= left
        
        handleServiceMessage:: Maybe SupervisorMessage -> EitherT HEPProcState HEP HEPProcState
        handleServiceMessage Nothing = lift procRunning >>= right
        handleServiceMessage (Just (ProcWorkerFailure cpid e wstate outbox)) = do
            left =<< lift (do
                case fromException e of
                    Just (IOError{ioe_type = ResourceVanished}) -> do
                        liftIO $! putStrLn  $! "supervisor: server connection got ResourceVanished"
                        _ <- if supervisorConnectionsCount ls <= 1 
                            then procFinish outbox
                            else do
                                liftIO $! putStrLn  $! "supervisor: awaiting next connection"
                                case procStateGetLocalState wstate of
                                    Nothing-> procFinish outbox
                                    Just oldstate_ -> do
                                        let newstate = case fromLocalState oldstate_ of
                                                Nothing -> Nothing
                                                Just oldstate -> Just $! toLocalState $! oldstate 
                                                    { workerHandle = Nothing
                                                    }
                                            newwstate = procStateSetLocalState wstate newstate
                                        procContinue outbox $! newwstate
                                        setLocalState $! Just $! ls 
                                            { supervisorConnectionsCount = 
                                                supervisorConnectionsCount ls- 1
                                            }
                        procRunning
                    Just (IOError{ioe_type = EOF}) -> do
                        liftIO $! putStrLn  "supervisor: server connection got EOF"
                        _ <- if supervisorConnectionsCount ls <= 1 
                            then procFinish outbox
                            else do
                                liftIO $! putStrLn  $! "supervisor: awaiting next connection"
                                case procStateGetLocalState wstate of
                                    Nothing-> procFinish outbox
                                    Just oldstate_ -> do
                                        let newstate = case fromLocalState oldstate_ of
                                                Nothing -> Nothing
                                                Just oldstate -> Just $! toLocalState $! oldstate 
                                                    { workerHandle = Nothing
                                                    }
                                            newwstate = procStateSetLocalState wstate newstate
                                        procContinue outbox $! newwstate
                                        setLocalState $! Just $! ls 
                                            { supervisorConnectionsCount = 
                                                supervisorConnectionsCount ls- 1
                                            }
                        procRunning
                    _ -> do
                        liftIO $! putStrLn  $! "supervisor: server " ++ show cpid ++ 
                            " failed with: " ++ show e ++ ", " ++ 
                            ( show $! typeOf e)
                        procFinish outbox
                        procRunning
                )
        handleServiceMessage (Just (ProcInitFailure cpid e _ outbox)) = 
            left =<< lift ( do
                liftIO $! print e
                procFinish outbox
                Just ls <- localState
                let PortNumber port = serverPort ls
                    !newport = PortNumber (port + 1)
                me <- self
                liftIO $! putStrLn  $! "port " ++ show port ++ " is busy"
                pid <- spawn $! procWithSubscriber me $! 
                    procWithBracket (serverInit newport me) 
                        serverShutdown $! 
                    proc $! serverWorker receiveAction (onOpen me)
                addSubscribe pid
                setLocalState $! Just $! ls
                    { serverPort = newport
                    , supervisorWorker = pid
                    }
                procRunning
                )

        handleWorkerFeedback:: Maybe WorkerFeedback-> EitherT HEPProcState HEP HEPProcState
        handleWorkerFeedback Nothing = right =<< lift procRunning
        handleWorkerFeedback (Just (WorkerStarted !port)) = do
            left =<< lift (do
                Just ls <- localState
                let !starter = supervisorStarter ls
                setLocalState $! Just $! ls {serverPort = port}
                liftIO$! sendMBox starter (ServerStarted port)
                procRunning
                )

        handleSupervisorCommand:: Maybe SupervisorCommand-> EitherT HEPProcState HEP HEPProcState
        handleSupervisorCommand Nothing = right =<< lift procRunning
        handleSupervisorCommand (Just (SetWorkerConsumer !handle)) = do
            left =<< lift (do
                Just ls <- localState
                let !worker = supervisorWorker ls
                H.send worker $! WorkerSetConsumer handle
                procRunning
                )
        handleSupervisorCommand (Just StopServer) = do
            left =<< lift (do
                subscribed <- getSubscribed
                forM subscribed $! \pid -> killProc pid
                procRunning
                )

    mreq <- runEitherT $! do
        handleChildLinkMessage $! fromMessage msg
        handleServiceMessage $! fromMessage msg 
        handleWorkerFeedback $! fromMessage msg
        handleSupervisorCommand $! fromMessage msg
    case mreq of
        Left some -> return some
        Right some -> return some

startTCPClient:: String
              -> PortID
              -> Handle
              -> (Int-> HEP ())
              -> HEP (Handle, Pid)
startTCPClient addr port hserver receiveAction = do
    !inbox <- liftIO newMBox
    sv <- spawn $! procWithBracket (clientSupInit addr port inbox hserver receiveAction) procFinished $!
        proc $! clientSupervisor
    ClientStarted !h <- liftIO $! receiveMBox inbox
    return (h,sv)

clientSupInit:: String
             -> PortID
             -> MBox ClientFeedback
             -> Handle
             -> (Int-> HEP ())
             -> HEPProc
clientSupInit addr port outbox hserver receiveAction = do
    me <- self
    pid <- spawn $! procWithSubscriber me $! 
        procWithBracket (clientInit addr port outbox hserver) 
        clientShutdown $! proc $! clientWorker receiveAction
    addSubscribe pid
    procRunning

clientInit:: String-> PortID-> MBox ClientFeedback-> Handle-> HEPProc
clientInit addr port outbox consumer = do
    h <- liftIO $! connectTo addr port 
    liftIO $! hSetBuffering h NoBuffering
    liftIO $! hSetBinaryMode h True
    buff <- liftIO $! mallocBytes bufferSize
    setLocalState $! Just $! ClientState
        { clientHandle = h
        , clientBuffer = buff
        , clientConsumer = consumer
        }
    liftIO $! sendMBox outbox $! ClientStarted h
    procRunning

clientShutdown:: HEPProc
clientShutdown = do
    ls <- localState
    case ls of
        Nothing-> procFinished
        Just some -> do
            liftIO $! do
                hClose (clientHandle some)
                free (clientBuffer some)
            procFinished

clientWorker:: (Int-> HEP ()) -> HEPProc
clientWorker receiveAction = do
    Just ls <- localState
    let !h = clientHandle ls
        !ptr = clientBuffer ls
        !consumer = clientConsumer ls
    {- isready <- liftIO $! hWaitForInput h 1000
    if isready == False 
        then do
            procRunning
        else do -}
    !mread <- liftIO $! timeout 1000000 $! hGetBufSome h ptr bufferSize
    case mread of
        Nothing -> procRunning
        Just 0 -> procFinished
        Just !read -> do
            liftIO $! hPutBuf consumer ptr read
            receiveAction read
            procRunning
            
stopTCPServer:: Pid-> HEP ()
stopTCPServer pid = do
    H.send pid StopServer
    
stopTCPClient:: Pid-> HEP ()
stopTCPClient pid = do
    H.send pid $! StopClient

clientSupervisor:: HEPProc
clientSupervisor = do
    msg <- receive
    let handleChildLinkMessage:: Maybe LinkedMessage -> EitherT HEPProcState HEP HEPProcState
        handleChildLinkMessage Nothing = lift procRunning >>= right
        handleChildLinkMessage (Just (ProcessFinished pid)) = do
            lift $! liftIO $! putStrLn  $! "supervisor: client thread exited "
            subscribed <- lift getSubscribed
            case subscribed of
                [] -> lift procFinished >>= left
                _ -> lift procRunning >>= left
        
        handleServiceMessage:: Maybe SupervisorMessage -> EitherT HEPProcState HEP HEPProcState
        handleServiceMessage Nothing = right =<< lift procRunning
        handleServiceMessage (Just (ProcWorkerFailure cpid e _ outbox)) = do
            left =<< lift (do
                case fromException e of
                    Just (IOError{ioe_type = ResourceVanished}) -> do
                        liftIO $! putStrLn  "supervisor: client connection got ResourceVanished"
                        procFinish outbox
                        procFinished
                    Just (IOError{ioe_type = EOF}) -> do
                        liftIO $! putStrLn  "supervisor: client connection got EOF"
                        procFinish outbox
                        procRunning
                    _ -> do
                        liftIO $! putStrLn  $! "supervisor: client " ++ show cpid ++ 
                            " failed with: " ++ show e
                        procFinish outbox
                        procRunning
                )
        handleServiceMessage (Just (ProcInitFailure cpid e _ outbox)) = do
            left =<< lift (do
                liftIO $! putStrLn $! "ERROR: " ++ show e
                liftIO $! putStrLn  $! "supervisor: client init " ++ show cpid ++ 
                    " failed with: " ++ show e
                procFinish outbox
                procRunning
                )
        handleClientSupervisorCommand:: Maybe ClientSupervisorCommand
                                     -> EitherT HEPProcState HEP HEPProcState
        handleClientSupervisorCommand Nothing =right =<< lift procRunning
        handleClientSupervisorCommand (Just StopClient) = left =<< lift
            ( do
                workers <- getSubscribed
                forM workers $! \pid -> do
                    liftIO $! putStrLn  $! "killing client " ++ show pid
                    killProc pid
                procRunning
            )
    mreq <- runEitherT $! do
        handleChildLinkMessage $! fromMessage msg
        handleServiceMessage $! fromMessage msg 
        handleClientSupervisorCommand $! fromMessage msg
        
    case mreq of
        Left some -> return some
        Right some -> return some

