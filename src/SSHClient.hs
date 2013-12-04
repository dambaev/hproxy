{-#LANGUAGE DeriveDataTypeable#-}
{-# LANGUAGE BangPatterns #-}

module SSHClient where

import Control.Concurrent.HEP as H
import Control.Concurrent
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.C.Types
import System.IO as Sys
import Data.ByteString.Char8 as C
import Data.Typeable
import System.Process as P
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import System.Exit
import Data.HProxy.Rules

sshclient = "setsid"

startSSHClient:: Destination
              -> String
              -> String
              -> (ByteString-> HEP ())
              -> (ByteString-> HEP ())
              -> (HEP ())
              -> HEP Pid
startSSHClient (DestinationAddrPort (IPAddress ip) port) username params onRead onError onExit = do
    prog <- startProgram sshclient 
        [ "ssh"
--        , "-n"
        , username++"@" ++ ip
        , "-p " ++ show port
        , params] 
        onRead
        onError
        onExit
    return prog


bufferSize = 64 * 1024

data ProgramState = ProgramState
    { progHIn:: Handle
    , progHOut:: Handle
    , progHErr:: Handle
    , progHandle:: ProcessHandle
    , progSupervisor:: Pid
    }
    deriving Typeable
instance HEPLocalState ProgramState

data ReaderState = ReaderState
    { readerBuffer:: Ptr CChar
    }
    deriving Typeable
instance HEPLocalState ReaderState


data WorkerMessage = WorkerStarted ProgramState
                   | WorkerStopped ExitCode
    deriving Typeable
instance Message WorkerMessage

data WorkerCommand = WorkerStop
    deriving Typeable
instance Message WorkerCommand

data SupervisorState = SupervisorState
    { superWorkerState:: ProgramState
    , superReadout :: Pid
    , superReaderr :: Pid
    }
    deriving Typeable
instance HEPLocalState SupervisorState

data SupervisorCommand = SupervisorStop
                       | SupervisorWrite String
    deriving Typeable
instance Message SupervisorCommand

data SupervisorFeedback = SupervisorStarted
    deriving Typeable
instance Message SupervisorFeedback


startProgram:: String -- cmd
            -> [String] --params
            -> (ByteString-> HEP ()) -- on read stdout
            -> (ByteString-> HEP ()) -- on read stderr
            -> HEP ()
            -> HEP Pid
startProgram cmd args onRead onError onExit = do
    inbox <- liftIO newMBox
    sv <- spawn $! procWithBracket (progSupInit cmd args inbox onRead onError) 
        (onExit >> progSupShutdown) $!
        H.proc $! supervisorWorker
    mmsg <- liftIO $! receiveMBoxAfter 10000 inbox
    return sv
        
progSupInit cmd args outbox onRead onError = do
    me <- self
    worker <- spawn $! procWithSubscriber me $! 
        procWithBracket (progInit me cmd args) (progShutdown) $!
        H.proc $! progWorker
    mmsg <- receiveAfter 10000
    case mmsg of
        Nothing-> procFinished
        Just mls-> do
            case fromMessage mls of
                Nothing-> procFinished
                Just (WorkerStarted ls) -> do
                    readStdout <- spawn $! procWithSubscriber me $! 
                        procWithBracket readerInit readerShutdown $!
                        H.proc $! readerWorker (progHOut ls) onRead
                    readStderr <- spawn $! procWithSubscriber me $! 
                        procWithBracket readerInit readerShutdown $!
                        H.proc $! readerWorker (progHErr ls) onError
                    setLocalState $! Just $! SupervisorState
                        { superWorkerState = ls
                        , superReadout = readStdout
                        , superReaderr = readStderr
                        }
                    liftIO $! sendMBox outbox $! SupervisorStarted
                    procRunning
    

progInit:: Pid-> String-> [String]-> HEPProc
progInit sv cmd args = do
    (Just hin, Just hout, Just herr, hcmd) <- liftIO $! 
        createProcess  (P.proc cmd args ) { std_in = CreatePipe, std_out = CreatePipe , std_err = CreatePipe}
    let ls = ProgramState
            { progHIn = hin
            , progHOut = hout
            , progHErr = herr
            , progHandle = hcmd
            , progSupervisor = sv
            }
    setLocalState $! Just ls
    send sv $! WorkerStarted ls
    procRunning


progWorker:: HEPProc
progWorker = do
    mmsg <- receiveAfter 1000
    Just ls <- localState
    case mmsg of
        Just msg -> do
            case fromMessage msg of
                Nothing-> procRunning
                Just WorkerStop-> do
                    liftIO $! terminateProcess $! progHandle ls
                    procFinished
        Nothing-> do
            mexit <- liftIO $! getProcessExitCode $! progHandle ls
            case mexit of
                Nothing-> procRunning
                Just somecode -> do
                    send (progSupervisor ls) $! WorkerStopped somecode
                    procFinished

progShutdown:: HEPProc
progShutdown = do
    Just ls <- localState
    liftIO $! do
        hClose $! progHErr ls
        hClose $! progHIn ls
        hClose $! progHOut ls
    procFinished
    
progSupShutdown = procFinished


readerInit:: HEPProc
readerInit = do
    buff <- liftIO $! mallocBytes bufferSize
    setLocalState $! Just $! ReaderState
        { readerBuffer = buff
        }
    procRunning

readerShutdown:: HEPProc
readerShutdown =do
    Just ls <- localState
    liftIO $! free $! readerBuffer ls
    procFinished

readerWorker:: Handle
            -> (ByteString-> HEP ())
            -> HEPProc
readerWorker h onRead = do
    Just ls <- localState 
    let  ptr = readerBuffer ls
    !read <- liftIO $! hGetBufSome h ptr bufferSize
    case read of
        0 -> procFinished
        _ -> do
            str <- liftIO $! packCStringLen ( ptr, read)
            onRead str
            procRunning


supervisorWorker:: HEPProc
supervisorWorker = do
    msg <- receive
    let handleChildLinkMessage:: Maybe LinkedMessage -> EitherT HEPProcState HEP HEPProcState
        handleChildLinkMessage Nothing = lift procRunning >>= right
        handleChildLinkMessage (Just (ProcessFinished pid)) = left =<< 
            lift (do
                subscribed <- getSubscribed
                case subscribed of
                    [] -> procFinished
                    _ -> procRunning
            )
        
        handleServiceMessage:: Maybe SupervisorMessage -> EitherT HEPProcState HEP HEPProcState
        handleServiceMessage Nothing = lift procRunning >>= right
        handleServiceMessage (Just (ProcWorkerFailure cpid e _ outbox)) = do
            left =<< lift (do
                    procFinish outbox
                    procRunning
                )
        handleServiceMessage (Just (ProcInitFailure cpid e _ outbox)) = 
            left =<< lift ( do
                procFinish outbox
                procRunning
                )


        handleSupervisorCommand:: Maybe SupervisorCommand-> EitherT HEPProcState HEP HEPProcState
        handleSupervisorCommand Nothing = right =<< lift procRunning
        handleSupervisorCommand (Just SupervisorStop) = do
            left =<< lift (do
                subscribed <- getSubscribed
                forM subscribed $! \pid -> killProc pid
                procRunning
                )
        handleSupervisorCommand (Just (SupervisorWrite str)) = do
            left =<< lift (do
                Just ls <- localState
                liftIO $! Sys.hPutStr (progHIn $! superWorkerState ls) str
                procRunning
                )

    mreq <- runEitherT $! do
        handleChildLinkMessage $! fromMessage msg
        handleServiceMessage $! fromMessage msg 
        handleSupervisorCommand $! fromMessage msg
    case mreq of
        Left some -> return some
        Right some -> return some

writeInput:: Pid-> String-> HEP ()
writeInput pid str = do
    send pid $! SupervisorWrite str
    return ()
