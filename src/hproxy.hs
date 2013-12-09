{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude as P
import Control.Concurrent.HEP as H
--import ProxyClient
import Network
import HElementary.Elementary
import HElementary.ElementaryFFI
import Data.ByteString.UTF8 as U
import Data.ByteString.Char8 as C8
import System.IO as S
import Foreign.Ptr
import Control.Monad
import Control.Concurrent
import Control.Monad.Trans
import Data.Typeable
import Data.HProxy.Rules
import System.Environment
import System.Console.GetOpt
import Data.HProxy.Session
import Data.HProxy.Rules
import Text.ParserCombinators.Parsec
import ProxyClient
import TCPServer
import Data.Maybe
import System.Process as S

data TCPMessage = ClientConnected Handle

data GuiMessage = GuiExit
                | GuiLogin ByteString (Ptr C'Evas_Object)
    deriving Typeable
instance Message GuiMessage

data MainFlag = FlagDestination Destination
              | FlagServer Destination
              | FlagClient String
              | FlagPreSrv String
              | FlagPrePort String
              | FlagPostPort String
              | FlagClientScript String
              | FlagConnectionsCount Int
    deriving Show

data MainOptions = MainOptions
    { optionDestination:: Maybe Destination
    , optionServer:: Maybe Destination
    , optionClientScript:: String
    , optionClient:: String
    , optionPreSrv:: String
    , optionPrePort:: String
    , optionPostPort:: String
    , optionConnectionsCount:: Int
    }
    deriving Typeable
instance HEPLocalState MainOptions

defaultMainOptions = MainOptions
    { optionDestination = Nothing
    , optionServer = Nothing
    , optionClient = ""
    , optionPreSrv = ""
    , optionPrePort = ":" -- to be addr:port
    , optionPostPort = ""
    , optionClientScript = ""
    , optionConnectionsCount = 1
    }

mainBasePort = PortNumber 10000
connectionTimeout = 20000

options :: [OptDescr MainFlag]
options = 
    [ Option ['d']     ["dest"]  (ReqArg getDestFlag "addr:port") "destination"
    , Option ['s']     ["server"]  (ReqArg getServerFlag "addr:port") "proxy server"
    , Option ['c']     ["client"]  (ReqArg getClientFlag "<program name>") "name of client program to run"
    , Option []     ["opt-pre-srv"]  (ReqArg getPreSrvFlag "\"some prefix\"") "command line prefix before server IP"
    , Option []     ["opt-pre-port"]  (ReqArg getPrePortFlag "\"some prefix\"") "command line prefix before server TCP Port"
    , Option []     ["opt-post-port"]  (ReqArg getPostPortFlag "\"some postfix\"") "command line postfix after server TCP Port"
    , Option []     ["client-script"]  (ReqArg getClientScriptFlag "\"script path\"") "script path, that receives $1 = connection IP, $2 = connection port"
    , Option []     ["conn-count"]  (ReqArg getConnectionsCountFlag "connections' count") "how many connections can be made by client"
    ]
    
getDestFlag:: String-> MainFlag
getDestFlag str = let parsed = parse parseAddrPort "arg" ("addr "++str)
    in case parsed of
        Right dest -> FlagDestination $! dest
        Left some -> error $! "option -d: " ++ show some

getConnectionsCountFlag:: String-> MainFlag
getConnectionsCountFlag str = 
    FlagConnectionsCount $! read str


getServerFlag:: String-> MainFlag
getServerFlag str = let parsed = parse parseAddrPort "arg" ("addr "++str)
    in case parsed of
        Right dest -> FlagServer $! dest
        Left some -> error $! "option -s: " ++ show some

getClientFlag:: String-> MainFlag
getClientFlag = FlagClient

getPreSrvFlag:: String-> MainFlag
getPreSrvFlag = FlagPreSrv

getPrePortFlag:: String-> MainFlag
getPrePortFlag = FlagPrePort

getPostPortFlag:: String-> MainFlag
getPostPortFlag = FlagPostPort

getClientScriptFlag:: String-> MainFlag
getClientScriptFlag = FlagClientScript

getHProxyOptions:: [String]-> IO [MainFlag]
getHProxyOptions argv =
    case getOpt Permute options argv of
        ([],_,_) -> ioError (userError (usageInfo header options))
        (!o,n,[]  ) -> return o
        (_,_,errs) -> ioError (userError (P.concat errs ++ usageInfo header options))
    where header = "Usage: hproxy -s ip:port -d ip:port --client-script myscript.sh | hproxy -s ip:port -d ip:port --client ssh --opt-pre-srv=user@ --opt-pre-port=\"-p\""


debug = True

main = withSocketsDo $! withElementary $! runHEPGlobal $! 
        procForker forkOS $!
        procWithBracket mainInit mainShutdown $! H.proc $! do
    mmsg <- receiveMaybe
    case mmsg of
        Nothing-> do
            liftIO $! do
                ecore_main_loop_iterate
                threadDelay 10000
            procRunning
        Just msg -> case fromMessage msg of
            Nothing -> procRunning
            Just GuiExit -> do
                procs <- getProcs
                liftIO $! print procs
                procFinished
            Just (GuiLogin login parent ) -> do
                Just ls <- localState 
                when debug $! liftIO $! C8.putStrLn $! login
                
                ret <- proxyLogin (unpack login) (fromJust $! optionServer ls) (fromJust $! optionDestination ls) (optionConnectionsCount ls)
                case ret of
                    Left !message -> do
                        showMessage message parent
                        procRunning
                    Right port -> do
                        when debug $! liftIO $! S.putStrLn $! "logged in"
                        Just ls <- localState
                        spawn $! H.proc $! startClient port (unpack login) ls
                        procRunning

mainShutdown = do
    liftIO elm_exit
    procFinished


startClient:: Int-> String-> MainOptions -> HEPProc
startClient remoteport login opts = do
    inbox <- liftIO $! newMBox
    (srv,(PortNumber localport)) <- startTCPServerBasePort mainBasePort 
        (optionConnectionsCount opts)
        (\_ -> return ()) -- receive
        onClientConnected
        --(\h -> liftIO $! sendMBox inbox $! ClientConnected h)
    startClientProg login "localhost" (fromIntegral localport) opts
    procFinished
        where
        onClientConnected:: Pid-> Handle -> HEP ()
        onClientConnected srv hlocal = do
            when debug $! liftIO $! S.putStrLn $! "client connected"
            let Just (DestinationAddrPort (IPAddress remotehost) _) = optionServer opts
            (hremote, clientpid) <- startTCPClient remotehost (PortNumber $! fromIntegral remoteport) hlocal (\_-> return ())
            setConsumer srv hremote
                
    {- mmsg  <- liftIO $! receiveMBoxAfter connectionTimeout inbox
    case mmsg of 
        Nothing-> do
            liftIO $! S.putStrLn $! "noone connected"
            stopTCPServer srv
            procFinished
        Just (ClientConnected !hlocal) -> do
            when debug $! liftIO $! S.putStrLn $! "client connected"
            let Just (DestinationAddrPort (IPAddress remotehost) _) = optionServer opts
            (hremote, clientpid) <- startTCPClient remotehost (PortNumber $! fromIntegral remoteport) hlocal (\_-> return ())
            setConsumer srv hremote
            procFinished
-}
startClientProg:: String
               -> String
               -> PortT
               -> MainOptions
               -> HEP ()
startClientProg login server port
    (MainOptions{ optionClientScript = script}) | P.length script>0 = do
        liftIO $! do
            S.putStrLn $! "exec: " ++ script ++ " " ++ login ++ " " ++ server ++ " " ++ show port
            createProcess $! ( S.proc script 
                [ login
                , server
                , show port
                ] )
        return ()


generateOptions:: [MainFlag]-> MainOptions-> MainOptions
generateOptions [] !tmp = tmp
generateOptions ((FlagServer dest):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionServer = Just dest
        }
generateOptions ((FlagDestination dest):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionDestination = Just dest
        }
generateOptions ((FlagClientScript str):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionClientScript = str
        }
generateOptions ((FlagClient str):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionClient = str
        }
generateOptions ((FlagPreSrv str):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionPreSrv = str
        }
generateOptions ((FlagPrePort str):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionPrePort = str
        }
generateOptions ((FlagPostPort str):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionPostPort = str
        }
generateOptions ((FlagConnectionsCount cnt):ls) tmp = generateOptions ls newtmp
    where
    !newtmp = tmp
        { optionConnectionsCount = cnt
        }

reactOnOptions:: MainOptions -> HEPProc
reactOnOptions opts@(MainOptions{ optionServer = Nothing}) = do
    error "error: no server (-s) specified!"
    procFinished
reactOnOptions opts@(MainOptions{ optionDestination = Nothing}) = do
    error "error: no destination (-d) specified!"
    procFinished
reactOnOptions opts@(MainOptions{ optionClientScript = ""
                    , optionClient = ""
                    }) = do
    error "error: you must specify client program OR client script"
    procFinished
reactOnOptions opts | P.length (optionClient opts) > 0 
                    && P.length (optionClientScript opts) > 0  = do
    error "error: you must specify client program OR client script"
    procFinished
reactOnOptions opts = do
    setLocalState $! Just $! opts
    mybox <- selfMBox
    liftIO $! do
        elm_win_util_standard_add (fromString "mainwnd") (fromString "hproxy") >>= \parent-> do
            elm_win_focus_highlight_enabled_set parent True

            -- on close
            evas_object_smart_callback_add parent (fromString "delete,request") (on_done mybox ) nullPtr
            evas_object_smart_callback_add parent (fromString "hproxy_done") (on_done mybox ) nullPtr
            -- set size
            --evas_object_size_hint_min_set parent 640 480
            evas_object_resize parent 300 80
            -- box
            box <- elm_box_add parent
            evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
            -- resize box to parent
            elm_win_resize_object_add parent box
            evas_object_show box
            
            (loginBox, login) <- addLogin parent mybox
            elm_box_pack_end box loginBox
            buttonsBox <- addButtons parent mybox login
            elm_box_pack_end box buttonsBox

            evas_object_show parent
    procRunning

mainInit = do
    
    opts <- liftIO $! getArgs >>= getHProxyOptions
    let !options = generateOptions opts defaultMainOptions
    reactOnOptions options
    




nullCallback:: EvasObjectSmartCallback
nullCallback _ _ _ = return ()

addButtons:: Ptr C'Evas_Object
          -> MBox SomeMessage
          -> Ptr C'Evas_Object -- login
          -> IO (Ptr C'Evas_Object)
addButtons parent outbox login = elm_box_add parent >>= \box-> do
    elm_box_horizontal_set box
    evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
    evas_object_size_hint_align_set box c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    
    -- ok
    okBtn <- elm_button_add parent
    elm_object_text_set okBtn (fromString "ОК")
    evas_object_smart_callback_add okBtn (fromString "clicked") (on_login outbox login parent) nullPtr
    evas_object_size_hint_align_set okBtn c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    evas_object_size_hint_weight_set okBtn c'EVAS_HINT_EXPAND 0.0
    elm_box_pack_end box okBtn
    evas_object_show okBtn

    -- exit
    exitBtn <- elm_button_add parent
    elm_object_text_set exitBtn (fromString "Выход")
    evas_object_smart_callback_add exitBtn (fromString "clicked") (on_done outbox) nullPtr
    evas_object_size_hint_align_set exitBtn c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    evas_object_size_hint_weight_set exitBtn c'EVAS_HINT_EXPAND 0.0
    elm_box_pack_end box exitBtn
    evas_object_show exitBtn
    
        
    evas_object_show box
    return box
    
addLogin:: Ptr C'Evas_Object
        -> MBox SomeMessage
        -> IO (Ptr C'Evas_Object, Ptr C'Evas_Object)
addLogin parent outbox = elm_box_add parent >>= \box-> do
    elm_box_horizontal_set box
    evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
    evas_object_size_hint_align_set box c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    
    label <- elm_label_add parent
    elm_object_text_set label $ fromString "Логин в домене:"
    elm_box_pack_end box label
    evas_object_show label

    entry <- elm_entry_add parent
    elm_entry_scrollable_set entry True
    evas_object_size_hint_weight_set entry c'EVAS_HINT_EXPAND c'EVAS_HINT_FILL
    evas_object_size_hint_align_set entry c'EVAS_HINT_FILL 0.5
    -- elm_object_text_set entry $ fromString ""
    elm_entry_single_line_set entry True
    elm_box_pack_end box entry
    --evas_object_size_hint_min_set entry 200 80
    --evas_object_resize entry 300 80
    evas_object_show entry
    elm_object_focus_set entry True
    
    evas_object_show box
    return (box, entry)
    


on_closeNew _ ptr _ = do
    evas_object_del ptr

on_done mbox _ _ _ = do
    sendMBox mbox $! toMessage GuiExit
    elm_exit

on_login mbox entry parent _ ptr _ = do    
    login <- elm_object_text_get entry
    sendMBox mbox $! toMessage $! GuiLogin login parent

showMessage:: String-> PEvas_Object -> HEP ()
showMessage str parent = do
    liftIO $! do
        notify <- elm_notify_add parent
        elm_notify_allow_events_set notify False
        evas_object_size_hint_weight_set notify c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
        --elm_notify_align_set notify 0.5 1.0
    return ()
    
