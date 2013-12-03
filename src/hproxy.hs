{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Concurrent.HEP as H
--import ProxyClient
import Network
import HElementary.Elementary
import HElementary.ElementaryFFI
import Data.ByteString.UTF8 as U
import Data.ByteString.Char8 as C8
import System.IO
import Foreign.Ptr
import Control.Monad
import Control.Monad.Trans
import Data.Typeable

data GuiMessage = GuiExit
                | GuiLogin ByteString
    deriving Typeable
instance Message GuiMessage

login = "testuser"
password = "Qwer1234"

main = withSocketsDo $! withElementary $! runHEPGlobal $! 
        procWithBracket guiInit procFinished $! H.proc $! do
    msg <- receive
    case fromMessage msg of
        Nothing -> procRunning
        Just GuiExit -> procFinished
        Just (GuiLogin login) -> do
            liftIO $! print login
            procFinished

guiInit = do
    mybox <- selfMBox
    worker <- spawn $! proc $! guiProc mybox
    procRunning

guiProc outbox = do
    liftIO $! do
        elm_win_util_standard_add (fromString "mainwnd") (fromString "hproxy") >>= \parent-> do
            -- on close
            evas_object_smart_callback_add parent (fromString "delete,request") (on_done outbox) nullPtr
            -- set size
            --evas_object_size_hint_min_set parent 640 480
            evas_object_resize parent 300 80
            -- box
            box <- elm_box_add parent
            evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
            -- resize box to parent
            elm_win_resize_object_add parent box
            evas_object_show box
            
            (loginBox, login) <- addLogin parent
            elm_box_pack_end box loginBox
            buttonsBox <- addButtons parent outbox
            elm_box_pack_end box buttonsBox

            evas_object_show parent
        
        elm_run
        return ()
    procFinished

nullCallback:: EvasObjectSmartCallback
nullCallback _ _ _ = return ()

menuButtons:: [(String, MBox SomeMessage-> EvasObjectSmartCallback)]
menuButtons = 
    [ ("Ок", on_login)
    , ("Выход", on_done)
    ]

addButtons:: Ptr C'Evas_Object-> MBox SomeMessage-> IO (Ptr C'Evas_Object)
addButtons parent outbox = elm_box_add parent >>= \box-> do
    elm_box_horizontal_set box
    evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
    evas_object_size_hint_align_set box c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    
    
    forM_ menuButtons $ \(desc, func)-> do
        button <- elm_button_add parent
        elm_object_text_set button (fromString desc)
        evas_object_smart_callback_add button (fromString "clicked") (func outbox) nullPtr
        evas_object_size_hint_align_set button c'EVAS_HINT_FILL c'EVAS_HINT_FILL
        evas_object_size_hint_weight_set button c'EVAS_HINT_EXPAND 0.0
        elm_box_pack_end box button
        evas_object_show button
    evas_object_show box
    return box
    
addLogin:: Ptr C'Evas_Object-> IO (Ptr C'Evas_Object, Ptr C'Evas_Object)
addLogin parent = elm_box_add parent >>= \box-> do
    elm_box_horizontal_set box
    evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
    evas_object_size_hint_align_set box c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    
    label <- elm_label_add parent
    elm_object_text_set label $ fromString "Логин в домене:"
    elm_box_pack_end box label
    evas_object_show label

    entry <- elm_entry_add parent
    elm_entry_scrollable_set entry True
    evas_object_size_hint_weight_set entry c'EVAS_HINT_EXPAND 0.0
    evas_object_size_hint_align_set entry c'EVAS_HINT_FILL 0.5
    elm_object_text_set entry $ fromString "test"
    elm_entry_single_line_set entry True
    elm_box_pack_end box entry
    evas_object_show entry
    
    evas_object_show box
    return (box, entry)
    


on_closeNew _ ptr _ = do
    evas_object_del ptr

on_done mbox _ _ _ = do
    sendMBox mbox $! toMessage GuiExit
    elm_exit

on_login mbox _ ptr _ = do    
    login <- elm_object_text_get ptr
    sendMBox mbox $! toMessage $! GuiLogin login
