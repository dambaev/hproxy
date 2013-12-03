
module Main where

import Control.Concurrent.HEP
--import ProxyClient
import Network
import HElementary.Elementary
import HElementary.ElementaryFFI
import Data.ByteString.UTF8 as U
import Data.ByteString.Char8 as C8
import System.IO
import Foreign.Ptr
import Control.Monad

login = "testuser"
password = "Qwer1234"

main = withSocketsDo $! withElementary $ do
    elm_win_util_standard_add (fromString "mainwnd") (fromString "genProxy") >>= \parent-> do
        -- on close
        evas_object_smart_callback_add parent (fromString "delete,request") on_done nullPtr
        -- set size
        evas_object_size_hint_min_set parent 640 480
        evas_object_resize parent 640 480
        -- box
        box <- elm_box_add parent
        evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
        -- resize box to parent
        elm_win_resize_object_add parent box
        evas_object_show box
        
        forM [addMenu, addSearch, addStatus] $ \foo-> do
            childbox <- foo parent
            elm_box_pack_end box childbox
        evas_object_show parent
    
    elm_run
    return ()


nullCallback:: EvasObjectSmartCallback
nullCallback _ _ _ = return ()

menuButtons:: [(String, EvasObjectSmartCallback)]
menuButtons = 
    [ ("Новый", newWindow)
    , ("Редактировать", nullCallback)
    , ("Удалить", nullCallback)
    , ("Выход", on_done)
    ]

addMenu:: Ptr C'Evas_Object-> IO (Ptr C'Evas_Object)
addMenu parent = elm_box_add parent >>= \box-> do
    elm_box_horizontal_set box
    evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND c'EVAS_HINT_EXPAND
    evas_object_size_hint_align_set box c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    
    forM_ menuButtons $ \(desc, func)-> do
        button <- elm_button_add parent
        elm_object_text_set button (fromString desc)
        evas_object_smart_callback_add button (fromString "clicked") func nullPtr
        evas_object_size_hint_align_set button c'EVAS_HINT_FILL c'EVAS_HINT_FILL
        evas_object_size_hint_weight_set button c'EVAS_HINT_EXPAND 0.0
        elm_box_pack_end box button
        evas_object_show button
    evas_object_show box
    return box
    
addSearch:: Ptr C'Evas_Object-> IO (Ptr C'Evas_Object)
addSearch parent = elm_box_add parent >>= \box-> do
    elm_box_horizontal_set box
    evas_object_size_hint_weight_set box c'EVAS_HINT_EXPAND 0.0
    evas_object_size_hint_align_set box c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    
    label <- elm_label_add parent
    elm_object_text_set label $ fromString "Фильтр:"
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
    return box
    
addStatus:: Ptr C'Evas_Object-> IO (Ptr C'Evas_Object)
addStatus parent = do
    label <- elm_label_add parent
    evas_object_size_hint_weight_set label c'EVAS_HINT_EXPAND 0.0
    evas_object_size_hint_align_set label c'EVAS_HINT_FILL c'EVAS_HINT_FILL
    elm_object_text_set label $ fromString "here goes my status"
    
    evas_object_show label
    return label

newWindow:: EvasObjectSmartCallback
newWindow _ _ _ = do
    win <- elm_win_util_standard_add (fromString "newwnd") (fromString "новое подключение")
    evas_object_smart_callback_add win (fromString "delete,request") on_closeNew nullPtr
    evas_object_show win


on_closeNew _ ptr _ = do
    evas_object_del ptr

on_done _ _ _= do
    elm_exit
