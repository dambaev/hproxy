{-# LANGUAGE BangPatterns #-}

module SSHClient where

import Control.Concurrent.HEP

data ProgramCommand 

data Program

startProgram:: String-> HEP ()
startProgram str = do
    spawn $! procWithBracket progSuperInit progSuperShutdown $! proc $!
        progSuper



progInit:: String-> [String] -> HEPProc
progInit cmd args = do
    () <- runInteractiveProcess cmd args Nothing Nothing

