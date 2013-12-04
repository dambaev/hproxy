module Config where

data Config = Config
    { configSSH:: String
    , configServer:: String
    }


defaultConfig = Config
    { configSSH = "ssh"
    , configServer = "127.0.0.1"
    }

loadConfig:: IO Config
loadConfig = do
    return defaultConfig
