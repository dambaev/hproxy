module Config where

data Config = Config
    { configSSH:: String
    }


defaultConfig = Config
    { configSSH = "ssh"
    }

loadConfig:: IO Config
loadConfig = do
    return defaultConfig
