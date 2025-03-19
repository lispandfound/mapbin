module CLI (cli, ServerConfig (..)) where

import Data.Text (Text, pack, unpack)
import Options.Applicative
import System.Directory
import System.Exit

data ServerConfig = ServerConfig
    { mapPath :: Text
    , port :: Int
    }

cliInterface :: ParserInfo ServerConfig
cliInterface = info (opts <**> helper) cliHelp
  where
    cliHelp = fullDesc <> progDesc "Run the mapbin server listening on PORT serving maps from DIRECTORY." <> header "mapbin - share maps with everyone."
    opts = ServerConfig <$> directory <*> port
    port :: Parser Int
    port = option auto (metavar "PORT" <> short 'p' <> long "port" <> help "Port to listen to." <> value 3000 <> showDefault)
    directory :: Parser Text
    directory = pack <$> argument str (metavar "DIRECTORY" <> help "Directory to store map data.")

cli :: IO ServerConfig
cli = execParser cliInterface >>= validateConfig
  where
    validateConfig config@(ServerConfig mapPath port) = do
        validDirectory <- doesDirectoryExist (unpack mapPath)
        if port < 0 || port > 65535
            then do
                putStrLn "Error: port must be an integer between 0 and 65535"
                exitWith (ExitFailure 1)
            else
                if not validDirectory
                    then do
                        putStrLn "Error: passed path must be a valid directory."
                        exitWith (ExitFailure 1)
                    else return config
