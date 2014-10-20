module System.Warp.Args where
import System.Warp.Types
import Options.Applicative
import Options.Applicative.Arrows

parseArgs :: Parser WarpArguments
parseArgs = WarpArguments
            <$> strOption
            ( long "config"
              <> short 'f'
              <> metavar "FILE"
              <> help "Configuration file"
              <> value "/etc/warp-agent.conf" )
            <*> flag Normal Verbose
            ( long "verbose"
              <> short 'v'
              <> help "Display more messages" )
            <*> (optional $ strOption
                 ( long "logfile"
                   <> short 'l'
                   <> metavar "FILE"
                   <> help "Log to file FILE" ))
