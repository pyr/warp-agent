module System.Fleet.Args where
import System.Fleet.Types
import Options.Applicative
import Options.Applicative.Arrows

parseArgs :: Parser FleetArguments
parseArgs = FleetArguments
            <$> strOption
            ( long "config"
              <> short 'f'
              <> metavar "FILE"
              <> help "Configuration file"
              <> value "/etc/fleet-agent.conf" )
