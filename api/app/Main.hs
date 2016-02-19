module Main where

import System.Environment
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)

import WhatsOpen (server, whatsOpenAPI)

main :: IO ()
main = do 
    port <- getEnv "PORT"
    run (read port) (serve whatsOpenAPI server)
