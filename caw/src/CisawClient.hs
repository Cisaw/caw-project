{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# LANGUAGE InstanceSigs          #-}

module CisawClient
    ( -- * High level API
      runTickets
      -- * Low level
    , submitTask, pushBitcode, runReq
    ) where

import Caw.API
import Caw.Types as CLI
import Servant.Client as HTTP
import Servant.API
import Network.HTTP.Client (newManager, defaultManagerSettings)
import qualified Network.HTTP.Client.TLS as TLS

runTickets :: String -> Int -> [CLI.Task] -> BitcodeDelivery -> IO (Either ServantError [CLI.Response])
runTickets _ _ [] _ = error "Internal error: No tasks provided to runTickets."
runTickets host port tasks bitcodeBytes =
  do runReq host port $
      do ticketNumbers <- mapM submitTask tasks
         pushBitcode (map assignedTicket ticketNumbers) bitcodeBytes
         return ticketNumbers

submitTask :: CLI.Task -> ClientM CLI.Response
pushBitcode :: [Ticket] -> BitcodeDelivery -> ClientM ()
submitTask :<|> pushBitcode = client ticketAPI

runReq :: String -> Int -> ClientM a -> IO (Either ServantError a)
runReq host port q
  | "http" == take 4 host && "https" /= take 5 host =
           -- N.B. we might need to strip the scheme first, what does BaseUrl
           -- expect?
        do manager' <- newManager defaultManagerSettings
           let host' = stripScheme host
           runClientM q (ClientEnv manager' (BaseUrl Http host' port "") Nothing)
  | otherwise =
        -- N.B. default
     do manager' <- TLS.newTlsManager
        let host' = stripScheme host
        runClientM q (ClientEnv manager' (BaseUrl Https host' port "") Nothing)

stripScheme :: String -> String
stripScheme host
    | take 7 host == "http://"  = drop 7 host
    | take 8 host == "https://" = drop 8 host
    | otherwise = host
