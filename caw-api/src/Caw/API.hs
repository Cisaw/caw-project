{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Caw.API where

import           Caw.Types as CLI
import           Data.Monoid
import           Data.Proxy
import           Servant.API

type TicketAPI =
         "cisaw" :> ReqBody '[JSON] CLI.Task
                 :> Post '[JSON] CLI.Response
    -- ^ domain.com/cisaw
    :<|> "bitcode" :> Capture "tickets" [CLI.Ticket]
                   :> ReqBody '[OctetStream] CLI.BitcodeDelivery
                   :> Post '[JSON] ()
    -- ^ domain.com/bitcode

ticketAPI :: Proxy TicketAPI
ticketAPI = Proxy

