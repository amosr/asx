module Asx.Internal.Http where

import Control.Applicative

import qualified Network.HTTP.Conduit   as H
import qualified Control.Exception      as E
import qualified Data.ByteString.Lazy   as BL

simpleHttpEither :: String -> IO (Either H.HttpException BL.ByteString)
simpleHttpEither str
 = (Right <$> H.simpleHttp str)
 `E.catch`
   (\he -> return $ Left he)
