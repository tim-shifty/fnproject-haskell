module Main where

import Control.Monad (forever)
import Control.Exception (bracket, throwIO, Exception)
import Control.Applicative ((<$>), (<*>))
import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.Binary.Builder as BB
import qualified Network.HTTP.Types as HttpT
import qualified Network.Socket as So
import qualified Data.Aeson as Aeson
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

(.:) = (Aeson..:)

data HelloRequest = RequestType { name :: T.Text } deriving Show

nameT = T.pack "name"

instance Aeson.FromJSON HelloRequest where
    parseJSON = Aeson.withObject "RequestType" $ \v -> RequestType
        <$> v .: nameT

handleJsonRequest :: HelloRequest -> IO Wai.Response
handleJsonRequest r = return
    $ Wai.responseBuilder HttpT.ok200 []
    $ BB.putStringUtf8 ("Hello " ++ T.unpack(name r) ++ "!\n")

handleJsonRequestEither :: Either String HelloRequest -> IO Wai.Response
handleJsonRequestEither (Left err) = return $
    Wai.responseBuilder HttpT.badRequest400 [] (BB.putStringUtf8 err)
handleJsonRequestEither (Right r) = handleJsonRequest r

-- given a buffer and its size and a socket to read from and write to,
-- read the request, parse it, handle it, then pack and send the response.
handleRequest :: Wai.Application
handleRequest req respond = do
    body <- Wai.lazyRequestBody req
    resp <- handleJsonRequestEither $ Aeson.eitherDecode body
    respond resp

data ConfigurationException = ConfigurationException String deriving Show

instance Exception ConfigurationException

socketListener :: String -> IO ()
socketListener socketString = case socketString of
    'u':'n':'i':'x':':':ss@('/':_) -> do
        bracket (So.socket So.AF_UNIX So.Stream So.defaultProtocol) So.close $ \sock -> do
            So.setSocketOption sock So.KeepAlive 1
            So.bind sock (So.SockAddrUnix ss)
            So.listen sock 1
            Warp.runSettingsSocket Warp.defaultSettings sock handleRequest
    _ -> throwIO $ ConfigurationException $ "Unknown FN_LISTENER format " ++ socketString

main :: IO ()
main = do
    format <- getEnv "FN_FORMAT"
    if format == "http-stream"
        then do
            listener <- getEnv("FN_LISTENER")
            socketListener listener
        else throwIO $ ConfigurationException $ "Unknown FN_FORMAT " ++ format
