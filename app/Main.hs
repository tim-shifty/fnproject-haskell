module Main where

import Control.Monad (forever, liftM, guard)
import Control.Exception (bracket, throwIO, Exception)
import System.Environment (getEnv)
import Data.List (lookup)
import Data.List.Extra (trim, lower)
import Data.Char (generalCategory, GeneralCategory(..), isSpace)
import qualified Network.HTTP.Types as HttpT
import qualified Network.Socket as So
import qualified Network.Socket.ByteString as Sobs
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as Utf8
import Foreign.Marshal.Alloc (malloc, free)
import Foreign.Marshal.Array (peekArray)
import qualified Data.ByteString as Bs
import qualified Text.JSON as Js
import Text.Parsec (parse, ParseError, (<|>), try, choice, skipMany1, between,
    eof, many, many1)
import Text.Parsec.Char (oneOf, noneOf, space, spaces, newline, crlf, satisfy,
    char, string, anyChar, endOfLine)
import Text.Parsec.ByteString (Parser)

accept' :: So.Socket -> IO So.Socket
accept' sock = do
    (conn,_) <- So.accept sock -- TODO: could check that this posted on the /call URL?
    return conn

data Request = Request
    { requestUrl :: String
    , requestProtocol :: String
    , requestHeaders :: [(String, String)]
    , requestMediaType :: Maybe MediaType
    , requestRawBody :: String
    , requestStringBody :: Maybe String
    , requestJsonBody :: Maybe Js.JSValue
    }

data Response = Response
    { responseCode :: HttpT.Status
    , responseHeaders :: [(String,String)]
    , responseBody :: String
    }

data MediaType = MediaType
    { mediaType :: String
    , mediaSubtype :: String
    , mediaParameters :: [(String,String)]
    } deriving (Show, Eq)

badRequest err = Response
    { responseCode = HttpT.badRequest400
    , responseHeaders = []
    , responseBody = err
    }

-- Here's where the magic happens
-- TODO: Need to pass the context (like FN_TMPSIZE)
handleRequest :: Request -> IO Response
handleRequest r = return $ hr (requestJsonBody r) where
    hr Nothing = badRequest "expected JSON"
    hr (Just (Js.JSObject obj)) = case Js.valFromObj "name" obj of
        Js.Ok (Js.JSString jss) -> Response
            { responseCode = HttpT.ok200
            , responseHeaders = []
            , responseBody = "Hello " ++ Js.fromJSString jss ++ "!"
            }
        Js.Ok other -> badRequest $ "expected 'name' to be a string but got: " ++ Js.showJSValue other ""
        Js.Error _ -> badRequest "did not find 'name' in input"
    hr (Just other) = badRequest $ "expected JSON object but got " ++ Js.showJSValue other ""

getRequest :: String -> String -> [(String,String)] -> String -> Request
getRequest url protocol headers body =
    Request
        { requestUrl = url
        , requestProtocol = protocol
        , requestHeaders = headers
        , requestMediaType = mType
        , requestRawBody = body
        , requestStringBody = stringBody
        , requestJsonBody = jsonBody
        }
    where
        contentHeader = lookup "content-type" headers
        mType = contentHeader >>= mediaTypeFromMaybe
        getEncoding mt = lookup "content-type" (mediaParameters mt)
        stringBody = mType >>= getEncoding
        jsResultToMaybe (Js.Error _) = Nothing
        jsResultToMaybe (Js.Ok a) = Just a
        jsonBody = do
            mt <- mType
            guard $ mediaSubtype mt == "json"
            sbody <- stringBody
            jsResultToMaybe $ Js.decode sbody

mediaTypeFromMaybe :: String -> Maybe MediaType
mediaTypeFromMaybe s = case parse mediaTypeParse "media type" (C.pack s) of
    Left _ -> Nothing
    Right r -> Just r

mediaTypeParse :: Parser MediaType
mediaTypeParse = do
    mt <- many1 $ noneOf "\r\n/;"
    mst <- mediaSubtypeParse <|> return ""
    mps <- mediaParametersParse
    return MediaType
        { mediaType = lower $ trim mt
        , mediaSubtype = mst
        , mediaParameters = mps
        }

mediaSubtypeParse :: Parser String
mediaSubtypeParse = do
    char '/'
    s <- many $ noneOf "\r\n;"
    return $ lower $ trim s

mediaParametersParse :: Parser [(String,String)]
mediaParametersParse = many $ do
    char ';'
    lhs <- many1 $ noneOf "\r\n;="
    char '='
    hspace
    rhs <- choice [ parseValue, do { char '"'; r <- parseValue; char '"'; return r } ]
    return (lower $ trim lhs, lower $ trim rhs)
    where
        parseValue = many1 $ noneOf "\r\n\";"

hspace :: Parser ()
hspace = skipMany1 $ oneOf " \t"

token :: Parser String
token = many1 $ satisfy (not . isSpace)

notEndOfLine :: Char -> Bool
notEndOfLine c = case generalCategory c of
    LineSeparator -> False
    ParagraphSeparator -> False
    _ -> True

restOfLine :: Parser String
restOfLine = many $ satisfy notEndOfLine

skipEol :: Parser ()
skipEol = do
    hspace
    endOfLine
    return ()

parseTopLine :: Parser (String,String)
parseTopLine = do
    hspace
    string "POST"
    hspace
    url <- token
    hspace
    protocol <- token
    skipEol
    return (url, protocol)

-- parses lines of the form xxxx: yyyyy\n followed by a blank line
parseHeaders :: Parser [(String,String)]
parseHeaders = (endOfLine >> return []) <|> parseAtLeastOneHeader where
    parseAtLeastOneHeader = do
        lhs <- token
        char ':'
        hspace
        rhs <- restOfLine
        endOfLine
        rest <- parseHeaders
        return $ (lhs,rhs):rest

-- just grabs anything else (for the moment)
parseBody :: Parser String
parseBody = many anyChar

requestParser :: Parser Request
requestParser = do
    (url, protocol) <- parseTopLine
    headers <- parseHeaders
    body <- parseBody
    return $ getRequest url protocol headers body

sendResponse :: So.Socket -> Response -> IO ()
sendResponse sock response = let
    pairToString (k,v) = k ++ ": " ++ v ++ "\n"
    headers = concat $ map pairToString $ responseHeaders response
    s = "HTTP/1.1 "
        ++ show (responseCode response)
        ++ "\nFn-Fdk-version:fdk-haskell/0.0.1\nContent-type: text/plain\n"
        ++ headers ++ "\n" ++ responseBody response
    bs = Utf8.fromString s
    in Sobs.sendAll sock bs

-- given a buffer and its size and a socket to read from and write to,
-- read the request, parse it, handle it, then pack and send the response.
handleRawRequest :: So.Socket -> IO ()
handleRawRequest sock = let
    parseAndHandle req = case parse requestParser "request" req of
        Left err -> return $ badRequest (show err)
        Right r -> handleRequest r
    in do
        request <- Sobs.recv sock 8192
        response <- parseAndHandle request
        sendResponse sock response

data ConfigurationException = ConfigurationException String deriving Show

instance Exception ConfigurationException

socketListener :: String -> IO ()
socketListener socketString = case socketString of
    'u':'n':'i':'x':':':ss@('/':_) -> do
        bracket (So.socket So.AF_UNIX So.Stream So.defaultProtocol) So.close $ \sock -> do
            So.setSocketOption sock So.KeepAlive 1
            So.bind sock (So.SockAddrUnix ss)
            So.listen sock 1
            -- can change from So.close to So.gracefulClose once we can get network version 3.1.1.0
            forever $ bracket (accept' sock) So.close handleRawRequest
    _ -> throwIO $ ConfigurationException $ "Unknown FN_LISTENER format " ++ socketString

main :: IO ()
main = do
    format <- getEnv "FN_FORMAT"
    if format == "http-stream"
        then do
            listener <- getEnv("FN_LISTENER")
            socketListener listener
        else throwIO $ ConfigurationException $ "Unknown FN_FORMAT " ++ format
