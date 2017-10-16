{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE LambdaCase #-}

module GitHub.Request.Agent
  ( runs
  , runs'
  , run
  , run'
  , getAuth
  ) where

import Data.ByteString.Char8         (pack)
import Data.Time.Clock               (diffUTCTime)
import Data.Time.Clock.POSIX         (getCurrentTime)
import Data.Time.LocalTime           (getCurrentTimeZone, utcToLocalTime)
import GitHub                        (Auth (OAuth))
import GitHub.Data.Definitions       (Error (HTTPError))
import GitHub.Data.Request           (RW (RO), Request)
import GitHub.Request                (executeRequest, executeRequest')
import Network.HTTP.Client           hiding (Request)
import System.Environment            (lookupEnv)

import GitHub.Request.Agent.Internal

runs :: Auth -> [Request 'RO a] -> IO ()
runs = mapM_ . run

runs' :: [Request 'RO a] -> IO ()
runs' = mapM_ run'

run :: Auth -> Request k a -> IO (Maybe a)
run auth req = executeRequest auth req >>= processResponce (run auth req)

run' :: Request 'RO a -> IO (Maybe a)
run' req = executeRequest' req >>= processResponce (run' req)

processResponce :: IO (Maybe a) -> Either Error a -> IO (Maybe a)
processResponce retry = \case
  Left (HTTPError (HttpExceptionRequest _ (StatusCodeException err _))) -> do
    let reset = pickRateLimitReset err
    waits <- truncate . diffUTCTime reset <$> getCurrentTime
    putStrLn $ "Caught API Limit, and wait " ++ show waits ++ "s"
    tz <- getCurrentTimeZone
    putStrLn $ "Reset: " ++ show (utcToLocalTime tz reset)
    sleeps waits
    sleeps 10 -- additional waiting to prevent too many connection queries
    retry
  Left err -> do
    putStrLn "Uncaught API Error, error json: "
    print err
    sleepms 500
    return Nothing
  Right res' -> do
    putStrLn "API Response, OK"
    sleepms 500
    return (Just res')

getAuth :: String -> IO (Maybe Auth)
getAuth envName = do
    token <- lookupEnv envName
    pure (OAuth . pack <$> token)
