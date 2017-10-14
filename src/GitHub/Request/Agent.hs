{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Request.Agent where

import           Control.Concurrent         (threadDelay)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Char8      (pack)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.CaseInsensitive       (CI)
import qualified Data.CaseInsensitive       as CI
import qualified Data.HashMap.Strict        as HashMap
import           Data.Maybe                 (fromJust)
import           Data.Time.Clock            (UTCTime, diffUTCTime)
import           Data.Time.Clock.POSIX      (getCurrentTime,
                                             posixSecondsToUTCTime)
import           Debug.Trace                (traceIO, traceShowM)
import           GitHub                     (Auth (OAuth))
import           GitHub.Data.Definitions    (Error (HTTPError))
import           GitHub.Data.Request        (RW (RO), Request)
import           GitHub.Request             (executeRequest')
import           Network.HTTP.Client        hiding (Request)
import           System.Environment         (getArgs, lookupEnv)
import           System.Posix.Types         (EpochTime)

run' :: Show a => [Request RO a] -> IO ()
run' (req:reqs) = do
  res <- executeRequest' req
  case res of
    Left (HTTPError (HttpExceptionRequest _ (StatusCodeException r _))) -> do
      let headers = HashMap.fromList $ responseHeaders r
      now <- getCurrentTime
      let reset = parseUTCTime . fromJust $ HashMap.lookup (CI.mk "X-RateLimit-Reset") headers
      let sleepTime = truncate $ diffUTCTime reset now
      traceIO $ "Caught API Limit, and wait " ++ show sleepTime ++ "s"
      sleep sleepTime
      run' (req:reqs)
    Left err -> error $ show err
    Right res' -> do
      traceShowM res'
      sleep 1
      run' reqs

sleep :: Int -> IO ()
sleep 0 = return ()
sleep n = threadDelay (1000 * 1000) >> sleep (n-1)

parseUTCTime :: ByteString -> UTCTime
parseUTCTime = posixSecondsToUTCTime
  . realToFrac
  . fromIntegral
  . fst
  . fromJust
  . BL.readInteger
  . BL.fromStrict

getAuth :: String -> IO (Maybe Auth)
getAuth envName = do
    token <- lookupEnv envName
    pure (OAuth . pack <$> token)
