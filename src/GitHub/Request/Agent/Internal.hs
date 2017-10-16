{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module GitHub.Request.Agent.Internal where

import Control.Concurrent         (threadDelay)
import Data.ByteString.Lazy.Char8 (fromStrict, readInteger)
import Data.CaseInsensitive       (mk)
import Data.Maybe                 (fromJust)
import Data.Time.Clock            (UTCTime)
import Data.Time.Clock.POSIX      (posixSecondsToUTCTime)
import Network.HTTP.Client

sleeps :: Int -> IO ()
sleeps = threadDelay . (1000 * 1000 *)

sleepms :: Int -> IO ()
sleepms = threadDelay . (1000 *)

pickRateLimitReset :: Response () -> UTCTime
pickRateLimitReset =
  posixSecondsToUTCTime
  . realToFrac
  . fst
  . fromJust
  . readInteger
  . fromStrict
  . fromJust
  . lookup (mk "X-RateLimit-Reset")
  . responseHeaders
