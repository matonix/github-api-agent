{-# LANGUAGE OverloadedStrings #-}

module Main where

import GitHub.Request.Agent
import GitHub

main :: IO ()
main = do
  let commitRequest = gitCommitR "matonix" "pfds" "919f5952ff6c3696cd4d805a995dcfa2b3677b60"
  runs' $ repeat commitRequest
