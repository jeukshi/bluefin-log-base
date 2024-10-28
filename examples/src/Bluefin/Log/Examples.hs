{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Bluefin.Log.Examples where

import Bluefin.Eff (runEff)
import Bluefin.Log (attention_, changeData, changeDomain, changeMaxLogLevel, info_, runLog)
import Data.Aeson ((.=))
import Data.Text (Text)
import Log (LogLevel (LogAttention), defaultLogLevel)
import Log.Backend.StandardOutput (withStdOutLogger)

main :: IO ()
main =
    withStdOutLogger \stdLogger -> do
        runEff \io -> do
            runLog "main" stdLogger defaultLogLevel io \logger -> do
                info_ logger "My log"

local :: IO ()
local =
    withStdOutLogger \stdLogger -> do
        runEff \io -> do
            runLog "main" stdLogger defaultLogLevel io \logger -> do
                changeDomain logger "localDomain" \localLogger -> do
                    info_ localLogger "changeDomain"

                changeMaxLogLevel logger LogAttention \localLogger -> do
                    info_ localLogger "changeMaxLogLevel"
                    attention_ localLogger "changeMaxLogLevel"

                changeData logger ["localKey" .= ("localVal" :: Text)] \localLogger -> do
                    info_ localLogger "changeData"
