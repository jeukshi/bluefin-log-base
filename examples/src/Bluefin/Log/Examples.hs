{-# LANGUAGE OverloadedStrings #-}

module Bluefin.Log.Examples where

import Bluefin.Compound (Handle (mapHandle), useImplIn)
import Bluefin.Eff (Eff, runEff, (:&), (:>))
import Bluefin.Log (Log, attention_, changeData, changeDomain, changeMaxLogLevel, info_, runLog)
import Bluefin.State (State, evalState)
import Data.Aeson ((.=))
import Data.Text (Text)
import Log (LogLevel (LogAttention), defaultLogLevel)
import Log.Backend.StandardOutput (withStdOutLogger)

-- Simple setup.
main :: IO ()
main = withStdOutLogger \stdLogger -> do
    runEff \io -> do
        runLog io "main" stdLogger defaultLogLevel \logger -> do
            info_ logger "My log"

-- Interacting with Log.
local :: IO ()
local = withStdOutLogger \stdLogger -> do
    runEff \io -> do
        runLog io "main" stdLogger defaultLogLevel \logger -> do
            changeDomain logger "localDomain" \localLogger -> do
                info_ localLogger "changeDomain"

            changeMaxLogLevel logger LogAttention \localLogger -> do
                info_ localLogger "changeMaxLogLevel"
                attention_ localLogger "changeMaxLogLevel"

            changeData logger ["localKey" .= ("localVal" :: Text)] \localLogger -> do
                info_ localLogger "changeData"

-- Mapping Log.
data MyEnv e = MkMyEnv
    { myState :: State Int e
    , myLog :: Log e
    }

runMyEnv
    :: (e1 :> es)
    => (e2 :> es)
    => State Int e1
    -> Log e2
    -> (forall e. MyEnv e -> Eff (e :& es) r)
    -> Eff es r
runMyEnv s l action =
    useImplIn
        action
        MkMyEnv
            { myState = mapHandle s
            , myLog = mapHandle l
            }

useMyEnv :: IO ()
useMyEnv = withStdOutLogger \stdLogger -> do
    runEff \io -> do
        evalState 0 \s -> do
            runLog io "main" stdLogger defaultLogLevel \logger -> do
                runMyEnv s logger \myEnv ->
                    info_ (myLog myEnv) "My log"
