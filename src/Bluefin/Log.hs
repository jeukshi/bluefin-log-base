module Bluefin.Log where

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff
import Bluefin.IO (IOE, effIO)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Types (emptyObject)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Log (LogLevel (..), Logger, LoggerEnv (..), logMessageIO)

data Log e = UnsafeMkLog
    { logIoe :: IOE e
    , logEnv :: LoggerEnv
    }

instance Handle Log where
    mapHandle :: (e :> es) => Log e -> Log es
    mapHandle (UnsafeMkLog io env) = UnsafeMkLog (mapHandle io) env

runLog
    :: (e1 :> es)
    => Text
    -> Logger
    -> LogLevel
    -> IOE e1
    -> (forall e. Log e -> Eff (e :& es) r)
    -> Eff es r
runLog component logger maxLogLevel io action = do
    let loggerEnv =
            LoggerEnv
                { leLogger = logger
                , leComponent = component
                , leDomain = []
                , leData = []
                , leMaxLogLevel = maxLogLevel
                }
    useImplIn
        action
        UnsafeMkLog
            { logIoe = mapHandle io
            , logEnv = loggerEnv
            }

changeMaxLogLevel
    :: (e1 :> es)
    => Log e1
    -> LogLevel
    -> (forall e. Log e -> Eff (e :& es) r)
    -> Eff es r
changeMaxLogLevel (UnsafeMkLog io env) maxLogLevel action = do
    let newEnv = env{leMaxLogLevel = maxLogLevel}
    useImplIn
        action
        (mapHandle (UnsafeMkLog io newEnv))

message :: (e :> es) => (ToJSON a) => Log e -> LogLevel -> Text -> a -> Eff es ()
message log logLevel = logInEff logLevel log

attention :: (e :> es) => (ToJSON a) => Log e -> Text -> a -> Eff es ()
attention = logInEff LogAttention

info :: (e :> es) => (ToJSON a) => Log e -> Text -> a -> Eff es ()
info = logInEff LogInfo

trace :: (e :> es) => (ToJSON a) => Log e -> Text -> a -> Eff es ()
trace = logInEff LogTrace

attention_ :: (e :> es) => Log e -> Text -> Eff es ()
attention_ log text = logInEff LogAttention log text emptyObject

info_ :: (e :> es) => Log e -> Text -> Eff es ()
info_ log text = logInEff LogInfo log text emptyObject

trace_ :: (e :> es) => Log e -> Text -> Eff es ()
trace_ log text = logInEff LogTrace log text emptyObject

logInEff :: (e :> es) => (ToJSON a) => LogLevel -> Log e -> Text -> a -> Eff es ()
logInEff logLevel log text json = do
    let io = logIoe log
    currentTime <- effIO io getCurrentTime
    effIO io $ logMessageIO (logEnv log) currentTime logLevel text (toJSON json)
