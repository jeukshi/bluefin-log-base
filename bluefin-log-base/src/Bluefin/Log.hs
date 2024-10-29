-- | @Bluefin@ logging via @log-base@.
module Bluefin.Log (
    -- $use

    -- * Handle
    Log,

    -- * Handlers
    runLog,

    -- * Effectful operations

    -- ** Logging functions
    message,
    attention,
    info,
    trace,
    attention_,
    info_,
    trace_,

    -- ** Modifying Log
    changeData,
    changeDomain,
    changeMaxLogLevel,
)
where

import Bluefin.Compound (Handle (..), useImplIn)
import Bluefin.Eff
import Bluefin.IO (IOE, effIO)
import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Types (Pair, emptyObject)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Log (LogLevel (..), Logger, LoggerEnv (..), logMessageIO)
import Prelude hiding (log)

{- $use
@
import Bluefin.Eff (runEff)
import Bluefin.Log (info_, runLog)
import Log (defaultLogLevel)
import Log.Backend.StandardOutput (withStdOutLogger)

main :: IO ()
main = 'Log.Backend.StandardOutput.withStdOutLogger' \stdLogger -> do
    'Bluefin.Eff.runEff' \io -> do
        'runLog' io "main" stdLogger 'Log.defaultLogLevel' \logger -> do
            'info_' logger "My log"
@
-}

{- |
A handle for logger.

@since 0.1.0.0
-}
data Log e = UnsafeMkLog
    { logIoe :: IOE e
    , logEnv :: LoggerEnv
    }

instance Handle Log where
    mapHandle :: (e :> es) => Log e -> Log es
    mapHandle (UnsafeMkLog io env) = UnsafeMkLog (mapHandle io) env

{- | @bluefin@ equivalent of 'Log.Monad.runLogT'.

@since 0.1.0.0
-}
runLog
    :: (e1 :> es)
    => IOE e1
    -> Text
    -> Logger
    -> LogLevel
    -> (forall e. Log e -> Eff (e :& es) r)
    -- ^ Computation with 'Log'.
    -> Eff es r
runLog io component logger maxLogLevel action = do
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

{- |
See 'Log.Class.logMessage'.

@since 0.1.0.0
-}
message :: (e :> es) => (ToJSON a) => Log e -> LogLevel -> Text -> a -> Eff es ()
message log logLevel = logInEff logLevel log

{- |
See 'Log.Class.logAttention'.

@since 0.1.0.0
-}
attention :: (e :> es) => (ToJSON a) => Log e -> Text -> a -> Eff es ()
attention = logInEff LogAttention

{- |
See 'Log.Class.logInfo'.

@since 0.1.0.0
-}
info :: (e :> es) => (ToJSON a) => Log e -> Text -> a -> Eff es ()
info = logInEff LogInfo

{- |
See 'Log.Class.logTrace'.

@since 0.1.0.0
-}
trace :: (e :> es) => (ToJSON a) => Log e -> Text -> a -> Eff es ()
trace = logInEff LogTrace

{- |
See 'Log.Class.logAttention_'.

@since 0.1.0.0
-}
attention_ :: (e :> es) => Log e -> Text -> Eff es ()
attention_ log text = logInEff LogAttention log text emptyObject

{- |
See 'Log.Class.logInfo_'.

@since 0.1.0.0
-}
info_ :: (e :> es) => Log e -> Text -> Eff es ()
info_ log text = logInEff LogInfo log text emptyObject

{- |
See 'Log.Class.logTrace_'.

@since 0.1.0.0
-}
trace_ :: (e :> es) => Log e -> Text -> Eff es ()
trace_ log text = logInEff LogTrace log text emptyObject

{- |
See 'Log.Class.localData'.

@since 0.1.0.0
-}
changeData
    :: (e1 :> es)
    => Log e1
    -- ^ Base 'Log'.
    -> [Pair]
    -- ^ Additional data.
    -> (forall e. Log e -> Eff (e :& es) r)
    -- ^ Computation with new 'Log'.
    -> Eff es r
changeData (UnsafeMkLog io env) data_ action = do
    let newEnv = env{leData = data_ ++ leData env}
    useImplIn
        action
        (mapHandle (UnsafeMkLog io newEnv))

{- |
See 'Log.Class.localDomain'.

@since 0.1.0.0
-}
changeDomain
    :: (e1 :> es)
    => Log e1
    -- ^ Base 'Log'.
    -> Text
    -- ^ Additional domain.
    -> (forall e. Log e -> Eff (e :& es) r)
    -- ^ Computation with new 'Log'.
    -> Eff es r
changeDomain (UnsafeMkLog io env) domain action = do
    let newEnv = env{leDomain = leDomain env ++ [domain]}
    useImplIn
        action
        (mapHandle (UnsafeMkLog io newEnv))

{- |
See 'Log.Class.localMaxLogLevel'.

@since 0.1.0.0
-}
changeMaxLogLevel
    :: (e1 :> es)
    => Log e1
    -- ^ Base 'Log'.
    -> LogLevel
    -- ^ New 'LogLevel'.
    -> (forall e. Log e -> Eff (e :& es) r)
    -- ^ Computation with new 'Log'.
    -> Eff es r
changeMaxLogLevel (UnsafeMkLog io env) maxLogLevel action = do
    let newEnv = env{leMaxLogLevel = maxLogLevel}
    useImplIn
        action
        (mapHandle (UnsafeMkLog io newEnv))

logInEff :: (e :> es) => (ToJSON a) => LogLevel -> Log e -> Text -> a -> Eff es ()
logInEff logLevel log text json = do
    let io = logIoe log
    currentTime <- effIO io getCurrentTime
    effIO io $ logMessageIO (logEnv log) currentTime logLevel text (toJSON json)
