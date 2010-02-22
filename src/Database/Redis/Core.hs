{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}

module Database.Redis.Core where

import Control.Exception          ( Exception(..), finally )
import Data.Convertible.Base      ( ConvertSuccess, convertSuccess )
import Data.Convertible.Instances ( )
import Data.Typeable              ( Typeable )
import Network                    ( HostName, PortID(..), PortNumber, 
                                    connectTo, withSocketsDo )
import System.IO                  ( BufferMode(..), Handle, Newline(..), 
                                    NewlineMode(..), hClose, hSetBuffering, 
                                    hSetNewlineMode )
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Types
-- 

-- | Provides a wrapper for a handle to a Redis server.
newtype Server = Server { handle :: Handle }

-- | Provides a type for a key parameter for Redis commands.
type RedisKey = T.Text

-- | Provides a type for a value parameter for Redis commands, where a value 
-- parameter is any parameter that isn't a key.
type RedisParam = T.Text

-- | Converts some type to a @RedisKey@ using a @ConvertSuccess@ instance.
toKey :: ConvertSuccess a T.Text => a -> RedisKey
toKey = convertSuccess

-- | Converts some type to a @RedisParam@ using a @ConvertSuccess@ instance.
toParam :: ConvertSuccess a T.Text => a -> RedisParam
toParam = convertSuccess

-- | Convenience function for converting an @Int@ to RedisParam.
iToParam :: Int -> T.Text
iToParam = convertSuccess

-- | Data type representing a return value from a Redis command.
data RedisValue = RedisString T.Text
                | RedisInteger Int
                | RedisMulti [RedisValue] 
                | RedisNil
                  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Failure
-- 

data RedisError = ReplyError T.Text
                | ServerError T.Text
                  deriving (Show, Typeable)

instance Exception RedisError

-- ---------------------------------------------------------------------------
-- Connection
-- 

-- | Establishes a connection with the Redis server at the given host and 
-- port.
connect :: HostName -> PortNumber -> IO Server
connect host port =
    withSocketsDo $ do
        h <- connectTo host (PortNumber port)
        hSetNewlineMode h (NewlineMode CRLF CRLF)
        hSetBuffering h NoBuffering
        return $ Server h

-- | Disconnects a server handle.
disconnect :: Server -> IO ()
disconnect = hClose . handle

-- | Runs some action with a connection to the Redis server at the given host 
-- and port.
withRedisConn :: HostName -> PortNumber -> (Server -> IO ()) -> IO ()
withRedisConn host port action = do
    r <- connect host port
    action r `finally` disconnect r

-- | Runs some action with a connection to the Redis server operation at 
-- localhost on port 6379 (this is the default Redis port).
withRedisConn' :: (Server -> IO ()) -> IO ()
withRedisConn' = withRedisConn "localhost" 6379

