{-# LANGUAGE FlexibleContexts #-}

module Database.Redis.Internal where

import Control.Monad.Trans        ( MonadIO, liftIO )
import Control.Failure            ( MonadFailure, failure )
import Data.Convertible.Base      ( convertUnsafe )
import Data.Convertible.Instances ( )
import Database.Redis.Core
import System.IO                  ( hGetChar )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- ---------------------------------------------------------------------------
-- Command
-- 

command :: (MonadIO m, MonadFailure RedisError m) => Server -> m a -> m RedisValue
command r f = f >> getReply r

multiBulk :: (MonadIO m, MonadFailure RedisError m) 
           => Server -> T.Text -> [T.Text] -> m ()
multiBulk (Server h) command' vs = do
    let vs' = concatMap (\a -> ["$" ~~ (toParam $ T.length a), a]) $ [command'] ++ vs
    liftIO $ TIO.hPutStrLn h $ "*" ~~ (toParam $ 1 + length vs)
    mapM_ (liftIO . TIO.hPutStrLn h) vs'

multiBulkT2 :: (MonadIO m, MonadFailure RedisError m) 
           => Server -> T.Text -> [(T.Text, T.Text)] -> m ()
multiBulkT2 r command' kvs = do
    multiBulk r command' $ concatMap (\kv -> [fst kv] ++ [snd kv]) kvs

-- ---------------------------------------------------------------------------
-- Reply
-- 


getReply :: (MonadIO m, MonadFailure RedisError m) => Server -> m RedisValue
getReply r@(Server h) = do
    prefix <- liftIO $ hGetChar h
    getReplyType r prefix

getReplyType :: (MonadIO m, MonadFailure RedisError m) 
             => Server -> Char -> m RedisValue
getReplyType r prefix =
    case prefix of
        '$' -> bulkReply r
        ':' -> integerReply r
        '+' -> singleLineReply r
        '-' -> singleLineReply r >>= \(RedisString m) -> failure $ ServerError m
        '*' -> multiBulkReply r
        _ -> singleLineReply r

bulkReply :: (MonadIO m, MonadFailure RedisError m) => Server -> m RedisValue
bulkReply r@(Server h) = do
    l <- liftIO $ TIO.hGetLine h
    let bytes = convertUnsafe l::Int
    if bytes == -1
        then return $ RedisNil
        else do
            v <- takeChar bytes r
            _ <- liftIO $ TIO.hGetLine h -- cleans up
            return $ RedisString v

integerReply :: (MonadIO m, MonadFailure RedisError m) => Server -> m RedisValue
integerReply (Server h) = do
    l <- liftIO $ TIO.hGetLine h
    return $ RedisInteger (convertUnsafe l::Int)

singleLineReply :: (MonadIO m, MonadFailure RedisError m) => Server -> m RedisValue
singleLineReply (Server h) = do
    l <- liftIO $ TIO.hGetLine h
    return $ RedisString l

multiBulkReply :: (MonadIO m, MonadFailure RedisError m) => Server -> m RedisValue
multiBulkReply r@(Server h) = do
    l <- liftIO $ TIO.hGetLine h
    let items = convertUnsafe l::Int
    multiBulkReply' r items []

multiBulkReply' :: (MonadIO m, MonadFailure RedisError m) 
                => Server -> Int -> [RedisValue] -> m RedisValue
multiBulkReply' _ 0 values = return $ RedisMulti values
multiBulkReply' r@(Server h) n values = do
    _ <- liftIO $ hGetChar h -- discard the type data since we know it's a bulk string
    v <- bulkReply r
    multiBulkReply' r (n - 1) (values ++ [v])

takeChar :: (MonadIO m, MonadFailure RedisError m) 
         => Int -> Server -> m (T.Text)
takeChar n r = takeChar' n r ""

takeChar' :: (MonadIO m, MonadFailure RedisError m) 
          => Int -> Server -> T.Text -> m (T.Text)
takeChar' 0 _ s = return s
takeChar' n r@(Server h) s = do
    c <- liftIO $ hGetChar h
    (takeChar' (n - 1) r (s ~~ (T.singleton c)))

-- ---------------------------------------------------------------------------
-- Helpers
-- 

(~~) :: T.Text -> T.Text -> T.Text
(~~) = T.append

boolify :: (Monad m) => m RedisValue -> m (Bool)
boolify v' = do
    v <- v'
    return $ case v of RedisString "OK" -> True
                       RedisInteger 1   -> True
                       _                -> False

discard :: (Monad a) => a b -> a ()
discard f = f >> return ()

