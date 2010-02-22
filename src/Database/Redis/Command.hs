{-# LANGUAGE FlexibleContexts #-}

module Database.Redis.Command where

import Control.Monad.Trans ( MonadIO )
import Control.Failure     ( MonadFailure )
import Database.Redis.Core
import Database.Redis.Internal

-- ---------------------------------------------------------------------------
-- Connection
-- 

-- | Authenticate with a password-protected Redis server. A Redis server can 
-- be configured to require a password before commands can be executed using 
-- the /requirepass/ directive in its configuration file.
auth :: (MonadIO m, MonadFailure RedisError m)
     => Server     -- ^ Redis server connection.
     -> RedisParam -- ^ Password.
     -> m ()       -- ^ Status. "OK" if successful, or an error message 
                   -- otherwise.
auth r pwd = discard $ command r $ inline r "AUTH" [pwd]

-- | Ask a server to silently close a connection.
quit :: (MonadIO m, MonadFailure RedisError m)
     => Server -- ^ Redis server connection.
     -> m () 
quit r = discard $ command r $ inline r "QUIT" []

ping :: (MonadIO m, MonadFailure RedisError m)
     => Server -- ^ Redis server connection.
     -> m RedisValue
ping r = command r $ inline r "PING" []

-- ---------------------------------------------------------------------------
-- Generic
-- 

-- | Test whether a specified RedisKey exists.
exists :: (MonadIO m, MonadFailure RedisError m)
       => Server   -- ^ Redis server connection.
       -> RedisKey -- ^ RedisKey to check.
       -> m Bool   -- ^ True if the RedisKey exists and False otherwise.
exists r k = boolify $ command r $ inline r "EXISTS" [k]

del :: (MonadIO m, MonadFailure RedisError m) 
    => Server -> RedisKey -> m RedisValue
del r k = command r $ inline r "DEL" [k]

type' :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> RedisKey -> m RedisValue
type' r k = command r $ inline r "TYPE" [k]

keys :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisParam -> m RedisValue
keys r pattern = command r $ inline r "KEYS" [pattern]

randomkey :: (MonadIO m, MonadFailure RedisError m) 
          => Server -> m RedisValue
randomkey r = command r $ inline r "RANDOMKEY" []

rename :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisKey -> RedisKey -> m RedisValue
rename r old new = command r $ inline r "RENAME" [old, new]

renamex :: (MonadIO m, MonadFailure RedisError m) 
        => Server -> RedisKey -> RedisKey -> m RedisValue
renamex r old new = command r $ inline r "RENAMEX" [old, new]

dbsize :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> m RedisValue
dbsize r = command r $ inline r "DBSIZE" []

expire :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisKey -> RedisParam -> m RedisValue
expire r k secs = command r $ inline r "RENAMEX" [k, secs]

ttl :: (MonadIO m, MonadFailure RedisError m) 
    => Server -> RedisKey -> m RedisValue
ttl r k = command r $ inline r "TTL" [k]

select :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisParam -> m Bool
select r index = boolify $ command r $ inline r "SELECT" [index]

move :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> RedisParam -> m RedisValue
move r k index = command r $ inline r "MOVE" [k, index]

flushdb :: (MonadIO m, MonadFailure RedisError m) 
        => Server -> m Bool
flushdb r = boolify $ command r $ inline r "FLUSHDB" []

flushall :: (MonadIO m, MonadFailure RedisError m) 
         => Server -> m Bool
flushall r = boolify $ command r $ inline r "FLUSHALL" []

-- ---------------------------------------------------------------------------
-- String
-- 

get :: (MonadIO m, MonadFailure RedisError m) 
    => Server -> RedisKey -> m RedisValue
get r k = command r $ multiBulk r "GET" [k]

set :: (MonadIO m, MonadFailure RedisError m) 
    => Server -> RedisKey -> RedisParam -> m Bool
set r k v = boolify $ command r $ multiBulkT2 r "SET" [(k, v)]

getset :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisKey -> RedisParam -> m RedisValue
getset r k v = command r $ multiBulkT2 r "GETSET" [(k, v)]

mget :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> [RedisKey] -> m RedisValue
mget r ks = command r $ multiBulk r "MGET" ks

setnx :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> RedisKey -> RedisParam -> m Bool
setnx r k v = boolify $ command r $ multiBulkT2 r "SETNX" [(k, v)]

mset :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> [(RedisKey, RedisParam)] -> m ()
mset r kvs = discard $ command r $ multiBulkT2 r "MSET" kvs

msetnx :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> [(RedisKey, RedisParam)] -> m Bool
msetnx r kvs = boolify $ command r $ multiBulkT2 r "MSETNX" kvs

incr :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> m RedisValue
incr r k = command r $ multiBulk r "INCR" [k]

incrby :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisKey -> RedisParam -> m RedisValue
incrby r k v = command r $ multiBulk r "INCRBY" [k, v]

decr :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> m RedisValue
decr r k = command r $ multiBulk r "DECR" [k]

decrby :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisKey -> RedisParam -> m RedisValue
decrby r k v = command r $ multiBulk r "DECRBY" [k, v]

-- ---------------------------------------------------------------------------
-- List
-- 

rpush :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> RedisKey -> RedisParam -> m RedisValue
rpush r k v = command r $ bulk r "RPUSH" [k] v

lpush :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> RedisKey -> RedisParam -> m RedisValue
lpush r k v = command r $ bulk r "LPUSH" [k] v

llen :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> m RedisValue
llen r k = command r $ inline r "LLEN" [k]

lrange :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisKey -> RedisParam -> RedisParam -> m RedisValue
lrange r k start end = command r $ inline r "LRANGE" [k, start, end]

ltrim :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> RedisKey -> RedisParam -> RedisParam -> m RedisValue
ltrim r k start end = command r $ inline r "LTRIM" [k, start, end]

lindex :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> RedisKey -> RedisParam -> m RedisValue
lindex r k index = command r $ inline r "LINDEX" [k, index]

lset :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> RedisParam -> RedisParam -> m RedisValue
lset r k index value = command r $ bulk r "LSET" [k, index] value

lrem :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> RedisParam -> RedisParam -> m RedisValue
lrem r k count value = command r $ bulk r "LREM" [k, count] value

lpop :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> m RedisValue
lpop r k = command r $ inline r "LPOP" [k]

rpop :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> m RedisValue
rpop r k = command r $ inline r "RPOP" [k]

blpop :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> [RedisKey] -> RedisParam -> m RedisValue
blpop r ks timeout = command r $ inline r "BLPOP" (ks ++ [timeout])

brpop :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> [RedisKey] -> RedisParam -> m RedisValue
brpop r ks timeout = command r $ inline r "BRPOP" (ks ++ [timeout])

rpoplpush :: (MonadIO m, MonadFailure RedisError m) 
          => Server -> RedisKey -> RedisKey -> m RedisValue
rpoplpush r source dest = command r $ inline r "RPOPLPUSH" [source, dest]

-- ---------------------------------------------------------------------------
-- Set
-- 

sadd :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> RedisParam -> m RedisValue
sadd r k member = command r $ bulk r "SADD" [k] member

srem :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> RedisParam -> m RedisValue
srem r k member = command r $ bulk r "SREM" [k] member

spop :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> m RedisValue
spop r k = command r $ inline r "SPOP" [k]

smove :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> RedisKey -> RedisKey -> RedisParam -> m RedisValue
smove r source dest member = command r $ inline r "SMOVE" [source, dest, member]

scard :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> RedisKey -> m RedisValue
scard r k = command r $ inline r "SCARD" [k]

sismember :: (MonadIO m, MonadFailure RedisError m) 
          => Server -> RedisKey -> RedisParam -> m RedisValue
sismember r k member = command r $ bulk r "SISMEMBER" [k] member

sinter :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> [RedisKey] -> m RedisValue
sinter r ks = command r $ inline r "SINTER" ks

sstoreinter :: (MonadIO m, MonadFailure RedisError m) 
            => Server -> RedisKey -> [RedisKey] -> m RedisValue
sstoreinter r dest ks = command r $ inline r "SINTERSTORE" ([dest] ++ ks)

sunion :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> [RedisKey] -> m RedisValue
sunion r ks = command r $ inline r "SUNION" ks

sunionstore :: (MonadIO m, MonadFailure RedisError m) 
            => Server -> RedisKey -> [RedisKey] -> m RedisValue
sunionstore r dest ks = command r $ inline r "SUNIONSTORE" ([dest] ++ ks)

sdiff :: (MonadIO m, MonadFailure RedisError m) 
      => Server -> [RedisKey] -> m RedisValue
sdiff r ks = command r $ inline r "SDIFF" ks

sdiffstore :: (MonadIO m, MonadFailure RedisError m) 
           => Server -> RedisKey -> [RedisKey] -> m RedisValue
sdiffstore r dest ks = command r $ inline r "SDIFFSTORE" ([dest] ++ ks)

smembers :: (MonadIO m, MonadFailure RedisError m) 
         => Server -> RedisKey -> m RedisValue
smembers r k = command r $ inline r "SMEMBERS" [k]

srandmember :: (MonadIO m, MonadFailure RedisError m) 
            => Server -> RedisKey -> m RedisValue
srandmember r k = command r $ inline r "SRANDMEMBER" [k]

-- ---------------------------------------------------------------------------
-- Zset
-- 

{-
zadd k score member = command r $ inline r "ZADD" [
zremove k member = "ZREM"
zIncrementBy k = "ZINCRBY"
zrange k start end = "ZRANGE"
reverseRange k start end = "ZREVRANGE"
rangeByScore k min max = "ZRANGEBYSCORE"
zcardinality k = "ZCARD"
score k element = "ZSCORE"
removeRangeByScore k min max = "ZREMRANGEBYSCORE"
-}

-- ---------------------------------------------------------------------------
-- Sort
-- 

data Direction = ASC | DESC deriving (Show)

data Sorting = Sorting { sortBy :: Maybe RedisParam
                       , sortLimit :: Maybe (Int, Int)
                       , sortGet :: Maybe RedisParam
                       , sortDirection :: Maybe Direction
                       , sortAlpha :: Maybe Bool
                       , sortStore :: Maybe RedisParam } deriving (Show)

defaultSorting :: Sorting
defaultSorting = Sorting { sortBy = Nothing
                         , sortLimit = Nothing
                         , sortGet = Nothing
                         , sortDirection = Nothing
                         , sortAlpha = Just False
                         , sortStore = Nothing }

sort :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> RedisKey -> Sorting -> m RedisValue
sort r k sorting =
    command r $ inline r "SORT" ([k] ++
        (case (sortBy sorting) of
            Nothing -> [] 
            Just val -> ["BY", val]) ++
        (case (sortLimit sorting) of
            Nothing -> []
            Just (start, end) -> ["LIMIT", toParam start, toParam end]) ++
        (case (sortGet sorting) of
            Nothing -> []
            Just val -> ["GET", val]) ++ 
        (case (sortDirection sorting) of
            Nothing -> []
            Just ASC -> ["ASC"]
            Just DESC -> ["DESC"]) ++
        (case (sortAlpha sorting) of
            Nothing -> []
            Just True -> ["ALPHA"]
            Just False -> []) ++
        (case (sortStore sorting) of
            Nothing -> []
            Just val -> ["STORE", val]))

-- ---------------------------------------------------------------------------
-- Persistence
-- 

save :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> m RedisValue
save r = command r $ inline r "SAVE" []

bgsave :: (MonadIO m, MonadFailure RedisError m) 
       => Server -> m RedisValue
bgsave r = command r $ inline r "BGSAVE" []

lastsave :: (MonadIO m, MonadFailure RedisError m) 
         => Server -> m RedisValue
lastsave r = command r $ inline r "LASTSAVE" []

shutdown :: (MonadIO m, MonadFailure RedisError m) 
         => Server -> m RedisValue
shutdown r = command r $ inline r "SHUTDOWN" []

bgrewriteaof :: (MonadIO m, MonadFailure RedisError m) 
             => Server -> m RedisValue
bgrewriteaof r = command r $ inline r "BGREWRITEAOF" []

-- ---------------------------------------------------------------------------
-- Remote Server
-- 

info :: (MonadIO m, MonadFailure RedisError m) 
     => Server -> m RedisValue
info r = command r $ inline r "INFO" []

monitor :: (MonadIO m, MonadFailure RedisError m) 
        => Server -> m RedisValue
monitor r = command r $ inline r "MONITOR" []

slaveof :: (MonadIO m, MonadFailure RedisError m) 
        => Server -> RedisParam -> RedisParam -> m RedisValue
slaveof r host port = command r $ inline r "SAVE" [host, port]

