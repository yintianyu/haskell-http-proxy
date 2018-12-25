-- cache.hs
-- Author: Tianyu Yin

module ProxyCache(
    tryGetFromCache,
    tryWriteToCache
)where

import Control.Concurrent        (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad             (unless, liftM2)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.List     (isInfixOf)
import Data.List.Split
import Data.String.Utils
import System.Environment


-- Define Datatype CacheItem
type URL = String
type Content = C.ByteString
type Size = Int
type CacheItem = (URL, Content, Size)

type Cache = TVar [CacheItem]

-- Define constants for cache
maxCacheSize = 1000000  -- cache空间最大1MB
maxObjectSize = 100000  -- 单个对象空间最大100KB

-- 判断是否符合cache的条件
needCaching:: Int -> Bool
needCaching objectSize = 
    objectSize <= maxObjectSize

-- 检查剩余的cache空间
cacheSpaceRemaining:: Cache -> STM (Int)
cacheSpaceRemaining tVarCache = do
    cache <- readTVar tVarCache
    let sizes = map (\(_, _, size) -> size) $ cache
    return (maxCacheSize - sum sizes)

-- 检查是否需要evict现有的cache
needEvicting:: Cache -> Int -> STM(Bool)
needEvicting tVarCache size = do
    spaceRemaining <- cacheSpaceRemaining tVarCache
    return (size > spaceRemaining)

-- evict最早没有用过的cache
evict::Cache -> STM()
evict tVarCache = do
    cache <- readTVar tVarCache
    let newCache = tail cache
    writeTVar tVarCache newCache

-- 向cache中写入一项
writeCache::Cache -> URL -> Content -> Size -> STM()
writeCache tVarCache url content size = do
    cache <- readTVar tVarCache
    let newItem = (url, content, size)
    let newCache = cache ++ [newItem]
    writeTVar tVarCache newCache

-- 检查该url是否在cache中
isCached:: Cache -> URL -> STM(Bool)
isCached tVarCache url = do
    cache <- readTVar tVarCache
    let urls = map (\(theUrl, _, _) -> theUrl) $ cache
    return (url `elem` urls)

getContentFromCacheItem (_, content, _) = content

-- 从cache中加载对应的对象
getCachedObject:: Cache -> URL -> STM(Content)
getCachedObject tVarCache url = do
    cache <- readTVar tVarCache
    let cacheItem = (filter (\(theUrl, _, _) -> theUrl == url) $ cache) !! 0
    let content = getContentFromCacheItem $ cacheItem
    let cacheRemoved = filter (\(theUrl, _, _) -> theUrl /= url) $ cache
    let newCache = cacheRemoved ++ [cacheItem]
    writeTVar tVarCache newCache
    return (content)

-- 尝试从cache中获取，返回成功或失败及其内容
tryGetFromCache:: Cache -> URL -> Bool -> STM((Bool, Content))
tryGetFromCache tVarCache url isGet = do
    if isGet
        then do
            getted <- isCached tVarCache url
            if (getted)
                then do
                    content <- getCachedObject tVarCache url
                    return (True, content)
                else return (False, C.pack "")
        else
            return (False, C.pack "")

-- 通过不断的evict保证有足够空间来容纳该cache项
_ensureEnoughSpace:: Cache -> Size -> STM()
_ensureEnoughSpace tVarCache size = do
    toBeEvicted <- needEvicting tVarCache size
    if not toBeEvicted
        then return()
        else do
            evict tVarCache
            _ensureEnoughSpace tVarCache size

-- 尝试将该对象写入cache中
tryWriteToCache:: Cache -> URL -> Content -> Size -> STM()
tryWriteToCache tVarCache url content size = do
    let toBeCached = needCaching size
    if not toBeCached
        then return ()
        else do
            _ensureEnoughSpace tVarCache size
            writeCache tVarCache url content size







