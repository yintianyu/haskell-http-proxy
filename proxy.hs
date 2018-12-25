-- proxy.hs
-- Author: Tianyu Yin

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
import qualified ProxyCache as PC
-- import Send

-- parseHost: 从HTTP请求中解析出host和port信息
parseHost:: String -> (String, String)
parseHost msg = 
    if isInfixOf ":" socket
        then (splitOn ":" socket !! 0, splitOn ":" socket !! 1)
        else (socket, "80")
    where socket = splitOn "/" (splitOn "\r\n" msg !! 0) !! 2

-- parsePath: 从HTTP请求中解析出目标资源的path信息
parsePath:: String -> String
parsePath msg = 
    splitOn socket (splitOn " " getLine !! 1) !! 1
    where
        getLine = splitOn "\r\n" msg !! 0   -- The first line of the request
        socket = splitOn "/" getLine !! 2   -- www.ip138.com or www.ip138.com:80

-- parseType: 从HTTP请求中解析出类型信息（当前版本支持GET和POST），及其是不是GET类型
parseType:: String -> (String, Bool)
parseType msg = 
    (str, str == "GET")
    where
        getLine = splitOn "\r\n" msg !! 0   -- The first line of the request
        str = splitOn " " getLine !! 0

        
-- parseUrl: 从HTTP请求中解析出URL
parseUrl:: String -> String
parseUrl msg = 
    splitOn " " getLine !! 1
    where
        getLine = splitOn "\r\n" msg !! 0   -- The first line of the request
        
-- concatStringList: 把一个字符串列表拼接起来，在拼接处插入另一个字符串
concatStringList:: [String] -> String -> String
concatStringList listStr insert = 
    if length listStr == 1
        then listStr !! 0
        else (listStr !! 0) ++ insert ++ (concatStringList (tail listStr) insert)

-- modifyConnectionHelper: 找到HTTP请求中原有的connection和proxy-connection头并修改
modifyConnectionHelper:: String -> String
modifyConnectionHelper element = 
    if isInfixOf "Proxy-Connection: " element
        then "Proxy-Connection: close"
    else if isInfixOf "Connection: " element
        then "Connection: close"
        else element

-- addConnectionHelper: 为HTTP请求添加以前没有的connection和proxy-connection头
addConnectionHelper:: [String] -> [String]
addConnectionHelper listStr = 
    if ("Proxy-Connection: close" `elem` listStr) && ("Connection: close" `elem` listStr)
        then listStr
        else if "Proxy-Connection: close" `elem` listStr
            then (init listStr) ++ ["Connection: close", ""]
            else if "Connection: close" `elem` listStr
                then (init listStr) ++ ["Proxy-Connection: close", ""]
                else (init listStr) ++ ["Proxy-Connection: close", "Connection: close", ""]

-- modifyConnection: 使得HTTP请求肯定包含connection和proxy-connection
modifyConnection:: String -> String
modifyConnection strMsg = 
    concatStringList added "\r\n"
    where
        modified = map modifyConnectionHelper (splitOn "\r\n" strMsg)
        added = addConnectionHelper modified

-- addHost: 为HTTP请求增加host头部
addHost:: String -> String -> String
addHost strMsg host = 
    if isInfixOf "Host: " (listStr !! 1)
        then strMsg
        else concatStringList modifiedListStr "\r\n"
    where
        listStr = splitOn "\r\n" strMsg
        modifiedListStr = (head listStr) : (("Host: " ++ host) : (tail listStr))


-- isEnd: 检查一个请求/响应是否结束（以空行结束，POST请求除外）
isEnd msg = endswith "\r\n\r\n" (C.unpack msg)


-- responseList: 从server获取response，因为一次只接收1024字节数据，所以需要递归多次接收并拼接
responseList:: Socket -> IO [C.ByteString]
responseList connFrom = do
    msg <- recv connFrom 1024
    if (BS.null msg || (isEnd msg))
        then
            return [msg]
            else do
                nextMsg <- responseList connFrom
                return (msg : nextMsg)

                
-- responseList: 从server获取response，因为一次只接收1024字节数据，所以需要递归多次接收并拼接
_getPostData:: Socket -> Bool -> IO(C.ByteString)
_getPostData conn isGet = do
    if (isGet)
        then
            return (C.pack "")
            else do
                postTable <- recv conn 2048
                return (postTable)


    
-- thread: 独立的线程，完成从客户端接收请求->修改请求->发送到服务器->从服务器接收->发送给客户端
thread conn tVarCache = do
    -- print "TCP server is waiting for a message..."
    msg <- recv conn 8192
    unless (BS.null msg) $ do 
        let msgStr = C.unpack msg
        -- print (msg)
        let (host, port) = parseHost msgStr
        let url = parseUrl msgStr
        let (httpType, isGet) = parseType msgStr
        msgPost <- _getPostData conn isGet
        let msgStrPost = C.unpack msgPost
        -- print("url: " ++ url)
        (getted, content) <- atomically(PC.tryGetFromCache tVarCache url isGet)
        if getted
            then do
                sendAll conn content
                print ("!!!URL " ++ url ++ " loaded from Cache!!!")
            else do
                let path = parsePath msgStr
                let msgStr1 = concatStringList ((httpType ++ " " ++ path ++ " HTTP/1.0"):tail (splitOn "\r\n" msgStr)) "\r\n"
                let msgStr2 = modifyConnection msgStr1
                let msgStr3 = (addHost msgStr2 host) ++ msgStrPost
                -- print ("HTTP1.1 Request: " ++ msgStr ++ msgStrPost)
                -- print ("HTTP1.0 Request: " ++ msgStr3)
                -- print ("HTTP Request End")
                addrinfos <- getAddrInfo Nothing (Just host) (Just port)
                let serveraddr = head addrinfos
                sockServer <- socket (addrFamily serveraddr) Stream defaultProtocol
                connect sockServer (addrAddress serveraddr)
                sendAll sockServer $ C.pack msgStr3
                -- unless (isEnd msg) $ do
                --     talk conn sockServer
                -- talk sockServer conn
                listResponse <- responseList sockServer
                let response = BS.concat listResponse
                atomically(PC.tryWriteToCache tVarCache url response $ BS.length response)
                sendAll conn response            
                -- print "TCP server is now sending a message to the client"
    close conn
    -- print "Client conn closed, thread end"
    


-- runTCPProxyServer: 完成打开端口、建立套接字、等待请求的过程，同时还要新建STM变量用于cache
runTCPProxyServer :: String -> IO ()
runTCPProxyServer port = do 
    -- Cache TVar
    cache <- atomically(newTVar [])
    addrinfos <- getAddrInfo Nothing (Just "0.0.0.0") (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1
    rrLoop sock cache
    -- print "TCP server socket is closing now."
    close sock
    where 
        rrLoop sock cache = do
            -- print ("listening")
            (conn, _) <- accept sock
            -- print "connection established"
            forkIO (thread conn cache)
            rrLoop sock cache

-- parseParameter: 解析指定的端口
parseParameter :: [String] -> String
parseParameter args = 
    if length args == 1
        then args !! 0
        else "7000"

main = do
    args <- getArgs
    let port = parseParameter args
    print ("Using port: " ++ port)
    runTCPProxyServer port
