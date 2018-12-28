Haskell HTTP Proxy
==================

This is a simple HTTP proxy that caches web objects implemented with haskell.

## Features
1. Dealing with multiple requests concurrently
2. Supporting HTTP 1.1 and HTTP 1.0
3. Supporting HTTP GET and POST method
4. Supporting to cache some objects from the web and send to another client request later

## Get Started
1. Install [Haskell Platform](https://www.haskell.org/downloads)
2. ```cabal update```
3. ```cabal install string```
4. ```cabal install MissingH```
5. Install Split
```sh
git clone https://github.com/byorgey/split.git
cd split
cabal install
```
6. Clone this repository
```sh
git clone https://github.com/yintianyu/haskell-http-proxy.git
cd haskell-http-proxy
```
7. Compile
```sh
make
```
8. Run
```sh
./proxy [Port Number]
```

