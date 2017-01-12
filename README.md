# Orestes

Simple implementation of a key-value server, aimed to learn basic concepts of
functional programming through Haskell.
It currently support just the common operations `PUT`, `GET` and `DEL` and
it lacks a suitable communication protocol. Concurrency is handled through
transactional memory model given by library STM.

## Build

```sh
$ cabal install --only-dependencies
$ cabal configure
$ cabal build
```

## Play

It currently can be tested using a generic TCP client like Telnet or Netcat,
just connect and issue commands.

```sh
$ nc 127.0.0.1 6373
put key value
OK
get key
value
```
