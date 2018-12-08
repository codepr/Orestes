[![Build Status](https://travis-ci.org/codepr/Orestes.svg?branch=distributed)](https://travis-ci.org/codepr/Orestes)

# Orestes

Simple implementation of a distributed key-value server, aimed to learn basic
concepts of functional programming through Haskell. For the distribution
it uses cloud-haskell libraries, based on asynchronous message protocol like
Erlang distribution actor model. It works on both a cluster of machines or on
a single one according to a master-slave topology.
It currently support just the common operations `PUT`, `GET` and `DEL` and
it lacks a suitable communication protocol, on the other side it is perfectly
usable by using a generic TCP client like Telnet or Netcat issuing commands
as strings.

## Quickstart

```sh
$ stack build
```

## Run

```sh
$ stack run <args>
```

It can be started either on a single node or distributed in a cluster of
machines

### Single machine

```sh
$ stack run <port>
```
`<port>` argument is optional and it fallback to 6373 if omitted.

### Cluster

Orestes is designed as Master-slave topology, in order to start a cluster,
it is first needed to start all slave nodes

```sh
$ stack run slave <ip-address> <backend-port>
```

and finally the master node, which will discover all his slaves

```sh
$ stack run master <ip-address> <backend-port> <port>
```

Backend port is needed to communicate between nodes, again port fallback to
6373 if not set.

It currently can be tested using a generic TCP client like Telnet or Netcat,
just connect and issue commands.

```sh
$ nc 127.0.0.1 6373
put key value
OK
get key
value
```

## License

See the [LICENSE](LICENSE) file for license rights and limitations (MIT).
