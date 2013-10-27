# Distributed BTC market fetching

This project is described in [this blog post](http://davnils.github.io/#distributed_btc_markets).

## Getting started

In order to use this project you need to:

* Setup a Cassandra cluster with a configured keyspace
* Setup all tables using htrade-admin
* Write configuration files for all markets
* Launch backend
* Launch one or several proxy nodes

## Subprojects

#### htrade-admin
Administration tool used to initialize and destroy Cassandra tables.

#### htrade-backend
Backend services interacting with database layer and proxy layer.

#### htrade-proxy
Proxy layer interacting with backend layer and public markets.

#### Shared
Functionality shared between the different subprojects.

#### Test
Test suites covering most parts of the backend and proxy.
