# htrade - bitcoin trading platform

## Subprojects

#### htrade-backend

Backend services interacting with database layer and proxy layer.

#### htrade-proxy

Proxy layer interacting with backend layer and public markets.

#### Other

Contains a shared library with various funtionality.

## Progress

Currently contains an almost complete implementation of htrade-proxy but is quite lacking on the htrade-backend.
A storage interface (or live-data-feed?) in the backend must also be implemented.
