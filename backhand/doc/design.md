# Introduction

The overall design of backhand is that of a structure that concurrently maps
connectors to services. So when you are making a backhand system you have an
incoming connection that is routed into a channel, or backhand lobby/room, and
the connector's manager gets a concurrent channel to talk through to the
services of the channel. Services can be anything from a server that has it's
own connection management and threading model, to an individual server that was
spin'd up for the channel itself (like a game match), or even to another system
on a remote host (you can tunnel the requests to a load balanced pool of
servers).

It's up to the implementer to decide which pursuit is best for their own case.
backhand itself will try to make each of these approaches as easy as it can be
to implement over it's own structure than it would be to implement the whole
stack themselves. backhand should also provide almost zero overhead to the
system as it allows both the connectors and services to talk to each other over
channel without having to directly interface with backhand structure (you have a
channel to talk to the thread concurrently and backhand doesn't interact with it
at all).
