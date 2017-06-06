# backhand (lib)

Core functionality and generic helpers.

## Inspiration

The main idea of backhand is that creating rooms, lobbies, and game sessions is
a reoccuring trend in within many server based projects. So whether you are
writing a chat server, a board game, or something else entirely then backhand
might be able to help you abstract away most of the messaging between users and
the server itself.

## Getting Started

Backhand's core library has a few concepts, but should be relatively simple to
get started with; some familiarity with Haskell packages like stm will help as
backhand uses copious amounts of it.

### Channels

A channel is a thread that manages the state of connections between modules and requesters.

### Modules

A module in backhand is a thread that handles messages within a channel.

### Requesters
