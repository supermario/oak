
:warning: Oak is in an experimental stage

**Oak is a minimal server-side application framework inspired by Elm and The Elm Architecture**

See [motivations](docs/motivations.md) for comparisons with other approaches.

Here is a full working example of an Oak application:

```haskell
module App where

import Oak

main :: Program
main = Program 0 update view subscriptions

type Model = Int
data Msg = Increment | Decrement | Noop deriving (Read, Show, Eq)

update :: Model -> Msg -> Model
update model msg = case msg of
  Increment -> model + 1
  Decrement -> model - 1
  Noop      -> model

view = show

subscriptions = [onKeyEnter Increment]
```

See the [examples](examples) folder for more complex examples, including a websocket server.

### Features

You can do anything you can do with Haskell (or even just use Oak in one area of an existing Haskell app).

Oak itself directly provides:

- A reactive app architecture (mirroring The Elm Architecture)
- Effect managers that work with that Architecture

Future things that may be explored are:

- reading and writing system files
- database querying (Postgres)
- HTTP endpoints

A design goal is to have an exception-free experience

### Guides (TBC)

- New to FP
- Coming from Elm
- Coming from Haskell

### Installation (TBC)
