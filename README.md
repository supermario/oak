
:warning: Oak is in an experimental stage

**Oak is a server-side application framework + full-stack toolkit for using Haskell and Elm**

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

See the [examples](examples) folder for more complex examples.

### Features

You can do anything you can do with Haskell, but Oak directly provides:

- An app framework and architecture (mirroring The Elm Architecture)
-

Future plans include:

- Tasks for reading and writing files
- Database querying (Postgres)


### Guides

- New to FP
- Coming from Elm
- Coming from Haskell
