# Quick Start

## Intallation

```shell
brew install hamler
```

## Hamler interpreter

```shell
hamler repl
```

```haskell
> -- List, range and enums
> [1,2,3]
> [1..10]
> ['a'..'z']

> -- erlang style maps
> import Data.Map as Map
> -- New map
> m = #{"foo" => "bar", "bar" => "foo"}
> -- Match Map
> #{"foo" := a, "bar" := b} = m
> -- get, put
> Map.get "foo" m -- a = "bar"
> Map.get "bar" m -- b = "foo"
> m1 = Map.put "key" "val"
> -- keys, values
> keys = Map.keys m
> values = Map.values m
```

## Create a project

```shell
mkdir demo-project
cd demo-project
hamler init
make
make run
```

