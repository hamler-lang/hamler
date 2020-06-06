# Quick Start

- [Install](#Install)
- [Hamler Interpreter](#Hamler%20Interpreter)
- [Create A Project](#Create%20A%20Project)
- [Module Structure](#Module%20Structure)
- [Hello Hamler](#Hello,%20Hamler%20!)

---

## Intall

**Homebrew(macOS)**

```shell
$ brew tap hamler-lang/hamler
$ brew install hamler
```

**Install from source code(macOS)**

1. Install Eralng

   ```shell
   $brew install erlang@22
   ```

2. Install Stack 

   Stack tutoriall https://docs.haskellstack.org/en/stable/install_and_upgrade/

3. Clone from the git repo

   ```shell
   $ git clone https://github.com/hamler-lang/hamler.git
   ```

4. Install hamler

   ```shell
   $ cd hamler
   $ make
   $ make install
   $ cp repl/replsrv /usr/local/lib/hamler/bin/
   $ cp lib /usr/local/lib/hamler/
   $ cp ebin /usr/local/lib/hamler/
   ```



---

## Hamler Interpreter

```shell
$ hamler repl
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



---

## Create A Project

```shell
$ mkdir demo-project
$ cd demo-project
$ hamler init
$ make
$ make run
```



---

## Module struture

A module is simply a bunch of related functions, types and type classes. This makes a program a collection of modules. This helps oraganize your code and make reuse some of the code easier.

### Module header

**Module declaration**

This how we declare a new module and specify which of the fucntions or types are exported.

```haskell
module Helllo (greet, farewell) where
{-the module name can be a word or words seperated by '.',
  in this case i it is just "Hello"-}

greet :: String -> String
greet n = "Hello " ++ n

farewell :: String -> String
farewell n = "Bye " ++ n
```

**Module import**

The syntax for import in Hamler is `import <module name>`. This has to be done before defining any functions. One module can import as many as modules if you wish, but there could be ambigiousity when there are two things with the same name.

```haskell
import Data.List       --Modules are imported using their full names
import Data.Maybe (isJust, isNothing)   -- We can choose which functions to import
import qualified Data.Funtion as F     
{- We can deal with ambiguisity by this, this means we need to add "F." Before every functions we imported from Data.Function to specify that it is from Data.Function-}
import Prelue hiding (fst)  -- Prelude is the module always get imported, this way we can define our own fst
```



---

## Hello, Hamler !

```Haskell
module Main where

import System.IO

main = print "Hello, World!"
```
