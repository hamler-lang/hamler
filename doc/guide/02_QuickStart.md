# Set Up 



## Intall

**Homebrew(macOS)**

```shell
brew tap hamler-lang/hamler
brew install hamler
```

**Install from source code(macOS)**

1. Install Stack 

   Stack tutoriall https://docs.haskellstack.org/en/stable/install_and_upgrade/

2. Clone from the git repo

   ```shell
   git clone https://github.com/hamler-lang/hamler.git
   ```

3. Install hamler

   ```shell
   cd hamler
   make 
   make install
   cp repl/replsrv /usr/local/lib/hamler/bin/
   cp lib /usr/local/lib/hamler/
   cp ebin /usr/local/lib/hamler/
   ```

## 



---

## Model struture

Module header

Module import

## Some Basic Syntax



## Hello, Hamler !

```Haskell
module Main where

import ???

main = print "Hello, World!"
```

