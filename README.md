# The Hamler Programming Language
[![GitHub Release](https://img.shields.io/github/release/hamler-lang/hamler?include_prereleases&color=brightgreen)](https://github.com/hamler-lang/hamler/releases)
[![Docker Pulls](https://img.shields.io/docker/pulls/hamlerlang/hamler)](https://hub.docker.com/r/hamlerlang/hamler)
[![Slack Invite](<https://hamler-invite.emqx.io/badge.svg>)](https://hamler-invite.emqx.io/)
[![Twitter](https://img.shields.io/badge/Follow-Hamler%20Lang-1DA1F2?logo=twitter)](https://twitter.com/HamlerLang)
[![Community](https://img.shields.io/badge/Community-Hamler-yellow?logo=github)](https://github.com/hamler-lang/hamler/discussions)

**Hamler** - Haskell-style functional programming language running on Erlang VM.

**Hamler** is a strongly-typed language with compile-time type-checking and built-in support for concurrency and distribution.

**Hamler** empowers industries to build the next generation of scalable, reliable, real-time applications, especially for 5G, IoT, and edge computing.

## Why Hamler?

For almost a decade, we have been developing software systems based on Erlang/OTP, especially our main product [EMQ X](https://github.com/emqx/emqx) - the scalable open-source MQTT broker. So, we have always believed that Erlang is a masterpiece of engineering. With amazing concurrency, distribution, and fault tolerance, it is one of the few general-purpose language platforms able to properly handle concurrency and soft realtime.

However, from all the experience writing Erlang, we believe that the following features can help Erlang programmers better adapt to the coming wave of 5G, IoT, and edge-programming and attract more people to using BEAM.

- Compile-time type checking and type reference
- ADTs, Function Composition, Type Classes
- More friendly syntax for prosperous communities
- Functor, Applicative, and Monad...:)

Now all the features are available in the Hamler programming language.

## Features

- Functional programming
- Haskell and ML style
- Compile-time type Checking/Inference
- Algebraic data type (ADT)
- Functions, higher-order functions
- Currying and partial application
- Pattern matching, and Guards
- List comprehension
- Applicative and Monad
- Advanced module system
- Built-in concurrency

## Design

The Hamler compiler architecture is shown as below:

![hamler-compiler](https://raw.githubusercontent.com/hamler-lang/hamler.github.io/master/images/compiler.png)

The Hamler 0.1 compiler was forked from [PureScript][PureScriptHamler] 0.13.6. The Hamler source code is parsed to generate **CST**, then CoreErlang's IR is generated after **CST -> AST -> CoreFn**'s syntax tree transformation, syntax analysis and type checking. The code is then used by the Erlang compiler to generate the final Beam bytecode.

[PureScriptHamler]: https://github.com/hamler-lang/purescript

## Installation

Get the installation package through [Github Release](https://github.com/hamler-lang/hamler/releases)

> packages for windows will be coming soon...

**Linux**

```shell
$ tar zxvf hamler-$version.tgz -C /usr/lib/
$ ln -s /usr/lib/hamler/bin/hamler /usr/bin/hamler
```

**Centos 7**

```shell
$ rpm -ivh hamler-$version-1.el7.x86_64.rpm
```

**Debian 8 / Debian 9 / Debian 10 / Ubuntu 16.04 / Ubuntu 18.04 / Ubuntu 20.04**

```shell
$ dpkg -i hamler_$version_amd64.deb
```

**Homebrew(macOS)**

```shell
$ brew tap hamler-lang/hamler
$ brew install hamler
```
## Project Layout

```shell

├── .github/workflows     # ci
├── app                   # hamler commands, such as `compile`, `repl`, ...
├── deploy
├── doc                   # link to hamler documents
├── lib                   # hamler standard libraries
∣   ├── Control           # common control structures, concurrent Process, and OTP behaviours. 
∣   ├── Data              # commonly used data structures and functions
∣   ├── Database          # common databases in erlang, such as ETS, DETS, and Mnesia
∣   ├── Foreign           
∣   ├── Network           # hamler network libraries.
∣   ├── System            # system libraries including IO, VM, OS, Env, File, Random, Timer, and Logger. 
∣   ├── Test              # hamler QuickCheck test framework
∣   ├── Foreign.hm        # hamler FFI functions
∣   └── Prelude.hm        # functions exported by default
├── repl
∣   └── replsrv           # hamler REPL server
├── src/Language          # hamler compiler source
∣   ├── Hamler
∣   ∣   ├── CodeGen.hs    # convert CoreFn to CoreErlang
∣   ∣   ├── Make
∣   ∣   ├── Make.hs
∣   ∣   └── Util.hs
∣   └── Hamler.hs
├── tests                 
∣   ├── Test              # test cases for hamler libraries
∣   ∣   ├── Control
∣   ∣   ├── Data          
∣   ∣   ├── Database
∣   ∣   ├── Network
∣   ∣   └── System 
∣   ├── Test.hm           # entrance to execute test cases
∣   └── Main.hs
├── .gitignore
├── LICENSE
├── Makefile
├── README.md
├── Setup.hs
├── hamler.cabal
├── package.yaml         
└── stack.yaml

```

## Build from source code

**Required**

+ [Erlang/OTP](https://www.erlang.org) >= 23
+ [Haskell Stack](https://haskellstack.org)
+ [Cabal](https://www.haskell.org/cabal)

**Building**
1. Install happy-1.19.9

   ```shell
   $ stack install happy-1.19.9 --resolver lts-13.26
   ```

2. Clone hamler from the git repo

   ```shell
   $ git clone https://github.com/hamler-lang/hamler.git
   $ cd hamler
   ```

3. Configuring

   ```shell
   $ export LC_ALL=en_US.UTF-8
   ```

4. Building

   ```shell
   $ make
   ```

5. Testing

   ```shell
   $ make test
   ```

6. Install hamler

   ```shell
   $ sudo make install
   ```

## Create a project

  ```shell
  $ mkdir demo-project
  $ cd demo-project
  $ hamler init
  $ make
  $ make run
  ```

## Try the interpreter

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
  > m1 = Map.put "key" "val" m
  > -- keys, values
  > keys = Map.keys m
  > values = Map.values m
  ```

### Documentation

- [Cheatsheet](https://github.com/hamler-lang/documentation/blob/master/Cheatsheet.md)
- [Guides](https://github.com/hamler-lang/documentation/tree/master/guides/)
- [FAQ](https://github.com/hamler-lang/documentation/blob/master/FAQ.md)

## Community, discussion, and supports

You can reach the **Hamler** community and core team via the following channels:

- [Slack - Hamler](https://hamler-invite.emqx.io/)
- [Twitter - @hamlerlang](https://twitter.com/hamlerlang)
- [Reddit - /r/HamlerLang](https://www.reddit.com/r/HamlerLang/)
- [Medium - @hamlerlang](https://medium.com/@hamlerlang)

## Contributing

To contribute to **Hamler** project:

- Report issues : submit any bugs, issues to [hamler/issues][hamler-issues]
- Contribute code: Reference [**roadmap**](https://github.com/hamler-lang/hamler/milestones) as a guidance for new features.
                   Fork the project, and submit feature requests to [hamler-lang/hamler][hamler-project]
- Submit a proposal: Fork the [hamler-wiki][hamler-wiki] project and submit a pull request

[hamler-issues]: https://github.com/hamler-lang/hamler/issues
[hamler-project]: https://github.com/hamler-lang/hamler
[hamler-wiki]: https://github.com/hamler-lang/hamler-wiki

## Core Team

The Hamler core team comes from [EMQ Technologies Co., Ltd.](https://emqx.io/) now:

- [Feng Lee](https://github.com/emqplus): Designer of Hamler Language
- [Yang M](https://github.com/EMQ-YangM): Implemented Hamler Compiler
- [S Hu](https://github.com/SjWho): Maintainer of the documentations
- [Shawn](https://github.com/terry-xiaoyu): Contributed [rebar3_hamler][rebar3_hamler] plugin
- [Rory Z](https://github.com/zhanghongtong): Contributed [homebrew][homebrew] install package
- [wivwiv](https://github.com/wivwiv): Designer of hamler-lang.org website
- [CrazyWisdom](https://github.com/CrazyWisdom): Maintainer of hamler-lang.org
- [ysfscream](https://github.com/ysfscream): Maintainer of hamler-lang.org
- [juan6666](https://github.com/juan6666)：Designer of Hamler language logo

[homebrew]: https://github.com/hamler-lang/homebrew-hamler
[rebar3_hamler]: https://github.com/hamler-lang/rebar3_hamler

## License & Credits

We would like to express our great appreciation to the [PureScript][PureScriptSite] team and community for all years of efforts making its amazing compiler, especially **CoreFn** and its incredible adaptability.

The hamler language project is licensed under BSD3, see [LICENSE](./LICENSE).

[PureScriptSite]: https://www.purescript.org/

## About EMQ

[**EMQ**](https://emqx.io/) is an open-source software company providing highly-scalable, real-time messaging and streaming platforms for IoT applications in the 5G Era.
