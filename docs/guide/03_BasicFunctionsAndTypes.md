# Basic Functions and Types



## 3. 1 Simple Types

Hamler is strongly typed, and has a powerful static type system. Let's start with some simple exmaples.

**Boolean**

```Haskell
true :: Boolean
false :: Boolean 
```

**Numbers** 

Hamler has Integer and Float, and since they have different types so they can't be mixed together. 

```Haskell
--Integer
1 :: Int

--Float 
0.1 :: Float 
```

**Atoms**

Atom is probably more familiar to Erlang user. It is  a literal, a constant with a name start with `:` .

```
:hello
:world
```

**Strings**

In Hamler `String` is just a list of `Char` 

```Haskell
"Hello World" :: String  -- ['H','e','l','l','o',',','W','o','r','l','d']
```

**Binaries**

This is the very unique datatype exists in Erlang, and notes for Haskell users `Binary` contains the same information as `ByteString` if you are not very familiar with binaries, this [link](https://erlang.org/doc/man/binary.html) should be helpful for some intuition. 

```

```



## 3. 2 Quantified Types

They are also known as **polymorhphic types**. 

```Haskell
>:type id
id :: forall a. a -> a
```

The key word `forall`indicates that id is univerally quantified, meaning that id can be applied with any type. 

```Haskell
>id 1
1
```

A more complicated example is `flip`. `flip`is also a [high-order function](), which will be explained in the later chapter. 

```Haskell
>:type flip
forall a b c. (a -> b -> c) - > b -> a -> c
```



## 3. 3 Notes On Indentation

Like all ML Language Family, Hamler is indentation sensitive. Any language in the same block should have same level of indentation. In the case of a declaration spans more than one line, the other lines have to be intended past the first line.

```Haskell
flip x f = f
x                   -- NOT OKAY, Hamler will see x as a seperate declaration

flip f x = f
    x               -- OKAY, but not recommended
```

**`Let` and `Where` Block**

Keywork such as Let and Where introduces new block, where further indentation is needed.

```haskell
distance x y = sqrt z
  where
    z = x' + y'
    x' = x * x
    y' = y * y
```



## 3. 4 Define your own types

**Type Synonym**

Type synonym can be used to simply a long type name to make code more readable. 

```Haskell
>:i String
type String = [Char]
```

Or you can define you own synonym name or a record.

## 3. 5 Record

```Haskell
type Name = String

type Person =
  { FisrtName  :: Name
  , SecondName ::Name
  }

{-
This is syntax sugared
"type Person = Record (FisrtName  :: Name , SecondName ::Name)" 
-}
```



