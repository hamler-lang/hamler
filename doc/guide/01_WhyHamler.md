# Why Hamler



























## What's Hamler

`Hamler`  A Haskell-style functional programming language running on Erlang VM. 

It is a strongly-typed language with compile-time typechecking and built-in support for concurrency and distribution. This makes it perfect for building the next generation of scalable, reliable, realtime applications, especially for 5G, IoT and edge computing.

Cool, let's quit the bragging and kick off.

## Prequisitcs

- Basic Programming Skils
- It will be good to have some experience with Haskell or Eralng (but this is not essential)

## Haskell Style

First of all, Hamler is purely functional. It has really similiar syntax to Haskell, so if you are familiar with Haskell it should not be a problem. However, if you are not, the guide should be able to walk through the basic syntax and make you more comfortable with programming functionally.

This is an example of implementing merge sort in Hamler.

```haskell
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = splitAt (length xs `div` 2) xs
               in merge (mergesort as) (mergesort bs)
               
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge [x|xs] [y|ys] = if x <= y
                      then [x | merge xs [y|ys]]
                      else [y | merge [x|xs] ys]
```



## Erlang and Concurrency

**BEAM** is the virtual machine at the core of the Erlang Open Telecom Platform (OTP).

