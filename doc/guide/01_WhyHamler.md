# Why Hamler?

- [What's Hamler](#What's%20Hamler)
- [Prequisitcs](#Prequisitcs)
- [Haskell Style](#haskell-style)
- [Type Checking](#Type%20Checking)
- [Erlang and Concurrency](#Erlang%20and%20Concurrency)

For almost a decade, we have been developing software systems based on Erlang/OTP, especially our main product [EMQ X](https://github.com/emqx/emqx) - the scalable open-source MQTT broker. So, we have always believed that Erlang is a masterpiece of engineering. With amazing concurrency, distribution and fault tolerance, it is one of the few general-purpose language platforms able to properly handle concurrency and soft realtime.

However, from all the experience writing Erlang, we believe that the following features can help Erlang programmer better adapt to the coming wave of 5G, IoT and edge-programming and attract more people for using BEAM.

- Compile-time type checking and type reference
- ADTs, Function Composition, Type Classes
- More friendly syntax for prosperous communities
- Functor, Applicative and Monad...:)

Now all the features are avaliable in the Hamler programming language.

---

## What's Hamler

`Hamler`  A Haskell-style functional programming language running on Erlang VM. 

It is a strongly-typed language with compile-time typechecking and built-in support for concurrency and distribution. This makes it perfect for building the next generation of scalable, reliable, realtime applications, especially for 5G, IoT and edge computing.

Cool, let's quit the bragging and kick off.



---

## Prequisitcs

- Basic Programming Skils

- It will be good to have some experience with Haskell or Eralng (but this is not essential)

  

---

## Haskell Style

First of all, Hamler is purely functional. It has really similiar syntax to Haskell, so if you are familiar with Haskell it should not be a problem. However, if you are not, the guide should be able to walk through the basic syntax and make you more comfortable with programming functionally.

This is an example of implementing merge sort in Hamler. It is normal that you don't understand what going, the purpose of the exmaple to is just let you get a gist of what will the code look. 

```haskell
merge :: forall a. Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge [x|xs] [y|ys] = if x <= y
                      then  [x |merge xs [y|ys]]
                      else  [y |merge [x|xs] ys]
merge _ _ = error "nice"  --this line is not neccessary and will be solved when new update released.

mergesort :: forall a. Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = splitAt (length xs / 2) xs
               in merge (mergesort as) (mergesort bs)
```



---

## Type Checking 

Hamler is strongly typed with compile type checking. So at compile time we can ensure that our program is type-safe, and this can help porgrammers to avoid silly mistakes like adding a string and an integer. 



---

## Erlang and Concurrency

Erlang is famous for its concrurrency. Concurrent porgramming can be used to imporve performance, gain scalability and fault-torlerance. **BEAM** is the virtual machine at the core of the Erlang Open Telecom Platform (OTP) which enables it to happen. By compiling Hamler to CoreErlang, we can essential take avantage from Erlang VM.
