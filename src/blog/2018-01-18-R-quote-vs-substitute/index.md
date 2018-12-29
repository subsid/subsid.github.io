---
layout: layouts/blog.liquid
title: "R: Quote vs Substitute"
tags: [post, R]
permalink: blog/quote-vs-substitute/
---

*Goal:* *Understand the use case of both `quote` and `substitute`.*

*What is the difference between the following 2 code blocks, even though they produce the same output?*
If you are not sure, this post will help you.

```r
rm(list=ls())
x <- 1:1e8
g <- function(a){
    b <- substitute(a)
    print(eval(b))
    print(eval(b))
}

g(mean(x))

## [1] 5e+07
## [1] 5e+07
```

```r
rm(list=ls())
x <- 1:1e8
g <- function(a){
    b <- quote(a)
    print(eval(b))
    print(eval(b))
}

g(mean(x))

## [1] 5e+07
## [1] 5e+07
```

One of the really (really) cool features of R is the idea of [Non Standard Evaluation](http://adv-r.had.co.nz/Computing-on-the-language.html).

## Capturing expressions with *substitute*.

Function arguments in R are evaluated [lazily](https://en.wikipedia.org/wiki/Lazy_evaluation). [Hadley's book](http://adv-r.had.co.nz/Functions.html#function-arguments) explains this in the context of R, if you want to learn more about the advantages and the idea in general, I recommend these readings in [haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/6-laziness) and [clojure](http://clojure-doc.org/articles/language/laziness.html).

In R, *function arguments are evaluated only if they're used.*

```r
f <- function(x) {
  10
}
f(stop("This is an error!"))

## [1] 10
```

At an implementation level, this means that the argument passed to `f` is not a *value*, it's a special type of object called **promise**. The advantage of this, is that we have access, not only to the **final value**, but also to the expression that **computes it**.

We can capture this **expression** using the `substitute` command and then evaluate it with `eval`.

```r
f <- function(x) {
  print(x)
  print(substitute(x))
  print(eval(substitute(x)))
}
f(10 * 5 + 2)

## [1] 52
## 10 * 5 + 2
## [1] 52
```

## What about *quote*?
Let's see.

```r
f <- function(x) {
  print(x)
  print(quote(x))
  print(eval(quote(x)))
}
f(10 * 5 + 2)

[1] 52
x
[1] 52
```

Interesting! In this case, we don't see the *expression* `10 * 5 + 2`, but we see the variable `x`. In other words, quote captures its input *as is*. In this case, it just captures the **promise**.

*How is this useful?*

`quote` let's us capture variables that are not bound to anything in the environment yet. We can capture it, and later evaluate it, when the environment has a value for it.

```r
f <- function(x) {
  print(x)
  a <- 10
  b <- 20
  print(eval(x))
}

f(quote(a * b + 5))
```

`a` and `b` were not defined when the function was called, but using `quote` we can still call the function!


## So how does this work in our initial example?

Since, substitute captures the *expression*, it has to **reevaluate** it everytime its called, but in case of **quote**, it evaluates it only once! (As the promise gets *resolved* the first time its called, and then just gets looked up) We can easily see this with some profiling.

```r
rm(list=ls())

time <- function(expr) {
  start <- proc.time()
  expr
  print(proc.time() - start)
}

x <- 1:1e8
g <- function(a){
    ## substitute
    print("substitute")
    time(print(eval(substitute(a))))
    time(print(eval(substitute(a))))
    print("quote")

    ## quote
    time(print(eval(quote(a))))
    time(print(eval(quote(a))))
}

g(mean(x))

[1] "substitute"
[1] 5e+07
   user  system elapsed 
  0.094   0.000   0.094 
[1] 5e+07
   user  system elapsed 
  0.092   0.000   0.091 
[1] "quote"
[1] 5e+07
   user  system elapsed 
  0.103   0.000   0.103 
[1] 5e+07
   user  system elapsed 
      0       0       0
```

### References
- [Hadley's Advanced R](http://adv-r.had.co.nz/).
- [Stat Computing](https://longjp.github.io/statcomp/) course by [James Long](http://www.stat.tamu.edu/~jlong/)

