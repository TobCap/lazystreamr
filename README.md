# lazystreamr

``` r
devtools::install_github("tobcap/lazystreamr")
library("lazystreamr")

# Compare to Haskell's code
# https://wiki.haskell.org/The_Fibonacci_sequence#Canonical_zipWith_implementation
lfib1 <- 0 %:% (1 %:% lzipWith('+', lfib1, ltail(lfib1)))
ltake(lfib1, 30)
# lfib1 %>% ltake(30) # with magrittr's %>%

# another fibonacci calculation
lfib2 <- lseq_gen(0, 1, f = function(x, y) x + y)
ltake(lfib2, 30)

ltake(lmap(liota(), function(x) x ^ 2), 10)
## great readability with pipe-operator
# liota() %>% lmap(function(x) x ^ 2) %>% ltake(10)
```

## Installation
You can install lazystreamr from github with:
``` r
# install.packages("devtools")
devtools::install_github("tobcap/lazystreamr")
```


## Motivation
You may or not know that R is internally based on scheme's concepts such as
language object, LANG SEXP, comprise of 'cons cell'. This turned on my idea that
lazy stream defined in [SRFI-45](http://srfi.schemers.org/srfi-45/srfi-45.html)
seemed to be able to transelate to R.


## Concept
The semantics is from Scheme, but function names are from Haskell adding header
letters "l" with few exceptions.
Almost all ideas are from Scheme. The most fundamental function is lcons() which
takes two arguments. The output is a R's pairlist object which has two lengths,
named head and tail respectively, and S3 class name of "lcons". The head part is
evaluated when constructing output but the tail part is unevaluated by using a
promise (using closure).

## Syntax differences from Hakell
* It is now widely getting prevailed in R to use pipe operator %>%. To accommodate
with idea, functions that take lambda and data for arguments are defined that data
object set as the first argument.
* There is tuple class in Haskell but not in R. So all the tuple type in Haskell's
context is converted to lcons in lazystreamr's context.

## Usage
``` r
  # Construct a lazy stream object
  l1 <- lcons(1, lcons(2, lcons(3, lempty)))
  l2 <- 1 %:% (2 %:% (3 %:% lempty)) # need `(` because R's infix operator has left-associativity.
  l3 <- llist(1, 2, 3)
  l4 <- llist_lazy(1, 2, 3) # not evaluated 2, 3 at first
  l5 <- 1 %..% 3
  l6 <- as.llist(1:3)
```

``` r
  # Infinit seq of 1
  ones1 <- 1 %:% ones1
  ones2 <- lrepeat(1)
  ones3 <- lseq_gen(1, f = function(x) x)
```

``` r
  # Basic sequence (natural numbers)
  nat1 <- liota()
  nat2 <- lrange()
  nat3 <- larith(0, 1)
```

The 'l()' functions and constructers return a `lcons` object.
If you want to convert it into R's list, use `lforce()`.
``` r
# by defalt, 50 elements are limitted to prevent from looping infinitely.
lforce(ones)
lforce(ones, elem_max=100)
```

## Examples of manipulations
``` r
  # Take subset of lazystream
  l <- liota()
  ltake(l, 10)
  ldrop(l, 10)
  ltakeWhile(l, function(x) x < 10)
  ldropWhile(l, function(x) x < 10)
```

``` r
  # Map, Filter, Fold
  lmap(liota(), function(x) x ^ 2)
  liota() %>% lmap(function(x) x ^ 2)

  lfilter(liota(), function(x) x %% 2 == 0)
  liota() %>% lfilter(function(x) x %% 2 == 0)

  lfoldl(liota(10), function(x, y) x + y, 0)
  lfoldr(liota(10), function(x, y) x + y, 0)

  liota(10) %>% lfoldl1(function(x, y) x + y)
  liota(10) %>% lfoldr1(function(x, y) x + y)
  
  liota(10) %>% lfoldl1(function(x, y) paste0("(", x, "+", y, ")"))
  liota(10) %>% lfoldr1(function(x, y) paste0("(", x, "+", y, ")"))
```

``` r
## https://www.haskell.org/ see top example
  sieve <- function(y) {
    p <- lhead(y); xs <- ltail(y)
    p %:% sieve(lfilter(xs, function(x) x %% p != 0))
  }
  lprimes <- sieve(2%..%Inf)
  ltake(lprimes, 100)
```

## References
- http://srfi.schemers.org/srfi-1/srfi-1.html
- http://srfi.schemers.org/srfi-41/srfi-41.html
- http://srfi.schemers.org/srfi-45/srfi-45.html
- http://practical-scheme.net/gauche/man/gauche-refe_59.html
- http://practical-scheme.net/gauche/man/gauche-refe_88.html
- http://hackage.haskell.org/package/base-4.7.0.0/docs/Prelude.html
- http://hackage.haskell.org/package/base-4.7.0.0/docs/Data-List.html
