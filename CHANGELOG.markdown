# 1.1.7
* Fixed an issue caused by changes in GHC 7.7's typechecker by using explicit `ScopedTypeVariables`.

# 1.1.6:
* Relaxed an unnecessary strictness annotation in the fast implementation

# 1.1.5
* Both implementations now work on Hugs; the fast implementation ascends
  from the ranks of completely unportable black magic to being merely
  /mostly/ unportable black magic.

# From 0.5 to 1.1:

* Much faster implementation available that is about 50 /times/ faster than
  0.9 and which runs purely on black magic. This version is now used by
  default. To turn it off install with the `slow` flag. If you encounter a
  problem with the implementation, please contact the author.
* Removed `ReifiedNum`, `reflectNum`, and `reifyIntegral`; `reify` and
  `reflect` are about 3 orders of magnitude faster than the special case
  combinators were.

# 0.5
* Generalized the type signatures in reflect to allow you to pass any type
  with kind `* -> *` wrapped around the desired type as the phantom type
  argument rather than just a `Proxy`.

# 0.4
* Converted from `Data.Tagged` to using `Data.Proxy` for reflection. This
  reduces the need for helper functions and scoped type variables in user
  code.
