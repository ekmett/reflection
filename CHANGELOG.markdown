# 1.5.3
* Added `reifyNat` and `reifySymbol` for GHC 7.8+, capable of reflecting into the `KnownNat` and `KnownSymbol` classes respectively for use with other APIs.
* Back-ported `reifyTypeable` from `lens`. This enables us to perform a (less efficient) form of `Typeable` reflection.

# 1.5.2
* Renamed the flag for disabling the use of `template-haskell`, to `-f-template-haskell` for consistency with my other packages.

# 1.5.1.2
* Builds warning-free on GHC 7.10.
* Added a dynamic FromJSON example.

# 1.5.1.1
* Updated the link to the paper.
* More examples.

# 1.5.1
* We no longer export Show (Q a) for GHC >= 7.4. This was causing random hangs when users tried to somehow run declaration splices from the REPL.
* We no longer depend on tagged for GHC >= 7.8, since `Proxy` is now in `base`.

# 1.5
* Added a flag to disable `template-haskell` support for GHC stage1 platforms.
* Added instances of `Reifies` for `GHC.TypeLits`

# 1.4
* Changed the behavior of the $(1) template haskell splices for Exp to use a Proxy rather than value-level numbers. This is more consistent with the role of this libraary and the other could always be generated via sa splice anyways.

# 1.3.1
* Added a workaround for changes in the behavior of the internal 'Magic' datatype under the current GHC PolyKinds implementation.

# 1.3
* Merged some functionality from Data.Analytics.Reflection. Notably the ability to use type nats to reflect numbers, and to splice numbers directly. This reduces orphan instances somewhat.

# 1.2
* Added `Given` and give.

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
