pure-stm
========

`pure-stm` is an implementation of Software Transactional Memory, written completely in Haskell.

Features
--------

* Disjoint Access Parallelism. If two threads access disjoint sets of variables, there will be absolutely no contention between them.
* Logarithmic time lookup in the cache. In the GHC implementation (I think) everything is represented as a list, so a read takes linear time in the length of a transaction. With this implementation, it is logarithmic.
* Pure Haskell. This has a number of advantages:
  * Correctness. I haven't actually proven this correct yet, but some things (like the correctness of retry/orElse) fall out incredible naturally.
  * Extensibility. Working with the internals of this library doesn't involve messing with C-- and the interfact between languages. For example, I think that implementing `unreadTVar` should be a relatively quick exercise.
  * Garbage Collection. Many implementations of STM struggle with the difficulty of working with the GC and not causing space leaks. Doing this library in Haskell prevented a number of space problems (though, admittedly, it caused a number as well).
  * Optimizations. The compiler could, theoretically, inline the library functions and fuse them with some of the user code, which is very hard to do across languages.

Missing Features
----------------

* Invariants. I don't actually plan to add these, as I don't like the current interface for them. The interface acts as if you are imposing some global properties, and nothing (in my opinion) should really be global.
* `unreadTVar`. This is a handy feature for high performance STM libraries and shouldn't be that hard to implement. Implementation:
  * Check that the current version fits what we know (that our cache is still valid).
  * Record the new maximum timestamp we can extend to.
  * Delete the TVar from the cache.
* Exception Safety. This really needs to get done.
* Exception Handling. This isn't as important, and probably could just be implemented with a monad transformer on top of the current implementation.
