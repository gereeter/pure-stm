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
* Perfomance-tuning operations. This consists of, so far, 3 functions:
  * `validateTVar`. By default, the implementation only checks to see if the transaction is invalid when it is about to commit. This function allows the user to selectively check validity and possibly abort early.
  * `unsafeIOToSTM`. This is a fairly standard one. It allows you to run arbitrary IO actions inside STM if you know what you are doing. This can be useful for things like random numbers.
  * `unsafeUnreadTVar`. Described in [UnreadTVar: Extending Haskell Software Transactional Memory for Performance (2007)](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.90.7784), this allows for efficient implementation of linked data structures by only requiring small sections of the data to be consistent.

Missing Features
----------------

* Invariants. I don't actually plan to add these, as I don't like the current interface for them. The interface acts as if you are imposing some global properties, and nothing (in my opinion) should really be global.
* Exception Safety. This really needs to get done.
* Exception Handling. This isn't as important, and probably could just be implemented with a monad transformer on top of the current implementation.
