0.6.3.0
=======
* Add `ToJSON`/`FromJSON` instances for Verbosity. Credit to [Aleksey Khudyakov](https://github.com/Shimuuar).

0.6.2.1
=======
* Add `mkHandleScribeWithFormatter`. This allows control over the format of log items going to a handle. Credit to [Tristan Bull](https://github.com/tmbull) for the implementation.
* Documentation fixes. Credit to [Domen Kožar](https://github.com/domenkozar) for the implementation.
* Add `jsonFormat` handle formatter.
* Deprecate `formatItem` which is now replaced by `bracketFormat`.

0.6.1.0
=======
* Loosen deps
* Support latest STM

0.6.0.0
=======
* Drop ListT instance due to deprecations.
* Loosen dependencies.

0.5.5.1
=======
* Decrease default timer resolution from 1s to 1ms.

0.5.5.0
=======
* Export `runNoLoggingT` and `NoLoggingT` constructor.
* Delegate MonadReader instance for `NoLoggingT`.

0.5.4.0
=======
* Loosen bounds on resourcet and template-haskell.
* Add convenience function askLoggerIO.

0.5.3.0
=======
* Add MonadUnliftIO instances.
* Add NoLoggingT

0.5.2.0
=======
* Allow newer versions of either by conditionally adding instances for the removed EitherT interface.

0.5.1.0
=======
* Add mkFileScribe, a specialization of mkHandleScribe for files that manages the handle automatically.

0.5.0.4
=======
* Loosen Win32 upper bound to run with GHC 8.2 on Windows.

0.5.0.3
=======
* Add worked example of Katip/KatipContext to the haddocks.

0.5.0.2
=======
* Export Katip.Compat for Windows users.

0.5.0.1
=======
* Fix numeric formatting in Handle scribe.
* Bump deps for GHC 8.2.1

0.5.0.0
=======
* Improved documentation.
* Add built-in buffering to scribes.
  Scribes now allocate a bounded queue (with configurable size). Rather than writes being synchronous to all scribes, they simply attempt to write into the bounded queue of each scribe. If any of the scribes is too far behind and the queue is full, the write is dropped. This also means that closing scribes is now an IO operation that happens synchrounsly.
* Added local-like functions to Katip and KatipContext typeclasses. This allows us to generalize `katipNoLogging`, `katipAddNamespace`, and `katipAddContext` to be available to anything with a `Katip` or `KatipContext` instance rather than having to reimplement these functions all the time.

0.4.1.0
=======
* Add Katip instances for Strict StateT, WriterT, RWST.
* Add Katip instances for Lazy RWST.

0.4.0.0
=======
* Drop unsafe _ioLogEnv for safe ioLogEnv

0.3.1.5
=======
* Add Semigroup instance for LogStr.

0.3.1.4
=======
* Loosen deps on aeson to allow 1.1.0.0

0.3.1.3
=======
* Fix build on windows

0.3.1.2
=======
* Add some missing test files

0.3.1.1
=======
* Fix some example code that wasn't building
* Make FromJSON instance for Severity case insensitive.

0.3.1.0
=======
* Add support for aeson 1.0.x
* Add Katip.Format.Time module and use much more efficient time formatting code in the Handle scribe.

0.3.0.0
=======
* Switch from `regex-tdfa-rc` to `regex-tdfa`.
* Add `katipNoLogging` combinator.
* Add `Semigroup` instances.
* Drop `ToJSON` superclass requirement fro `ToObject`. Instead,
  `ToObject` will provide a default instance for types with an
  instance for `ToJSON`. This gets us to the same place as before
  without having to add a broader instance for something that's only
  going to show up in logs as an Object.
* Add a simple MVar lock for file handle scribes to avoid interleaved
  log lines from concurrent inputs.

0.2.0.0
=======

* Add GHC implicit callstack support, add logLoc.
* Drop lens in favor of type-compatible, lighter microlens.
* Renamed `logEnvNs` to clearer `logEnvApp`
* Added `katipAddNamespace` and `katipAddContext`
* Fixed nested objects not rendering in Handle scribe.
* LogContexts Monoid instance is now right-biased rather than left
  biased. This better fits the use case. For instance `ctx1 <> ctx2`
  will prefer keys in `ctx2` if there are conflicts. This makes the
  most sense because functions like `katipAddContext` will `mappend`
  on the right side.
* LogContext internally uses a `Seq` instead of a list for better
  complexity on context add.
* Improved documentation.

0.1.1.0
==============

* Set upper bounds for a few dependencies.
* Add ExceptT instance for Katip typeclass

0.1.0.0
==============

* Initial release
