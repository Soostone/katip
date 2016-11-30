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
