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
