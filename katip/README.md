# Katip [![Build Status](https://travis-ci.org/Soostone/katip.svg?branch=master)](https://travis-ci.org/Soostone/katip)

Katip is a structured logging framework for Haskell.

![Katip](./katip.jpg)

Kâtip (pronounced kah-tip) is the Turkish word for scribe.

## Features

* *Structured:* Logs are structured, meaning they can be individually
  tagged with key value data (JSON Objects). This helps you add
  critical details to log messages before you need them so that when
  you do, they are available. Katip exposes a typeclass for log
  payloads so that you can use rich, domain-specific Haskell types to
  add context that will be automatically merged in with existing log
  context.

* *Easy to Use:* Katip was designed to be easily integrated into
  existing systems. By using a typeclass for logging facilities,
  individual subsystems and even libraries can easily add their own
  namespacing and context without having any knowledge of their
  logging environment.

* *Extensible:* Can be easily extended (even at runtime) to output to
  multiple backends at once (known as scribes). See
  `katip-elasticsearch` as an example.

* *Debug-Friendly:* Critical details for monitoring production systems
  such as host, PID, thread id, module and line location are
  automatically captured. User-specified attributes such as
  environment (e.g. Production, Test, Dev) and system name are also
  captured.

* *Configurable:* Can be adjusted on a per-scribe basis both with
  verbosity and severity.

    * *Verbosity* dictates how much of the log structure should
      actually get logged. In code you can capture highly detailed
      metadata and decide how much of that gets emitted to each backend.

    * *Severity* AKA "log level" is specified with each message and
      individual scribes can decide whether or not to record that
      severity. It is even possible to at runtime swap out and replace
      loggers, allowing for swapping in verbose debug logging at runtime
      if you want.

* *Battle-Tested:* Katip has been integrated into several production
  systems since 2015 and has logged hundreds of millions of messages
  to files and ElasticSearch.


## Examples
Be sure to look in the [examples](./examples) directory for some examples of how
to integrate Katip into your own stack.


## Contributors

* [Ozgun Ataman](https://github.com/ozataman)
* [Michael Xavier](https://github.com/MichaelXavier)
* [Doug Beardsley](https://github.com/mightybyte)
