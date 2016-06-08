0.2.1.0
==============

* Drop direct dependency on random, upgrade uuid to >= 1.3.12 for
  safer id generation. Previously, UUID was using randomIO, which uses
  the system clock as a seed. So if multiple nodes happened to start
  at the same time, they would produce conflicting UUID sequences.

0.2.0.0
==============

* Default index sharding policy to daily. Previously it was no
  sharding. The reasoning here is that no sharding creates very large
  indices which become very difficult to manage in
  production. Rotating data out on a time basis is very slow compared
  to deleting date-based indices.

  Upgrade note: if you were using the defaults before and switch to
  daily, rather than having the index name of `my-index`, you'll start
  seeing `my-index-2016-3-14`. The good news is that whatever you're
  using to use to search against your logs (such as kibana) will
  support index patterns, so just use the pattern of `my-index*` to
  get everything. Eventually if you have a retention period, you can
  manually delete the `my-index` index without disruption.


0.1.1.0
==============

* Set upper bounds for a few dependencies.

0.1.0.0
==============

* Initial release
