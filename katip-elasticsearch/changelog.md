0.5.1.0
=======
* Loosen deps

0.5.0.0
=======
* Update template for ESv1 to not analyze certain fields like host and
  namespace. These are not fields that benefit from tokenization. If
  you're on ESv1, after running `mkEsScribe`, the logs template will
  be updated to stop analyzing those fields. That means the next
  index that is cut (e.g. tomorrow's logs if you're using daily index
  sharding) will no longer analyze some fields.

0.4.2.0
=======
* Allow newer bloodhound, aeson, async.

0.4.1.0
=======
* Fix bug where index was created in `mkEsScribe` when it would not be used due to index sharding.
* Update some index settings if index already exists and sharding is not used.
* For ES V5 and higher, stop using the deprecated (and in 6.x, removed) `string` type for index templates, instead using `text` and `keyword` as appropriate. This makes `katip-elasticsearch` compatible with ES 6.x.

0.4.0.4
=======
* Allow http-types 0.12

0.4.0.3
=======
* Fix benchmark and test builds for stackage nightly

0.4.0.2
=======
* Add repository/homepage info to cabal file

0.4.0.1
=======
* Bump dependencies to allow GHC 8.2.1

0.4.0.0
=======
* Update to bloodhound >= 0.13.0.0. This version adds support for both ElasticSearch versions 1 and 5. Previously, we implicitly supported one and maybe would work on 5. The types in `EsScribeCfg` had to change to be able to specify which version was being targeted.
* Improved documentation.

0.3.1.0
=======
* Widen dependency on katip

0.3.0.2
=======
* Loosen deps on aeson to allow 1.1.0.0

0.3.0.1
=======
* Loosen deps on bloodhound, aeson, and http-client.

0.3.0.0
==============
* Added zero padding to date-based indices. This shouldn't negatively
  impact most users but to be safe, this was put behind a breaking
  version number. Previously, you may see indices
  `log-index-prefix-2016-1-2`. That index will now be
  `log-index-prefix-2016-01-02`, so at worst when you deploy, the day
  of the change will have 2 indices: one zero-padded and one not. If
  you are using custom index sharding, you will not be affected by
  this. The reasoning here is that most existing elasticsearch tools
  such as `curator` expects date indices to be zero padded. By
  switching to the standard, users can easily get log rotation and
  other features from `curator` rather than having to roll their own
  or add them to Katip.

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
