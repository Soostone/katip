# Katip Elasticsearch [![Build Status](https://travis-ci.org/Soostone/katip.svg?branch=master)](https://travis-ci.org/Soostone/katip)

katip-elasticsearch is a scribe for the Katip logging framework that
sends structured logs to ElasticSearch.

## Features

* Built in bounded buffering.

* Configurable pool of logging workers to help with high write
  volume.

* Optional field type annotation to avoid mistyping values.

* Optional automatic date sharding, so logs can be filed into monthly,
  weekly, daily, hourly, every minute indices. You can even specify
  your own index routing logic. This pattern can be seen in the ELK
  stack as a way of keeping indexes reasonably sized and easy to
  optimize, rotate, and manage.

* Customizable retry policy for temporary outages and errors.

* Automatic index and mapping setup.

