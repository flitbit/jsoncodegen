#jsoncodegen [![Build Status](https://travis-ci.org/flitbit/jsoncodegen.png)](http://travis-ci.org/flitbit/jsoncodegen)

Erlang module for transforming well-defined records to JSON.

## Features

* Generates `to_json` and `from_json` according to record definitions, either as part of a Makefile process, or as a pre-processor.

## Dependencies

* Round-trip ISO 8601 formatting of datetimes using [`etz`](https://github.com/flitbit/etz)