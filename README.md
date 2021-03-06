#jsoncodegen [![Build Status](https://travis-ci.org/flitbit/jsoncodegen.png)](http://travis-ci.org/flitbit/jsoncodegen)

Erlang module for transforming well-defined records to JSON.

## Features

* Generates `to_json` and `from_json` according to record definitions, either as part of a Makefile process, or as a pre-processor.

## Dependencies

* Code generation made easier using [`merl`](https://github.com/richcarl/merl)
* Round-trip ISO 8601 formatting of datetimes using [`etz`](https://github.com/flitbit/etz)

## Change Log

Not yet released - first push on 2014-04-18. Commentary, issues, suggestions, pull reqeusts welcome.