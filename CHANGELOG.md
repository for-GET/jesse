# CHANGELOG

## Unreleased

* Add partial support for JSON Schema draft 2019-09. New dialect module
  `jesse_validator_draft2019_09`, dispatched from the schema's `$schema` URI
  (`https://json-schema.org/draft/2019-09/schema`, with or without a trailing
  `#`). Implemented keywords: `dependentRequired`, `dependentSchemas`,
  `if`/`then`/`else`, `minContains`/`maxContains`, `$defs`, local `$anchor`
  resolution, and `$ref` evaluated alongside sibling keywords. `format` is
  annotation-only (non-asserting), per the 2019-09 default.
  Not yet implemented — `unevaluatedProperties`, `unevaluatedItems` and
  `$recursiveRef` — raise a `keyword_not_supported` schema error instead of
  being silently ignored, so they can never false-accept invalid data.
  Validated against the official JSON-Schema-Test-Suite (833/1003 individual
  draft 2019-09 tests pass; the remainder are for the unsupported keywords,
  remote-schema fetching, and in-document `$id` scoping).

## 1.5.6

* Improving the error messages from jesse when using oneOf/anyOf
* Make it compatible with jsx 3.0

## 1.5.5

* Fix rebar3 and rebar3_hex versions incompatibility #88
* OTP-22.3 added to Travis

## 1.5.4

* Specify build matrix "dist" for old OTP releases #83
* Fix anyOf / OneOf returns unexpected error with allowed_errors=infinity #81
* Fixes for dialyzer #77
* Ensure correct dependencies in app.src
* OTP-22 added to Travis
* Avoid deprecated http_uri functions for OTP 21+ #86
* Migrate to rebar3 #87

## 1.5.3

* Drop support for OTP versions below OTP-18
* Work around unexported `http_uri:uri()` type #75
* Add validator draft3/draft4 schema unique check missed case #74
* Add missing error types to handle json parser exceptions #73

## 1.5.2

* Upgrade jsx dependency #67
* Stacktrace compatibility macro updated #70

## 1.5.1

* Correct schema on error #65
* OTP-21 compatibility #66

## 1.5.0 - 2018-01-14

### Breaking changes

* errors are now returned in the order they occur
* specifying any other properties in a schema with a `$ref` throws an error #47 #54

### Notable changes

* default schema loader now supports `file:`, `http:` and `https:` URI schemes.
  When a schema cannot be found in the internal storage, a schema hosted under
  one of the supported URI schemes will be fetched and stored.
  To maintain the old behavior (internal storage only) give the option
  `schema_loader_fun` the value `fun jesse_database:load/1`
* RFC 3339 validator for date and time formats
* a schema referenced by `$ref` can now follow a different JSON Schema draft
  than the parent
* allow an external validator to be given as option e.g. to verify runtime requirements #42
* schema id needs to be a fully qualified URI. jesse will build a canonical one otherwise
  based on the context - parent schema's id, loading path, etc.

[Full list of changes since 1.4.0](https://github.com/for-GET/jesse/compare/for-GET:1.4.0...1.5.0)


## 1.4.0

* Added jesse_error:to_json
* Spec fixes
* Test improvements


## 1.3.0

* Support for maps


## 1.2.0

* Standalone jesse executable (with Erlang/JSON output)
* Support for $ref
* Support for JSON Schema draft 4


## 1.1.0

* Big refactoring
* Start to respect $schema
* Add possibility of adding validators for different schemas


## 1.0.0

* Start using semantic versioning (http://semver.org/)
* Minor improvements


## 0.5.0

* Add path to errors (error format changed, so it's backward incompatible change)


## 0.4.0

* Change API
* Introduce 'state' in the validator
* Add possibility to collect errors
* Change errors format


## 0.3.0

* Add support for jsx format


## 0.2.0

* Big refactoring of jesse_schema_validator.erl
* Add additional API functions
* Add tests
* Add more documentation


## 0.1.0

* Initial release.
