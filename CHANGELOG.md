# CHANGELOG

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
* Add posibility of adding validators for different schemas


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
