**This repository is the actively-maintained follow-up of https://github.com/klarna/jesse. Please update your references.**

# jesse [![Build Status][2]][1]

jesse (JSON Schema Erlang) is an implementation of a JSON Schema validator
for Erlang.

jesse implements the following specifications:

* [Draft 03](http://tools.ietf.org/html/draft-zyp-json-schema-03)
* [Draft 04](http://tools.ietf.org/html/draft-zyp-json-schema-04)

## Erlang API Docs

Automatically generated docs are available https://dev.erldocs.com/github.com/for-get/jesse/ .

Please keep in mind that the public API is the `jesse.erl` module alone.

## Quick start - CLI

You can fire up `jesse` from the CLI, with
```bash
bin/jesse [path_to_json_schema] path_to_json_schema -- path_to_json_instance [path_to_json_instance]
```

You can also output the result in JSON format, with `--json`, and beautify it e.g. with python
```bash
bin/jesse [path_to_json_schema] path_to_json_schema --json -- path_to_json_instance [path_to_json_instance] | python -m json.tool
```

You can pass multiple JSON schemas which should be loaded into jesse in-memory storage, but JSON instances will be validated against the last JSON schema passed.

## Quick start - Erlang

There are two ways of using jesse:

* to use jesse internal in-memory storage to keep all your schema definitions
  In this case jesse will look up a schema definition in its own storage,
  and then validate given a JSON instance.
* it is also possible to provide jesse with schema definitions when jesse is called.

## Examples

    NOTE: jesse doesn't have any parsing functionality. It currently works with four
          formats: mochijson2, jiffy, jsx and Erlang 17+ maps, so JSON needs to be
          parsed in advance, or you can specify a callback which jesse will use to
          parse JSON.

          In examples below and in jesse test suite jiffy parser is used.

* Use jesse's internal in-memory storage:

(parse JSON in advance)

```erlang
1> Schema = jiffy:decode(<<"{\"items\": {\"type\": \"integer\"}}">>).
{[{<<"items">>,{[{<<"type">>,<<"integer">>}]}}]}
2> jesse:add_schema(some_key, Schema).
ok
3> Json1 = jiffy:decode(<<"[1, 2, 3]">>).
[1,2,3]
4> jesse:validate(some_key, Json1).
{ok,[1,2,3]}
5> Json2 = jiffy:decode(<<"[1, \"x\"]">>).
[1,<<"x">>]
6> jesse:validate(some_key, Json2).
{error,[{data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,<<"x">>,
                      [1]}]}
```

The `[1]` in the error is the path in the original value to `<<"x">>` where the
validation failed. See *Validation errors* below for the full error format.

(using a callback)

```erlang
1> jesse:add_schema(some_key,
1>                  <<"{\"uniqueItems\": true}">>,
1>                  [{parser_fun, fun jiffy:decode/1}]).
ok
2> jesse:validate(some_key,
2>                <<"[1, 2]">>,
2>                [{parser_fun, fun jiffy:decode/1}]).
{ok,[1, 2]}
3> jesse:validate(some_key,
3>                <<"[{\"foo\": \"bar\"}, {\"foo\": \"bar\"}] ">>,
3>                [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"uniqueItems">>,true}]},
                      {not_unique,{[{<<"foo">>,<<"bar">>}]}},
                      [{[{<<"foo">>,<<"bar">>}]},{[{<<"foo">>,<<"bar">>}]}],
                      []}]}
```

* Call jesse with schema definition in place (do not use internal storage)

(parse JSON in advance)

```erlang
1> Schema = jiffy:decode(<<"{\"pattern\": \"^a*$\"}">>).
{[{<<"pattern">>,<<"^a*$">>}]}
2> Json1 = jiffy:decode(<<"\"aaa\"">>).
<<"aaa">>
3> jesse:validate_with_schema(Schema, Json1).
{ok,<<"aaa">>}
4> Json2 = jiffy:decode(<<"\"abc\"">>).
<<"abc">>
5> jesse:validate_with_schema(Schema, Json2).
{error,[{data_invalid,{[{<<"pattern">>,<<"^a*$">>}]},
                      no_match,
                      <<"abc">>,[]}]}
```

(using a callback)

```erlang
1> Schema = <<"{\"patternProperties\": {\"f.*o\": {\"type\": \"integer\"}}}">>.
<<"{\"patternProperties\": {\"f.*o\": {\"type\": \"integer\"}}}">>
2> jesse:validate_with_schema(Schema,
2>                            <<"{\"foo\": 1, \"foooooo\" : 2}">>,
2>                            [{parser_fun, fun jiffy:decode/1}]).
{ok,{[{<<"foo">>,1},{<<"foooooo">>,2}]}}
3> jesse:validate_with_schema(Schema,
3>                            <<"{\"foo\": \"bar\", \"fooooo\": 2}">>,
3>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,<<"bar">>,
                      [<<"foo">>]}]}
```

* Validate an instanse against a particular definition from schema definitions

```erlang
1> Schema = <<"{\"definitions\": {\"Foo\": {\"properties\": {\"foo\": {\"type\": \"integer\"}}}, \"Bar\": {\"properties\": {\"bar\": {\"type\": \"boolean\"}}}}}">>.
<<"{\"definitions\": {\"Foo\": {\"properties\": {\"foo\": {\"type\": \"integer\"}}}, \"Bar\": {\"properties\": {\"bar\": {\"type\": \"boolea"...>>
2> jesse:validate_definition("Foo",
2>                            Schema,
2>                            <<"{\"foo\": 1}">>,
2>                            [{parser_fun, fun jiffy:decode/1}]).
{ok,[{<<"foo">>,1}]}
3> jesse:validate_definition("Bar",
3>                            Schema,
3>                            <<"{\"bar\": 2}">>,
3>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,[{<<"type">>,<<"boolean">>}],
                      wrong_type,2,
                      [<<"bar">>]}]}
4> jesse:validate_definition("FooBar",
4>                            Schema,
4>                            <<"{\"bar\": 2}">>,
4>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{schema_invalid,[{<<"definitions">>,
                          [{<<"Foo">>,
                            [{<<"properties">>,
                              [{<<"foo">>,[{<<"type">>,<<"integer">>}]}]}]},
                           {<<"Bar">>,
                            [{<<"properties">>,
                              [{<<"bar">>,[{<<"type">>,<<"boolean">>}]}]}]}]}],
                        {schema_not_found,"#/definitions/FooBar"}}]}
```


* Since 0.4.0 it's possible to instruct jesse to collect errors, and not stop
  immediately when it finds an error in the given JSON instance:

```erlang
1> Schema = <<"{\"properties\": {\"a\": {\"type\": \"integer\"}, \"b\": {\"type\": \"string\"}, \"c\": {\"type\": \"boolean\"}}}">>.
<<"{\"properties\": {\"a\": {\"type\": \"integer\"}, \"b\": {\"type\": \"string\"}, \"c\": {\"type\": \"boolean\"}}}">>
2> jesse:validate_with_schema(Schema,
2>                            <<"{\"a\": 1, \"b\": \"b\", \"c\": true}">>,
2>                            [{parser_fun, fun jiffy:decode/1}]).
{ok,{[{<<"a">>,1},{<<"b">>,<<"b">>},{<<"c">>,true}]}}
```

now let's change the value of the field "b" to an integer

```erlang
3> jesse:validate_with_schema(Schema,
3>                            <<"{\"a\": 1, \"b\": 2, \"c\": true}">>,
3>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]}]}
```

works as expected, but let's change the value of the field "c" as well

```erlang
4> jesse:validate_with_schema(Schema,
4>                            <<"{\"a\": 1, \"b\": 2, \"c\": 3}">>,
4>                            [{parser_fun, fun jiffy:decode/1}]).
{error,[{data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]}]}
```

still works as expected, jesse stops validating as soon as finds an error.

Let's use the `allowed_errors` option, and set it to 1

```erlang
5> jesse:validate_with_schema(Schema,
5>                            <<"{\"a\": 1, \"b\": 2, \"c\": 3}">>,
5>                            [{parser_fun, fun jiffy:decode/1},
5>                             {allowed_errors, 1}]).
{error,[{data_invalid,{[{<<"type">>,<<"boolean">>}]},
                      wrong_type,3,
                      [<<"c">>]},
        {data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]}]}
```

now we got a list of two errors.

Let's now change the value of the field "a" to a boolean

```erlang
6> jesse:validate_with_schema(Schema,
6>                            <<"{\"a\": true, \"b\": 2, \"c\": 3}">>,
6>                            [{parser_fun, fun jiffy:decode/1},
6>                             {allowed_errors, 1}]).
{error,[{data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]},
        {data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,true,
                      [<<"a">>]}]}
```

we stil got only two errors.

Let's try using 'infinity' as the argument for the `allowed_errors` option

```erlang
7> jesse:validate_with_schema(Schema,
7>                            <<"{\"a\": true, \"b\": 2, \"c\": 3}">>,
7>                            [{parser_fun, fun jiffy:decode/1},
7>                             {allowed_errors, infinity}]).
{error,[{data_invalid,{[{<<"type">>,<<"boolean">>}]},
                      wrong_type,3,
                      [<<"c">>]},
        {data_invalid,{[{<<"type">>,<<"string">>}]},
                      wrong_type,2,
                      [<<"b">>]},
        {data_invalid,{[{<<"type">>,<<"integer">>}]},
                      wrong_type,true,
                      [<<"a">>]}]}
```

Maps example

```erlang
8> jesse:validate_with_schema(Schema,
8>                            <<"{\"a\": 1, \"b\": 2, \"c\": true}">>,
8>                            [{parser_fun, fun(Bin) -> jiffy:decode(Bin, [return_maps]) end}]).
{error,[{data_invalid,#{<<"type">> => <<"string">>},
                      wrong_type,2,
                      [<<"b">>]}]}
9> jesse:validate_with_schema(Schema,
9>                            <<"{\"a\": 1, \"b\": \"val\", \"c\": true}">>,
9>                            [{parser_fun, fun(Bin) -> jiffy:decode(Bin, [return_maps]) end}]).
{ok, #{<<"a">> => 1, <<"b">> => <<"val">>, <<"c">> => true}}
```

## JSON Schema versions

Currently there are two popular drafts of JSON Schema: draft3 and draft4. jesse
supports both. To decide which validator to use jesse tries to read $schema
property from the given schema, and checks if it's a supported one, otherwise it
will return an error. If $schema property isn't provided in the given schema,
jesse will use the default validator (currently the validator for draft3).

To specify which validator to use by default (if there's no $schema property in
the given schema), one should use 'default_schema_ver' option when call
`jesse:validate/3` or `jesse:validate_with_schema/3`, the value should be
a binary consisting a schema path,
 i.e. <<"http://json-schema.org/draft-03/schema#">>.

It is also possible to specify a validator module to use via `validator` option.
This option supersedes the mechanism with the $schema property described above.
Custom validator module can be specified as well. Such module should implement
`jesse_schema_validator` behaviour.

## Validation errors

The validation functions `jesse:validate/2` and `jesse:validate_with_schema/2,3`
return `{ok, Value}` on success and `{error, ListOfErrors}` on failure. An error
is either `data_invalid` or `schema_invalid`.

A `data_invalid` error is a tuple on the form `{data_invalid, Schema, ErrorType,
Value, Path}` where

* Schema is the part of the schema where validation failed
* ErrorType is the type of error, usually an atom such as `wrong_type`,
  `not_in_range` or `no_match`
* Value is The part of the value where failed validation agains Schema
* Path is a path to where validation failed within the original value. The path
  is a list of property names and zero-based array indices referencing the
  properties and array items within a JSON document; e.g. in the JSON document
  `{"foo": [42, 43, 44]}`, the path `[<<"foo">>, 0]` refers to the value 42. An
  empty list refers to the whole JSON document.

A `schema_invalid` error is a tuple on the form `{schema_invalid, Schema,
ErrorType}` where
* Schema is the part of the schema which is invalid
* ErrorType is an atom such as `missing_id_field` or a tuple such as
  `{wrong_type_dependency, Dependency}`.

## Caveats

* pattern and patternProperty attributes:

  jesse uses standard erlang module `re` for regexp matching, therefore there could be
  some incompatible regular expressions in schemas you define.

  From erlang docs: "re's matching algorithms are currently based on the PCRE library,
  but not all of the PCRE library is interfaced"

  But most of common cases should work fine.

* internal references (id attribute) are NOT supported

  http://json-schema.org/latest/json-schema-core.html#rfc.section.8.2.1

## Contributing

If you see something missing or incorrect, a pull request is most welcome!

## License

[Apache 2.0](LICENSE)


  [1]: https://travis-ci.org/for-GET/jesse
  [2]: https://travis-ci.org/for-GET/jesse.png
