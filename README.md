[![Build Status](https://travis-ci.org/alavrik/piqi-erlang.png)](https://travis-ci.org/alavrik/piqi-erlang)


Piqi is a multi-format data serialization system for Erlang. It provides a
uniform interface for serializing Erlang data structures to JSON, XML and
Protocol Buffers formats.


A typical Piqi usage scenario involves the following steps:

**1. Include Piqi as a rebar depedency** -- add this entry to your
`rebar.config` file:

    {deps, [
        ...
        {piqi, "", {git, "git://github.com/alavrik/piqi-erlang.git", {branch, "master"}}},
        ...
    ]}.


**2. Describe data structures using the Piqi data definition language or
Protocol Buffers `.proto` files**

The [Piqi](http://piqi.org/doc/piqi/) data definition language can describe many
Erlang types, both primitive and user-defined. This includes integers, floats,
booleans, strings, binaries, lists, records and variants (i.e. `{tag, Value}`
tuples).

`.piqi` modules can be converted to and from Protocol Buffers `.proto` files:

    piqi to-proto X.piqi
    piqi of-proto X.proto


**3. Call the Piqi compiler to generate Erlang type definitions and
serialization code**


**4. Use generated serializes/deserializers in a user's program** -- the desired
serialization format can be specified at runtime. For
[examples](examples/addressbook/src/io_json_xml_pb.erl):

    % deserialize a data structure from Protocol Buffers
    AddressBook = addressbook_piqi:parse_address_book(Bytes, 'pb'),

    % serialize it as JSON
    Json = addressbook_piqi:gen_address_book(AddressBook, 'json'),

    % serialize it as pretty-printed JSON
    JsonPretty = addressbook_piqi:gen_address_book(AddressBook, 'json_pretty'),

    % serialize it as XML
    Xml = addressbook_piqi:gen_address_book(AddressBook, 'xml').


Examples
--------

See [examples/addressbook](examples/addressbook/) and other projects in the
[examples](examples/) directory.


Documentation
-------------

Piqi Erlang documentation is available at http://piqi.org/doc/erlang/

The master copy is located in the repository:
[doc/piqi-erlang.md](doc/piqi-erlang.md)


Bugs
----

Please report found problems using [GitHub
issues](http://github.com/alavrik/piqi-erlang/issues).


Mailing list
------------

http://groups.google.com/group/piqi


Advanced installation
---------------------

Sometimes, it may be useful or even necessary to use a custom version of the
`piqi` binary executable instead of the pre-built one that comes with
`piqi-erlang`.

For example, there may not be a pre-built `piqi` for your platform. Or
organization policies may prohibit installing third-party binaries.

In this case, you can build the `piqi` binary from [source
code](https://github.com/alavrik/piqi), put it somewhere in the local filesystem
and point to it using the `PIQI` environment variable _before_ running any
`rebar` commands and starting the `piqi` Erlang application. For example:

    export PIQI=/usr/local/bin/piqi


Contributing
------------

Your contributions are always welcome. Just open a pull request. Check [TODO
list](TODO) for ideas.

Some useful commands:

    make deps  # the same as "rebar get-deps"
    make       # the same as "rebar compile"

    make test
    make dialyzer
    make -C tests all test
    make -C tests clean


License
-------

[Apache License Version 2.0](LICENSE)

