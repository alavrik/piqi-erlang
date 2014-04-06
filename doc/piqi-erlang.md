Overview
--------

Piqi includes a data serialization system for Erlang. It can be used for
serializing Erlang values in 4 different formats: Google Protocol Buffers,
[JSON](/doc/encodings/#json), [XML](/doc/encodings/#xml) and [Piq](/doc/piq/).

A typical Piqi usage scenario involves the following steps:

**1. Include Piqi as a rebar depedency**
: add this entry to your `rebar.config` file:

    {deps, [
        ...
        {piqi, "", {git, "git://github.com/alavrik/piqi-erlang.git", {branch, "master"}}},
        ...
    ]}.

**2. Describe data structures using the Piqi data definition language**
:   The [Piqi](/doc/piqi/) data definition language can describe many Erlang
    types, both primitive and user-defined. This includes integers, floats,
    booleans, strings, binaries, lists, records and variants (i.e.
    `{tag, Value}` tuples).

    In addition to types supported by default, Piqi has a mechanism for adding
    support for arbitrary Erlang types. It can be used, for example, to add
    support for Erlang's bigints or `any()`.

    Refer to the "Piqi to Erlang mapping" section below for details.

**3. Call the Piqi compiler to generate Erlang type definitions and serialization code**
:   See the next section for detailed description.

**4. Use generated serializes/deserializers in a user's program**
:   the desired serialization format can be specified at runtime.

The [Examples](#examples) section contains links to several sample Erlang
projects that use Piqi for data serialization and demonstrate steps 2--4.


Piqi compiler and generated Erlang code
---------------------------------------

Piqi compiler for Erlang generates Erlang header files and Erlang code for
Protocol Buffers, JSON, XML and [Piq](/doc/piq/) serialization.

`piqic-erlang` command takes a Piqi module `<dir path>/<file>.piqi` and produces
two files in the current directory: `<erlang-module>.erl` and
`<erlang-module>.hrl`.

For each specified, imported or included module `<m>.piqi`, the compiler tries
to load and automatically include `<m>.erlang.piqi`. This mechanism is called
\_Extension Modules\_. It is described in detail in the Piqi language
[section](/doc/piqi#extensionmodules).

By default, `<erlang-module>` is set to `<file>_piqi`. It can also be specified
explicitly (see below). Output directory may be overridden by using the `-C`
command-line option.

Generated `<erlang-module>.hrl` file contains Erlang
[type](http://www.erlang.org/doc/reference_manual/typespec.html) and record
definitions.

Generated `<erlang-module>.erl` file includes `<erlang-module>.hrl` and contains
functions for serializing and deserializing Erlang values.

Generated `<erlang-module>.hrl` file contains Erlang type definitions and
functions for serializing and deserializing Erlang values.

For each defined data type `<typename>`, `piqic-erlang` will produce several
functions:

-   `gen_<typename>` -- for serializing a value of this type to Protocol Buffers

-   `gen_<typename>/2` -- for serializing a value of this type to Protobuf,
    JSON, XML and Piq

-   `gen_<typename>/3` -- same as `gen_<typename>/2` but with extra options

-   `parse_<typename>` -- for deserializing from Protocol Buffers

-   `parse_<typename>/2` -- for deserializing from Protobuf, JSON, XML and Piq

-   `parse_<typename>/3` -- same as `parse_<typename>/2` but with extra options

-   `default_<typename>` -- returns the minimal serializable instance of this
    type

The `parse_<typename>/2` and `gen_<typename>/2` functions from this module take
an additional parameter specifying which serialization format to use:

    -type input_format() :: bp | json | xml | piq.
    -type output_format() :: input_format() | 'json_pretty' | 'xml_pretty'.

There are two variants of parse and gen functions. The simple variant accept two
arguments as described above. `parse_<typename>/3` and `gen_<typename>/3` accept
an additional third argument representing a list of serialization options:

    % Serialization options to be passed as an argument to * gen_<typename>/3 and
    % parse_<typename>/3 functions
    %
    % pretty_print
    %
    %      Pretty-print generated JSON and XML output (default = true)
    %
    % json_omit_missing_fields
    %
    %      Omit missing optional and empty repeated fields from JSON
    %      output instead of representing them as {"field_name": null} and
    %      {"field_name", []} JSON fields (default = true)
    %
    % use_strict_parsing
    %
    %      Treat unknown and duplicate fields as errors when parsing JSON,
    %      XML and Piq formats (default = false)
    %
    % piq_frameless_output
    %
    %      Print a frame (i.e. :<typename> []) around a single output Piq object
    %      (default=false)
    %
    % piq_frameless_input
    %
    %      Expect a frame around a single input Piq object (default=false)
    %
    % piq_relaxed_parsing
    %
    %      Parse Piq format using "relaxed" mode (default=false);
    %
    %      For instance, when set to `true`, single-word string literals don't have
    %      to be quoted
    %
    -type piqi_convert_option() ::
           'pretty_print'
        | {'pretty_print', boolean()}
        |  'json_omit_missing_fields'
        | {'json_omit_missing_fields', boolean()}
        |  'use_strict_parsing'
        | {'use_strict_parsing', boolean()}
        |  'piq_frameless_output'
        | {'piq_frameless_output', boolean()}
        |  'piq_frameless_input'
        | {'piq_frameless_input', boolean()}
        |  'piq_relaxed_parsing'
        | {'piq_relaxed_parsing', boolean()}.

### Command-line parameters

`piqic-erlang` accepts the following command-line parameters.

-   `-C <dir>` -- specify output directory for the generated `.erl` and `.hrl`
    files.

-   `-I <dir>` -- add directory to the list of imported .piqi search paths

--  `-e <name>` -- try including extension <name> for all loaded modules (can be
    used several times)

-   `--normalize-names true|false` -- convert "CamelCase"-style identifiers from
    the original type spec into "camel-case" Erlang names. When the argument is
    `false`, the original identifiers will be lowercased without performing any
    additional transformations, e.g. "CamelCase" turns into "camelCase". The
    default value is `true`.

-   `--gen-preserve-unknown-fields` -- generate code that preserves unknown
    Protobuf fields when they are serialized back. When enabled, unknown
    (unrecognized) Protobuf fields are captured during de-serialization in a
    special 'piqi_unknown_pb' field and automatically written back when the
    record is serialized to Protobuf.

-   `--trace` -- turn on tracing (verbose output)

-   `--no-warnings` -- don't print warnings

-   `-h, --help`  -- print command-line options help


Piqi to Erlang mapping
----------------------

The following sections describe how different Piqi constructs such as modules
and types are mapped to Erlang.

### Modules

The name of Erlang module is derived from Piqi module name, unless overridden by
`erlang-module` top-level field.

If Piqi module's name is "example.com/foo/bar", then "bar" (the last part of
Piqi module name) will be used as Erlang module name. It is possible to override
such default name assignment by specifying `.erlang-module "<other-name>"` in
the Piqi module.

All type and record names in generated `.hrl` file are prefixed with Erlang
module's name to avoid name conflicts between types defined in different
modules. For example, if a Piqi module named `mod` defines type `foo`, the
resulting `.hrl` file will contain this type defined as `mod_foo`.

Sometimes prefixing type names with long module names can produce really long
identifiers, which may be inconvenient, -- especially for record names. Piqi
provides a mechanism for shortening such prefixes.

Instead of using module names as prefixes, it is possible to specify custom
prefix using top-level `erlang-type-prefix` property. For example, if we have a
`some-long-modname` module we can defined a shorted type prefix:

    .erlang-type-prefix "m_"

That will result in shorter Erlang names for types defined in that module, as
they will be prefixed by "m\_" instead "some\_long\_modname\_".

It is also possible to specify an empty prefix which may be useful for short
standalone programs.

#### Includes

There is no direct representation of Piqi includes in Erlang. Piqic takes all
"include" directives of a Piqi module, resolves them internally and produces a
compound Piqi module which is then mapped to the resulting Erlang module.

#### Imports

Piqi "import" directives are mapped to Erlang includes in the following way.

For example, we have two Piqi modules: `a.piqi` and `b.piqi`, and Piqi module
`b` imports module `a` using Piqi "import" directive:

    .import [ .module b ]

After running `piqic-erlang` on each of these modules, we will get 4 files:
`a_piqi.hrl`, `a_piqi.erl`, `b_piqi.hrl`, `b_piqi.erl`, where `b_piqi.hrl`
includes `a_piqi.hrl` and `b_piqi.erl` uses functions from `a_piqi.erl` to
serialize and deserialize values of types that were defined in module `a`.

Local Piqi import names defined using `name` attribute of `import` directive are
ignored as Erlang doesn't have correspondent constructs.

### Primitive types

The table below represents correspondence between Piqi primitive types and
Erlang types.

(Mapping between Piqi and Protocol Buffers primitive type is documented
[here](/doc/protobuf/#primitivetypes)).

  Piqi type(s)                                                                    Erlang type                Protobuf type(s)
  ---------------------------------------------------------------------------     -------------------------- --------------------------------------------------
  bool                                                                            boolean()                  bool
  string                                                                          string() | binary()        string
  binary                                                                          binary()                   bytes
  int, int32, int64, int32-fixed, int64-fixed, protobuf-int32, protobuf-int64     integer()                  sint32, sing64, sfixed32, sfixed64, int32, int64
  uint, uint32, uint64, uint32-fixed, uint64-fixed                                non\_negative\_integer()   uint32, uing64, fixed32, fixed64
  float, float64, float32                                                         number()                   double, float

`string` and `float` types are treated slightly differently during serialization
and deserialization. For instance, when serializing `float` value, it can be
passed as Erlang `number()`, that is can be either `float()` or `integer()`.
However, when deserializing `float` values, they are always returned as Erlang
`float()`.

Similarly, when serializing `string` value, it can be passed either as Erlang
`string()` (i.e. as a list of integers representing Unicode codepoints) or as
Erlang `binary()` representing a valid utf8-encoded string.

When deserializing strings, they will be returned as binaries by default. This
behavior can be changed to return lists by setting the following top-level
property in the Piqi module:

    .erlang-string-type.list

Note that the above property affects string deserialization behavior for all
data types defined in the module along with their content (i.e. fields and
options).

If there is a need to add serialization support for other Erlang types, such as
`atom()`, `reference()` or "big" integers, refer to [Custom Erlang
types](#customerlangtypes) section which describes a method for mapping custom
Erlang types to Piqi types.

### User-defined types

-   Type names

    Each user-defined type is identified by its name. Piqi type names are
    converted to Erlang type name using the following rule:

    By default, Piqi identifiers are normalized and all hyphen characters are
    replaced by underscores. Normalization means converting "CamelCase" to
    "camel-case".

    If `--normalize false` command-line option is specified, then instead of
    full normalization, the first letter of the type name is uncapitalized.

    Sometimes it is necessary to override this rule and specify a custom Erlang
    name for a type. For example, Piqi type name can conflict with one of Erlang
    keywords. In such case, custom Erlang name can be specified using
    `.erlang-name "<erlang name>"` field next to the original `.name <name>`
    entry. (This feature also works for field names, option names and import
    names.)

    For those Piqi fields or options which do not specify names, Erlang name is
    derived from the name of the Piqi type for that field.

-   Records are mapped to Erlang records.

    For example, Piqi record

        .record [
            .name r
            .field [ .name a .type int ]
            .field [ .name b .type binary .repeated ]
        ]

    will be mapped to the following Erlang record:

        -record(r, { a :: integer(), b :: [ binary() ] }).

    **required** Piqi fields are mapped directly to Erlang record fields.

    **optional** Piqi fields of type `<t>` are mapped to fields with type `<t>`.
    If the field value for an optional field is missing (i.e. undefined), the
    value of such field is set to Erlang atom `undefined`.

    **optional** Piqi fields without type (i.e. "flags") are mapped to
    `boolean()` Erlang fields. The value of the field will be set to `true` if
    flag is present in the record.

    **repeated** Piqi fields of type `<t>` are mapped to Erlang fields with type
    `[ <t> ]`.

    In addition to Erlang record definitions, Erlang type aliases will be
    generated for each record type. For example, for the above record
    definition, `.hrl` file will also include the following Erlang type alias:

        -type(r :: #r{}).

    These type alases are used instead of the original record types in other
    type definitions. Overall, this mechanism makes it possible to define
    recursive Erlang records.

-   Enums and Variants are mapped directly to Erlang union types.

    Depending on whether options have associated values or not, union subtypes
    are represented either by atoms or by `{<atom-tag>, <value>}` pairs.

    For example, these definitions:

        .enum [
            .name e
            .option [ .name a ]
            .option [ .name b ]
        ]
        .varint [
            .name v
            .option [ .type e ]
            .option [ .name f ]
            .option [ .name i .type int ]
        ]

    are mapped to:

        -type e() :: a | b.
        -type v() :: e() | f | {i, integer()}.

-   List type is mapped Erlang list type.

    For example,

        .list [
            .name l
            .type x
        ]

    is mapped to:

        -type l() :: [ x() ].

-   Aliases are mapped to Erlang type definitions.

    For example,

        .alias [
            .name a
            .type x
        ]

    is mapped to:

        -type a() :: x().

    For aliases, it is possible to specify an optional `erlang-default` string
    property. When present, it overrides the Piqi-default value for the type in
    the generated `*_piqi:default_<typename>/0` functions. It is especially
    useful with custom Erlang types described in the following section.

    Example:

        .alias [
            .name positive-int64
            .type int64
            .erlang-type "piqirun_custom:pos_int64"

            % we need to specify a custom default value for this type to prevent
            % Dialyzer and runtime errors caused by the default value of 0
            %
            % NOTE: the value is an arbitrary Erlang expression

            .erlang-default "1"
        ]


### Custom Erlang types

Piqi provides a way to define mappings between custom Erlang types and Piqi
types. Such mechanism is useful when there is a need to automatically serialize
an Erlang type using some relevant Piqi type, but there is no way to describe
the desired Erlang type using Piqi.

Inability to use Piqi to define an Erlang type would mean that the Erlang type
is either a primitive built-in type that has no direct equivalent in Piqi (e.g.
`atom` or "big" integer), or it is some higher-order type (e.g. function closure
or an arbitrary Erlang term).

The mapping mechanism works as follows. Suppose we need to add support for
serializing Erlang's `term()` type (i.e. *any* Erlang term) as Piqi `binary`.
This can be done in a few steps:

1.  First, define Piqi alias for such mapping:

        .alias [
            % the new Piqi type
            .name erlang-term

            % the original Piqi type
            .type binary

            % Erlang type (should be point to the Erlang module with the mapping
            % implementation -- see below)
            .erlang-type "piqirun_custom:term_t"

            % optionally, define a custom Erlang name for this type
            % .erlang-name "term_t"
        ]

    (Note that we use `term_t` instead of `term`, because Erlang forbids reusing
    names of built-in types for user-defined types. In particular, it is not
    allowed to refer to a foreign type as `piqirun_custom:term()`).

2.  Second, implement runtime functions for mapping the custom Erlang type to
    the Piqi type:

    In module `piqirun_custom.erl`:

        -module(piqirun_custom).

        -export_type([term_t/0]).
        -export([term_t_to_binary/1, term_t_of_binary/1]).

        -type term_t() :: any().

        -spec term_t_of_binary/1 :: (binary()) -> any().
        -spec term_t_to_binary/1 :: (any()) -> binary().

        term_t_of_binary(X) -> binary_to_term(X).

        term_t_to_binary(X) -> term_to_binary(X).

After that, the only thing that's left is to make `piqirun_custom` module
accessible from your Erlang application.

More examples of how to map various Erlang types to Piqi types can be found
[here](http://github.com/alavrik/piqi-erlang/tree/master/examples/custom-types/).

### Piqi extensions

There is no direct notion of Piqi extensions in Erlang: Piqi extensions are all
resolved and applied to Piqi types before generating Erlang types from them.

Examples
--------

-   The first example is based on the "addressbook" example from Protocol
    Buffers source distribution. It contains Erlang implementation of two simple
    programs: for adding a record to an addressbook and for listing addressbook
    contents. The programs implement the same functionality as programs from the
    Protocol Buffers
    [examples](http://protobuf.googlecode.com/svn/trunk/examples/) written in
    C++, Java and Python.

    [examples/erlang](http://github.com/alavrik/piqi-erlang/tree/master/examples/addressbook/)

-   Data serialization in XML, JSON and Piq formats

    In the same directory, there is the `io_json_xml_pb.erl` Erlang module. It
    reads and writes the addressbook data structure from the previous example in
    various formats.

-   More complicated example demonstrating complex types and module imports

    Piq compiler for Erlang (`piqic-erlang`) produces Erlang parsers and
    generators from Piqi self-specification
    ([piqi.piqi](/self-definition/#piqi_piqi)). After that, an Erlang program
    reads (and writes back) Piqi self-specification represented as a binary
    object.

    [tests/erlang\_piqi](http://github.com/alavrik/piqi-erlang/tree/master/tests/erlang_piqi/)

-   Examples of serializing custom Erlang types using Piqi

    [examples/erlang-custom-types](http://github.com/alavrik/piqi-erlang/tree/master/examples/custom-types/)


Limitations
-----------

-   No support for IEEE 754 floating point infinities and NaN.

    These special values are supported by Protocol Buffers, but not by the
    Erlang language.

    Although it is not hard to add support for serializing/deserializing such
    values, it is not obvious what would be the best way to represent them in
    Erlang.

    Currently, Piqi Erlang runtime will throw an exception when it encounters
    infinities or NaN during deserialization.

The way how Piqi records are mapped to Erlang records introduces several
limitations:

-   Limited support for defaults.

    There is no way to tell if the value of an optional field came from the
    original serialized object or it is the default value.

-   No other dynamic properties.

    For example, in Protocol Buffer, there is a way to get the count of repeated
    fields and access them using field index.

-   JSON and XML serialization relies on external program (Erlang port).

    The external program is used for converting data between JSON or XML and
    natively supported Protocol Buffers format.

    Although implementation is fairly optimized and supports running a pool of
    workers on multi-core, communication over Unix pipe will still cause some
    latency for JSON or XML serialization.

Other limitations:

-   Piqi runtime library hasn't been heavily optimized for performance yet.

-   No integer overflow checks in Piqi runtime library during serialization

    Currently, if an integer value doesn't fit into the range of the specified
    integer type, it will be silently stripped down.

Supported Erlang and Protocol Buffers versions
----------------------------------------------

Piqi works with Erlang \>= R13B04 and Protocol Buffers \>= 2.3.0
