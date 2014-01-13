-ifndef(__PIQI_PIQI_HRL__).
-define(__PIQI_PIQI_HRL__, 1).


-type piq_format() ::
      word
    | text.

-type protobuf_wire_type() ::
      varint
    | zigzag_varint
    | fixed32
    | fixed64
    | signed_varint
    | signed_fixed32
    | signed_fixed64
    | block.

-type word() :: string() | binary().

-type name() :: word().

-type typedef() ::
      {piqi_record, piqi_record()}
    | {variant, variant()}
    | {enum, enum()}
    | {alias, alias()}
    | {piqi_list, piqi_list()}.

-type piqi_type() ::
      int
    | float
    | bool
    | string
    | binary
    | any.

-type type() :: name().

-record(piqi_record, {
    name :: name(),
    field = [] :: [field()],
    protobuf_name :: string() | binary(),
    protobuf_custom = [] :: [string() | binary()],
    json_name :: string() | binary(),
    erlang_name :: string() | binary()
}).

-record(field, {
    name :: name(),
    type :: type(),
    mode = required :: field_mode() | 'undefined',
    default :: piqi_any(),
    deprecated = false :: boolean(),
    piq_format :: piq_format(),
    piq_positional :: boolean(),
    protobuf_name :: string() | binary(),
    code :: integer(),
    protobuf_packed = false :: boolean(),
    json_name :: string() | binary(),
    json_omit_missing :: boolean(),
    getopt_letter :: word(),
    getopt_doc :: string() | binary(),
    erlang_name :: string() | binary()
}).

-type field_mode() ::
      required
    | optional
    | repeated.

-record(variant, {
    name :: name(),
    option = [] :: [option()],
    protobuf_name :: string() | binary(),
    protobuf_custom = [] :: [string() | binary()],
    json_name :: string() | binary(),
    erlang_name :: string() | binary()
}).

-record(option, {
    name :: name(),
    type :: type(),
    deprecated = false :: boolean(),
    piq_format :: piq_format(),
    protobuf_name :: string() | binary(),
    code :: integer(),
    json_name :: string() | binary(),
    getopt_letter :: word(),
    getopt_doc :: string() | binary(),
    erlang_name :: string() | binary()
}).

-record(enum, {
    name :: name(),
    option = [] :: [option()],
    protobuf_name :: string() | binary(),
    protobuf_custom = [] :: [string() | binary()],
    protobuf_prefix :: string() | binary(),
    json_name :: string() | binary(),
    erlang_name :: string() | binary()
}).

-record(alias, {
    name :: name(),
    type :: type(),
    piqi_type :: piqi_type(),
    piq_format :: piq_format(),
    protobuf_name :: string() | binary(),
    protobuf_type :: string() | binary(),
    protobuf_wire_type :: protobuf_wire_type(),
    json_name :: string() | binary(),
    erlang_name :: string() | binary(),
    erlang_type :: string() | binary(),
    erlang_default :: string() | binary()
}).

-record(piqi_list, {
    name :: name(),
    type :: type(),
    piq_format :: piq_format(),
    protobuf_name :: string() | binary(),
    protobuf_custom = [] :: [string() | binary()],
    protobuf_packed = false :: boolean(),
    json_name :: string() | binary(),
    erlang_name :: string() | binary()
}).

-record(piqi, {
    module :: word(),
    typedef = [] :: [typedef()],
    import = [] :: [import()],
    func = [] :: [func()],
    protobuf_custom = [] :: [string() | binary()],
    protobuf_package :: string() | binary(),
    file :: string() | binary(),
    erlang_module :: string() | binary(),
    erlang_type_prefix :: string() | binary(),
    erlang_string_type = binary :: erlang_string_type() | 'undefined'
}).

-record(import, {
    module :: word(),
    name :: name()
}).

-record(piqi_any, {
    type :: string() | binary(),
    protobuf :: binary(),
    json :: string() | binary(),
    xml :: string() | binary()
}).

-record(func, {
    name :: name(),
    input :: type(),
    output :: type(),
    error :: type(),
    erlang_name :: string() | binary()
}).

-record(piqi_bundle, {
    piqi = [] :: [piqi()]
}).

-type erlang_string_type() ::
      binary
    | list.

-type piqi_record() :: #piqi_record{}.

-type field() :: #field{}.

-type variant() :: #variant{}.

-type option() :: #option{}.

-type enum() :: #enum{}.

-type alias() :: #alias{}.

-type piqi_list() :: #piqi_list{}.

-type piqi() :: #piqi{}.

-type import() :: #import{}.

-type piqi_any() :: #piqi_any{}.

-type func() :: #func{}.

-type piqi_bundle() :: #piqi_bundle{}.


-endif.
