-ifndef(__PIQI_PIQI_HRL__).
-define(__PIQI_PIQI_HRL__, 1).

-include_lib("piqi/include/piqi_any_piqi.hrl").


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
    piq_positional :: boolean() | 'undefined',
    protobuf_name :: string() | binary() | 'undefined',
    protobuf_custom = [] :: [string() | binary()],
    json_name :: string() | binary() | 'undefined',
    erlang_name :: string() | binary() | 'undefined'
}).

-record(field, {
    name :: name() | 'undefined',
    type :: type() | 'undefined',
    mode = required :: field_mode() | 'undefined',
    default :: piqi_any() | 'undefined',
    deprecated = false :: boolean() | 'undefined',
    piq_format :: piq_format() | 'undefined',
    piq_positional :: boolean() | 'undefined',
    piq_alias :: name() | 'undefined',
    protobuf_name :: string() | binary() | 'undefined',
    code :: integer() | 'undefined',
    protobuf_packed = false :: boolean() | 'undefined',
    json_name :: string() | binary() | 'undefined',
    json_omit_missing :: boolean() | 'undefined',
    getopt_letter :: word() | 'undefined',
    getopt_doc :: string() | binary() | 'undefined',
    erlang_name :: string() | binary() | 'undefined'
}).

-type field_mode() ::
      required
    | optional
    | repeated.

-record(variant, {
    name :: name(),
    option = [] :: [option()],
    protobuf_name :: string() | binary() | 'undefined',
    protobuf_custom = [] :: [string() | binary()],
    json_name :: string() | binary() | 'undefined',
    erlang_name :: string() | binary() | 'undefined'
}).

-record(option, {
    name :: name() | 'undefined',
    type :: type() | 'undefined',
    deprecated = false :: boolean() | 'undefined',
    piq_format :: piq_format() | 'undefined',
    piq_alias :: name() | 'undefined',
    protobuf_name :: string() | binary() | 'undefined',
    code :: integer() | 'undefined',
    json_name :: string() | binary() | 'undefined',
    getopt_letter :: word() | 'undefined',
    getopt_doc :: string() | binary() | 'undefined',
    erlang_name :: string() | binary() | 'undefined'
}).

-record(enum, {
    name :: name(),
    option = [] :: [option()],
    protobuf_name :: string() | binary() | 'undefined',
    protobuf_custom = [] :: [string() | binary()],
    protobuf_prefix :: string() | binary() | 'undefined',
    json_name :: string() | binary() | 'undefined',
    erlang_name :: string() | binary() | 'undefined'
}).

-record(alias, {
    name :: name(),
    type :: type() | 'undefined',
    piqi_type :: piqi_type() | 'undefined',
    piq_format :: piq_format() | 'undefined',
    protobuf_name :: string() | binary() | 'undefined',
    protobuf_type :: string() | binary() | 'undefined',
    protobuf_wire_type :: protobuf_wire_type() | 'undefined',
    json_name :: string() | binary() | 'undefined',
    erlang_name :: string() | binary() | 'undefined',
    erlang_type :: string() | binary() | 'undefined',
    erlang_default :: string() | binary() | 'undefined'
}).

-record(piqi_list, {
    name :: name(),
    type :: type(),
    piq_format :: piq_format() | 'undefined',
    protobuf_name :: string() | binary() | 'undefined',
    protobuf_custom = [] :: [string() | binary()],
    protobuf_packed = false :: boolean() | 'undefined',
    json_name :: string() | binary() | 'undefined',
    erlang_name :: string() | binary() | 'undefined'
}).

-record(piqi, {
    module :: word() | 'undefined',
    typedef = [] :: [typedef()],
    import = [] :: [import()],
    func = [] :: [func()],
    custom_field = [] :: [word()],
    protobuf_custom = [] :: [string() | binary()],
    protobuf_package :: string() | binary() | 'undefined',
    file :: string() | binary() | 'undefined',
    erlang_module :: string() | binary() | 'undefined',
    erlang_type_prefix :: string() | binary() | 'undefined',
    erlang_string_type = binary :: erlang_string_type() | 'undefined'
}).

-record(import, {
    module :: word(),
    name :: name() | 'undefined'
}).

-record(func, {
    name :: name(),
    input :: type() | 'undefined',
    output :: type() | 'undefined',
    error :: type() | 'undefined',
    erlang_name :: string() | binary() | 'undefined'
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

-type func() :: #func{}.

-type piqi_bundle() :: #piqi_bundle{}.


-endif.
