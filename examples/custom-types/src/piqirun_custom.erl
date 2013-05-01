%
% runtime support for Erlang custom types defined in example.piqi
%
-module(piqirun_custom).

-export_type([bigint/0]).
-export([bigint_to_string/1, bigint_of_string/1]).

-export_type([term_t/0]).
-export([term_t_to_binary/1, term_t_of_binary/1]).

-export_type([pos_int64/0]).
-export([pos_int64_of_int64/1, pos_int64_to_int64/1, default_pos_int64/0]).


-type bigint() :: integer().
-type term_t() :: any().


-spec bigint_of_string/1 :: (string() | binary()) -> integer().
-spec bigint_to_string/1 :: (integer()) -> list().


bigint_of_string(X) when is_list(X) -> list_to_integer(X);
bigint_of_string(X) when is_binary(X) ->
  bigint_of_string(binary_to_list(X)).

bigint_to_string(X) -> integer_to_list(X).


-spec term_t_of_binary/1 :: (binary()) -> any().
-spec term_t_to_binary/1 :: (any()) -> binary().

term_t_of_binary(X) -> binary_to_term(X).

term_t_to_binary(X) -> term_to_binary(X).


% example of how to use to use custom types for runtime type validation
-type(pos_int64() :: pos_integer()).

pos_int64_of_int64(I) when is_integer(I) andalso I >= 1 ->
    I.
pos_int64_to_int64(I) when is_integer(I) andalso I >= 1 ->
    I.

% overridden default -- referenced by .erlang-default alias property
default_pos_int64() -> 1.

