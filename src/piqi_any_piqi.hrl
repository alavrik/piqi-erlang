-ifndef(__PIQI_ANY_PIQI_HRL__).
-define(__PIQI_ANY_PIQI_HRL__, 1).


-record(piqi_any, {
    type :: string() | binary(),
    protobuf :: binary(),
    json :: string() | binary(),
    xml :: string() | binary()
}).

-type piqi_any() :: #piqi_any{}.


-endif.
