-ifndef(__PIQI_ANY_PIQI_HRL__).
-define(__PIQI_ANY_PIQI_HRL__, 1).


-record(piqi_any, {
    type :: string() | binary() | 'undefined',
    protobuf :: binary() | 'undefined',
    json :: string() | binary() | 'undefined',
    xml :: string() | binary() | 'undefined'
}).

-type piqi_any() :: #piqi_any{}.


-endif.
