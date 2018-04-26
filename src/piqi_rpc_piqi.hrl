-ifndef(__PIQI_RPC_PIQI_HRL__).
-define(__PIQI_RPC_PIQI_HRL__, 1).


-record(piqi_rpc_request, {
    name :: string() | binary(),
    data :: binary()
}).

-type piqi_rpc_response() ::
      ok
    | {ok, binary()}
    | {error, binary()}
    | {rpc_error, piqi_rpc_rpc_error()}.

-type piqi_rpc_client_error() ::
      unknown_function
    | {invalid_input, string() | binary()}
    | missing_input
    | {protocol_error, string() | binary()}.

-type piqi_rpc_server_error() ::
      {invalid_output, string() | binary()}
    | {internal_error, string() | binary()}
    | {service_unavailable, string() | binary()}.

-type piqi_rpc_rpc_error() ::
      piqi_rpc_client_error()
    | piqi_rpc_server_error().

-type piqi_rpc_request() :: #piqi_rpc_request{}.


-endif.
