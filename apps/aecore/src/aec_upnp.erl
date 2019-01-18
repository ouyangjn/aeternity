%%%=============================================================================
%%% @copyright (C) 2019, Aeternity Anstalt
%%% @doc
%%%    Handling of UPnP and NAT-PMP protocols discovery and port mappings,
%%%    to help to achieve better P2P connectivity. This works for routers with
%%%    UPnP or NAT-PNP enabled, and should be used when there is no port
%%%    forwarding configured.
%%% @end
%%%=============================================================================

-module(aec_upnp).

%% API
-export([add_port_mapping/0]).

%%%===================================================================
%%% API
%%%===================================================================

add_port_mapping() ->
    case is_upnp_enabled() of
        true  -> do_add_port_mapping();
        false -> ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_upnp_enabled() ->
    case aeu_env:find_config([<<"sync">>, <<"upnp_enabled">>],
                             [user_config, schema_default]) of
        {ok, IsEnabled} -> IsEnabled;
        undefined       -> false
    end.

do_add_port_mapping() ->
    case nat:discover() of
        {ok, Ctx} ->
            InternalPort = aec_connection_sup:sync_port(),
            ExternalPort = aec_connection_sup:ext_sync_port(),
            case nat:maintain_port_mapping(Ctx, tcp, InternalPort, ExternalPort) of
                {ok, _, _, _, _} ->
                    epoch_sync:info("UPnP/NAT-PMP mapping between ~p and ~p added", [InternalPort, ExternalPort]);
                {error, _Reason} = Error ->
                    epoch_sync:warning("Adding UPnP/NAT-PMP mapping between ~p and ~p failed: ~p", [InternalPort, ExternalPort, Error])
            end;
        no_nat ->
            epoch_sync:warning("UPnP/NAT-PMP discovery failed")
    end.
