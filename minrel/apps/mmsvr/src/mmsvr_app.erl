%%%-------------------------------------------------------------------
%% @doc mmsvr public API
%% @end
%%%-------------------------------------------------------------------

-module(mmsvr_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_',
         [
          {"/ws", mmsvr_ws_handler, []}
         ]}
                                     ]),
    {ok, _} = cowboy:start_clear(mmsvr_http_listener,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    %db_sup:start_link(),
    mmsvr_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
