-module(brawl_server_app).
-include("include/brawl_req.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  random:seed(now()),
  ets:new(brawl_servers, [set, named_table, public]),
  ets:new(connections, [named_table, bag, public, {keypos, #brawl_connection.game_id}]),
  start_cowboy_listeners(),
  brawl_server_sup:start_link().

stop(_State) ->
  ok.

start_cowboy_listeners() ->
  Dispatch = cowboy_router:compile([ { '_', 
                                      [ { [<<"/play">>], brawl_connection_listener, []} ,
                                        { [<<"/new_game">>], brawl_connection_listener, []} ] } ]),
      cowboy:start_http(brawl_connection_listener, 100,
                                [{port, 8080}],
                        [{env, [{dispatch, Dispatch}]}]).
