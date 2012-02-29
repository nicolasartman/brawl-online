
-module(brawl_server_sup).
-include("include/brawl_req.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  random:seed(now()),
  ets:new(brawl_servers, [set, named_table, public]),
  ets:new(connections, [named_table, bag, public, {keypos, #brawl_connection.game_id}]),
  Dispatch = [ { '_', [ { [<<"play">>], brawl_connection_listener, []} ,
                        { [<<"new_game">>], brawl_connection_listener, []} ] } ],
  CowboySpec = cowboy:child_spec(brawl_connection_listener, 2024, 
                            cowboy_tcp_transport, [{port, 8080}],
                            cowboy_http_protocol, [{dispatch, Dispatch}]),
  {ok, { {one_for_one, 5, 10}, 
       [ CowboySpec ]}}.

