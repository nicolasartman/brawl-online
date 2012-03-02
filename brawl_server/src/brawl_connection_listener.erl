-module(brawl_connection_listener).
-behavior(cowboy_http_handler).
-behavior(cowboy_http_websocket_handler).
-include("deps/cowboy/include/http.hrl").
-include("include/brawl_req.hrl").
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
  io:format("Initializing http handler~n"),
  case cowboy_http_req:header('Upgrade', Req) of
    {undefined, Output} -> {ok, Output, undefined};
    {<<"websocket">>, _Output} -> {upgrade, protocol, cowboy_http_websocket};
    {<<"WebSocket">>, _Output} -> {upgrade, protocol, cowboy_http_websocket}
  end.

handle(Req, State) ->
  io:format("Handling request~n"),
  GameId = brawl_server:new_game(),
  io:format("Created game ~s~n", [GameId]),
  Encoded = jiffy:encode({[ { <<"GameId:">>, GameId } ]}),
  {ok, Output} = cowboy_http_req:reply(200, [], Encoded, Req),
  {ok, Output, State}.

terminate(_Req, _State) ->
  ok.

websocket_init(_Any, Req, []) ->
  Output = cowboy_http_req:compact(Req),
  %Pid = Req#http_req.pid,
  %{reply, {text, create_error("No game found")}, Output, undefined}.
  {ok, Output, undefined, hibernate}.

websocket_handle({text, Msg}, Req, PlayerId) ->
  io:format("PlayerId: ~s Got data ~s ~n", [PlayerId, Msg]),
  case decode_request(Msg) of
    #brawl_req{message_type = "connect", game_id = GameId } ->
      io:format("Connect message received~n"),
      case brawl_server:exists(GameId) of
        true ->
          ets:insert(connections, #brawl_connection{game_id=GameId, websocket=Req#http_req.pid}),
          {P1, P2} = brawl_server:get_players(GameId),
          Reply = create_connected(P1 == none, P2 == none, brawl:decks()),
          io:format("Game found, players: ~w ~w, reply:~s~n", [P1, P2, Reply]),
          {reply, {text,Reply}, Req, PlayerId};
        Result ->
          io:format("No match found: ~w~n~n", [Result]),
          {reply, {text, create_error("No game found", "game_not_found")}, Req, PlayerId}
      end;
    #brawl_req{message_type = "join", game_id = GameId, deck=Deck, player_type=PlayerType } ->
      io:format("Join message received, GameID: ~w~n", [GameId]),
      case brawl_server:join(GameId, PlayerType, Deck) of
        {error, already_joined} ->
          {reply, {text, create_error("Player slot taken", "join_failed")}, Req, PlayerId};
        spectator ->
          case brawl_server:start_game(GameId) of
            {started, GameState} ->
              {reply, {text, create_started(GameState, brawl_server:get_decks(GameId))}, Req, PlayerId};
            _ ->
              {ok, Req, PlayerId}
          end;
        NewPlayerId when is_binary(NewPlayerId)->
          Reply = create_joined(NewPlayerId),
          io:format("Join reply: ~s~n", [Reply]),
          %TODO wait until a start message before starting
          case brawl_server:start_game(GameId) of
            {started, GameState} ->
              broadcast_start(GameId, GameState);
            _ -> ok
          end,
          {reply, {text, Reply}, Req, NewPlayerId};
        Result ->
          io:format("No game found: ~w~n", [Result]),
          {reply, {text, create_error("No game found", "game_not_found")}, Req, PlayerId}
      end;
    #brawl_req{message_type= "card_move", game_id=GameId,
        from=From, to=To, base_id=BaseId } ->
      case {From, To} of
        {undefined, _To} ->
          {reply, {text, create_error("Bad start location", "bad_move")}, Req, PlayerId};
        {_From , undefined} ->
          {reply, {text, create_error("Bad end location", "bad_move")}, Req, PlayerId};
        {ValidFrom, ValidTo} ->
          io:format("Got base id of ~s ~n", [BaseId]),
          GameState = case BaseId of
            undefined ->
              brawl_server:play(GameId, PlayerId, ValidFrom, ValidTo);
            ToBase ->
              brawl_server:play(GameId, PlayerId, ValidFrom, {ToBase, ValidTo})
          end,
          case GameState of
            {error, not_enough_players} ->
              {reply, {text, create_error("Not enough players!", "bad_move")}, Req, PlayerId};
            NextState ->
              broadcast_state(GameId, NextState),
              {ok, Req, PlayerId}
          end
      end;
    #brawl_req{message_type="game_state", game_id=GameId} ->
      GameState = brawl_server:state(GameId),
      io:format("Sending ~w ~n", [GameState]),
      {reply, create_game_state(GameState), Req, PlayerId};
    _ ->
      io:format("Bad message format!~n"),
      {ok, Req, PlayerId}
  end;
websocket_handle(_Any, Req, PlayerId) ->
  {ok, Req, PlayerId}.

websocket_info(JsonMsg, Req, PlayerId) ->
  io:format("Sending message ~s to ~w~n", [JsonMsg, self()]),
  {reply, {text, JsonMsg}, Req, PlayerId}.

websocket_terminate(Reason, Req, PlayerId) ->
  Ws = Req#http_req.pid,
  io:format("Closing socket for reason: ~w~n", [Reason]),
  Games = ets:match(connections, #brawl_connection{game_id='$1', websocket=Ws, _='_' }),
  cleanup_games(Games, Ws, PlayerId),
  ok.

send_start([], _Message) ->
  true;
send_start([[Ws] | Wses], Message) ->
  io:format("Broadcasting to: ~w~n", [Ws]),
  Ws ! Message,
  send_start(Wses, Message).

broadcast_start(GameId, GameState) ->
  io:format("Got state: ~w~n", [GameState]),
  Message = create_started(GameState, brawl_server:get_decks(GameId)),
  Wses = ets:match(connections, #brawl_connection{game_id = GameId, websocket='$1'}),
  io:format("Broadcasting message: ~s~n", [Message]),
  send_start(Wses, Message).

create_message(Type, Content) ->
  jiffy:encode({[ { <<"messageType">>, Type }, { <<"data">>, Content} ]}).

create_error(Msg, ErrorType) ->
  create_message(<<"error">>, {[ {<<"errorMessage">>, list_to_bitstring(Msg) },
                                 {<<"errorType">>, list_to_bitstring(ErrorType)} ]}).

create_connected(P1, P2, Decks) ->
  create_message(<<"connected">>, {[ {<<"player1">>, P1 }, {<<"player2">>, P2},
                  {<<"decks">>, lists:foldl(fun(Name, List) -> 
                                              [ list_to_bitstring(Name) | List ]
                                            end, [], Decks)} ]}).

create_joined(Id) ->
  create_message(<<"joined">>,{[ { <<"playerID">>, Id} ]}). 

create_game_state(State) ->
  create_message(<<"game_state">>, {[game_to_json_form(State)]}).

create_started(State, {Player1Deck, Player2Deck}) ->
  create_message(<<"started">>, {[game_to_json_form(State),
                                  {<<"player1Deck">>, list_to_bitstring(Player1Deck) },
                                  {<<"player2Deck">>, list_to_bitstring(Player2Deck) } ]}).

create_game_over(Winner) ->
  create_message( <<"game_over">>, {[ { <<"winner">>, list_to_bitstring(atom_to_list(Winner)) } ]}).

send_state([], _GameJson) ->
  true;
send_state([#brawl_connection{websocket=Ws} | Wses], GameJson) ->
  io:format("Sending to: ~w~n", [Ws]),
  Ws ! GameJson,
  send_state(Wses, GameJson).
broadcast_state(GameId, Game) ->
  Msg = case Game of
    {winner, Winner} ->
      create_game_over(Winner);
    GameState ->
      create_game_state(GameState)
  end,
  %io:format("Msg ~s ~n", [bitstring_to_list(Msg)]),
  send_state(ets:lookup(connections, GameId), Msg).

cleanup_games([], _Ws, _PlayerId) ->
  ok;
cleanup_games([[GameId] | Games], Ws, PlayerId) ->
  ets:match_delete(connections, #brawl_connection{game_id=GameId, websocket=Ws }),
  brawl_server:leave(GameId, PlayerId),
  case ets:lookup(connections, GameId) of
    Connections when length(Connections) > 0 ->
      cleanup_games(Games, Ws, PlayerId);
    [] ->
      brawl_server:stop(GameId),
      cleanup_games(Games, Ws, PlayerId)
  end.

%%%%%
% Game Encoding Logic
%%%%%

bitstring_to_position(Bitstring) ->
  case Bitstring of
    <<"hand">> -> hand;
    <<"deck">> -> deck;
    <<"discard">> -> discard;
    <<"base_left">> -> base_left;
    <<"base_right">> -> base_right;
    <<"base_p1">> -> p1;
    <<"base_p2">> -> p2;
    <<"base">> -> mods;
    _ -> undefined
  end.

decode_request(Request) ->
  interpret_request(jiffy:decode(Request)).

interpret_request({Parameters}) ->
  interpret_parameters(Parameters, #brawl_req{}).

interpret_parameters([], OutParams) ->
  OutParams;
interpret_parameters([{<<"messageType">>, Type} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{message_type = bitstring_to_list(Type)});
interpret_parameters([{<<"deck">>, Deck} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{deck = bitstring_to_list(Deck)});
interpret_parameters([{<<"playerID">>, PlayerId} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{player_id=PlayerId});
interpret_parameters([{<<"gameID">>, GameId} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{game_id=GameId});
interpret_parameters([{<<"from">>, From} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{from=bitstring_to_position(From)});
interpret_parameters([{<<"to">>, To} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{to=bitstring_to_position(To)});
interpret_parameters([{<<"playerType">>, Type} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{player_type=interpret_player_type(Type)});
interpret_parameters([{<<"toBase">>, ToBase} | Params], OutParams) ->
  interpret_parameters(Params, OutParams#brawl_req{base_id=ToBase});
interpret_parameters([{BadKey, BadValue} | Params], OutParams) when is_binary(BadKey), is_binary(BadValue) ->
  io:format("Unknown parameter ~s~s~n", [BadKey, BadValue]),
  interpret_parameters(Params, OutParams);
interpret_parameters([BadParam | Params], OutParams) ->
  io:format("Unknown parameter ~w~n", [BadParam]),
  interpret_parameters(Params, OutParams).
  

interpret_player_type(Type) ->
  case Type of
    <<"player1">> ->  player1;
    <<"player2">> ->  player2;
    _ ->  spectator
  end.

color_to_string(blue) ->
  <<"blue">>;
color_to_string(red) ->
  <<"red">>;
color_to_string(green) ->
  <<"green">>.

card_type_to_string(hit) ->
  <<"hit">>;
card_type_to_string(null) ->
  <<"null">>;
card_type_to_string(double) ->
  <<"double">>;
card_type_to_string(hit2) ->
  <<"hit-2">>;
card_type_to_string(block) ->
  <<"block">>;
card_type_to_string(press) ->
  <<"press">>;
card_type_to_string(hold) ->
  <<"hold">>;
card_type_to_string(clear) ->
  <<"clear">>;
card_type_to_string(reverse) ->
  <<"reverse">>;
card_type_to_string(freeze) ->
  <<"freeze">>;
card_type_to_string(base) ->
  <<"base">>.

card_to_json_form({Type, Color})->
  {[{<<"cardType">>, card_type_to_string(Type)}, {<<"color">>, color_to_string(Color)}]};
card_to_json_form(Type)->
  {[{<<"cardType">>, card_type_to_string(Type)}, {<<"color">>, <<"none">>}]}.

card_stack_to_json_form(CardStack) ->
  card_stack_to_json_form(CardStack, []).

card_stack_to_json_form([], JsonCardStack) ->
  JsonCardStack;
card_stack_to_json_form([base_mod_stack], JsonCardStack) ->
  JsonCardStack;
card_stack_to_json_form([base_player_stack], JsonCardStack) ->
  JsonCardStack;
card_stack_to_json_form([CardType | CardStack], JsonCardStack) ->
  card_stack_to_json_form(CardStack, [ card_to_json_form(CardType) | JsonCardStack] ).

base_to_json_form({BaseId, P1, Mods, P2}) ->
  {[ {<<"id">>, BaseId}, 
     {<<"P1">>, card_stack_to_json_form(P1) },
     {<<"modifiers">>, card_stack_to_json_form(Mods) },
     {<<"P2">>, card_stack_to_json_form(P2) } ]}.

bases_to_json_form(Bases) ->
  { <<"bases">>, bases_to_json_form(Bases, []) }.

bases_to_json_form([], BaseList) ->
  lists:reverse(BaseList);
bases_to_json_form([Base| Bases], BaseList) ->
  bases_to_json_form(Bases, [base_to_json_form(Base) | BaseList] ).

hand_to_json_form([]) ->
  null;
hand_to_json_form([Card]) ->
  card_to_json_form(Card).
discard_to_json_form([]) ->
  null;
discard_to_json_form([Card | _Cards]) ->
  card_to_json_form(Card).

game_to_json_form({ Bases, { P1Hand, P1Discard }, { P2Hand, P2Discard } }) ->
  io:format("Encoding game~n"),
  JSON_Form = {<<"gameState">>, {[ bases_to_json_form(Bases), 
    {<<"P1">>, 
      {[ { <<"hand">>, hand_to_json_form(P1Hand)},
       { <<"discard">>, discard_to_json_form(P1Discard)}
       ]} },
    {<<"P2">>, 
      {[ { <<"hand">>, hand_to_json_form(P2Hand)},
       { <<"discard">>, discard_to_json_form(P2Discard)}
       ]} }
   ]} } ,
  io:format("Json form:~w~n", [JSON_Form]),
  JSON_Form.

