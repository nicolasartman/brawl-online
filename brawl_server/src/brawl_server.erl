-module(brawl_server).
-behavior(gen_server).
-include("include/brawl_req.hrl").
-export([start/0, start_link/0, init/1, handle_call/3, terminate/2]).
-export([new_game/0, start_game/1, play/4, pick_deck/3,
         state/1, stop/1, exists/1, leave/2, join/2, get_players/1, get_decks/1]).

new_game() ->
  GameId = brawl:generate_id(),
  Result = gen_server:start_link(?MODULE, [GameId], []),
  case Result of
    {ok, PID} ->
      ets:insert(brawl_servers, {GameId, PID}),
      GameId;
    _ ->
      error
  end.

start_game(GameId) ->
  call_game(GameId, start_game).

play(GameId, PlayerId, From, To) ->
  call_game(GameId, {move, PlayerId, From, To}).
get_players(GameId) ->
  call_game(GameId, get_players).
state(GameId) ->
  call_game(GameId, visible_state).
join(GameId, Player) ->
  call_game(GameId, {join, Player}).
pick_deck(GameId, Player, Deck) ->
  call_game(GameId, {pick_deck, Player, Deck}).
leave(GameId, Player) ->
  call_game(GameId, {leave, Player}).
get_decks(GameId) ->
  call_game(GameId, get_decks).

stop(GameId) ->
  case call_game(GameId, stop) of
    game_not_found ->
      false;
    ok ->
      io:format("Found server for Game ~s, stopped~n", [GameId]),
      ets:delete(brawl_servers, GameId),
      true
  end.

exists(GameId) ->
  case ets:lookup(brawl_servers, GameId) of
    [{ _GameId, _PID }] ->
      true;
    _ ->
      false
  end.

call_game(GameId, Message) ->
   case ets:lookup(brawl_servers, GameId) of
    [{ GameId, PID }]->
      gen_server:call(PID, Message);
    _ ->
      game_not_found
  end. 

start() ->
  gen_server:start({local, ?MODULE}, ?MODULE, [], []).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_GameId) ->
  random:seed(now()),
  {ok, #brawl_game{}}.

terminate(_Reason, _State) ->
  ok.

visible_state(none) ->
  none;
visible_state({Bases, { P1Hand, P1Discard, _ }, { P2Hand, P2Discard, _ }}) ->
  {Bases, {P1Hand, P1Discard}, {P2Hand, P2Discard}}.

handle_call(stop, _From, Game) ->
  {stop, normal, ok, Game};
handle_call({pick_deck, PlayerId, Deck}, _From, Game) ->
  P1 = Game#brawl_game.player1,
  P2 = Game#brawl_game.player2,
  NextGame = case PlayerId of
    P1 ->
      case Game#brawl_game.state of
        none ->
          Game#brawl_game{player1deck=Deck};
        _InProgress  ->
          Game
      end;
    P2 ->
      case Game#brawl_game.state of
        none ->
          Game#brawl_game{player2deck=Deck};
        _InProgress  ->
          Game
     end
  end,
  {reply, ok, NextGame};
handle_call({join, Player}, _From, Game) ->
  case Player of
    player1 ->
      
      case Game#brawl_game.player1 of
        none ->
          Id=brawl:generate_id(),
          {reply, Id, Game#brawl_game{player1=Id}};
        _ ->
          {reply, {error, already_joined}, Game}
      end;
    player2 ->
      case Game#brawl_game.player2 of
        none ->
          Id=brawl:generate_id(),
          {reply, Id, Game#brawl_game{player2=Id}};
        _ ->
          {reply, {error, already_joined}, Game}
      end;
    _ ->
      {reply, spectator, Game}
  end;
handle_call({leave, PlayerId}, _From, Game) ->
  %TODO fix to work with uninitialized games
  %io:format("Player ~s leaving game, players: ~s ~s~n", [PlayerId, Game#brawl_game.player1, Game#brawl_game.player2]),
  case Game of
    #brawl_game{player1=PlayerId} ->
      io:format("Player 1 left~n", []),
      {reply, player1, Game#brawl_game{player1=none}};
    #brawl_game{player2=PlayerId} ->
      io:format("Player 2 left~n", []),
      {reply, player2, Game#brawl_game{player2=none}};
    _ ->
      io:format("spectator left~n", []),
      {reply, spectator, Game}
  end;
handle_call(get_decks, _from, Game) ->
  {reply, {Game#brawl_game.player1deck, Game#brawl_game.player2deck}, Game};
handle_call(get_players, _from, Game) ->
  {reply, {Game#brawl_game.player1, Game#brawl_game.player2}, Game};
handle_call(start_game, _From, Game) ->
  case Game of
    #brawl_game{state=State} when State /= none ->
      {reply, {started, visible_state(State)}, Game};
    #brawl_game{player1=Player1, player2=Player2, state=none,
                player1deck = Deck1, player2deck = Deck2} when
        Player1 /= none, Player2 /= none,
        Deck1 /= none, Deck2 /= none ->
      {Deck1Name, Deck2Name, GameState} = brawl:new_game(Deck1, Deck2),
      {reply, {started, visible_state(GameState)}, Game#brawl_game{state=GameState, player1deck=Deck1Name, player2deck=Deck2Name}};
    _ ->
      {reply, {error, not_enough_players}, Game}
  end;
handle_call(state, _From, Game) ->
  {reply, Game#brawl_game.state, Game};
handle_call(visible_state,_From, Game) ->
  {reply, visible_state(Game#brawl_game.state), Game};
handle_call({move, PlayerId, RawFrom, RawTo},_From, Game) ->
  case Game of
    #brawl_game{player1=P1, player2=P2} when P1 /= none, P2 /= none->
      {From, To} = translate_move(PlayerId, RawFrom, RawTo, Game),
      case brawl:play(From, To, Game#brawl_game.state) of
        {winner, Winner} ->
          {reply, {winner, Winner}, Game};
        NextState ->
          {reply, visible_state(NextState), Game#brawl_game{state=NextState}}
      end;
    _ ->
      {reply, {error, not_enough_players}, Game}
  end.

translate_move(PlayerId, From, To, Game) ->
  case Game of
    #brawl_game{player1=PlayerId} ->
      CombinedFrom = combine_player_location(p1, From),
      CombinedTo = combine_player_location(p1, To),
      {CombinedFrom, CombinedTo};
    #brawl_game{player2=PlayerId} ->
      CombinedFrom = combine_player_location(p2, From),
      CombinedTo = combine_player_location(p2, To),
      {CombinedFrom, CombinedTo};
    _ ->
      {error, invalid_player}
  end.

combine_player_location(Player, Location) ->
  case Location of
    hand when Player == p1 -> p1hand;
    hand when Player == p2 -> p2hand;
    deck when Player == p1 -> p1deck;
    deck when Player == p2 -> p2deck;
    discard when Player == p1 -> p1discard;
    discard when Player == p2 -> p2discard;
    _ -> Location
  end.
