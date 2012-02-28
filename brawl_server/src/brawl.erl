-module(brawl).
-export([play/3, new_game/2, generate_id/0]).

generate_id() ->
  list_to_bitstring(lists:concat(tuple_to_list(now()))).

get_index(List, 1) ->
  hd(List);
get_index(List, Index) when Index > 1->
  get_index(tl(List), Index - 1).

remove_index(Front, [_ | List], 1) ->
  lists:append(Front, List);
remove_index(Front, List, Index) when Index > 1->
  remove_index(lists:append(Front, [hd(List)]), tl(List), Index - 1).

shuffle_deck([], DeckOut) ->
  DeckOut;
shuffle_deck(DeckIn, DeckOut) ->
  Index = random:uniform(length(DeckIn)),
  %io:format("Shuffling deck, picking card ~w~n", [Index]),
  Card = get_index(DeckIn, Index),
  shuffle_deck( remove_index([], DeckIn, Index), [ Card | DeckOut]).

create_deck("darwin") ->
  Deck = build_deck_from_def([{{hit, blue}, 4}, {{hit2, blue}, 1}, {{block, blue}, 3},
    {{hit, green}, 4}, {{block, green}, 1},
    {{hit, red}, 8}, {{hit2, red}, 1}, {{block, red}, 3},
    {base, 3}, {clear, 1}, {press, 3}], []),
  lists:append(shuffle_deck(Deck, []), [freeze, freeze, freeze]);
create_deck("tamiya") ->
  Deck = build_deck_from_def([{{hit, blue}, 3}, {{hit2,blue}, 1}, {{block,blue}, 2},
    {{hit,green}, 3}, {{hit2,green}, 1}, {{block,green}, 2},
    {{hit, red}, 10}, {{hit2, red}, 1},
    {base, 3}, {clear, 5}, {reverse, 1}], [] ),
  lists:append(shuffle_deck(Deck, []), [freeze, freeze, freeze]);
create_deck(DeckName) ->
  io:format("No deck matching ~s, going with default~n", [DeckName]),
  create_deck("darwin").

add_card(_, Deck, 0) ->
  Deck;
add_card(Card, Deck, Count) when Count > 0 ->
  add_card(Card, [Card | Deck], Count - 1).

build_deck_from_def([], Deck) ->
  Deck;
%Subtract 1 from base total
build_deck_from_def([ {base, Number} | Cards], Deck) ->
  build_deck_from_def(Cards, add_card(base, Deck, Number - 1));
build_deck_from_def([ {Card, Number} | Cards], Deck) ->
  build_deck_from_def(Cards, add_card(Card, Deck, Number)).
  
new_game(Deck1, Deck2) ->
  Game = { [], { [], [], create_deck(Deck1)}, { [], [], create_deck(Deck2)}},
  OneBase = add_base_left(Game),
  add_base_left(OneBase).

% Game = { [{id, p1, mods, p2}, base2, base3], { p1hand, p1discard, p1deck}, {p2hand, p2discard, p2deck} }

do_draw(From, Hand, Game) ->
  case length(get_stack(Hand, Game)) of
    Length when Length == 0 ->
      simple_move(From, Hand, Game);
    _ ->
      Game
  end.
  

simple_move(From, To, Game) ->
  FromStack = get_stack(From, Game),
  io:format("Making move from ~w to ~w~n", [From, To]),
  case length(FromStack) of
    Length when Length > 0 ->
      [Card | OutFromStack] = get_stack(From, Game),
      ToStack = get_stack(To, Game),
      OutGame = set_stack(From, OutFromStack, Game),
      set_stack(To, [Card | ToStack], OutGame);
    Length ->
      io:format("Not enough cards in stack! ~w~n", [Length]),
      Game
  end.

% Can discard from hand
play(p1hand, p1discard, Game) ->
  simple_move(p1hand, p1discard, Game);
play(p2hand, p2discard, Game) ->
 simple_move(p2hand, p2discard, Game);
% Can draw from discard
play(p2discard, p2hand, Game) ->
  do_draw(p2discard, p2hand, Game);
play(p1discard, p1hand, Game) ->
  do_draw(p1discard, p1hand, Game);
% Can draw from deck
play(p2deck, p2hand, Game) ->
  do_draw(p2deck, p2hand, Game);
play(p1deck, p1hand, Game) ->
  do_draw(p1deck, p1hand, Game);
% Play cards from hand
play(From, To, Game) ->
  case From of
    p1hand ->
      play_card_from_hand(p1hand, To, Game);
    p2hand ->
      play_card_from_hand(p2hand, To, Game);
    _ ->
      io:format("No Match! ~w ~w ~w~n", [From, To, Game]),
      Game
  end.

play_card_from_hand(Hand, To, Game) ->
  io:format("Playing Card: ~w ~w~n", [Hand, To]),
  HandStack = get_stack(Hand, Game),
  ToStack = case To of
    base_left ->
      base_left;
    base_right ->
      base_right;
    Other ->
      get_stack(Other, Game)
  end,
  if 
    length(HandStack) < 1 ->
      Game;
    ToStack == no_base ->
      Game;
    Hand == p1hand, To == p2discard ->
      Game;
    Hand == p2hand, To == p1discard ->
      Game;
    true ->
      [PlayCard | _ ] = HandStack,
      { Bases, _, _ } = Game,
      ToBaseId =  case To of
        { CurBaseId, _ } ->
          CurBaseId;
        _ ->
          no_base
      end,
      PlayOk = case PlayCard of
        base ->
          can_play(base, Bases);
        clear ->
          if
             ToBaseId == no_base ->
               false;
             length(Bases) == 1 ->
               false;
             true ->
               CenterBaseTest = is_center_base(ToBaseId, Bases, 0),
               if
                 CenterBaseTest -> false;
                 true -> can_play(clear, ToStack)
               end
          end;
        Card ->
          io:format("Playing card ~w on stack ~w~n", [Card, ToStack]),
          case is_frozen(ToBaseId, Game) of
            true ->
              false;
            false ->
              can_play(Card, ToStack)
          end
      end,
      io:format("Play OK: ~w~n", [PlayOk]),
      OutGame = if
        PlayOk ->
          make_move(Hand, To, Game);
        true ->
          Game
      end,
      io:format("State: ~w~n", [OutGame]),
      case is_game_over(OutGame) of
        true ->
          get_winner(OutGame);
        _ ->
          OutGame
      end
  end.

is_frozen(BaseId, Game) ->
  case get_stack({BaseId, mods}, Game) of
    [ freeze | _ ] ->
      true;
    _ ->
      false
  end.

is_center_base(_, [], _) ->
  false;
is_center_base(BaseId, [Base | Bases], BaseCount) ->
  NewBaseCount = BaseCount + 1,
  case NewBaseCount of
    1 ->
      is_center_base(BaseId, Bases, NewBaseCount);
    2 ->
      {CurBaseId, _, _, _} = Base,
      case CurBaseId of
        BaseId ->
          is_center_base(BaseId, Bases, NewBaseCount);
        _ ->
          false
      end;
    3 ->
      true
  end.
        

get_winner({Bases, _, _}) ->
  get_winner(Bases, 0).

get_winner([Base | Bases], Score) ->
  get_winner(Bases, Score + score(Base));
get_winner([], Score) ->
  if
    Score > 0 ->
      {winner, p1};
    Score < 0 ->
      {winner, p2};
    true ->
      {winner, no_winner}
  end.

score({_Id, P1, Mods, P2}) ->
  Strength = score_player_stack(P1) - score_player_stack(P2),
  apply_hit_modifiers(Mods, Strength),
  if 
    Strength < 0 ->
      - count_points(Mods);
    Strength > 0 ->
      count_points(Mods);
    true ->
      0
  end.

count_points(Mods) ->
  count_points(Mods, 1).

count_points([], Points) ->
  Points;
count_points([null | _], _) ->
  0;
count_points([double | Mods], Points) ->
  count_points(Mods, Points * 2);
count_points([_ | Mods], Points) ->
  count_points(Mods, Points).

apply_hit_modifiers([], Score) ->
  Score;
apply_hit_modifiers([null | _], _Score) ->
  0;
apply_hit_modifiers([press| _], Score) ->
  Score;
apply_hit_modifiers([reverse| Mods], Score) ->
  apply_hit_modifiers(Mods, Score * -1);
apply_hit_modifiers([_OtherMod | Mods], Score) ->
  apply_hit_modifiers(Mods, Score).

score_player_stack(Stack) ->
  score_player_stack(Stack, 0).

score_player_stack([], Score) ->
  Score;
score_player_stack([{hit, _} | Stack], Score) ->
  score_player_stack(Stack, Score + 1);
score_player_stack([{hit2, _} | Stack], Score) ->
  score_player_stack(Stack, Score + 2);
score_player_stack([_ | Stack], Score) ->
  score_player_stack(Stack, Score).

is_game_over({Bases, _, _}) ->
  is_game_over(Bases);
is_game_over([]) ->
  true;
is_game_over([{_, _, [freeze | _ ], _} | Bases]) ->
  is_game_over(Bases);
is_game_over(_) ->
  false.

get_stack(p1hand, {  _, { Hand, _, _ }, _ })->
  Hand;
get_stack(p1discard, {  _, { _, Discard, _ }, _ }) ->
  Discard;
get_stack(p1deck, {  _, { _, _, Deck}, _ }) ->
  Deck;
get_stack(p2hand, { _, _, { Hand, _, _ } }) ->
  Hand;
get_stack(p2discard, { _, _, { _, Discard, _ } }) ->
  Discard;
get_stack(p2deck, { _, _, { _, _, Deck} } )->
  Deck;
get_stack({BaseId, Stack}, {Bases, _, _ }) ->
  get_base_stack(Bases, BaseId, Stack);
get_stack(Stack, _Game) ->
  io:format("Couldn't get stack ~w~n", [Stack]).


set_stack(p1hand, Stack, { Bases, { _, Discard, Deck }, P2 }) ->
  { Bases, { Stack, Discard, Deck }, P2 };
set_stack(p1discard, Stack, { Bases, { Hand, _, Deck }, P2 }) ->
  { Bases, { Hand, Stack, Deck }, P2 };
set_stack(p1deck, Stack, { Bases, { Hand, Discard, _ }, P2 }) ->
  { Bases, { Hand, Discard, Stack }, P2 };
set_stack(p2hand, Stack, { Bases, P1, { _, Discard, Deck } }) ->
  { Bases, P1, { Stack, Discard, Deck } };
set_stack(p2discard, Stack, { Bases, P1, { Hand, _, Deck } }) ->
  { Bases, P1, { Hand, Stack, Deck } };
set_stack(p2deck, Stack, { Bases, P1, { Hand, Discard, _ } }) ->
  { Bases, P1,{ Hand, Discard, Stack } };
set_stack({BaseId, StackId}, Stack, { Bases, P1, P2 })->
  { set_base_stack(Bases, BaseId, StackId, Stack), P1, P2 }.

set_base_stack([{BaseId, _, Mods, P2} | Bases], BaseId, p1, Stack) ->
  [{BaseId, Stack, Mods, P2} | Bases];
set_base_stack([{BaseId, P1, _, P2} | Bases], BaseId, mods, Stack) ->
  [{BaseId, P1, Stack, P2} | Bases];
set_base_stack([{BaseId, P1, Mods, _} | Bases], BaseId, p2, Stack) ->
  [{BaseId, P1, Mods, Stack} | Bases];
set_base_stack([ Base | Bases], BaseId, StackId, Stack) ->
  [ Base | set_base_stack(Bases, BaseId, StackId, Stack)].


get_base_stack([], _, _) ->
  no_base;
get_base_stack([{BaseId, P1, Mods, P2} | Bases], BaseId, Stack) ->
  case Stack of
    p1 ->
      P1;
    p2 ->
      P2;
    mods ->
      Mods;
    _ ->
      get_base_stack(Bases, BaseId, Stack)
  end;
get_base_stack([ _ | Bases], BaseId, Stack) ->
  get_base_stack(Bases, BaseId, Stack).
  
can_play(base, Bases) when length(Bases) < 3 ->
  true;
can_play(base, Bases) when length(Bases) >= 3 ->
  false;
can_play({hit, _}, [base_player_stack]) ->
  io:format("Playing hit on empty stack~n"),
  true;
can_play({hit, Color} , [{TopCardType, Color} | _]) ->
  io:format("Playing hit~n"),
  case TopCardType of
    block ->
      false;
    press ->
      true;
    hit ->
      true;
    base_player_stack ->
      true;
    hit2 ->
      true
  end;
can_play({hit2, Color} , [{TopCardType, Color} | _ ]) ->
  case TopCardType of
    press ->
      false;
    block ->
      false;
    hit ->
      true;
    hit2 ->
      true
  end;
can_play({block, Color} , [{TopCardType, Color} | _]) ->
  case TopCardType of
    hit ->
      true;
    hit2 ->
      true;
    _ ->
      false
  end;
can_play(press, [{block, _} | _]) ->
  true;
can_play(clear, [TopCard | _]) ->
  case TopCard of
    hold ->
      false;
    _ ->
      is_base_mod(TopCard)
  end;
can_play(Card, [TopCard | _]) ->
  case Card of
    press ->
      is_base_mod(TopCard);
    hold ->
      is_base_mod(TopCard);
    null ->
      is_base_mod(TopCard);
    reverse ->
      is_base_mod(TopCard);
    double ->
      is_base_mod(TopCard);
    freeze ->
      is_base_mod(TopCard);
    _ ->
      false
  end;
can_play(Card, Stack) ->
  io:format("No matches: ~w ~w~n", [Card, Stack] ),
  false.

is_base_mod(Card) ->
  case Card of
    press ->
      true;
    hold ->
      true;
    null ->
      true;
    reverse ->
      true;
    double ->
      true;
    base_mod_stack ->
      true;
    _ ->
      false
  end.

remove_base(BaseId, [Base | Bases]) ->
  io:format("Trying to remove ~w, cur: ~w~n", [BaseId, Base]),
  case Base of
    {BaseId, _, _, _} ->
      Bases;
    _ ->
      [ Base | remove_base(BaseId, Bases)]
  end.

add_base_left({Bases, P1, P2}) ->
  { [ { generate_id(), [base_player_stack], [base_mod_stack], [base_player_stack] } | Bases], P1, P2}.

add_base_right({Bases, P1, P2}) -> 
  {lists:append(Bases, [{generate_id(), [base_player_stack], [base_mod_stack], [base_player_stack]} ]), P1, P2}.

make_move(From, To, Game) ->
  {Bases, P1, P2 } = Game,
  case get_stack(From, Game) of
    [clear | FromStack] ->
      {BaseId , _} = To,
      OutGame = { remove_base(BaseId, Bases), P1, P2 },
      set_stack(From, FromStack, OutGame);
    [base|FromStack] ->
      case To of
        base_left ->
          OutGame = add_base_left(Game),
          set_stack(From,FromStack, OutGame);
        base_right ->
          OutGame = add_base_right(Game),
          set_stack(From,FromStack, OutGame);
        _ ->
          Game
      end;
    [PlayCard | FromStack] ->
      ToStack = get_stack(To, Game),
      ActualCard = case PlayCard of
        press ->
          case ToStack of
            [ { _, Color } | _ ] -> {press, Color};
            _ -> press
          end;
        _ ->
          PlayCard
      end,
      OutGame = set_stack(From, FromStack, Game),
      set_stack(To, [ActualCard | ToStack ], OutGame);
    [] ->
      Game
  end.

  
