-module(brawl_tests).
-compile([export_all]).
-export([run_tests/0,
         can_block/0,
         hold_prevents_clear/0,
         mods_override_hold/0,
         draw_from_empty_discard/0,
         draw_from_empty_deck/0,
         draw_from_discard_empty_hand/0,
         draw_from_discard_full_hand/0,
         draw_from_deck_empty_hand/0,
         draw_from_deck_full_hand/0]).

run_tests() ->
  Tests = [ draw_from_deck_empty_hand,
            draw_from_deck_full_hand,
            draw_from_discard_empty_hand,
            draw_from_discard_full_hand,
            draw_from_empty_deck,
            draw_from_empty_discard,
            mods_override_hold,
            can_block,
            can_block_hits,
            can_press_blocks,
            can_hit_pressed_blocks,
            can_hit_empty_base,
            can_freeze_empty_base,
            hold_prevents_clear
          ],
  test_loop(Tests, 0, 0).

test_loop([], Passes, Fails) ->
  io:format("Passed: ~w~n", [Passes]),
  io:format("Failed: ~w~n", [Fails]),
  true;
test_loop([Test | Tests], Passes, Fails) ->
  Result = apply(brawl_tests, Test, []),
  case Result of
    true ->
      io:format("Test ~w: PASS~n", [Test]),
      test_loop(Tests, Passes + 1, Fails);
    false ->
      io:format("Test ~w: FAIL~n", [Test]),
      test_loop(Tests, Passes, Fails + 1)
  end.


%Tests:

draw_from_empty_discard() ->
  Game = { [], { [], [], [] }, {} },
  case brawl:play(p1discard, p1hand, Game) of
    Game ->
      true;
    _ ->
      false
  end.

draw_from_empty_deck() ->
  Game = { [], { [card], [], [card] }, {} },
  case brawl:play(p1deck, p1hand, Game) of
    Game ->
      true;
    _ ->
      false
  end.


draw_from_deck_empty_hand() ->
  Game = { [], { [], [], [card] }, {} },
  case brawl:play(p1deck, p1hand, Game) of
    { [], { [card], [], [] }, {} } ->
      true;
    _ ->
      false
  end.

draw_from_deck_full_hand() ->
  Game = { [], { [card], [], [card] }, {} },
  case brawl:play(p1deck, p1hand, Game) of
    Game ->
      true;
    _ ->
      false
  end.

draw_from_discard_empty_hand() ->
  Game = { [], { [], [card], [] }, {} },
  case brawl:play(p1discard, p1hand, Game) of
    { [], { [card], [], [] }, {} } ->
      true;
    _ ->
      false
  end.

draw_from_discard_full_hand() ->
  Game = { [], { [card], [card], [] }, {} },
  case brawl:play(p1discard, p1hand, Game) of
    Game ->
      true;
    _ ->
      false
  end.

hold_prevents_clear() ->
  Game = { [ {base1, [], [hold], [] }, {base2, [], [], []} ], { [clear], [], [] }, {} },
  case brawl:play(p1hand, {base1, mods}, Game) of
    Game ->
      true;
    _ ->
      false
  end.

mods_override_hold() ->
  Game = { [ {base1, [], [press, hold], [] }, {base2, [], [], []} ], { [clear], [], [] }, {} },
  case brawl:play(p1hand, {base1, mods}, Game) of
    { [{base2, [], [], []}], { [], [], [] }, {} } ->
      true;
    _ ->
      false
  end.

can_block() ->
  Game = { [ {base1, [{hit, green}], [], [] } ], { [{block, green}], [], [] }, {} },
  case brawl:play(p1hand, {base1, p1}, Game) of
    { [ {base1, [{block, green}, {hit, green}], [], [] } ], { [], [], [] }, {} } ->
      true;
    _ ->
      false
  end.

can_block_hits() ->
  Game = { [ {base1, [{block, green}, {hit, green}], [], [] } ], { [{hit, green}], [], [] }, {} },
  case brawl:play(p1hand, {base1, p1}, Game) of
    Game ->
      true;
    _ ->
      false
  end.

can_press_blocks() ->
  Game = { [ {base1, [{block, green}, {hit, green}], [], [] } ], { [press], [], [] }, {} },
  case brawl:play(p1hand, {base1, p1}, Game) of
    { [ {base1, [{press, green}, {block, green}, {hit, green}], [], [] } ], { [], [], [] }, {} } ->
      true;
    _ ->
      false
  end.
  
can_hit_pressed_blocks() ->
  Game = { [ {base1, [{press, green}, {block, green}, {hit, green}], [], [] } ], 
           { [{hit, green}], [], [] }, {} },
  case brawl:play(p1hand, {base1, p1}, Game) of
    { [ {base1, [{hit, green}, {press, green}, {block, green}, {hit, green}], [], [] } ], { [], [], [] }, {} } ->
      true;
    _ ->
      false
  end.
 
can_hit_empty_base() ->
  Game = { [ {base1, [base_player_stack], [], [] } ], { [{hit, green}], [], [] }, {} },
  case brawl:play(p1hand, {base1, p1}, Game) of
    { [ {base1, [{hit, green}, base_player_stack], [], [] } ], { [], [], [] }, {} } ->
      true;
    _ ->
      false
  end.
  
can_freeze_empty_base() ->
  Game = { [ {base2, [], [], [] }, {base1, [base_player_stack], [base_mod_stack], [base_player_stack] } ], { [freeze], [], [] }, {} },
  case brawl:play(p1hand, {base1, mods}, Game) of
    { [ {base2, [], [], [] }, {base1, [base_player_stack], [freeze, base_mod_stack], [base_player_stack] } ], { [], [], [] }, {} } ->
      true;
    _ ->
      false
  end.
   
