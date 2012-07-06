-record(brawl_req, {
        message_type = undefined :: undefined | connect | join | card_move | get_game_state,
        game_id = "" :: list(),
        player_id = "" :: list(),
        base_id = undefined :: undefined | list(),
        from = undefined :: undefined | deck | discard | hand | basep1 | basep2 | baseleft | baseright,
        to = undefined :: undefined | deck | discard | hand | basep1 | basep2 | baseleft | baseright,
        player_type = spectator :: player1 | player2 | spectator,
        deck = "random" ::  list()
}).

-record(brawl_game, {
        player1 = none :: none | binary(),
        player2 = none :: none | binary(),
        player1deck = none :: none | list(),
        player2deck = none :: none | list(),
        player1_rematch = false :: true | false,
        player2_rematch = false :: true | false,
        state = none :: none | tuple()
}).


-record(brawl_connection, {
        game_id = none :: none | binary(),
        websocket = none :: none | pid()
}).

