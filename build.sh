#!/bin/bash
cd brawl_server
rebar get-deps
rebar compile
cd ..
erl -env ERL_LIBS brawl_server:brawl_server/deps -run systools make_script brawl_online-1.1 -run init stop -noshell
erl -env ERL_LIBS brawl_server:brawl_server/deps -run systools make_tar brawl_online-1.1 -run init stop -noshell
echo "Done!"
