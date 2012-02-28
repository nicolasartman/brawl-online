#!/bin/bash

if [ ! -d tmp-deploy ] ; then
  mkdir tmp-deploy
fi
cd tmp-deploy
tar zxf ../brawl_online-1.1.tar.gz
erl -boot releases/1.1/start
cd ..
