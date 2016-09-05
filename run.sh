#!/bin/sh

export QUICKLISP_ENV_SETUP="$HOME/workspace/lisp/quicklisp_env/dummy/setup.lisp" \
       CONFIG_FILE="./twitag.conf"

sbcl --noinform \
     --load "$QUICKLISP_ENV_SETUP" \
     --eval "(ql:quickload :twitag)" \
     --eval "(vom:config :twitag :debug)" \
     --eval "(vom:config :twitag/db :debug)" \
     --eval "(twitag:main \"$CONFIG_FILE\")" \
     --quit
