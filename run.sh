#!/bin/sh

export QUICKLISP_ENV_SETUP="$HOME/workspace/lisp/quicklisp_env/dummy/setup.lisp" \
       CONSUMER_KEY="$1" \
       CONSUMER_SECRET="$2" \
       DB_FILE="$3"

sbcl --noinform \
     --load "$QUICKLISP_ENV_SETUP" \
     --eval "(ql:quickload :twitag)" \
     --eval "(vom:config :twitag :debug)" \
     --eval "(vom:config :twitag/twitter-api :debug)" \
     --eval "(twitag:main \"$CONSUMER_KEY\" \"$CONSUMER_SECRET\" \"$DB_FILE\")" \
     --quit
