#!/bin/sh

export QUICKLISP_ENV_SETUP="$HOME/workspace/lisp/quicklisp_env/dummy/setup.lisp" \
       COVERAGE_REPORT_DIR="./.cover/"

sbcl --noinform \
     --load "$QUICKLISP_ENV_SETUP" \
     --eval "(ql:quickload :twitag-test)" \
     --eval "(test-with-coverage:main '(:twitag) \"$COVERAGE_REPORT_DIR\")"\
     --quit
