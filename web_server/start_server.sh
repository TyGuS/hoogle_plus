#!/bin/bash

HOOGLE_DB=~/.hoogle/default-haskell-5.0.17.hoo
FLASK=~/anaconda3/envs/hplus_env/bin/flask
if test -f "$HOOGLE_DB"; then
    echo "$HOOGLE_DB exist"
else
    # install and generate hoogle database
    stack install hoogle
    hoogle generate
fi

# set flask environment variables
export FLASK_APP=hplus
export FLASK_ENV=development
export FLASK_RUN_PORT=5000

# start flask server
$FLASK run -h 0.0.0.0
