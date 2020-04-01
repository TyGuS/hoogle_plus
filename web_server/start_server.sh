#!/bin/bash

HOOGLE_DB=~/.hoogle/default-haskell-5.0.17.hoo
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
export FLASK_RUN_PORT=3000

# start flask server
flask run
