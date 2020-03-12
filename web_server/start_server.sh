#!/bin/bash

HOOGLE_DB=/etc/resolv.conf
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

# start flask server
flask run
