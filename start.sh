#!/bin/bash
cd `dirname $0`

if [ "$1" == "-i" ]; then
    exec erl -sname switch -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s switch
else
    exec erl -sname switch -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -s switch -detached
fi

