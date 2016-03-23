#!/bin/bash
#----------------------------------------------------------------------------
# DATE        : 2016, March 23rd
# AUTHOR      : Paolo Viotti
# DESCRIPTION : Bash script to compile and run Conver tests
#----------------------------------------------------------------------------

usage() {
  echo "Usage: ./`basename $0` make|clean|run <nc> <store>|proper"
  echo "    make: compile Conver"
  echo "    clean: remove compiled artifacts"
  echo "    proper: run PropEr test on mock datastore"
  echo "    run <nc> <store>: run Conver"
  echo "        nc: number of clients"
  echo "        store: datastore under test (e.g., mock, riak, zk)"
}


if [ $# -eq 0 ]
then
  usage
  exit 1
fi

case $1 in
make)
    ./rebar3 compile
    ;;
clean)
    ./rebar3 clean
    ;;
proper)
    ./rebar3 compile >/dev/null
    erl -pa ./_build/default/lib/*/ebin -eval "conver_statem:test()." -s init stop -noshell
    ;;
run)
    if [ $# -ne 3 ]
    then
        usage
        exit 1
    else
        ./rebar3 compile >/dev/null
        erl -pa ./_build/default/lib/*/ebin -noshell -eval "conver:main($2, $3)." -s init stop
    fi    
    ;;
*)
    usage
    exit 1
    ;;
esac



