#!bin/bash

if [ $# -eq 0 ]
then
    fuser -k -n tcp 8080
else
    fuser -k -n tcp $1
fi

stack build && stack exec lucid-htmx-ex1-exe
