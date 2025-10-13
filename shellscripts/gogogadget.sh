#!/bin/sh

gogogadget() {
    if [ $# -eq 0 ]; then
        emacsclient -nc .
    else
        emacsclient -nc $1
    fi
}
