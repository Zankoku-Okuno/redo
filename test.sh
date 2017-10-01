#!/bin/bash

export PATH=${PWD}/.cabal-sandbox/bin:${PATH}

# FIXME make it so I don't have to redo-init
cabal install && (
    cd test;
    rm -rf .redo/redo.sqlite build/;
    redo-init;
    echo FIRST;
    redo-always all;
) && tree -a test && cat test/build/subdir/foo.bar.baz && sqlite3 test/.redo/redo.sqlite 'select * from file; select * from build; select * from dependency;' && (
    cd test;
    echo SECOND;
    redo-always all;
) && sqlite3 test/.redo/redo.sqlite 'select * from file; select * from build; select * from dependency;'

