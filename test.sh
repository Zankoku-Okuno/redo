#!/bin/bash

export PATH=${PWD}/.cabal-sandbox/bin:${PATH}

# FIXME make it so I don't have to redo-init
cabal install && (cd test; rm -rf .redo/redo.sqlite build/; redo-init && redo-always subdir/foo.bar.baz) && tree -a test && cat test/build/subdir/foo.bar.baz && sqlite3 test/.redo/redo.sqlite 'select * from file; select * from dependency;'

