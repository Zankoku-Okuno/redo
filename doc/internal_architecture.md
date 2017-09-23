# Internal Architecture

## Domain Model

There are three levels of information needed to perform redo tasks.
First, the project needs to be identified.
Then, a build needs to be identified.
Finally, a target needs to be identified.

Currently, we error out if no project can be found.
It could be okay to instead initialize a project in the working directory in this case.
Projects hold overall configuration:
    * where is the database?
    * where are source/output/script files located?
    * TODO what is the default scripting language?

A build is created when redo is executed, and any redo instances forked from there use that build.
The build is a random identifier associated with:
    * TODO any logging that needs to be captured during the build
    * TODO any information required to coordinate multi-threading
    * TODO any command line arguments passed with the initial invocation
TODO Probably, only one build should be running at any given time. We still need to make a decision about what to do when someone does something dumb like `redo &; redo`. My first instince would be to either join in or (more annoyingly) just fail to start.

A target is a description of an (intermediate) artifact.
It includes:
    * The parent file who asked for a redo of this target (if any).
    * The locations of the source and output.
    * The script which would be used to build it (if any).
    * The locations of any scripts that could take priority over the known script if they were to exist.
    * Whether the target is a source or (generated) output file.

## State Database

NOTE the database is not just the sqlite db; it's everything under `.redo` (e.g. config files)
TODO there should be a magic file in the database so we know when we've found a database we can understand

## Build Process

TODO

