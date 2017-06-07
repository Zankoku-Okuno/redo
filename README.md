Redo
====

Redo is a [design](http://cr.yp.to/redo.html) for a recursive build system that is both simpler and technologically superior to make.

If you're getting annoyed at how clumsy make and automake are, but are writing in a language that doesn't have its own build system, redo is for you.
Redo, although it seems like magic at first, is easy to learn, requring only the knowledge of a couple shell commands and some filename conventions, and is more robust and flexible than make.

There already exist a few implementatons of redo: in [shell](http://grosskurth.ca/papers/mmath-thesis.pdf), [Python](https://github.com/apenwarr/redo), and [C++](http://homepage.ntlworld.com/jonathan.deboynepollard/FGA/introduction-to-redo.html), and even a [live-coded tutorial](http://www.youtube.com/playlist?list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B) [implementation](https://github.com/jekor/redo), so why do we need another implementation?
The fact is, there are several tricky engineering choices that need to be made before the design becomes an implementation.
This implementation is an attempt to "do it right," incorporating the concerns and issues that have arisen in other implementations.
Although we'll try to maintain compatibility where we can, we specifically won't support the ill-advised decisions from other projects.


Usage
=====

Install from the [git repository](https://github.com/Zankoku-Okuno/redo).
Use cabal install to build the binary and put it in place, then `mk_redo_links.sh` creates appropriate symlinks.

```
git clone https://github.com/Zankoku-Okuno/redo.git
cd redo
cabal build
sudo cabal install
sudo sh mk_redo_links.sh
```

To enable redo for a project, use `redo-init` at the top-level project folder.
To rebuild a target file `sna.foo`, invoke `redo sna.foo`.
To rebuild only when necessary, invoke `redo-ifchange sna.foo`.
Redo builds a target like `sna.foo` by looking for a build script `sna.foo.do` or `default.foo` script (called a do-script) which is sister to the target;
it then runs that script, which should generate the target.
If `redo` or `redo-ifchange` is called from the build script, then those subsequent targets are marked as dependencies for the first.
You can pass multiple files to the various redo commands, and it will redo all of them.

Example
-------

In a new folder, try the following commands and see what happens:

```
redo-init
echo 'redo-ifchange sna.foo' > quux.do
echo 'cat sna.foo' >> quux.do
echo 'echo "Goodbyte, and good luck."' >> quux.do
echo 'echo "Mesdames, messiuerres, bon soir."' > default.foo.do
redo quux
redo quux
echo '#nothing' >> default.foo.do
redo quux
```

Debugging is turned on at the moment, so it shouldn't be too hard to trace what's going on.
Play around with the idea, or read on for more details.
TODO At some point, we'll have a larger example to show off.

Overview
-------

For each file you pass in, redo will look for a do-script, which contains the instructions for building the file.
It will then run that script, redirecting stdout to a temporary file, and when it completes successfully, the newly-generated file will be moved into place, overwriting any older version only after we know we have a good result.

A do-script might require other files to also be regenerated.
The best thing to do is to call `redo-ifchange`.
The difference from `redo` is that the ifchange version will skip any files that are already up-to-date.

How does redo track whether a file needs to be rebuilt?
Let's say you run `redo{,-ifchange}` on a file which we'll call the 'parent.'
When the parent's build script calls `redo{,-ifchange}` on some files, which we'll call the 'children,' the children are registered as dependencies for the parent.
When you next run `redo-ifchange` on the parent, all the dependency information from the last run will be used to check if any (transitive) dependency was altered.
If there was no change, then we don't run the do-script, and you get an incremental build.

Details
-------

TODO oh, look in the code! I'm bored of writing docs right now.

Source Files and Built Files
----------------------------

If there's a do-script available for a target file, then it must be that the file was or should be built (by that script).
Whether the target exists is immaterial: if it exists, it might be out-of-date, and a file that doesn't exist is certainly out-of-date.

A target file is a source file if a) it exists and b) there's no do-script for it.
That is, when we run out if things to do, we stop trying to do things, which only makes sense.

If a file neither exists nor has a build script, we can't help you.
The build system can't generate your source code for you.

Engineering
===========

What's a Project?
-----------------

A project is a folder that directly contains a `.redo` file/folder.
We don't really want to specify whether it's a file or folder, but it is a folder, the structure of which might change radically as we move to a real release.

When you run `redo`, we'll figure out what project you're in by looking in the current directory for the existence of `.redo`, and then traverse parent directories until we find a `.redo`.
This means you'll have to be somewhere inside the project folder to run redo, but that's exactly what your VCS does, too.
Simply put, when you're making a new project, now you type `redo-init` along with your usual `git init` or `hg init`.

Source Files
------------

We are the only implementation I know of that has the correct base case for the recursion:
a source file is a target that has no build script.
A non-existent do-script trivally references no files, and so its target must have no dependencies.
What else do you call a file without dependencies?

Other systems have chosen differently, by talking about what's in the databse or not in the database, but they are wrong.
The user shouldn't need to know about what gets tracked in some magical dependency database.
Instead, it should be enough that a user examines their do-scripts to see what files are referenced from where.

Default Scripts
---------------

We don't use default scripts when a target has no extension.

We only look in the same directory for a default script.
Looking higher in the directory tree makes the build process dependent on whether the project is placed inside other projects.
If you grab a default script from a different project, that could cause all sort of "hilarious" errors.

TODO we hope to allow the user to put default scripts in the project database. Those scripts would be accessible from anywhere in the project.

Scripting Language
------------------

We aren't going to duplicate `#!` processing, since that's full of race conditions that (as far as I know) have to be managed in the kernel.
That said, we don't want to tie the user to one language, even one as universal as `sh`.
After all, I still want to get set up with [rc](http://plan9.bell-labs.com/sys/doc/rc.html), and no doubt others already are.

A simple solution would be to make your do-scripts executable, and just make the OS run them directly.
However, as it has been noted elsewhere, that strategy means that there end up being lots of executables in the project, and the user may not know whether to run them directly (they shouldn't!).

The question is: does it make sense to have multiple languages implementing your build system?
No. No it does not.
There's a way to configure your build shell and any arguments you want to pass to it.
TODO the interface for configuring is unstable. Probably easiest to use the default `sh -xe` for now, unless your heart is set elsewise.

TODO in the future, we might allow executables to be run directly when no shell has been configured. We'd keep a default-configured shell, though.
Ideally, we end up with a multi-level configuration system like git's.

Contributing
============

Submit an [issue](https://github.com/Zankoku-Okuno/redo/issues), or a [pull request](https://github.com/Zankoku-Okuno/redo/pulls) on Github.
I have no coding standards for this hacked-together project.
Frankly, it shows just how well Haskell is suited to small projects; take that Lisp!

There's stuff missing, and some of it (multithreading across many processes, use a sqlite db, logging) because I need to learn the Haskell libraries involved.

If there's a question of (or answer to) design, though, it's better to talk before implementing.

Limitations
===========

Phony Targets
-------------
If a build script, like `all.do`, shouldn't directly generate a file, it will get a zero-byte file anyway, `all`, that will cause pain.
I have a plan, though: if there was no output, and the target didn't exist to begin with, then we just remove the zero-byte output.

Build Ratios
------------

It's easy for a build script to specify many dependencies, but shouldn't there be a way for one script to generate many files?
Also, the output files should be updated all atomically.
And each of the output files should have their dependencies tracked correctly.

I'll see what I can do.

Separate Directories
--------------------

You can't specify separate source and build directories.
I'd like to fix this, probably with a configuration option somewhere.
Ideally, I'd like to allow to have three different directory trees for source files, build scripts, and build results.

Multi-targeted Builds
---------------------

I think a solution to the Separate Directories limitation will imply a solution to this problem as well.
Simply set up three projects, all using the same source and do-script trees, but with different build trees.
The only question is one of getting the config files to be different for each build.
Honestly, the easiest way may be to set an environment variable before invoking `redo`, and generating different config files based on that as the first step of the build.

Generated Do-Scripts
--------------------

I find the use-case a bit unlikely, since redo is more flexible than make to begin with, but, what if a user wants to generate their do-scripts?
At the moment, you would have to engage in a multi-step process, something like `redo all.do`, `redo all`.
Even if a target depends on a do-script, it doens't currently depend transitively on its script, so some dependency information might get lost there.
That said, automake also doesn't track this dependency information, so we aren't losing anything.
It may not be worth it to complicate the logic by looking for the do-script of a do-script of a do-script of a do-script of a do-script...

Windows Support
---------------

I don't have a Windows machine, so don't come crying to me; come to me with pull requests.

redo-ifcreate
-------------

The only difference I can spot between `redo-ifchange` and `redo-ifcreate` is that the ifcreate variant doens't try to run a do-script of the file doesn't already exist.
Since just about every other build system does not handle this case, I'm not losing anything w.r.t. them if I ignore this case.
Maybe I should add it in, but I want to know a real use-case, and not just "oh, the `PATH` changed", because you should be using some sort of sandbox anyway.

That's right: my recommendation is to explicitly note your external dependencies in a file.
Use that file to generate precise include arguments to your compiler.
Make sure that file is a dependeny for everything, and then if you ever change your external dependencies, your project will rebuild correctly.
