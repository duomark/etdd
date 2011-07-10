ETDD - (Eleven, Twelve, Dig and Delve)
======================================

ETDD is a visual browser of erlang code. It relies on YAWS to provide a web interface of the dynamically constructed code model. The model is represented using processes, with at least one process per source file. A user can interact with the model to transform the view.

While more interaction was envisioned, the initial capability only allows a flexible table display of source files loaded, behaviour type, lines of code and relative percentages of whitespace, comments and directives (for example, '-define' or '-include').


The Grand Scheme:
-----------------

Future plans are to use gproc for each source code file process. This allows querying and filtering of gproc attributes to choose the subset of code to ddisplay on the webpage.

The following features are anticipated:

  * More Isotope dynamic animation
  * Sorting by source code attributes (LOC, Percentages, Names, etc.)
  * Elimination of table entries (processes) that are no longer needed
  * Interaction with a source code summary to obtain more information:
     * Menu of choices: analyze functions, records, messaging
     * Spawns a process for each action to delve deeper into the code
     * Draw a hierarchy of application and supervisor processes
     * Draw a graph of message interactions among modules


Components used:
----------------

  * rebar (github.com/basho/rebar)
  * yaws-1.90 (github.com/klacke/yaws)
  * jquery-1.6.2.js
  * jquery.isotope


Software Organization:
----------------------

There are 4 main components:

  * dig_and_delve (top level application)
  * etdd_dig (included_application for loading source code)
  * etdd_dlv (included application for analyzing loaded source code)
  * etdd_yaws (included application for embedding yaws into this app)


Compiling:
----------

The top level directory has a Makefile which relies on rebar to do a lot of work.  The typical sequence of compilation should be:

  * make clean all     (builds all src dirs to produce beam files)
  * make dialyze       (run dialyzer checks on the code)
  * make relclean rel  (generate the shell cmd line tool)

You can also use the following commands:

  * make gc         (removes all files with a trailing '~')
  * make realclean  (removes all dependencies plus does everything that 'make clean' does)
  * make deps       (build the local dependencies, but 'make all' will do this automatically)


Running:
--------

Do the following to start the server and view source code summary data:

  * cd to the top level directory of your installation (e.g., 'cd ~/Git/etdd')
  * rel/dig_and_delve/bin/dig_and_delve console (starts an embedded erlang node running the server)
  * Go to http://localhost:8888/ in your browser (HTML5, CSS3, JQuery, Isotope.js)


Command line execution:

Do the following to see results in the erlang shell:

  * cd to the top level directory of your installation (e.g., 'cd ~/Git/etdd')
  * rel/dig_and_delve/bin/dig_and_delve console (starts an embedded erlang node running the server)

Now executing the following commands in the shell for a sampling of features:

  1> [etdd_dig_server:load_src_dir("../../" ++ D) || D <- ["dig_and_delve/src/","etdd_dig/src/","etdd_dlv/src/"]].

  > ok

  2> etdd_dig_server:files_loaded().

  > Shows a tuple of {files_loaded, [{NowTime, FullFileName}, ...]}

  3> [etdd_dig_server:find_file(element(2,T)) || T <- element(2,v(2))].

  > Shows a list of [{find_file, Pid}, ...] with one entry per file

  4> [etdd_dlv_server:summary(element(2,P)) || P <- v(3)].

  > Shows a list of [{summary, erl | app, PropList}, ...]
  > Each entry shows statistics about the source file

The same request sequence can be repeated, but at the first step use load_app_src_dir instead of load_src_dir, giving an analysis of .app.src files.

At any point you can run 'appmon:start()' to look at a hierarchy of the processes currently active.
