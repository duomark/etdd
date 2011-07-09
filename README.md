ETDD - (Eleven, Twelve, Dig and Delve)
======================================

ETDD is a visual browser of erlang code. It relies on YAWS to provide a web interface of the dynamically constructed code model. The model is represented using gproc processes, with at least one process per source file. A user can interact with the model to cause individual processes to transform their view to the wishes of the user.

Components used:
----------------

  * rebar
  * yaws-1.90 (github.com/klacke/yaws)
  * gproc (github.com/esl/gproc)


Software Organization:
----------------------

There are 3 main components:

  * dig_and_delve (top level application)
  * etdd_dig (included_application for loading source code)
  * etdd_dlv (included application for analyzing loaded source code)

There is also an included application 'etdd_yaws' which is used to deliver a web-based interface to the model of loaded source code.


Compiling:
----------

The top level directory has a Makefile which relies on rebar to do a lot of work.  The typical sequence of compilation should be:

  * make clean all     (builds all src dirs to produce beam files)
  * make dialyze       (run dialyzer checks on the code)
  * make relclean rel  (generate the shell cmd line tool)

You can also use the following commands:

  * make gc         (removes all files with a trailing '~')
  * make realclean  (removes all dependencies and everything that 'make clean' does)
  * make deps       (build the local dependencies)

Running:
--------

cd to the top level directory and perform the following commands:

  rel/dig_and_delve/bin/dig_and_delve console

  1> [etdd_dig_server:load_dir("../../" ++ D) || D <- ["dig_and_delve/src/","etdd_dig/src/","etdd_dlv/src/"]].

  > ok

  2> etdd_dig_server:files_loaded().

  > {files_loaded,[{{1310,252709,226305},
  >                 "../../etdd_dlv/src/etdd_dlv_sup.erl"},
  >                {{1310,252709,226775},
  >                 "../../etdd_dlv/src/etdd_dlv_server.erl"},
  >                {{1310,252709,227120},"../../etdd_dlv/src/etdd_dlv_app.erl"},
  >                {{1310,252709,224991},"../../etdd_dig/src/etdd_dig_sup.erl"},
  >                {{1310,252709,225640},
  >                 "../../etdd_dig/src/etdd_dig_server.erl"},
  >                {{1310,252709,225842},"../../etdd_dig/src/etdd_dig_app.erl"},
  >                {{1310,252709,224110},
  >                 "../../dig_and_delve/src/dig_and_delve_sup.erl"},
  >                {{1310,252709,224535},
  >                 "../../dig_and_delve/src/dig_and_delve_app.erl"}]}

  3> [etdd_dig_server:find_file(element(2,T)) || T <- element(2,v(2))].

  > [{find_file,<0.84.0>}, {find_file,<0.85.0>}, {find_file,<0.86.0>},
  >  {find_file,<0.81.0>}, {find_file,<0.82.0>}, {find_file,<0.83.0>},
  >  {find_file,<0.79.0>}, {find_file,<0.80.0>}]

  4> [etdd_dlv_server:summary(element(2,P)) || P <- v(3)].

  [{summary,[{behav_type,supervisor},
           {mod_type,etdd_dlv_sup},
           {get_file,"../../etdd_dlv/src/etdd_dlv_sup.erl"},
           {src_line_count,65}]},
   {summary,[{behav_type,gen_server},
           {mod_type,etdd_dlv_server},
           {get_file,"../../etdd_dlv/src/etdd_dlv_server.erl"},
           {src_line_count,127}]},
   {summary,[{behav_type,application},
           {mod_type,etdd_dlv_app},
           {get_file,"../../etdd_dlv/src/etdd_dlv_app.erl"},
           {src_line_count,26}]},
   {summary,[{behav_type,supervisor},
           {mod_type,etdd_dig_sup},
           {get_file,"../../etdd_dig/src/etdd_dig_sup.erl"},
           {src_line_count,36}]},
   {summary,[{behav_type,gen_server},
           {mod_type,etdd_dig_server},
           {get_file,"../../etdd_dig/src/etdd_dig_server.erl"},
           {src_line_count,225}]},
   {summary,[{behav_type,application},
           {mod_type,etdd_dig_app},
           {get_file,"../../etdd_dig/src/etdd_dig_app.erl"},
           {src_line_count,26}]},
   {summary,[{behav_type,supervisor},
           {mod_type,dig_and_delve_sup},
           {get_file,"../../dig_and_delve/src/dig_and_delve_sup.erl"},
           {src_line_count,36}]},
   {summary,[{behav_type,application},
           {mod_type,dig_and_delve_app},
           {get_file,"../../dig_and_delve/src/dig_and_delve_app.erl"},
           {src_line_count,26}]}]

