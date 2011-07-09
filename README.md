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

  1> etdd_dig_server:load_file("/Users/jay/Git/etdd/dig_and_delve/src/dig_and_delve_app.erl").

  > ok

  2> etdd_dig_server:files_loaded().

  > {files_loaded,[{{1310,208701,416696},
  >               "/Users/jay/Git/etdd/dig_and_delve/src/dig_and_delve_app.erl"}]}

  3> etdd_dig_server:find_file("/Users/jay/Git/etdd/dig_and_delve/src/dig_and_delve_app.erl").

  > {find_file,<0.76.0>}

  4> Pid = element(2, v(3)).

  > <0.76.0>

  5> etdd_dlv_server:src_line_count(v(4)).

  > {src_line_count,26}

  6> etdd_dlv_server:src_lines(v(4)).

  > {src_lines,{<<"-module(dig_and_delve_app).">>,<<>>,
  >             <<"-behaviour(application).">>,<<>>,
  >             <<"%% Application callbacks">>,
  >             <<"-export([start/0, start/2, stop/1]).">>,<<>>,
  >             <<"%% ===================================================================">>,
  >             <<"%% Application callbacks">>,
  >             <<"%% ================================================================="...>>,
  >             <<>>,<<"-spec start() -> {ok, pid()}.">>,
  >             <<"-spec start(any(), any()) -> {ok, pid()}.">>,
  >             <<"-spec stop([]) -> ok.">>,<<>>,
  >             <<"%% @doc Start the application's root supervi"...>>,
  >             <<"start() ->">>,<<"    dig_and_delve_sup:start_link().">>,
  >             <<>>,<<"%% @doc Start the applicatio"...>>,
  >             <<"start(_StartType, _Start"...>>,<<"    dig_and_delve_su"...>>,
  >             <<>>,<<"%% @doc Stop"...>>,<<"stop(_St"...>>,<<>>}}

