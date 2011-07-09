ETDD - (Eleven, Twelve, Dig and Delve)
======================================

ETDD is a visual browser of erlang code. It relies on YAWS to provide a web interface of the dynamically constructed code model. The model is represented using gproc processes, with at least one process per source file. A user can interact with the model to cause individual processes to transform their view to the wishes of the user.

Components used:

  * rebar
  * yaws-1.90 (github.com/klacke/yaws)
  * gproc (github.com/esl/gproc)


