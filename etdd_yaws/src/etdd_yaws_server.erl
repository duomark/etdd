-module(etdd_yaws_server).

-export([start_link/0, run/0]).

start_link() ->
    {ok, proc_lib:spawn_link(?MODULE, run, [])}.

run() ->
    Id = "yaws_embedded",
    Docroot = "priv/",
    GconfList = [{id, Id}],
    SconfList =
        [
         %% HTTP listener...
         {port, 8888},
         {servername, "yon"},
         {listen, {0,0,0,0}},
         {docroot, Docroot}
        ],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(etdd_yaws_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.

