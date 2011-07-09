-module(etdd_dlv_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

-spec start() -> {ok, pid()}.
-spec start(any(), any()) -> {ok, pid()}.
-spec stop([]) -> ok.

%% @doc Start the application's root supervisor in erl listener.
start() ->
    etdd_dlv_sup:start_link().

%% @doc Start the application's root supervisor from boot.
start(_StartType, _StartArgs) ->
    etdd_dlv_sup:start_link().

%% @doc Stop the application.
stop(_State) -> ok.
