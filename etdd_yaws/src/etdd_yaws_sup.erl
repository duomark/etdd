-module(etdd_yaws_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).

-spec start_link() -> {ok, pid()}.
-spec init(Args::{}) -> {ok, any()}.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init({}) ->
    YawsServer = ?CHILD(etdd_yaws_server, worker),
    {ok, { {one_for_all, 0, 1}, [YawsServer]} }.
