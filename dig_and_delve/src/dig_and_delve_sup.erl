-module(dig_and_delve_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, ARGS), {I, {I, start_link, ARGS}, permanent, 5000, worker, [I]}).
-define(SUPER(I, ARGS), {I, {I, start_link, ARGS}, permanent, infinity, supervisor, [I]}).

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
    YawsSup = ?SUPER(etdd_yaws_sup, []),
    DigServer = ?CHILD(etdd_dig_sup, []),
    DelveServer = ?CHILD(etdd_dlv_sup, []),
    {ok, { {one_for_one, 5, 10},
           [YawsSup, DigServer, DelveServer]} }.

