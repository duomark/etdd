%%%-------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Supervisor for delve workers. Each source code file to be
%%%   analyzed should be handled by a separate worker by launching
%%%   a new child of this supervisor.
%%% @since v0.0.1
%%% @end
%%%-------------------------------------------------------------------
-module(etdd_dlv_sup).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(supervisor).

%% API
-export([start_link/0, analyze_source/1, get_live_workers/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("etdd_dig/include/etdd_source.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, ARGS), {I, {I, start_link, ARGS}, permanent, 5000, worker, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.
-spec get_live_workers() -> [pid()].
-spec analyze_source([#etdd_src{}]) -> {ok, pid()} | {error, any()}.

%% @doc Start the delve workers' supervisor.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {}).

%% @doc Create a delve worker to analyze the list of code for a source file.
analyze_source(SourceData) ->
    supervisor:start_child(?SERVER, [SourceData]).

%% Horribly slow messaging hack to be replaced by gproc later.
%% @doc Find the delve worker that is handling a particular source file.
get_live_workers() ->
    [Child || {_Id, Child, _Type, _Modules}
                  <- supervisor:which_children(?SERVER),
              is_process_alive(Child)].
    

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(Args::{}) -> {ok, any()}.

init({}) ->
    DelveServer = ?CHILD(etdd_dlv_server, []),
    {ok, { {simple_one_for_one, 5, 10}, [DelveServer]} }.

