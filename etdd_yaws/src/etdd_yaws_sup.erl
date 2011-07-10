%%%-------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   The etdd_yaws_sup supervisor restarts the embedded yaws server.
%%% @since v0.0.1
%%% @end
%%%-------------------------------------------------------------------
-module(etdd_yaws_sup).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {}).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

-spec init(Args::{}) -> {ok, any()}.

init({}) ->
    YawsServer = ?CHILD(etdd_yaws_server, worker),
    {ok, { {one_for_all, 5, 10}, [YawsServer]} }.
