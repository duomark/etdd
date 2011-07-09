%%%-------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Server to find and load the source code files.
%%% @since v0.0.1
%%% @end
%%%-------------------------------------------------------------------
-module(etdd_dlv_server).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE). 

-record(dlv_state, {}).


%%%===================================================================
%% External function API
%%%===================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


%%%===================================================================
%%% init, terminate, code_change callbacks
%%%===================================================================

-spec init({}) -> {ok, #dlv_state{}}.
-spec terminate(atom(), #dlv_state{}) -> ok.
-spec code_change(string(), #dlv_state{}, any()) -> {ok, #dlv_state{}}.

init({}) -> {ok, #dlv_state{}}.

%% Unused gen_stream exported callbacks.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% handle message callbacks
%%%===================================================================

-type call_rqst() :: any().
-spec handle_call(call_rqst(), {pid(), reference()}, #dlv_state{})
                 -> {reply, ok, #dlv_state{}}.

%% Interface for requesting benchmark runs.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-spec handle_cast(any(), #dlv_state{}) -> {noreply, #dlv_state{}}.
-spec handle_info(any(), #dlv_state{}) -> {noreply, #dlv_state{}}.

handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
