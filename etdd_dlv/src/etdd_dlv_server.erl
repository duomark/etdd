%%%-------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   A delve server receives a source code data record and can do
%%%   any in depth analysis requested of the source code.
%%% @since v0.0.1
%%% @end
%%%-------------------------------------------------------------------
-module(etdd_dlv_server).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(gen_server).

%% API
-export([start_link/1, get_file/1, src_line_count/1, src_lines/1,
         mod/1, behav/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("etdd_dig/include/etdd_source.hrl").

-record(dlv_state, {
          src_code :: #etdd_src{}
         }).

%% Macro hacks to clean up repetitive code
-define(HC(RQST, FLD, VAL),     handle_call(RQST, _From, #dlv_state{src_code=#etdd_src{FLD=VAL}} = State)).
-define(HCL(RQST, FLD, LINENR), handle_call(RQST, _From, #dlv_state{src_code=#etdd_src{FLD=LINENR, lines=Src}} = State)).


%%%===================================================================
%% External function API
%%%===================================================================

-spec start_link(#etdd_src{}) -> {ok, pid()}.

start_link(SourceData) ->
    gen_server:start_link(?MODULE, {SourceData}, []).


-spec get_file(pid()) -> {get_file, string()}.
-spec src_line_count(pid()) -> {src_line_count, non_neg_integer()}.
-spec src_lines(pid()) -> {src_lines, tuple(binary())}.

get_file(Pid) ->       gen_server:call(Pid, get_file).
src_line_count(Pid) -> gen_server:call(Pid, src_line_count).
src_lines(Pid) ->      gen_server:call(Pid, src_lines).
mod(Pid) ->            gen_server:call(Pid, mod).
behav(Pid) ->          gen_server:call(Pid, behav).


%%%===================================================================
%%% init, terminate, code_change callbacks
%%%===================================================================

-spec init({#etdd_src{}}) -> {ok, #dlv_state{}}.
-spec terminate(atom(), #dlv_state{}) -> ok.
-spec code_change(string(), #dlv_state{}, any()) -> {ok, #dlv_state{}}.

init({#etdd_src{} = SourceData}) ->
    {ok, #dlv_state{src_code=SourceData}}.

%% Unused gen_stream exported callbacks.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% handle message callbacks
%%%===================================================================

-type call_rqst()  :: get_file | src_line_count | src_lines
                    | module | behaviour.
-type call_reply() :: {get_file, string()}
                    | {src_line_count, non_neg_integer()}
                    | {src_lines, #etdd_src{}}
                    | {module, non_neg_integer(), binary()}
                    | {behaviour, non_neg_integer(), binary()}
                    | ignored.

-spec handle_call(call_rqst(), {pid(), reference()}, #dlv_state{})
                 -> {reply, call_reply(), #dlv_state{}}.

%% Report simple field information about the current code analysis.
?HC(get_file,       file, File) ->        {reply, {get_file, File},        State};
?HC(src_line_count, line_count, Count) -> {reply, {src_line_count, Count}, State};
?HC(src_lines,      lines, Lines) ->      {reply, {src_lines, Lines},      State};

%% Report information about the current code analysis involving a specific line of code.
?HCL(mod,   module, LineNr) ->    {reply, {mod,   LineNr, element(LineNr, Src)}, State};
?HCL(behav, behaviour, LineNr) -> {reply, {behav, LineNr, element(LineNr, Src)}, State};
    
%% Ignore all other requests silently.
handle_call(_Request, _From, State) -> {reply, ignored, State}.


-spec handle_cast(any(), #dlv_state{}) -> {noreply, #dlv_state{}}.
-spec handle_info(any(), #dlv_state{}) -> {noreply, #dlv_state{}}.

handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
