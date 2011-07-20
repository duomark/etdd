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
-export([start_link/1, code_type/1, start_module/1,
         get_file/1, src_line_count/1, src_lines/1,
         comm_count/1, white_count/1, directive_count/1,
         mod/1, mod_type/1, behav/1, behav_type/1,
         code_ratios/1, summary/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("etdd_dig/include/etdd_source.hrl").

-record(dlv_state, {
          src_code :: #etdd_src{} | #etdd_app_src{}
         }).


%%%===================================================================
%% External function API
%%%===================================================================

-spec start_link(#etdd_src{}) -> {ok, pid()}.

start_link(SourceData) ->
    gen_server:start_link(?MODULE, {SourceData}, []).


-spec code_type(pid()) ->       {code_type, app | erl}.
-spec get_file(pid()) ->        {get_file, string()}.
-spec src_lines(pid()) ->       {src_lines, tuple(binary())}.
-spec src_line_count(pid()) ->  {src_line_count, non_neg_integer()}.
-spec start_module(pid()) ->    {start_module, atom()} | ignored.

-spec mod(pid()) ->             {mod, non_neg_integer(), binary()}   | ignored.
-spec mod_type(pid()) ->        {mod_type, atom() | {}}              | ignored.
-spec behav(pid()) ->           {behav, non_neg_integer(), binary()} | ignored.
-spec behav_type(pid()) ->      {behav_type, atom() | {}}            | ignored.
-spec comm_count(pid()) ->      {comm_count, non_neg_integer()}      | ignored.
-spec white_count(pid()) ->     {white_count, non_neg_integer()}     | ignored.
-spec directive_count(pid()) -> {comm_count, non_neg_integer()}      | ignored.
-spec summary(pid()) ->         {summary, app | erl, list(tuple())}.
-spec code_ratios(pid()) ->     {code_ratios, list(tuple())}.

code_type(Pid) ->       gen_server:call(Pid, code_type).
get_file(Pid) ->        gen_server:call(Pid, get_file).
src_lines(Pid) ->       gen_server:call(Pid, src_lines).
src_line_count(Pid) ->  gen_server:call(Pid, src_line_count).
start_module(Pid) ->    gen_server:call(Pid, start_module).

mod(Pid) ->             gen_server:call(Pid, mod).
mod_type(Pid) ->        gen_server:call(Pid, mod_type).
behav(Pid) ->           gen_server:call(Pid, behav).
behav_type(Pid) ->      gen_server:call(Pid, behav_type).
comm_count(Pid) ->      gen_server:call(Pid, comm_count).
white_count(Pid) ->     gen_server:call(Pid, white_count).
directive_count(Pid) -> gen_server:call(Pid, directive_count).

summary(Pid) ->
    case code_type(Pid) of
        {code_type, app} ->
            {summary, app,
             [gen_server:call(Pid, Msg)
              || Msg <- [get_file, src_line_count, start_module]]};
        {code_type, erl} ->
            {code_ratios, PctAttrs} = code_ratios(Pid),
            {summary, erl,
             [gen_server:call(Pid, Msg)
              || Msg <- [behav_type, mod_type, get_file, src_line_count]]
             ++ PctAttrs}
    end.

code_ratios(Pid) ->
    Counts = [element(2, gen_server:call(Pid, Msg))
              || Msg <- [comm_count, white_count, directive_count]],
    {src_line_count, SLC} = gen_server:call(Pid, src_line_count),
    Pcts = [round(C / SLC * 100) || C <- Counts],
    Code = 100 - lists:sum(Pcts),
    {code_ratios,
     lists:zip([code_pct, comm_pct, white_pct, directive_pct], [Code | Pcts])}.


%%%===================================================================
%%% init, terminate, code_change callbacks
%%%===================================================================

-spec init({#etdd_src{} | #etdd_app_src{}}) -> {ok, #dlv_state{}}.
-spec terminate(atom(), #dlv_state{}) -> ok.
-spec code_change(string(), #dlv_state{}, any()) -> {ok, #dlv_state{}}.

init({#etdd_src{} = SourceData}) ->
    {ok, #dlv_state{src_code=SourceData}};
init({#etdd_app_src{} = SourceData}) ->
    {ok, #dlv_state{src_code=SourceData}}.

%% Unused gen_stream exported callbacks.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% handle message callbacks
%%%===================================================================

-type call_rqst()  :: code_type | get_file | src_lines | src_line_count
                    | start_module | mod | mod_type | behav | behav_type
                    | comm_count | white_count | directive_count.

-type call_reply() :: {code_type, app | erl}
                    | {get_file, string()}
                    | {src_lines, #etdd_src{}}
                    | {src_line_count, non_neg_integer()}
                    | {start_module, atom()}
                    | {mod, non_neg_integer(), binary()}
                    | {mod_type, atom() | {}}
                    | {behav, non_neg_integer(), binary()}
                    | {behav_type, atom() | {}}
                    | {comm_count, non_neg_integer()}
                    | {white_count, non_neg_integer()}
                    | {directive_count, non_neg_integer()}
                    | ignored.

-spec handle_call(call_rqst(), {pid(), reference()}, #dlv_state{})
                 -> {reply, call_reply(), #dlv_state{}}.

%% Macro hacks to clean up repetitive code
-define(HC_APP(RQST, FLD, VAL),     handle_call(RQST, _From, #dlv_state{src_code=#etdd_app_src{FLD=VAL}} = State)).
-define(HC_ERL(RQST, FLD, VAL),     handle_call(RQST, _From, #dlv_state{src_code=#etdd_src{FLD=VAL}} = State)).
-define(HCL_ERL(RQST, FLD, LINENR), handle_call(RQST, _From, #dlv_state{src_code=#etdd_src{FLD=LINENR, src_lines=Src}} = State)).

%% Indicate the type of the source code.
handle_call(code_type, _From, #dlv_state{src_code=#etdd_src{}} = State) ->     {reply, {code_type, erl}, State};
handle_call(code_type, _From, #dlv_state{src_code=#etdd_app_src{}} = State) -> {reply, {code_type, app}, State};

%% Report file and line count information for any code type.
?HC_APP(get_file,        file,         File) ->     {reply, {get_file,        File},  State};
?HC_APP(src_lines,       app_lines,    Lines) ->    {reply, {src_lines,       Lines}, State};
?HC_APP(src_line_count,  line_count,   Count) ->    {reply, {src_line_count,  Count}, State};
?HC_APP(start_module,    start_module, Mod) ->      {reply, {start_module,    Mod},   State};

?HC_ERL(get_file,        file,       File) ->       {reply, {get_file,        File},  State};
?HC_ERL(src_lines,       src_lines,  Lines) ->      {reply, {src_lines,       Lines}, State};
?HC_ERL(src_line_count,  line_count, Count) ->      {reply, {src_line_count,  Count}, State};

%% Report line counts and mod/behaviour types only for .erl files.
?HC_ERL(mod_type,        module_type,    Lines) ->  {reply, {mod_type,        Lines},     State};
?HC_ERL(behav_type,      behaviour_type, Lines) ->  {reply, {behav_type,      Lines},     State};
?HC_ERL(comm_count,      comments,       Comms) ->  {reply, {comm_count,      tuple_size(Comms)},  State};
?HC_ERL(white_count,     whitespace,     White) ->  {reply, {white_count,     tuple_size(White)},  State};
?HC_ERL(directive_count, directives,     Direct) -> {reply, {directive_count, tuple_size(Direct)}, State};

%% Report information about the current code analysis involving a specific line of code.
?HCL_ERL(mod,   module, LineNr) ->    {reply, {mod,   LineNr, element(LineNr, Src)}, State};
?HCL_ERL(behav, behaviour, LineNr) -> {reply, {behav, LineNr, element(LineNr, Src)}, State};
    
%% Ignore all other requests silently.
handle_call(_Request, _From, State) -> {reply, ignored, State}.


-spec handle_cast(any(), #dlv_state{}) -> {noreply, #dlv_state{}}.
-spec handle_info(any(), #dlv_state{}) -> {noreply, #dlv_state{}}.

handle_cast(_Msg, State) ->  {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
