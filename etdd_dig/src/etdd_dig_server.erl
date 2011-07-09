%%%-------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc.  All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Server to find and load the source code files.
%%% @since v0.0.1
%%% @end
%%%-------------------------------------------------------------------
-module(etdd_dig_server).
-copyright("(c) 2011, DuoMark International, Inc.  All rights reserved").
-author(jayn).

-behaviour(gen_server).

%% API
-export([start_link/0, load_file/1, loaded_file/2, find_file/1, files_loaded/0]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% spawned functions
-export([delve/1]).

-include("etdd_source.hrl").

-define(SERVER, ?MODULE).
-define(DELVE_SUPERVISOR, etdd_dlv_sup).

-type now() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type file_snap() :: {now(), string}.

-record(dig_state, {
          files_loaded = [] :: list(file_snap()),
          delve_workers = ets:new(dw,[]) :: ets:tid()
         }).


%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()}.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).


%% Asynchronous requests...
-spec load_file(string()) -> ok.
-spec loaded_file(string(), pid()) -> ok.

load_file(File) ->        gen_server:cast(?SERVER, {load_file, File}).
loaded_file(File, Pid) -> gen_server:cast(?SERVER, {loaded_file, File, Pid}).


%% Synchronous requests...
-spec files_loaded() -> list(file_snap()).
-spec find_file(string()) -> pid() | none.

files_loaded() ->  gen_server:call(?SERVER, files_loaded).
find_file(File) -> gen_server:call(?SERVER, {find_file, File}).


%%%===================================================================
%%% init, terminate, code_change callbacks
%%%===================================================================

-spec init({}) -> {ok, #dig_state{}}.
-spec terminate(atom(), #dig_state{}) -> ok.
-spec code_change(string(), #dig_state{}, any()) -> {ok, #dig_state{}}.

init({}) -> {ok, #dig_state{}}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Reason, #dig_state{delve_workers=DwTable}) ->
    ets:delete(DwTable),
    ok.


%%%===================================================================
%%% handle message callbacks
%%%===================================================================

-type call_rqst() :: files_loaded | {find_file, string()} | any().
-type call_reply() :: {files_loaded, list(file_snap())} | ok.
-spec handle_call(call_rqst(), {pid(), reference()}, #dig_state{})
                 -> {reply, call_reply(), #dig_state{}}.

%% Return the set of files loaded and when they were loaded.
handle_call(files_loaded, _From, #dig_state{files_loaded=FL} = State) ->
    {reply, {files_loaded, FL}, State};

%% Find the pid corresponding to a previously loaded source file.
handle_call({find_file, File}, _From, #dig_state{delve_workers=DwTable} = State) ->
    Reply = case ets:lookup(DwTable, File) of
                [] ->            none;
                [{File, Pid}] -> Pid
            end,
    {reply, {find_file, Reply}, State};

%% Ignore all other calls.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.


-type cast_rqst() :: {load_file, string()}
                   | {loaded_file, string(), pid()}
                   | any().
-spec handle_cast(cast_rqst(), #dig_state{}) -> {noreply, #dig_state{}}.

%% Spawn a process to load a source file and hand it to a delve worker.
handle_cast({load_file, File},
            #dig_state{files_loaded=FilesLoaded} = State) ->
%%    proc_lib:spawn_link(?MODULE, delve, [File]),
    delve(File),
    NewFiles = [ {now(), File} | FilesLoaded ],
    {noreply, State#dig_state{files_loaded=NewFiles}};

%% Insert a newly spawned delve worker {File, Pid} pair into an ets table.
handle_cast({loaded_file, File, Pid},
            #dig_state{delve_workers=DwTable} = State) ->
    ets:insert(DwTable, {File, Pid}),
    {noreply, State};

%% Ignore other cast requests.
handle_cast(_Msg, State) ->
    {noreply, State}.


-spec handle_info(any(), #dig_state{}) -> {noreply, #dig_state{}}.

handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% Spawned internal functions
%%%===================================================================

%% Load and transfer data to a delve worker.
-spec delve(string()) -> ok.

delve(File) ->
    case file:read_file(File) of
        {error, Reason} ->
            error_logger:error_msg("~p:delve/1 cannot read ~s: ~p~n",
                                   [?MODULE, File, Reason]);
        {ok, RawSource} ->
            NL = list_to_binary(io_lib:nl()),
            Lines = binary:split(RawSource, NL, [global]),
            case ?DELVE_SUPERVISOR:analyze_source(skim_lines(File, Lines)) of
                {ok, Pid} -> loaded_file(File, Pid);
                {error, Problem} ->
                    error_logger:error_msg("~p:analyze_source error: ~p~n",
                                           [?MODULE, Problem])
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec skim_lines(string(), list(binary())) -> #etdd_src{}.

skim_lines(File, Lines) ->
    skim_lines(File, Lines, Lines, 1, [], [], [], 0, 0).

skim_lines(File, [], Lines, _LineNr, White, Comments, Directives,
           Module, Behaviour) ->
    TupleLines = list_to_tuple(Lines),
    #etdd_src{
               file = File,
               line_count = tuple_size(TupleLines),
               lines = TupleLines,
               whitespace = list_to_tuple(lists:reverse(White)),
               comments = list_to_tuple(lists:reverse(Comments)),
               directives = list_to_tuple(lists:reverse(Directives)),
               module = Module,
               behaviour = Behaviour
             };
skim_lines(File, [H|T], Lines, LineNr, White, Comments, Directives, Mod, Beh) ->
    case line_type(H) of
        whitespace -> skim_lines(File, T, Lines, LineNr+1, [LineNr | White], Comments, Directives, Mod, Beh);
        comment ->    skim_lines(File, T, Lines, LineNr+1, White, [LineNr | Comments], Directives, Mod, Beh);
        directive ->  skim_lines(File, T, Lines, LineNr+1, White, Comments, [LineNr | Directives], Mod, Beh);
        module ->     skim_lines(File, T, Lines, LineNr+1, White, Comments, [LineNr | Directives], LineNr, Beh);
        behaviour ->  skim_lines(File, T, Lines, LineNr+1, White, Comments, [LineNr | Directives], Mod, LineNr);
        _Other ->     skim_lines(File, T, Lines, LineNr+1, White, Comments, Directives, Mod, Beh)
    end.

%% Quick approximation for initial integration testing.
line_type(<<"-module", _Rest/binary>>)    -> module;
line_type(<<"-behaviour", _Rest/binary>>) -> behaviour;
line_type(<<"-", _Rest/binary>>)          -> directive;
line_type(<<"%", _Rest/binary>>)          -> comment;
line_type(<<>>)                           -> whitespace;
line_type(_Other)                         -> other.
