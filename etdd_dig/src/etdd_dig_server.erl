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
-export([start_link/0,
         load_app_src_dir/1, load_src_dir/1,
         load_src_file/1, loaded_file/2, find_file/1, files_loaded/0]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3,
         handle_call/3, handle_cast/2, handle_info/2]).

%% spawned functions
-export([delve_src/1, delve_app_src/1]).

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
-spec load_src_file(string()) -> ok.
-spec load_src_dir(string()) -> ok.
-spec load_app_src_dir(string()) -> ok.
-spec loaded_file(string(), pid()) -> ok.

load_src_file(File) ->    gen_server:cast(?SERVER, {load_src_file, File}).
load_src_dir(Dir) ->      gen_server:cast(?SERVER, {load_src_dir, erl, Dir}).
load_app_src_dir(Dir) ->  gen_server:cast(?SERVER, {load_src_dir, app, Dir}).
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


-type cast_rqst() :: {load_src_file, string()}
                   | {load_src_dir, app | erl, string()}
                   | {loaded_file, string(), pid()}
                   | any().
-spec handle_cast(cast_rqst(), #dig_state{}) -> {noreply, #dig_state{}}.

%% Spawn a delve worker process to analyze a source file.
handle_cast({load_src_file, File}, State) ->
    case file_type(File) of
        erl_file -> cast_file_load(erl_file, File, State);
        app_file -> cast_file_load(app_file, File, State);
        _Other -> {noreply, State}
    end;

%% Spawn a delve worker process to analyze each source file in a dir.
handle_cast({load_src_dir, SrcType, Dir},
            #dig_state{files_loaded=FilesLoaded} = State) ->
    NewFiles = case file:list_dir(Dir) of 
                   {error, _Any} -> FilesLoaded;
                   {ok, Files} ->
                       Srcs = case SrcType of
                                  erl -> get_src_files(Dir, Files);
                                  app -> get_app_src_files(Dir, Files)
                              end,
                       [gen_server:cast(?SERVER, {load_src_dir, SrcType, Dir ++ D ++ "/"})
                        || D <- Files, D == "src"],
                       lists:append(Srcs, FilesLoaded)
               end,
    {noreply, State#dig_state{files_loaded=NewFiles}};

%% Insert a newly spawned delve worker {File, Pid} pair into an ets table.
handle_cast({loaded_file, File, Pid},
            #dig_state{delve_workers=DwTable} = State) ->
    ets:insert(DwTable, {File, Pid}),
    {noreply, State};

%% Ignore other cast requests.
handle_cast(_Msg, State) ->
    {noreply, State}.


%% Load either a .erl file or a .app.src file
cast_file_load(Type, File, #dig_state{files_loaded=FilesLoaded} = State) ->
    case file:read_file_info(File) of
        {error, _Any} -> {noreply, State};
        {ok, _FileExists} ->
            case Type of
                erl_file -> delve_src(File);
                app_file -> delve_app_src(File)
            end,
            NewFiles = [ {now(), File} | FilesLoaded ],
            {noreply, State#dig_state{files_loaded=NewFiles}}
    end.



-spec handle_info(any(), #dig_state{}) -> {noreply, #dig_state{}}.

handle_info(_Info, State) -> {noreply, State}.


%%%===================================================================
%%% Spawned internal functions
%%%===================================================================

%% Load and transfer data to a delve worker.
-spec delve_src(string()) -> ok.
-spec delve_app_src(string()) -> ok.

delve_src(File) ->
    case file:read_file(File) of
        {error, Reason} ->
            error_logger:error_msg("~p:delve_src/1 cannot read ~s: ~p~n",
                                   [?MODULE, File, Reason]);
        {ok, RawSource} ->
            NL = list_to_binary(io_lib:nl()),
            Lines = binary:split(RawSource, NL, [global]),
            case ?DELVE_SUPERVISOR:analyze_source(skim_lines(File, Lines)) of
                {ok, Pid} -> loaded_file(File, Pid);
                {error, Problem} ->
                    error_logger:error_msg("~p:analyze_source of ~p error: ~p~n",
                                           [?MODULE, File, Problem])
            end
    end.

delve_app_src(File) ->
    case file:consult(File) of
        {error, Reason} ->
            error_logger:error_msg("~p:delve_app_src/1 cannot read ~s: ~p~n",
                                   [?MODULE, File, Reason]);
        {ok, Terms} ->
            case ?DELVE_SUPERVISOR:analyze_source(skim_terms(File, Terms)) of
                {ok, Pid} -> loaded_file(File, Pid);
                {error, Problem} ->
                    error_logger:error_msg("~p:analyze_source of ~p error: ~p~n",
                                           [?MODULE, File, Problem])
            end
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_src_files(string(), list(string())) -> list(string()).
-spec get_app_src_files(string(), list(string())) -> list(string()).
-spec skim_terms(string(), list(any())) -> #etdd_app_src{}.
-spec skim_lines(string(), list(binary())) -> #etdd_src{}.

%% Get either *.erl source files or *.app.src source files.    
get_src_files(Dir, Files) ->
    [begin Fname = Dir ++ F, delve_src(Fname), {now(), Fname} end
     || F <- Files, length(F) > 3, string:sub_string(F, length(F)-3) == ".erl"].

get_app_src_files(Dir, Files) ->
    [begin Fname = Dir ++ F, delve_app_src(Fname), {now(), Fname} end
     || F <- Files, length(F) > 7, string:sub_string(F, length(F)-7) == ".app.src"].


%% Convert app.src code lines to an #etdd_src{} record.
skim_terms(File, Terms) ->
    #etdd_app_src{
            file = File,
            app_lines = Terms,
            line_count = length(Terms)
           }.


%% Convert source code lines to an #etdd_src{} record.
skim_lines(File, Lines) ->
    skim_lines(File, Lines, Lines, 1, [], [], [], 0, {}, 0, {}).

skim_lines(File, [], Lines, _LineNr, White, Comments, Directives,
           Module, ModType, Behaviour, BehavType) ->
    TupleLines = list_to_tuple(Lines),
    #etdd_src{
               file = File,
               module = Module,
               module_type = ModType,
               behaviour = Behaviour,
               behaviour_type = BehavType,

               src_lines = TupleLines,
               line_count = tuple_size(TupleLines),
               whitespace = list_to_tuple(lists:reverse(White)),
               comments = list_to_tuple(lists:reverse(Comments)),
               directives = list_to_tuple(lists:reverse(Directives))
             };
skim_lines(File, [H|T], Lines, LineNr, White, Comments, Directives, Mod, ModType, Beh, BehType) ->
    case line_type(H) of
        whitespace ->        skim_lines(File, T, Lines, LineNr+1, [LineNr | White], Comments, Directives, Mod, ModType, Beh, BehType);
        comment ->           skim_lines(File, T, Lines, LineNr+1, White, [LineNr | Comments], Directives, Mod, ModType, Beh, BehType);
        directive ->         skim_lines(File, T, Lines, LineNr+1, White, Comments, [LineNr | Directives], Mod, ModType, Beh, BehType);
        {module, Type} ->    skim_lines(File, T, Lines, LineNr+1, White, Comments, [LineNr | Directives], LineNr, Type, Beh, BehType);
        {behaviour, Type} -> skim_lines(File, T, Lines, LineNr+1, White, Comments, [LineNr | Directives], Mod, ModType, LineNr, Type);
        _Other ->            skim_lines(File, T, Lines, LineNr+1, White, Comments, Directives, Mod, ModType, Beh, BehType)
    end.


%% Classify source code lines to specific types.
-spec line_type(binary()) -> whitespace | comment | directive | other
                                 | {module, atom()} | {behaviour, atom()}.
                              
%% If all whitespace was thrown away and nothing left, it was a whitespace line...
line_type(<<>>)                  -> whitespace;
line_type(<<" ", Rest/binary>>)  -> line_type(Rest);
line_type(<<"\t", Rest/binary>>) -> line_type(Rest);

%% If first non-whitespace character is a '%' then it is a comment...
line_type(<<"%", _Rest/binary>>) -> comment;

%% Module and module type are both extracted in a naive way...
line_type(<<"-module", Rest/binary>>)    ->
    Type =  binary:replace(Rest, [<<"(">>,<<" ">>,<<")">>,<<".">>], <<>>, [global]),
    {module, list_to_atom(binary_to_list(Type))};

%% Behaviour and behaviour type are both extracted in a naive way...
line_type(<<"-behaviour", Rest/binary>>) ->
    Type =  binary:replace(Rest, [<<"(">>,<<" ">>,<<")">>,<<".">>], <<>>, [global]),
    {behaviour, list_to_atom(binary_to_list(Type))};

%% All others are marked with an atom identifying type.
line_type(<<"-", _Rest/binary>>) -> directive;
line_type(_Other)                -> other.


%% ------ Last minute refactoring not in correct place --------------

%% Differentiate file types on .erl vs .app.src
file_type(Entry) ->
  Len = length(Entry),
  Last = Entry == "" orelse lists:last(Entry),
  Tail3 = Len > 3 andalso string:sub_string(Entry, Len-3),
  Tail7 = Len > 7 andalso string:sub_string(Entry, Len-7),
  if
    Tail7 == ".app.src" -> app_file;
    Tail3 == ".erl" -> erl_file;
    Last =:= $/ -> dir;
    true -> invalid
  end.

