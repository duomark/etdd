%%%-------------------------------------------------------------------
%%% @copyright (c) 2011, DuoMark International, Inc. All rights reserved
%%% @author Jay Nelson <jay@duomark.com>
%%% @doc
%%%   Shell utilities used to report code loaded on a node.
%%% @since v0.0.2
%%% @end
%%%-------------------------------------------------------------------
-module(dk_node_info).
-copyright('Copyright (c) 2011 DuoMark International, Inc. All rights reserved').
-author(jayn).

-export([
         report_applications/0,
         report_modules/1
        ]).

%%--------------------------------------------------------------------
%% @doc
%%   Report the loaded applications and versions.
%%
%%   Print a text formatted table of the loaded applications.
%%   This is typically used when in the shell or on startup
%%   when booting a foreground node to the shell.
%% @end
%%--------------------------------------------------------------------
-spec report_applications() -> ok.

report_applications() ->
    TableFmt = "  ~-20s ~-10s  ~-50s~n", 
    io:format("~nLoaded Applications:~n~n"),
    io:format(TableFmt, ["Application", "  Version", "  Description"]),
    io:format("  ~s ~s  ~s~n", [lists:duplicate(N,$-) || N <- [20,10,50]]),
    {UserApps, SysApps} =
        lists:partition(
          fun(AppDesc) ->
                  case (atom_to_list(element(1,AppDesc))) of
                      "etdd_" ++ _Rest -> true;
                      "dig_and_delve" ++ _Rest -> true;
                      _Other -> false
                  end
          end,
          application:which_applications()
         ),

    [io:format(TableFmt, [App, Version, Desc]) || {App, Desc, Version} <- SysApps],
    io:format("~n"),

    [io:format(TableFmt, [App, Version, Desc]) || {App, Desc, Version} <- UserApps],
    io:format("~n").

%%--------------------------------------------------------------------
%% @doc
%%   Report the loaded functions within a module.
%%
%%   Print a text formatted table of the loaded functions.
%%   The Path argument is the name of a module with or without
%%   its version. It is used to find all modules that are
%%   loaded from a .../lib/Path* directory
%%
%%   This is typically used when in the shell or on startup
%%   when booting a foreground node to the shell.
%% @end
%%--------------------------------------------------------------------
-spec report_modules(atom() | string()) -> ok.

report_modules(Path) ->
    io:format("  ~-20s  ~-90s~n", ["Module", "Path"]),
    io:format("  ~s  ~s~n", [lists:duplicate(20,$-),
                             lists:duplicate(90,$-)]),
    PathStr = case Path of
          Path when is_atom(Path) -> atom_to_list(Path);
          _ -> Path
          end,
    PathMatch = "/lib/" ++ PathStr,
    Modules = [{M,P} || {M,P} <- code:all_loaded(), is_list(P),
                        string:str(P, PathMatch) > 0],
    SortedMods = lists:sort(Modules),
    [io:format("  ~-20w  ~s~n", [M, P]) || {M,P} <- SortedMods],
    io:format("~n").
