%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Main zoolander interface
%%% @end
%%% @author Thomas Järvstrand <tjarvstrand@gmail.com>
%%% @copyright
%%% Copyright 2016 Thomas Järvstrand <tjarvstrand@gmail.com>
%%%
%%% This file is part of Zoolander.
%%%
%%% Zoolander is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% Zoolander is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public License
%%% along with Zoolander. If not, see <http://www.gnu.org/licenses/>.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(zoolander).

%%%_* Exports ==================================================================

-export([ default_options/0
        , format/1
        , format/2
        , format_files/1
        , format_files/2]).

-export_type([return/0]).

%%%_* Definitions ==============================================================

%%%_* Types ====================================================================

-type return() :: ok | {error, term()}.

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc Return the default set of options
%%
-spec default_options() -> #{}.
%%------------------------------------------------------------------------------
default_options() ->
  #{indent_width => 2}.

%%------------------------------------------------------------------------------
%% @equiv format(Code, default_options()).
%%
-spec format(Code :: string()) -> return().
%%------------------------------------------------------------------------------
format(Code0) ->
  format(Code0, default_options()).


%%------------------------------------------------------------------------------
%% @doc Format Code according to Options.
%%
-spec format(Code :: string(), Options :: #{}) -> return().
%%------------------------------------------------------------------------------
format(Code0, Options) ->
  lists:foldl(fun(Step, Code) ->
                  Module = list_to_atom("zoolander_" ++ atom_to_list(Step)),
                  Module:format(Code, Options)
              end,
              Code0,
              [ strip_trailing_whitespace
              , untabify
              , indent]).

%%------------------------------------------------------------------------------
%% @equiv format(Code, default_options()).
%%
-spec format_files(Code :: string()) -> return().
%%------------------------------------------------------------------------------
format_files(Code0) ->
  format_files(Code0, default_options()).

%%------------------------------------------------------------------------------
%% @doc Read file from Filename, format it according to Options and save.
%%
-spec format_files(Filename :: file:filename(), Options :: #{}) -> return().
%%------------------------------------------------------------------------------
format_files("", _Options) ->
  erlang:error(enoent);
format_files(Pattern, Options) ->
  case filelib:wildcard(Pattern) of
    []    ->
      erlang:error(enoent);
    Filenames ->
      lists:foreach(fun(Filename) -> format_file(Filename, Options) end,
                    Filenames)
  end.

%%%_* Internal =================================================================

format_file(Filename, Options) ->
  io:format("Formatting ~s...", [Filename]),
  {ok, Code} = file:read_file(Filename),
  file:write_file(Filename, format(binary_to_list(Code), Options)),
  io:format("ok~n").


%%%_* Tests ====================================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
