%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Unit tests for zoolander_code.erl
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

-module(zoolander_code_tests).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ====================================================================

string_test_() ->
    [ ?_assertEqual(
         { ok
         , {[ {atom,        [{line, 1}, {column, 1},  {text, "foo"}], foo}
            , {'(',         [{line, 1}, {column, 4},  {text, "("}]}
            , {')',         [{line, 1}, {column, 5},  {text, ")"}]}
            , {white_space, [{line, 1}, {column, 6},  {text, " "}], " "}
            , {'->',        [{line, 1}, {column, 7},  {text, "->"}]}
            , {white_space, [{line, 1}, {column, 9},  {text, " "}], " "}
            , {'?',         [{line ,1}, {column, 10}, {text, "?"}]}
            , {atom,        [{line, 1}, {column, 11}, {text, "ok"}], ok}
            , {dot,         [{line, 1}, {column, 13}, {text, "."}]}]
           ,  [{ function
               , [{line, 1}, {column, 1},  {text, "foo"}]
               , foo
               , 0
               , [{ clause
                  , [{line, 1}, {column, 1}, {text, "foo"}]
                  , []
                  , []
                  , [{atom, [{line, 1}, {column, 11}, {text, "ok"}], ok}]}]}]}}
                   , vandamme_parse:string("foo() -> ?ok."))].

%%%_* Internal =================================================================


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
