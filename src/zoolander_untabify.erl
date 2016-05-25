%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Zoolander untabify engine
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

-module(zoolander_untabify).

%%%_* Exports ==================================================================

-export([format/2]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Definitions ==============================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

format(Code, #{indent_width := Width}) ->
  Replacement = lists:append(lists:duplicate(Width, " ")),
  re:replace(Code, "\t", Replacement, [global, multiline, {return, list}]).

%%%_* Internal =================================================================

%%%_* Tests ====================================================================

format_test_() ->
  [ ?_assertEqual( "\n"
                   "foo() ->\n"
                   "  case  a of  \n"
                   "    b -> c;\n"
                   "  end."
                 , format( "\n"
                        "foo() ->\n"
                        "  case\ta of\t\n"
                        "\t  b -> c;\n"
                        "  end."
                      , #{indent_width => 2}))].

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

