%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Unit tests for zoolander.erl
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

-module(zoolander_tests).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ====================================================================

shape_indent_test_() ->
  [%%  ?_assertEqual("", vandamme:shape(""))
   %% ?_assertEqual("foo() ->\n"
   %%                "\n"
   %%                "\n"
   %%                 "  ok.",
   %%                 vandamme:shape("foo() -> \n"
   %%                                "  \n"
   %%                                "\n"
   %%                                "ok."))
   ?_assertEqual( "foo() -> \n"
                  "  case x of\n"
                  "    a -> b;\n"
                  "    c -> d\n"
                  "  end,\n"
                  "\n"
                  "\n"
                  "  ok."
                , vandamme:shape("\n"
                                 "\n"
                                 "foo() -> \n"
                                "case x of\n"
                                "a -> b;\n"
                                "c -> d\n"
                                "end,\n"
                                "  \n"
                                "\n"
                                "ok."))
  ].

%%%_* Internal =================================================================


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
