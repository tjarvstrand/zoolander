%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @Doc Unit tests for zoolander_indent.erl
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

-module(zoolander_indent_tests).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Tests ====================================================================

%%%_* Tests ====================================================================

-define(assertFmt(Str, Ctx),
        [ ?_assertEqual(Str,
                        zoolander_indent:format( Str
                                               , zoolander:default_options()
                                               , Ctx))
        , ?_assertEqual(Str,
                        zoolander_indent:format( re:replace( Str
                                                           , "\n *"
                                                           , "\n"
                                                           , [ multiline
                                                             , global
                                                             , {return, list}])
                                               , zoolander:default_options()
                                               , Ctx))
        ]).

-define(assertFmt(Str), ?assertFmt(Str, [])).

format_basic_test_() ->
  [ ?assertFmt("")
  ].

format_expression_test_() ->
  [ ?assertFmt("a")

  , ?assertFmt("a\n")

  , ?assertFmt("a b")

  , ?assertFmt("\n"
               "b")

  , ?assertFmt("a,\n"
               "b")

  , ?assertFmt("a + b")

  , ?assertFmt( "a +\n"
                "  b"
              , [{expressions, 0}])

  , ?assertFmt( "A=a+\n"
                "    b"
              , [{expressions, 0}])


  , ?assertFmt( "A=\n"
                "  a+\n"
                "    b"
              , [{expressions, 0}])


  , ?assertFmt( "a+\n"
                "  b,\n"
                "c"
              , [{expressions, 0}])


  , ?assertFmt( "A = d,\n"
                "B =\n"
                "  \"abc\"\n"
              , [{expressions, 0}])

    % erl_parse splits whitespace of more than 16 spaces
  , ?assertFmt( "abcdefghijklmno:p( a,\n"
                "                   b),"
              , [{expressions, 0}])

  , ?assertFmt("catch\n"
               "  a + b"
              , [{expressions, 0}])

  , ?assertFmt("a()->\n"
               "  catch b:c().")

  ].

format_string_test_() ->
  [ ?assertFmt("\"\"")

  , ?assertFmt("\"a\"")

  , ?assertFmt( "\"a\"+\n"
                "  \"a\""
              , [{expressions, 0}])

  , ?assertFmt( "\"a\"\n"
                "\"a\"\n"
                "\"a\"\n"
                "\"a\""
              , [{expressions, 0}])

  , ?assertFmt( "A = \"a\"\n"
                "    \"a\""
              , [{expressions, 0}])

  , ?assertFmt( "A = \"a\"\n"
                "    \"a\"\n"
                "    \"a\"\n"
                "    \"a\""
              , [{expressions, 0}])

  , ?assertFmt("[\"a\"\n"
               " \"a\"]",
               [{expressions, 0}])
  ].

format_parameter_list_test_() ->
  [ ?assertFmt( "(A, B)"
              , [{expressions, 0}])

  , ?assertFmt( "(A,\n"
                " B)"
              , [{expressions, 0}])

  , ?assertFmt( "( A,\n"
                "  B)"
              , [{expressions, 0}])

  , ?assertFmt( "( A\n"
                ",B)"
              , [{expressions, 0}])

  , ?assertFmt( "( A\n"
                ", B)"
              , [{expressions, 0}])

  , ?assertFmt( "( A\n"
                ", B +\n"
                "    C)"
              , [{expressions, 0}])

  , ?assertFmt( "( A\n"
                ", B),\n"
                "C"
              , [{expressions, 0}])

  , ?assertFmt( "(\n"
                "  A\n"
                ")"
              , [{expressions, 0}])

  , ?assertFmt( "(\n"
                "  A,\n"
                "  B,\n"
                "  C"
                ")"
              , [{expressions, 0}])

  , ?assertFmt( "(\n"
                "  A,\n"
                "  B\n"
                ",C\n"
                ")"
              , [{expressions, 0}])

  , ?assertFmt( "a(\n"
                "  b)"
              , [{expressions, 0}])

  , ?assertFmt( "abcd()"
              , [{expressions, 0}])

  , ?assertFmt( "abcd(\n"
                "  )"
              , [{expressions, 0}])

  , ?assertFmt( "abcd(e)"
              , [{expressions, 0}])

  , ?assertFmt( "abcd(e\n"
                "  )"
              , [{expressions, 0}])

  , ?assertFmt( "abcd(\n"
                "  e\n"
                "  )"
              , [{expressions, 0}])

  , ?assertFmt( "abcd(e, f)"
              , [{expressions, 0}])

  , ?assertFmt( "abcd(e, f\n"
                "  )"
              , [{expressions, 0}])

  , ?assertFmt( "abcd( e,\n"
                "      f\n"
                "  )"
              , [{expressions, 0}])

    %% This one is a bit weird but kind of a corner case so let's accept it
    %% for now...
  , ?assertFmt( "abcd( e\n"
                "    , f\n"
                "  )"
              , [{expressions, 0}])

  , ?assertFmt( "abcd(\n"
                "  e, f\n"
                "  )"
              , [{expressions, 0}])

  , ?assertFmt( "a:b(\n"
                "  a)"
                 , [{expressions, 0}])

  , ?assertFmt( "a:b(c:d(\n"
                "      a))"
                 , [{expressions, 0}])
  ].

format_list_test_() ->
  [ ?assertFmt( "[A, B]"
              , [{expressions, 0}])

  , ?assertFmt( "[A,\n"
                " B]"
              , [{expressions, 0}])

  , ?assertFmt( "[ A,\n"
                "  B]"
              , [{expressions, 0}])

  , ?assertFmt( "[ A\n"
                ",B]"
              , [{expressions, 0}])

  , ?assertFmt( "[ A\n"
                ", B]"
              , [{expressions, 0}])

  , ?assertFmt( "[ A\n"
                ", B +\n"
                "    C]"
              , [{expressions, 0}])

  , ?assertFmt("->\n"
               "  [ A\n"
               "  ,B],\n"
               "  C"
              , [{expressions, 0}])

  , ?assertFmt( "[\n"
                " A\n"
                "]"
              , [{expressions, 0}])

  , ?assertFmt( "[\n"
                " A,\n"
                " B,\n"
                " C"
                "]"
              , [{expressions, 0}])

  , ?assertFmt( "[\n"
                " A,\n"
                " B\n"
                ",C\n"
                "]"
              , [{expressions, 0}])

  , ?assertFmt( "[A,B,\n"
                " C]"
              , [{expressions, 0}])
  ].


format_list_comprehension_test_() ->
  [ ?assertFmt("[A||A<-B]", [{expressions, 0}])

  , ?assertFmt("[A||A<-B]", [{expressions, 0}])

  , ?assertFmt("[A\n"
               "||A<-B]", [{expressions, 0}])

  , ?assertFmt("[A||\n"
               " A<-B]", [{expressions, 0}])

  , ?assertFmt("[A\n"
               "||A<-B\n"
               ", A =:= C]", [{expressions, 0}])

  , ?assertFmt("[A\n"
               "||A<-B\n,"
               " A =:= C]", [{expressions, 0}])
  ].


format_block_test_() ->
  [ ?assertFmt( "case x of a", [{expressions, 0}])

  , ?assertFmt( "case\n"
                "  x"
              , [{expressions, 0}])

  , ?assertFmt( "case\n"
                "  x\n"
                "of a"
              , [{expressions, 0}])

  , ?assertFmt( "try\n"
                "  x\n"
                "catch\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "case x of\n"
                "  a"
              , [{expressions, 0}])

  , ?assertFmt( "end,\n"
                "b"
              , [{expressions, 0}])

  , ?assertFmt( "case x of\n"
                "  a ->\n"
                "    b\n"
                "end,\n"
                "b"
              , [{expressions, 0}])

  , ?assertFmt( "case (a:b()) of\n"
                "  c -> d\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "try\n"
                "  {ok, A} =\n"
                "    b\n"
              , [{expressions, 0}])

  , ?assertFmt( "      try\n"
                "        ok\n"
                "      catch\n"
                "        _ -> a\n"
                "      end\n"
              , [])

  , ?assertFmt( "a() ->\n"
                "  case x of\n"
                "    y ->\n"
                "      try\n"
                "        ok\n"
                "      catch\n"
                "        _ -> a\n"
                "      end\n"
                "  end."
              , [])


  , ?assertFmt( "try\n"
                "  catch 1 + a\n"
                "catch\n"
                "  _ -> ok\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "try"
                "  mochijson2:decode(Body)\n"
                "catch _:_ ->\n"
                "        throw(bad_body)\n"
                "end."
              , [{expressions, 0}])

  , ?assertFmt( "catch\n"
                "  error:_ ->\n"
                "    ok;\n"
                "  throw:_ ->\n"
                "    ok"
              , [{expressions, 0}])

  , ?assertFmt( "catch\n"
                "  error:_ ->\n"
                "    catch a;\n"
                "  throw:_ ->\n"
                "    ok"
              , [{expressions, 0}])



  , ?assertFmt( "try a of\n"
                "  b ->\n"
                "    c\n"
                "catch\n"
                "  _ -> d\n"
                "after\n"
                "  e:f()\n"
                "end\n"
              , [{expressions, 0}])

  , ?assertFmt( "try a of\n"
                "  b ->\n"
                "    c\n"
                "catch\n"
                "  _ -> d\n"
                "end\n"
              , [{expressions, 0}])

  , ?assertFmt( "receive a -> b;\n"
                "        c -> d\n"
                "after 1 -> e;\n"
                "      2 -> f\n"
                "end."
              , [{expressions, 0}])


  , ?assertFmt( "receive\n"
                "  a -> b;\n"
                "  c -> d\n"
                "after\n"
                "  1 -> e;\n"
                "  2 -> f\n"
                "end."
              , [{expressions, 0}])

  , ?assertFmt( "receive\n"
                "  a ->\n"
                "    b;\n"
                "  c ->\n"
                "    d\n"
                "after\n"
                "  1 ->\n"
                "    e;\n"
                "  2 ->\n"
                "    f\n"
                "end."
              , [{expressions, 0}])

  ].


format_clause_test_() ->
  [ ?assertFmt( "->ok", [{clause, 0}])

  , ?assertFmt( "-> ok" , [{clause, 0}])

  , ?assertFmt( "-> a,\n"
                "   b"
              , [{clause, 0}, {clauses, 0}])

  , ?assertFmt( "foo->\n"
                "  a"
              , [{clause, 0}, {clauses, 0}, {{block, 'case'}, 0}])

  , ?assertFmt( "foo()-> a.\n"
                "b"
              , [])

  , ?assertFmt( "foo()-> a,\n"
                "        b.\n"
                "c"
              , [])
  ].

format_function_test_() ->
  [ ?assertFmt( "foo()->ok.")

  , ?assertFmt( "foo()->\n"
                "  ok."
              , [])

  , ?assertFmt( "foo()->\n"
                "  ok.\n"
                "\n"
              , [])

  , ?assertFmt( "foo()-> a,\n"
                "        b."
              , [])

  , ?assertFmt( "foo(a,b)-> c,\n"
                "           d."
              , [])

  , ?assertFmt( "a(A, B) when B > 0 ->\n"
                "  case b() of\n"
                "    {ok, C} ->\n"
                "      C;\n"
                "    D ->\n"
                "      error\n"
                "  end;\n"
                "a(A, B) ->\n"
                "  ok."
              , [])

  , ?assertFmt( "foo(a)-> b;\n"
                "foo(c)-> d."
              , [])

  , ?assertFmt( "fun()-> a end"
              , [{expressions, 0}])

  , ?assertFmt( "fun()-> a,\n"
                "        b\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun()->\n"
                "     a,\n"
                "     b\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun()->\n"
                "     a,\n"
                "     b\n"
                "end,"
                "a"
              , [{expressions, 0}])


  , ?assertFmt( "fun() when A =:= B ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun() when A =:= B,\n"
                "           C =:= D ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun() when A =:= B;\n"
                "           C =:= D ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])


  , ?assertFmt( "fun() when A =:= B\n"
                "         , C =:= D ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun(A) ->\n"
                "     A;\n"
                "   (B) ->\n"
                "     B\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun(A) when A =:= B\n"
                "          , C =:= D ->\n"
                "     A;\n"
                "   (B) ->\n"
                "     B\n"
                "end"
              , [{expressions, 0}])


  , ?assertFmt( "fun()\n"
                "     when"
              , [{expressions, 0}])

  , ?assertFmt( "fun()\n"
                "     when A =:= B ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun()\n"
                "     when\n"
                "       A =:= B ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun()\n"
                "     when\n"
                "       A=:=\n"
                "         B ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])


  , ?assertFmt( "fun()\n"
                "     when\n"
                "       A=:=\n"
                "         B,\n"
                "       C =:= D ->\n"
                "     a\n"
                "end"
              , [{expressions, 0}])

  , ?assertFmt( "fun\n"
                "  a/1"
              , [{expressions, 0}])

  ].

format_case_test_() ->
  [ ?assertFmt( "foo()->\n"
                "  case x of\n"
                "    a ->\n"
                "      b\n"
                "  end,\n"
                "  ok.\n"
                "\n"
              , [])

  , ?assertFmt( "foo()->\n"
                "  A = case x of\n"
                "        a ->\n"
                "          b\n"
                "      end,\n"
                "  ok.\n"
                "\n"
              , [])
  ].

format_map_test_() ->
  [ ?assertFmt( "#{A, B}"
              , [{expressions, 0}])

  , ?assertFmt( "#{A,\n"
                "  B}"
              , [{expressions, 0}])

  , ?assertFmt( "#{ A,\n"
                "   B}"
              , [{expressions, 0}])

  , ?assertFmt( "#{ A\n"
                " ,B}"
              , [{expressions, 0}])

  , ?assertFmt( "#{ A\n"
                " , B}"
              , [{expressions, 0}])

  , ?assertFmt( "#{ A\n"
                " , B +\n"
                "     C}"
              , [{expressions, 0}])

  , ?assertFmt( "#{ A\n"
                " , B),\n"
                "C"
              , [{expressions, 0}])
  ].

format_export_test_() ->
  [ ?assertFmt( "-export([a/1])" , [])

  , ?assertFmt( "-export([a/1,\n"
                "         b/1])"
              , [])

  , ?assertFmt( "-export([a/1\n"
                "        ,b/1])"
              , [])

  , ?assertFmt( "-export([a/1\n"
                "        ,b/1])."
                "a"
              , [])
  ].

format_define_test_() ->
  [ ?assertFmt( "-define(a,b)"
              , [])

  , ?assertFmt( "-define(a,\n"
                   "        b)"
                 , [])

  , ?assertFmt( "-define(a,\n"
                   "        b)"
                 , [])

  , ?assertFmt( "-define(a,\n"
                   "        [1\n"
                   "        ,2])"
                 , [])
  ].

format_type_test_() ->
  [ ?assertFmt( "-type a(a,b) :: [c,d]"
              , [])

  , ?assertFmt( "-type\n"
                "  a(a,b)::[c,d]"
              , [])

  , ?assertFmt( "-type a(a,b)::\n"
                "        [c,d]"
              , [])

  , ?assertFmt( "-type\n"
                "  a(a,b)::\n"
                "    [c,\n"
                "     d\n"
                "    ,e]"
              , [])
  ].

format_spec_test_() ->
  [ ?assertFmt( "-spec a(A::a,B::b())->[C::c,D::d()]"
              , [])

  , ?assertFmt( "-spec a(A::a,B::b())->\n"
                "        [C::c,D::d()]"
              , [])

  , ?assertFmt( "-spec a(A::a,B::b())->C::c|\n"
                "                      D::d()"
              , [])

  , ?assertFmt( "-spec a(A::a,B::b())->C::c\n"
                "                     |D::d()"
              , [])


  , ?assertFmt( "-spec abc()\n"
                "      ->d."
              , [])


  , ?assertFmt("-spec(a(b(),fun(()->A))->A|a).")
  , ?assertFmt("-spec(a(b(),fun(()->A))->A\n"
               "                        |a).")
  ].


format_record_test_() ->
  [ ?assertFmt("-record(street_info, {is_numbered = false :: boolean()}).")
  , ?assertFmt("-record(street_info, {is_numbered = false :: boolean()\n"
               "                     }).")
  ].


format_comment_test_() ->
  [ ?assertFmt( "[a,\n"
                " % commment\n"
                " b]"
              , [{expressions, 0}])

  , ?assertFmt( "[ a\n"
                "% commment\n"
                ", b]"
              , [{expressions, 0}])
  ].


%%%_* Internal =================================================================


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:

