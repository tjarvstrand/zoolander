%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @Doc Zoolander indentation engine
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

-module(zoolander_indent).

%%%_* Exports ==================================================================

-export([ format/2
        % Exported for tests
        , format/3]).

%%%_* Includes =================================================================

-include_lib("eunit/include/eunit.hrl").

%%%_* Definitions ==============================================================

-define(BLOCK_START_P(T),
        (T =:= 'case' orelse
         T =:= 'try' orelse
         T =:= 'receive' orelse
         T =:= 'begin' orelse
         T =:= 'if' orelse
         T =:= 'fun')).

-define(BLOCK_START_MATCHES,
        [ 'case'
        , 'try'
        , 'receive'
        , 'begin'
        , 'if'
        , 'fun']).

-define(SEQUENCE_START_MATCHES,
        [ '('
        , '['
        , '{'
        , '<<']).

-define(BLOCK_END_P(T),
        (T =:= 'end')).

-define(FORM_END_P(T),
        (T =:= dot)).

-define(SEQUENCE_START_P(T),
        (T =:= '(' orelse
         T =:= '[' orelse
         T =:= '[' orelse
         T =:= '<<')).

-define(SEQUENCE_END_P(T),
        (T =:= ')' orelse
         T =:= ']' orelse
         T =:= '}' orelse
         T =:= '>>')).


-define(MATCHING_SYMBOL_P(A, B),
        (A =:= ')' andalso B =:= '(' orelse
         A =:= '}' andalso B =:= '{' orelse
         A =:= ']' andalso B =:= '[' orelse
         A =:= '>>' andalso B =:= '<<' orelse
         A =:= '(' andalso B =:= ')' orelse
         A =:= '{' andalso B =:= '}' orelse
         A =:= '[' andalso B =:= ']' orelse
         A =:= '<<' andalso B =:= '>>' orelse
         (?BLOCK_END_P(A) andalso ?BLOCK_START_P(B)) orelse
         (?BLOCK_START_P(A) andalso ?BLOCK_END_P(B)))).

-define(NON_CODE_P(A),
        (A =:= comment orelse A =:= white_space)).


%%%_* Types ====================================================================

%%%_* API ======================================================================

format(Code, Options) ->
  format(Code, Options, []).

format(Code, Options, _Ctx) ->
  Tokens0 = zoolander_code:lex(Code),
  Tokens = indent(Tokens0, Options, []),
  lists:append([Text || #{text := Text} <- Tokens]).

%%%_* Internal =================================================================

indent([], _Opts, Done) ->
  lists:reverse(Done);

indent( [ #{type := white_space, text := "\n" ++ _}
        , #{type := white_space, text := "\n" ++ _}|_] = Tokens0
      , Opts
      , Done) ->
  [Token|Tokens] = indent_current_line_to(0, Tokens0, Opts),
  indent(Tokens, Opts, [Token|Done]);

indent( [ #{type := white_space, text := "\n" ++ _}] = Tokens0
      , Opts
      , Done) ->
  [Token|Tokens] = indent_current_line_to(0, Tokens0, Opts),
  indent(Tokens, Opts, [Token|Done]);

indent( [#{type := white_space, text := "\n" ++ _}|_] = Tokens0
      , #{indent_width := IndentWidth} = Opts
      , Done) ->
  NewLvl = level( IndentWidth
                , drop_non_code_tokens(Done)
                , drop_non_code_tokens(Tokens0)),
  [Token|Tokens] = indent_current_line_to(NewLvl, Tokens0, Opts),
  indent(Tokens, Opts, [Token|Done]);

indent( [Token|Tokens]
      , Opts
      , Done) ->
  indent(Tokens, Opts, [Token|Done]).

level(IndentWidth, Preceeding, Following) ->
  case Following of
    [#{type := Type}|_] when Type =:= ',' orelse
                             Type =:= '|' ->
      case back_to_start_of_first_expr(Preceeding, Following) of
        {ok, {[#{'end' := {_, EndCol}}|_], _}} ->
          EndCol - 2;
        _ ->
          0
      end;
    [#{type := Type}|_Rest] when Type =:= '||' ->
      {ok, {[#{start := {_, ListStartCol}}|_], _}} = back_to(['['], Preceeding),
      ListStartCol - 1;
    [#{type := ')'}|_] ->
      {ok, {[#{start := {_, ParenStartCol}}|Preceeding1], _}} =
        back_to(['('], Preceeding),

      case start_of_call(Preceeding1) of
        undefined -> ParenStartCol - 1;
        #{start := {_, FunRefStartCol}} -> FunRefStartCol - 1 + IndentWidth
      end;
    [#{type := Type}|_Rest] when Type =:= '}' orelse
                                 Type =:= ']' orelse
                                 Type =:= '>>' ->
      {ok, {[#{start := {_, SeqStart}}|_], _}} =
        back_to([corresponding_delimiter(Type)], Preceeding, Following),
      SeqStart - 1;
    [#{type := Type}|_] when Type =:= '->' ->
      lvl_at_start_of_expr(Preceeding);
    [#{type := Type}|_] when Type =:= 'end' ->
      {ok, {[#{start := {_, StartCol}}|_], _}} =
        back_to( ?BLOCK_START_MATCHES, Preceeding),
      StartCol - 1;
    [#{type := Type}|_] when Type =:= 'of' ->
      {ok, {[#{start := {_, StartCol}}|_], _}} = back_to( ['try', 'case']
                                                        , Preceeding),
      StartCol - 1;
    [#{type := Type}|_] when Type =:= 'after' ->
      {ok, {[#{start := {_, StartCol}}|_], _}} = back_to( ['receive', 'try']
                                                        , Preceeding),
      StartCol - 1;
    [#{type := Type}|_] when Type =:= 'when' ->
      {ok, {_, [#{start := {_, StartCol}}|_]}} = back_to( [ dot
                                                          , 'of'
                                                          , 'if'
                                                          , 'receive'
                                                          , 'fun'
                                                          , 'after'
                                                          , 'catch'
                                                          , ['-', atom, atom]]
                                                        , Preceeding
                                                        , Following),
      StartCol - 1 + IndentWidth;
    [#{type := Type}|_] when Type =:= string ->
      case Preceeding of
        [#{type := string, start := {_, StringStartCol}}|_] ->
          StringStartCol - 1;
        _ ->
          level_based_on_preceeding(IndentWidth, Preceeding, Following)
      end;
    [#{type := Type} = Token|Rest] when Type =:= 'catch' ->
      case forward_to(['end', 'catch'], [Token|Preceeding], Rest) of
        {ok, {_, [#{type := 'end'}|_]}} -> %% Indent from try
          {ok, {[#{start := {_, StartCol}}|_], _}} = back_to( ['try']
                                                            , Preceeding),
          StartCol - 1;
        _ -> %% Indent as normal expression,
          level_based_on_preceeding(IndentWidth, Preceeding, Following)
      end;
    [#{type := dot}|_] ->
      0;
    _ ->
      level_based_on_preceeding(IndentWidth, Preceeding, Following)
  end.


level_based_on_preceeding(IndentWidth, Preceeding, Following) ->
  case Preceeding of
    [#{type := Type}|Rest] when Type =:= '=' ->
      lvl_at_start_of_expr(Rest) + IndentWidth;
    [#{type := Type}|Rest] when Type =:= '|' orelse
                                Type =:= '||' ->
      lvl_at_start_of_expr(Rest, match_as_single_expr);
    [#{type := Type}|_] when Type =:= ',' ->
      case back_to_start_of_first_expr(Preceeding, Following) of
        {ok, {_, [#{start := {_, ExprStartCol}}|_]}} -> ExprStartCol - 1;
        {error, _}                                   -> 0
      end;
    [#{type := Type, start := {_, ParenStartCol}}|Rest] when Type =:= '(' ->
      case start_of_call(Rest) of
        undefined -> ParenStartCol - 1 + IndentWidth;
        #{start := {_, FunRefStartCol}} -> FunRefStartCol - 1 + IndentWidth
      end;

    [#{type := Type}|Rest] when Type =:= 'of' ->
      {ok, {[#{start := {_, StartCol}}|_], _}} = back_to( ['try', 'case']
                                                        , Rest),
      StartCol - 1 + IndentWidth;
    [#{type := Type}|Rest] when Type =:= 'catch' ->
      case back_to( ['try'], Rest) of
        {error, _} ->
          IndentWidth;
        {ok, {[#{start := {_, StartCol}}|_], _}} ->
          StartCol - 1 + IndentWidth
      end;
    [#{type := Type}|Rest] when Type =:= ';' ->
      case back_to(['when', '->'], Rest, Following) of
        {ok, {[#{type := 'when'}|_], [#{start := {_, ClauseStartCol}}|_]}} ->
          %% We're in a guard
          ClauseStartCol - 1;
        {ok, {Preceeding1, Following1}} ->
          %% We're not in a guard, keep going until the start of the clause
          case
            back_to([dot, 'of', 'if', 'receive', 'fun', 'after', 'catch', ';']
                   , Preceeding1
                   , Following1)
          of
            {ok, {_, [#{start := {_, ClauseStartCol}}|_]}} ->
              ClauseStartCol - 1;
            {error, {[], _}} ->
              0
          end
      end;
    [#{type := Type}|Rest] when Type =:= '->' ->
      ClauseMatches = [ dot
                      , 'of'
                      , 'if'
                      , 'receive'
                      , 'fun'
                      , 'after'
                      , 'catch'
                      , '->'
                      , ';'
                      , ['-', atom, atom]],
      case back_to(ClauseMatches, Rest) of
        {ok, { [#{type := ';'}|_] = Preceeding1, [#{start := {_, FollowingStartCol0}}|_] = Following1}} ->
          %% Check if we're in a guard
          case back_to(['when', '->'], Preceeding1, Following1) of
            {ok, {[#{type := 'when'}|Preceeding2], _}} ->
              %% We're in a guard, keep going until the end of previous clause
              {ok, {_, [#{start := {_, FollowingStartCol1}}|_]}} =
                back_to(ClauseMatches, Preceeding2),
              FollowingStartCol1 - 1 + IndentWidth;
            {ok, {_, _}} ->
              %% We're not in a guard
              FollowingStartCol0 - 1 + IndentWidth
          end;
          %% FollowingStartCol - 1 + IndentWidth;
        {ok, {[#{type := '->'}|_], [#{start := {_, FollowingStartCol}}|_]}} ->
          FollowingStartCol - 1;
        {ok, {Preceeding1, [#{start := {_, FollowingStartCol}}|_]}} ->
          %% Check if were in a '-spec' or '-type'
          case n_next_code_tokens(3, Preceeding1) of
            [ #{type := atom, start := {_, PreceedingStartCol}}
            , #{type := atom}
            , #{type := '-'}] ->
              PreceedingStartCol - 1 + IndentWidth;
            _ ->
              FollowingStartCol - 1 + IndentWidth
          end;
        {error, {[], _}} ->
          IndentWidth
      end;
    [#{type := Type, 'end' := {_, EndCol} }|_] when Type =:= '{' orelse
                                                    Type =:= '[' orelse
                                                    Type =:= '<<' ->
      EndCol - 1;
    [] ->
      0;
    [#{type := dot}|_] ->
      0;
    _ ->
      case back_to_start_of_expr(Preceeding) of
        {error, _} ->
          IndentWidth;
        {ok, {[#{start := {_, StartCol}}|_] = Preceeding, _}} ->
          StartCol - 1 + IndentWidth;
        {ok, {Preceeding1, [#{type := '(', start := {_, StartCol}}|_]}} ->
          case start_of_call(Preceeding1) of
            undefined ->
              StartCol - 1 + IndentWidth;
            #{start := {_, FunRefStartCol}} ->
              FunRefStartCol - 1 + IndentWidth
          end;
        {ok, {_, [#{start := {_, StartCol}}|_]}} ->
          StartCol - 1 + IndentWidth
      end
  end.

n_next_code_tokens(N, Tokens) ->
  n_next_code_tokens(N, drop_non_code_tokens(Tokens), []).

n_next_code_tokens(N, Tokens, Done) when N =< 0 orelse Tokens =:= [] ->
  lists:reverse(Done);
n_next_code_tokens(N, [Token|Tokens], Done) ->
  n_next_code_tokens(N - 1,  drop_non_code_tokens(Tokens), [Token|Done]).

drop_non_code_tokens(Tokens) ->
  lists:dropwhile(fun(#{type := T}) ->
                      T =:= white_space orelse T =:= comment
                  end,
                  Tokens).

indent_current_line_to(Lvl, [Token|Tokens0], #{indent_width := TabWidth}) ->
  case max(Lvl, 0) - white_space_width(TabWidth, Token) of
    0     -> [Token|Tokens0];
    Diff  ->
      Tokens1 = [adjust_whitespace_width(TabWidth, Diff, Token) | Tokens0],
      shift_current_line(Diff, Tokens1)
  end.


shift_current_line( Chars
                  , [ #{ type := white_space
                       , text := "\n" ++ _
                       , 'end' := {Line, Col}} = Token
                    | Tokens]) ->
  [ Token#{'end' => {Line, Col + Chars}}
  | shift_current_line(Line, Chars, Tokens, [])].


shift_current_line(_Line, _Chars, [], Done) ->
  lists:reverse(Done);

shift_current_line(Line
                  , Chars
                  , [#{ start := {Line, StartCol}
                      , 'end' := {Line, EndCol}} = Token0|Tokens]
                  , Done) ->
  Token = Token0#{ start := {Line, StartCol + Chars}
                 , 'end' := {Line, EndCol + Chars}},
  shift_current_line(Line, Chars, Tokens, [Token|Done]);
shift_current_line( Line
                  , Chars
                  , [#{start := {Line, StartCol}} = Token0|Tokens]
                  , Done) ->
  Token = Token0#{start := {Line, StartCol + Chars}},
  lists:reverse([Token|Done]) ++ Tokens.

white_space_width(TabWidth, #{text := "\n" ++ WhiteSpace}) ->
  white_space_width(TabWidth, WhiteSpace, 0).

white_space_width(_TabWidth, [], Width) ->
  Width;
white_space_width(TabWidth, [$\t|Rest], Width) ->
  white_space_width(TabWidth, Rest, Width + TabWidth);
white_space_width(TabWidth, [_|Rest], Width) ->
  white_space_width(TabWidth, Rest, Width + 1).

adjust_whitespace_width( _TabWidth
                       , Width
                       , #{text := "\n" ++ WhiteSpace0} = Token)
  when Width > 0 ->
  WhiteSpace = "\n" ++ string:copies(" ", Width) ++ WhiteSpace0,
  Token#{text => WhiteSpace};
adjust_whitespace_width(TabWidth
                       , Width
                       , #{text := "\n" ++ WhiteSpace0} = Token) ->
  WhiteSpace = adjust_whitespace_width(TabWidth, Width, WhiteSpace0, []),
  Token#{text => "\n" ++ WhiteSpace}.

adjust_whitespace_width(_TabWidth, 0, Rest, _Done) ->
  Rest;
adjust_whitespace_width(TabWidth, Width, [$\t = Char|Rest], Done)
  when Width >= TabWidth ->
  adjust_whitespace_width(TabWidth, Width + TabWidth, Rest, [Char|Done]);
adjust_whitespace_width(TabWidth, Width, [$\t = Char|Rest], Done) ->
  adjust_whitespace_width( TabWidth
                         , Width - TabWidth
                         , string:copies(" ", Width) ++ Rest
                         , [Char|Done]);
adjust_whitespace_width(TabWidth, Width, [Char|Rest], Done) ->
  adjust_whitespace_width(TabWidth, Width + 1, Rest, [Char|Done]).

lvl_at_start_of_expr(Tokens) ->
  lvl_at_start_of_expr(Tokens, undefined).

lvl_at_start_of_expr(Tokens, Mode) ->
  case back_to_start_of_expr(Tokens, Mode) of
    {error, _}                      -> 0;
    {ok, {Preceeding, [#{start := {_, FollowingStartCol}}|_]}} ->
      %% Check if were in a '-spec' or '-type'
      case n_next_code_tokens(3, Preceeding) of
        [ #{type := atom, start := {_, PreceedingStartCol}}
        , #{type := atom}
        , #{type := '-'}] ->
          PreceedingStartCol - 1;
        _ ->
          FollowingStartCol - 1
      end
  end.

back_to_start_of_first_expr(Preceeding, Following) ->
  back_to( ?BLOCK_START_MATCHES
           ++ ?SEQUENCE_START_MATCHES
           ++ ['of', 'after', 'when', '->']
         , Preceeding
         , Following).

back_to_start_of_expr(Tokens) ->
    back_to_start_of_expr(Tokens, undefined).

back_to_start_of_expr(Tokens, Mode) ->
  ExprBreak0 = [ [atom, atom]
               , 'fun'
               , 'case'
               , 'try'
               , 'receive'
               , 'begin'
               , 'if'
               , 'of'
               , 'after'
               , 'when'
               , '->'
               , '{'
               , '['
               , '('
               , '<<'
               , ','
               , ';'
               , dot],
    ExprBreak = case Mode =:= match_as_single_expr of
                  false  -> ['='|ExprBreak0];
                  true   -> ExprBreak0
                end,
  back_to(ExprBreak, Tokens, []).


back_to(Matches, Preceeding) ->
  back_to(Matches, Preceeding, []).

back_to(Matches, Preceeding, Following) ->
  back_to(Matches, Preceeding, drop_non_code_tokens(Following), []).

back_to(_, [], Following, _Ctx) ->
  {error, {[], Following}};

back_to( Matches
       , [#{type := Type}|Preceeding]
       , Following
       , Ctx) when ?NON_CODE_P(Type) ->
  back_to(Matches, Preceeding, Following, Ctx);

back_to( Matches
       , [#{type := Type} = Token|Preceeding]
       , Following
       , [CurCtx|Ctx]) when ?MATCHING_SYMBOL_P(CurCtx, Type) ->
  back_to(Matches, Preceeding, [Token|Following], Ctx);

back_to( Matches
       , [#{type := Type} = Token|Preceeding0] = Preceeding
       , Following
       , [] = Ctx) ->
  case
    lists:filter( fun(Match) when is_list(Match) ->
                      Tokens = lists:reverse(n_next_code_tokens( length(Match)
                                                               , Preceeding)),
                      Match =:= [T || #{type := T} <- Tokens];
                     (Match) ->
                      Match =:= Type
                  end
                , Matches)
  of
    [] when ?BLOCK_END_P(Type) orelse
            ?SEQUENCE_END_P(Type) orelse
            ?FORM_END_P(Type) ->
      back_to(Matches, Preceeding0, [Token|Following], [Type|Ctx]);
    [] ->
      back_to(Matches, Preceeding0, [Token|Following], Ctx);
    _  ->
      {ok, {Preceeding, Following}}
  end;

back_to( Matches
       , [#{type := Type} = Token|Preceeding]
       , Following
       , Ctx) when ?BLOCK_END_P(Type) orelse
                   ?SEQUENCE_END_P(Type) orelse
                   ?FORM_END_P(Type) ->
  back_to(Matches, Preceeding, [Token|Following], [Type|Ctx]);

back_to( Matches
       , [Token|Preceeding]
       , Following
       , Ctx) ->
  back_to(Matches, Preceeding, [Token|Following], Ctx).

forward_to(Matches, Preceeding, Following) ->
  forward_to(Matches, drop_non_code_tokens(Preceeding), Following, []).

forward_to(_, Preceeding, [], _Ctx) ->
  {error, {Preceeding, []}};

forward_to( Matches
       , Preceeding
       , [#{type := Type}|Following]
       , Ctx) when ?NON_CODE_P(Type) ->
  forward_to(Matches, Preceeding, Following, Ctx);

forward_to( Matches
       , Preceeding
       , [#{type := Type} = Token|Following]
       , [CurCtx|Ctx]) when ?MATCHING_SYMBOL_P(CurCtx, Type) ->
  forward_to(Matches, [Token|Preceeding], Following, Ctx);

forward_to( Matches
       , Preceeding
       , [#{type := Type} = Token|Following]
       , Ctx) when ?BLOCK_START_P(Type) orelse
                   ?SEQUENCE_START_P(Type) ->
  forward_to(Matches, [Token|Preceeding], Following, [Type|Ctx]);

forward_to( Matches
       , Preceeding
       , [#{type := Type} = Token|Following0] = Following
       , [] = Ctx) ->
  case
    lists:filter( fun(Match) when is_list(Match) ->
                      Match =:= lists:reverse(n_next_code_tokens(length(Match),
                                                                 Preceeding));
                     (Match) ->
                      Match =:= Type
                  end
                , Matches)
  of
    [] -> forward_to(Matches, [Token|Preceeding], Following0, Ctx);
    _  -> {ok, {Preceeding, Following}}
  end;

forward_to( Matches
       , Preceeding
       , [Token|Following]
       , Ctx) ->
  forward_to(Matches, [Token|Preceeding], Following, Ctx).

%% Returns the first token function call expression we're currently standing at
start_of_call(Tokens0) ->
  Token1 = case start_of_call_component(Tokens0) of
             undefined -> undefined;
             Token0 ->
               case drop_to_after_token(Token0, Tokens0) of
                 [#{type := ':'}|Tokens1] -> start_of_call_component(Tokens1);
                 _                        -> Token0
               end
           end,

  case drop_to_after_token(Token1, Tokens0) of
    [#{type := 'fun'} = Token2|_] -> Token2;
    _                             -> Token1
  end.


start_of_call_component([#{type := ')'}|Tokens0]) ->
  Token0 = back_to(['('], Tokens0, []),
  Tokens = drop_token(lists:dropwhile(fun(T) -> T =/= Token0 end, Tokens0)),
  case start_of_call(Tokens) of
    undefined -> Token0;
    Token     -> Token
  end;
start_of_call_component([#{type := Type} = Token|_]) when Type =:= var orelse
                                                          Type =:= 'fun' ->
  Token;
start_of_call_component([#{type := integer}|Rest0]) ->
  case drop_non_code_tokens(Rest0) of
    [#{type := '/'}|Rest1] -> start_of_call_component(Rest1);
    _                      -> undefined
  end;
start_of_call_component([#{type := atom} = Token0|Tokens]) ->
  case drop_non_code_tokens(Tokens) of
    [#{type := '?'} = Token|_] -> Token;
    _                          -> Token0
  end;
start_of_call_component(_) ->
  undefined.

drop_to_after_token(Token, Tokens) ->
  drop_token(drop_to_token(Token, Tokens)).

drop_to_token(Token, Tokens) ->
  lists:dropwhile(fun(T) -> T =/= Token end, Tokens).

drop_token([]) ->
  [];
drop_token(Tokens) ->
  drop_non_code_tokens(tl(Tokens)).

corresponding_delimiter(')')  -> '(';
corresponding_delimiter(']')  -> '[';
corresponding_delimiter('}')  -> '{';
corresponding_delimiter('>>') -> '<<';
corresponding_delimiter('(')  -> ')';
corresponding_delimiter('[')  -> ']';
corresponding_delimiter('{')  -> '}';
corresponding_delimiter('<<') -> '>>'.

%%%_* Tests ===================================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
