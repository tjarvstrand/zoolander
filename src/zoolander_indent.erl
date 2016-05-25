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

-define(MATCHING_SYMBOL_P(A, B),
        (A =:= '(' andalso B =:= ')' orelse
         A =:= '{' andalso B =:= '}' orelse
         A =:= '[' andalso B =:= ']' orelse
         A =:= '<<' andalso B =:= '>>' orelse
         A =:= 'end' andalso B =:= 'case' orelse
         A =:= 'end' andalso B =:= 'try' orelse
         A =:= 'end' andalso B =:= 'receive' orelse
         A =:= 'end' andalso B =:= 'begin' orelse
         A =:= 'end' andalso B =:= 'if'orelse
         A =:= 'end' andalso B =:= 'fun')).

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

indent( [ #{type := white_space, text := "\n" ++ _} = Token0
        , #{type := white_space, text := "\n" ++ _}|_] = Tokens0
      , Opts
      , Done) ->
  %io:fwrite(user, <<"-------------------~n">>, []),
  %io:fwrite(user, <<"~p ~p ~p: Token0 = ~p~n">>, [self(), ?MODULE, ?LINE, Token0]),
  [Token|Tokens] = indent_current_line_to(0, Tokens0, Opts),
  indent(Tokens, Opts, [Token|Done]);

indent( [ #{type := white_space, text := "\n" ++ _} = Token0] = Tokens0
      , Opts
      , Done) ->
  %io:fwrite(user, <<"-------------------~n">>, []),
  %io:fwrite(user, <<"~p ~p ~p: Token0 = ~p~n">>, [self(), ?MODULE, ?LINE, Token0]),
  [Token|Tokens] = indent_current_line_to(0, Tokens0, Opts),
  indent(Tokens, Opts, [Token|Done]);

indent( [#{type := white_space, text := "\n" ++ _} = Token0|_] = Tokens0
      , #{indent_width := IndentWidth} = Opts
      , Done) ->
  %io:fwrite(user, <<"-------------------~n">>, []),
  %io:fwrite(user, <<"~p ~p ~p: Token0 = ~p~n">>, [self(), ?MODULE, ?LINE, Token0]),
  NewLvl = level(IndentWidth, Done, Tokens0),
  %io:fwrite(user, <<"~p ~p ~p: NewLvl = ~p~n">>, [self(), ?MODULE, ?LINE, NewLvl]),
  [Token|Tokens] = indent_current_line_to(NewLvl, Tokens0, Opts),
  indent(Tokens, Opts, [Token|Done]);

indent( [Token|Tokens]
      , Opts
      , Done) ->
  %io:fwrite(user, <<"-------------------~n">>, []),
  %io:fwrite(user, <<"~p ~p ~p: Token = ~p~n">>, [self(), ?MODULE, ?LINE, Token]),
  indent(Tokens, Opts, [Token|Done]).

level(IndentWidth, Preceeding, Following) ->
  case drop_non_code_tokens(Following) of
    [#{type := Type}|_Rest] when Type =:= ',' orelse
                                 Type =:= '|' orelse
                                 Type =:= '||' ->
      %io:fwrite(user, <<"~p ~p ~p~n">>, [self(), ?MODULE, ?LINE]),
      case start_of_enclosing_expr(Preceeding) of
        undefined ->
          0;
        #{type := Type2, 'end' := {_, EndCol}} when Type2 =:= '(' orelse
                                                    Type2 =:= '[' orelse
                                                    Type2 =:= '{' orelse
                                                    Type2 =:= '<<' orelse
                                                    Type2 =:= 'when' ->
          EndCol - 2;
        #{type := Type2, 'end' := {_, EndCol}} when Type2 =:= 'case' orelse
                                                    Type2 =:= 'try' orelse
                                                    Type2 =:= 'receive' orelse
                                                    Type2 =:= 'begin' orelse
                                                    Type2 =:= 'if' orelse
                                                    Type2 =:= 'of' orelse
                                                    Type2 =:= 'after' orelse
                                                    Type2 =:= 'when' orelse
                                                    Type2 =:= '->' orelse
                                                    Type2 =:= '{' orelse
                                                    Type2 =:= '[' orelse
                                                    Type2 =:= '(' orelse
                                                    Type2 =:= '<<' orelse
                                                    Type2 =:= '->' ->
          EndCol - 2;
        Token ->
          [#{start := {_, StartCol}}|_] =
            lists:dropwhile(fun(#{type := T}) ->
                                not lists:member(T, ['(', '[', '{', '<<'])
                            end,
                            drop_to_token(Token, lists:reverse(Preceeding))),
          StartCol - 1
      end;
    [#{type := ')'}|_] ->
      case start_of_enclosing_expr(Preceeding) of
        #{type := '(', start := {_, StartCol}} ->
          StartCol - 1;
        #{start := {_, StartCol}} ->
          StartCol - 1 + IndentWidth
      end;
    [#{type := Type}|_Rest] when Type =:= '}' orelse
                                 Type =:= ']' orelse
                                 Type =:= '>>' ->
      #{start := {_, StartCol}} = start_of_enclosing_expr(Preceeding),
      StartCol - 1;
    [#{type := Type}|_] when Type =:= '->' ->
      #{start := {_, StartCol}} = start_of_expr(Preceeding),
      StartCol - 1;
    [#{type := Type} = Token|_] when Type =:= 'end' ->
      #{start := {_, StartCol}} = start_of_expr([Token|Preceeding]),
      StartCol - 1;
    [#{type := Type}|_] when Type =:= 'of' orelse
                             Type =:= 'after' ->
      #{start := {_, StartCol}} = start_of_enclosing_block(Preceeding),
      StartCol - 1;
    [#{type := Type}|Rest] when Type =:= 'catch' ->
      case subsequent_catch_p(Rest) of
        true -> %% Indent as normal expression
          case start_of_expr(Preceeding) of
            undefined ->
              case start_of_enclosing_expr(Preceeding) of
                undefined                 -> IndentWidth;
                #{start := {_, StartCol}} -> StartCol - 1 + IndentWidth
              end;
            #{start := {_, StartCol}} ->
              StartCol - 1 + IndentWidth
          end;
        false -> %% Indent from try
          case start_of_enclosing_block(Preceeding) of
            undefined                 -> 0;
            #{start := {_, StartCol}} -> StartCol - 1
          end
      end;
    [#{type := dot}|_] ->
      0;
    _ ->
      case drop_non_code_tokens(Preceeding) of
        [#{type := Type}|Rest] when Type =:= '=' ->
          #{start := {_, StartCol}} = start_of_expr(Rest),
          StartCol - 1 + IndentWidth;
        [#{type := Type}|Rest] when Type =:= ',' orelse
                                    Type =:= '|' orelse
                                    Type =:= '||' ->
          #{start := {_, StartCol}} = start_of_expr(Rest, match_as_single_expr),
          StartCol - 1;
        [#{type := Type}|Rest] when Type =:= 'of' orelse
                                    Type =:= 'after' orelse
                                    Type =:= 'catch' ->
          case start_of_enclosing_block(Rest) of
            undefined                 -> IndentWidth;
            #{start := {_, StartCol}} -> StartCol - 1 + IndentWidth
          end;
        [#{type := Type}|Rest] when Type =:= ';' ->
          case start_of_enclosing_expr(Rest) of
            #{type := 'when'} = When ->
              [_, #{start := {_, StartCol}}|_] =
                drop_to_token(When, lists:reverse(Rest)),
              StartCol - 1;
            _ ->
              case start_of_clause(Rest) of
                undefined                 -> IndentWidth;
                #{start := {_, StartCol}} -> StartCol - 1
              end
          end;
        [#{type := Type}|Rest] when Type =:= '->' ->
          case start_of_clause(Preceeding) of
            undefined                 ->
              IndentWidth;
            #{type := '(', start := {_, ParenStartCol}} = OpenParen ->
              Tokens = drop_to_after_token(OpenParen, Rest),
              case n_next_code_tokens(2, Tokens) of
                [#{type := ';'}|_] ->
                  ParenStartCol - 1 + IndentWidth;
                %% Fun in code
                [#{type := 'fun', start := {_, FunStartCol}}|_] ->
                  FunStartCol - 1 + IndentWidth;
                %% fun in spec
                [ #{type := '(', start := {_, ClauseStartCol}}
                , #{type := 'fun'}|_] ->
                  ClauseStartCol - 1 + IndentWidth;
                Rest1 ->
                  #{start := {_, FunStartCol}} = start_of_call(Rest1),
                  FunStartCol - 1 + IndentWidth
              end;
            #{start := {_, StartCol}} ->
              StartCol - 1 + IndentWidth
          end;
        [#{type := Type, 'end' := {_, EndCol} }|_] when Type =:= '{' orelse
                                                        Type =:= '[' orelse
                                                        Type =:= '<<' ->
          EndCol - 1;
        [] ->
          0;
        [#{type := dot}|_] ->
          0;
        Tokens ->
          case start_of_expr(Tokens) of
            undefined ->
              case start_of_enclosing_expr(Tokens) of
                undefined                 -> IndentWidth;
                #{start := {_, StartCol}} -> StartCol - 1 + IndentWidth
              end;
            #{start := {_, StartCol}} ->
              StartCol - 1 + IndentWidth
          end
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

%% Returns the token that terminates the current block
subsequent_catch_p(Tokens) ->
    subsequent_catch_p(Tokens, []).

subsequent_catch_p([], _Ctx) ->
  false;
subsequent_catch_p( [#{type := 'end'}|Rest]
            , [Block|Ctx]) when Block =:= 'case' orelse
                                Block =:= 'try' orelse
                                Block =:= 'receive' orelse
                                Block =:= 'begin' orelse
                                Block =:= 'if' orelse
                                Block =:= 'fun' orelse
                                Block =:= 'when' ->
  subsequent_catch_p(Rest, Ctx);
subsequent_catch_p( [#{type := Type}|_]
            , []) when Type =:= 'of' orelse
                       Type =:= 'after' orelse
                       Type =:= 'end' orelse
                       Type =:= ',' orelse
                       Type =:= ';' orelse
                       Type =:= '.' ->
  false;
subsequent_catch_p( [#{type := Type}|_], []) when  Type =:= 'catch' ->
  true;
subsequent_catch_p([#{type := Type}|Rest], Ctx) when Type =:= white_space orelse
                                                       Type =:= comment ->
  subsequent_catch_p(Rest, Ctx);
subsequent_catch_p([#{type := Type}|Rest]
            , Ctx) when Type =:= 'case' orelse
                        Type =:= 'try' orelse
                        Type =:= 'receive' orelse
                        Type =:= 'begin' orelse
                        Type =:= 'fun' orelse
                        Type =:= 'if' ->
  subsequent_catch_p(Rest, [Type|Ctx]);

subsequent_catch_p([_|Rest], Ctx) ->
  subsequent_catch_p(Rest, Ctx).

%% Returns the token before the first token  of the first expression in the
%% current comma-separated list.
start_of_sequence(Tokens) ->
  case start_of_first_expr(Tokens) of
    undefined ->
      case
        lists:dropwhile(fun(#{type := T}) ->
                            not lists:member(T, ['(', '[', '{', '<<'])
                        end,
                        Tokens)
      of
        []        -> undefined;
        [Token|_] -> Token
      end;
    Token ->
      hd(drop_to_after_token(Token, Tokens))
  end.

%% Returns the first token of the first expression in the current comma-
%% separated list
start_of_first_expr(Tokens) ->
    start_of_first_expr(Tokens, [], undefined).

start_of_first_expr([], _Ctx, Last) ->
  Last;
start_of_first_expr( [#{type := Type} = Token|Rest]
                   , ['end'|Ctx]
                   , _Last) when Type =:= 'case' orelse
                                 Type =:= 'try' orelse
                                 Type =:= 'receive' orelse
                                 Type =:= 'begin' orelse
                                 Type =:= 'if' orelse
                                 Type =:= 'fun' ->
  start_of_first_expr(Rest, Ctx, Token);
start_of_first_expr( [#{type := Type} = Token|Rest]
        , [Ctx0|_] = Ctx
        , _Last) when Ctx0 =/= 'end' andalso
                      (Type =:= 'of' orelse
                       Type =:= 'after' orelse
                       Type =:= 'catch') ->
  start_of_first_expr(Rest, [Type|Ctx], Token);
start_of_first_expr( [#{type := Type} = Token|Rest], [CurCtx|Ctx], _Last)
  when ?MATCHING_SYMBOL_P(Type, CurCtx) ->
  start_of_first_expr(Rest, Ctx, Token);
start_of_first_expr( [#{type := Type}|_]
        , []
        , Last) when Type =:= 'case' orelse
                     Type =:= 'try' orelse
                     Type =:= 'receive' orelse
                     Type =:= 'begin' orelse
                     Type =:= 'if' orelse
                     Type =:= 'of' orelse
                     Type =:= 'after' orelse
                     Type =:= 'when' orelse
                     Type =:= '->' orelse
                     Type =:= '{' orelse
                     Type =:= '[' orelse
                     Type =:= '(' orelse
                     Type =:= '<<' orelse
                     Type =:= dot ->
  Last;
start_of_first_expr( [#{type := Type}|Rest]
                   , Ctx
                   , Last) when Type =:= white_space orelse
                                Type =:= comment ->
  start_of_first_expr(Rest, Ctx, Last);
start_of_first_expr([#{type := Type} = Token|Rest], Ctx, _Last) when Type =:= ')' orelse
                                                          Type =:= ']' orelse
                                                          Type =:= '}' orelse
                                                          Type =:= '>>' orelse
                                                          Type =:= 'end' ->
  start_of_first_expr(Rest, [Type|Ctx], Token);

start_of_first_expr([Token|Rest], Ctx, _Last) ->
  start_of_first_expr(Rest, Ctx, Token).

start_of_expr(Tokens) ->
    start_of_expr(Tokens, undefined).

start_of_expr(Tokens, Mode) ->
    start_of_expr(Tokens, [], undefined, Mode).

start_of_expr([], _Ctx, Last, _Mode) ->
  Last;
start_of_expr( [#{type := Type} = Token|Rest]
             , ['end'|Ctx]
             , _Stack
             , Mode) when Type =:= 'case' orelse
                          Type =:= 'try' orelse
                          Type =:= 'receive' orelse
                          Type =:= 'begin' orelse
                          Type =:= 'if' orelse
                          Type =:= 'fun'->
  start_of_expr(Rest, Ctx, Token, Mode);
start_of_expr( [#{type := Type} = Token|Rest]
             , [Ctx0|_] = Ctx
             , _Last
             , Mode) when Ctx0 =/= 'end' andalso
                          (Type =:= 'of' orelse
                           Type =:= 'after' orelse
                           Type =:= 'catch') ->
  start_of_expr(Rest, [Type|Ctx], Token, Mode);
start_of_expr( [#{type := Type} = Token|Rest], [CurCtx|Ctx], _Last, Mode)
  when ?MATCHING_SYMBOL_P(Type, CurCtx) ->
  start_of_expr(Rest, Ctx, Token, Mode);
start_of_expr( [#{type := Type}|_]
             , []
             , #{type := LastType} = Last
             , _Mode) when (Type =:= atom andalso
                            Type =:= LastType) ->
  %% stop before things like -spec and -type
  Last;
start_of_expr( [#{type := Type}|_]
             , []
             , Last
             , Mode) when Type =:= 'case' orelse
                          Type =:= 'try' orelse
                          Type =:= 'receive' orelse
                          Type =:= 'begin' orelse
                          Type =:= 'if' orelse
                          Type =:= 'of' orelse
                          Type =:= 'after' orelse
                          Type =:= 'when' orelse
                          Type =:= '->' orelse
                          Type =:= '{' orelse
                          Type =:= '[' orelse
                          Type =:= '(' orelse
                          Type =:= '<<' orelse
                          Type =:= ',' orelse
                          Type =:= ';' orelse
                          Type =:= dot orelse
                          (Type =:= '=' andalso
                           Mode =/= match_as_single_expr) ->
  Last;
start_of_expr([#{type := Type}|Rest]
             , Ctx
             , Last
             , Mode) when Type =:= white_space orelse
                          Type =:= comment ->
  start_of_expr(Rest, Ctx, Last, Mode);
start_of_expr([#{type := Type} = Token|Rest]
             , Ctx
             , _Last
             , Mode) when Type =:= ')' orelse
                          Type =:= ']' orelse
                          Type =:= '}' orelse
                          Type =:= '>>' orelse
                          Type =:= 'end' ->
  start_of_expr(Rest, [Type|Ctx], Token, Mode);

start_of_expr([Token|Rest], Ctx, _Last, Mode) ->
  start_of_expr(Rest, Ctx, Token, Mode).


start_of_enclosing_block(Tokens) ->
  case start_of_enclosing_expr(Tokens) of
    undefined ->
      undefined;
    #{type := Type} = Token when Type =:= 'case' orelse
                                 Type =:= 'if' orelse
                                 Type =:= 'begin' orelse
                                 Type =:= 'receive' orelse
                                 Type =:= 'try' orelse
                                 Type =:= 'fun' ->
      Token;
    #{type := '->'} = Token ->
      Token1 = start_of_clause(drop_to_token(Token, Tokens)),
      Tokens1 = drop_to_token(Token1, Tokens),
      case drop_non_code_tokens(drop_token(Tokens1)) of
        [#{type := ';'}|Rest] ->
          start_of_enclosing_block(Rest);
        _                     ->
          start_of_enclosing_block(Tokens1)
      end;
    Token ->
      start_of_enclosing_block(drop_to_after_token(Token, Tokens))
  end.

start_of_clause(Tokens0) ->
  case start_of_enclosing_expr(Tokens0) of
    undefined ->
      undefined;
    Token0 = #{type := Type} when Type =:= '->' ->
      Tokens1 = drop_to_after_token(Token0, Tokens0),
      Tokens2 = case start_of_enclosing_expr(Tokens1) of
                  #{type := 'when'} = When ->
                    drop_non_code_tokens(drop_token(drop_to_token(When, Tokens0)));
                  _ ->
                    Tokens1
                end,
      case Tokens2 of
        [#{type := ')'}|Tokens3] = Tokens4 ->
          case start_of_enclosing_expr(Tokens3) of
            #{type := 'fun'} = Token ->
              %% Get the token after Token
              hd(drop_token(drop_to_token(Token, lists:reverse(Tokens3))));
            _Token ->
              start_of_expr(Tokens4)
          end;
        _ ->
          start_of_expr(drop_to_after_token(Token0, Tokens0))
      end
  end.

%% Returns the first token of the expression enclosing the current one
start_of_enclosing_expr(Tokens0) ->
  Tokens = case start_of_first_expr(Tokens0) of
             undefined -> Tokens0;
             Token0 ->
               case drop_to_after_token(Token0, Tokens0) of
                 [#{type := Type}|Rest] when Type =:= 'of' orelse
                                             Type =:= 'after' ->
                   drop_to_token(
                     start_of_enclosing_expr(drop_non_code_tokens(Rest)),
                     Rest);
                 Tokens1 ->
                   Tokens1
               end
           end,
  case drop_non_code_tokens(Tokens) of
    []                 -> undefined;
    [#{type := '('} = Token1|Tokens2] ->
      case start_of_call(Tokens2) of
        undefined ->
          Token1;
        Token     ->
          Token
      end;
    [Token1|_] ->
      Token1
  end.

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
  Token0 = start_of_enclosing_expr(Tokens0),
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


%%%_* Tests ===================================================================

start_of_clause_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_clause/1, ""))

  , ?_assertEqual("d",
                  string(fun start_of_clause/1, "case a of"
                                                "  b -> c;"
                                                "  d -> e;"))

  , ?_assertEqual("(",
                  string(fun start_of_clause/1, "A = fun() -> "))

  , ?_assertEqual("foo",
                  string(fun start_of_clause/1, "foo() -> "))

  , ?_assertEqual("foo",
                  string(fun start_of_clause/1, "a.\n"
                                                "foo() -> "))

  , ?_assertEqual("foo",
                  string(fun start_of_clause/1, "-spec foo() -> "))

  , ?_assertEqual("foo",
                  string(fun start_of_clause/1, "define(foo, bar).\n"
                                                "-spec foo() -> "))

  , ?_assertEqual("1",
                  string(fun start_of_clause/1, "1 + (2 + 4) -> "))

  , ?_assertEqual("(",
                  string(fun start_of_clause/1, "() -> "))

  , ?_assertEqual("(",
                  string(fun start_of_clause/1, "fun() when A =:= B ->\n"))
  ].

start_of_enclosing_block_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_enclosing_block/1, ""))

  , ?_assertEqual("case", string(fun start_of_enclosing_block/1, "case"))

  , ?_assertEqual("case", string(fun start_of_enclosing_block/1, "case b of a"))

  , ?_assertEqual("case",
                  string(fun start_of_enclosing_block/1, "case b of a -> c"))

  , ?_assertEqual("case",
                  string(fun start_of_enclosing_block/1, "case a of"
                                                         "  b -> c;"
                                                         "  d -> e;"))

  , ?_assertEqual("try",
                  string(fun start_of_enclosing_block/1, "try a of"
                                                         "  b -> c;"
                                                         "  d -> e\n"
                                                         "catch\n"
                                                         "  _ ->"))

  , ?_assertEqual("fun",
                  string(fun start_of_enclosing_block/1, "fun(A) -> a"))

  , ?_assertEqual("fun",
                  string(fun start_of_enclosing_block/1, "fun(A) -> a;"
                                                         "   (B) -> b;"
                                                         "   (C) -> c\n"))
  ].


start_of_enclosing_expr_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_enclosing_expr/1, ""))
  , ?_assertEqual(undefined, string(fun start_of_enclosing_expr/1, "fun()"))
  , ?_assertEqual(undefined, string(fun start_of_enclosing_expr/1, "foo()"))
  , ?_assertEqual("foo", string(fun start_of_enclosing_expr/1, "foo( a"))
  , ?_assertEqual("fun", string(fun start_of_enclosing_expr/1, "fun( a"))
  , ?_assertEqual("case", string(fun start_of_enclosing_expr/1, "case"))
  , ?_assertEqual("(", string(fun start_of_enclosing_expr/1, "("))
  , ?_assertEqual("[", string(fun start_of_enclosing_expr/1, "["))

  , ?_assertEqual("case", string(fun start_of_enclosing_expr/1, "case a"))
  , ?_assertEqual("(", string(fun start_of_enclosing_expr/1, "( a"))
  , ?_assertEqual("[", string(fun start_of_enclosing_expr/1, "[ a"))

  , ?_assertEqual("case", string(fun start_of_enclosing_expr/1, "case b, a"))
  , ?_assertEqual("when", string(fun start_of_enclosing_expr/1, "when b, a"))
  , ?_assertEqual("(", string(fun start_of_enclosing_expr/1, "( b, a"))
  , ?_assertEqual("[", string(fun start_of_enclosing_expr/1, "[ b, a"))

  , ?_assertEqual("->", string(fun start_of_enclosing_expr/1, "case b of a -> c"))
  , ?_assertEqual("case", string(fun start_of_enclosing_expr/1, "case b of a"))
  , ?_assertEqual("when", string(fun start_of_enclosing_expr/1, "when A=:=B,C =:= D"))
  , ?_assertEqual("when", string(fun start_of_enclosing_expr/1, "when A=:=B;C =:= D"))

  , ?_assertEqual("f", string(fun start_of_enclosing_expr/1, "f:e(a, b"))
  , ?_assertEqual("?", string(fun start_of_enclosing_expr/1, "?f:e(a, b"))

  , ?_assertEqual("->", string(fun start_of_enclosing_expr/1, "fun() -> a"))

  , ?_assertEqual("->", string(fun start_of_enclosing_expr/1, "case a of a -> case b of c when d end"))

  ].


start_of_sequence_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_sequence/1, ""))
  , ?_assertEqual("(", string(fun start_of_sequence/1, "("))
  , ?_assertEqual("[", string(fun start_of_sequence/1, "["))
  , ?_assertEqual("(", string(fun start_of_sequence/1, "foo("))
  , ?_assertEqual("(", string(fun start_of_sequence/1, "foo( a, "))

  , ?_assertEqual("(", string(fun start_of_sequence/1, "( a"))
  , ?_assertEqual("[", string(fun start_of_sequence/1, "[ a"))
  , ?_assertEqual("{", string(fun start_of_sequence/1, "{ a"))
  , ?_assertEqual("<<", string(fun start_of_sequence/1, "<< a"))

  , ?_assertEqual("(", string(fun start_of_sequence/1, "( b, a"))
  , ?_assertEqual("[", string(fun start_of_sequence/1, "[ b, a"))

  , ?_assert(
       begin
         Tokens = zoolander_code:lex("foo:(A)(a, "),
         #{text := "(", start := {_, 8}} = start_of_sequence(lists:reverse(Tokens)),
         true
       end)

  ].


start_of_first_expr_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_first_expr/1, ""))
  , ?_assertEqual(undefined, string(fun start_of_first_expr/1, "case"))
  , ?_assertEqual(undefined, string(fun start_of_first_expr/1, "("))
  , ?_assertEqual(undefined, string(fun start_of_first_expr/1, "["))
  , ?_assertEqual("fun", string(fun start_of_first_expr/1, "fun()"))
  , ?_assertEqual("foo", string(fun start_of_first_expr/1, "foo()"))
  , ?_assertEqual("a", string(fun start_of_first_expr/1, "foo:(A)( a, "))

  , ?_assertEqual("a", string(fun start_of_first_expr/1, "case a"))
  , ?_assertEqual("a", string(fun start_of_first_expr/1, "( a"))
  , ?_assertEqual("a", string(fun start_of_first_expr/1, "[ a"))

  , ?_assertEqual("b", string(fun start_of_first_expr/1, "case b, a"))
  , ?_assertEqual("b", string(fun start_of_first_expr/1, "when b, a"))
  , ?_assertEqual("b", string(fun start_of_first_expr/1, "( b, a"))
  , ?_assertEqual("b", string(fun start_of_first_expr/1, "[ b, a"))

  , ?_assertEqual("c", string(fun start_of_first_expr/1, "case b of a -> c"))
  , ?_assertEqual("case", string(fun start_of_first_expr/1, "case a of a -> case b of c when d end"))
  ].

start_of_expr_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_expr/1, ""))
  , ?_assertEqual(undefined, string(fun start_of_expr/1, "case"))
  , ?_assertEqual(undefined, string(fun start_of_expr/1, "fun("))
  , ?_assertEqual(undefined, string(fun start_of_expr/1, "fun() ->"))
  , ?_assertEqual(undefined, string(fun start_of_expr/1, "("))
  , ?_assertEqual(undefined, string(fun start_of_expr/1, "["))

  , ?_assertEqual("a", string(fun start_of_expr/1, "case a"))
  , ?_assertEqual("a", string(fun start_of_expr/1, "( a"))
  , ?_assertEqual("a", string(fun start_of_expr/1, "[ a"))

  , ?_assertEqual("a", string(fun start_of_expr/1, "case b, a"))
  , ?_assertEqual("case", string(fun start_of_expr/1, "case a of b -> c end"))
  , ?_assertEqual("fun", string(fun start_of_expr/1, "fun()"))
  , ?_assertEqual("fun", string(fun start_of_expr/1, "fun() -> a end"))
  , ?_assertEqual("foo", string(fun start_of_expr/1, "foo:bar()"))
  , ?_assertEqual("foo", string(fun start_of_expr/1, "foo()"))
  , ?_assertEqual("fun", string(fun start_of_expr/1, "fun() -> a end"))
  , ?_assertEqual("fun", string(fun start_of_expr/1, "fun() when A =:= B -> a end"))
  , ?_assertEqual("a", string(fun start_of_expr/1, "when b, a"))
  , ?_assertEqual("a", string(fun start_of_expr/1, "( b, a"))
  , ?_assertEqual("a", string(fun start_of_expr/1, "[ b, a"))

  , ?_assertEqual("c", string(fun start_of_expr/1, "case b of a -> c"))

  , ?_assertEqual("a", string(fun start_of_expr/1, "a + (1 + 2)"))
  , ?_assertEqual("a", string(fun start_of_expr/1, "(a + (1 + 2)"))
  , ?_assertEqual("A", string(fun start_of_expr/1, "A + \"abc\" \"cde\""))

  , ?_assertEqual("catch", string(fun start_of_expr/1, "catch A()"))

  , ?_assertEqual("b", string(fun start_of_expr/1, "A = b"))
  ].

start_of_call_component_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_call_component/1, ""))
  , ?_assertEqual("a", string(fun start_of_call_component/1, "a"))
  , ?_assertEqual("a", string(fun start_of_call_component/1, "a/1"))
  , ?_assertEqual("b", string(fun start_of_call_component/1, "a:b"))
  , ?_assertEqual("(", string(fun start_of_call_component/1, "a:(foo:bar())"))
  , ?_assertEqual("?", string(fun start_of_call_component/1, "?a"))
  , ?_assertEqual("?", string(fun start_of_call_component/1, "?a(a)"))
  ].

start_of_call_test_() ->
  [ ?_assertEqual(undefined, string(fun start_of_call_component/1, ""))
  , ?_assertEqual("a", string(fun start_of_call/1, "a"))
  , ?_assertEqual("a", string(fun start_of_call/1, "a:b"))
  , ?_assertEqual("a", string(fun start_of_call/1, "a:(foo:bar())"))
  , ?_assertEqual("?", string(fun start_of_call/1, "?a(a)"))
  , ?_assertEqual("fun", string(fun start_of_call/1, "fun"))
  , ?_assertEqual("fun", string(fun start_of_call/1, "fun foo/1"))
  , ?_assertEqual("fun", string(fun start_of_call/1, "fun foo:bar/1"))

  , ?_assertEqual("?", string(fun start_of_call/1, "?a(a)"))
  , ?_assertEqual("?", string(fun start_of_call/1, "?a(a)"))

  , ?_assertEqual("a", string(fun start_of_call/1, "-spec a"))
  , ?_assertEqual("a", string(fun start_of_call/1, "-spec a:b"))
  ].

adjust_whitespace_width_test_() ->
  [ ?_assertEqual( #{text => "\n"}
                 , adjust_whitespace_width(2, -2, #{text => "\n  "}))
  ].

subsequent_catch_p_test_() ->
  [ ?_assertNot(subsequent_catch_p(zoolander_code:lex("try")))
  , ?_assert(subsequent_catch_p(zoolander_code:lex("catch")))
  , ?_assertNot(subsequent_catch_p(zoolander_code:lex("try a catch a -> b end, catch")))
  , ?_assert(subsequent_catch_p(zoolander_code:lex("try a catch a -> b end catch ")))
  ].

%%%_* Test helpers =============================================================

string(Fun, String) ->
  Tokens0 = zoolander_code:lex(String),
  case Fun(lists:reverse(Tokens0)) of
    #{text := Text} -> Text;
    undefined -> undefined
  end.

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
