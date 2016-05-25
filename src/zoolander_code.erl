%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% @doc Zoolander code utilities
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

-module(zoolander_code).

%%%_* Exports ==================================================================

-export([ lex/1
        , parse/1]).

%%%_* Includes =================================================================
-include_lib("eunit/include/eunit.hrl").

%%%_* Definitions ==============================================================

%%%_* Types ====================================================================

%%%_* API ======================================================================

%%------------------------------------------------------------------------------
%% @doc Lexically analyze Code
%%
-spec lex(Code :: string()) -> [term()].
%%------------------------------------------------------------------------------
lex(Code) ->
  case erl_scan:string(Code, {1, 1}, [return, text]) of
    {ok, Tokens, _} -> convert_tokens(Tokens);
    {error, Reason} -> exit(Reason)
  end.

%%------------------------------------------------------------------------------
%% @doc Parse Code.
%%
-spec parse(Code :: string()) ->
        {Tokens :: [term()], ParseTree :: zipper:zipper()}.
%%------------------------------------------------------------------------------
parse(Code) ->
  Tokens = lex(Code),
  F = parse_forms(Tokens),
  io:fwrite(user, <<"~p ~p ~p: F = ~p~n">>, [self(), ?MODULE, ?LINE, F]),
  ParsedForms = convert_parse_tree(Tokens, parse_forms(Tokens)),
  {Tokens, ParsedForms}.

%%%_* Internal =================================================================

wrap_token_for_parsing(#{type := T, value := V} = Token) -> {T, Token, V};
wrap_token_for_parsing(#{type := T} = Token)             -> {T, Token}.

convert_tokens(Tokens)  ->
  convert_tokens(Tokens, []).

convert_tokens([], Done)  ->
  lists:reverse(Done);
convert_tokens([{dot, Props}|Tokens], Done)  ->
  %% erl_parse adds a newline following a dot into the same token.
  case proplists:get_value(text, Props) of
    "."         ->
      convert_tokens(Tokens, [convert_token_props(dot, Props)|Done]);
    "." ++ WhiteSpace ->
      {Line, Col} = proplists:get_value(location, Props),
      Converted = convert_token_props(dot, [{text, "."}|Props]),
      WSToken = {white_space, [ {text, WhiteSpace}
                            , {location, {Line, Col + 1}}
                            | Props]},
      convert_tokens([WSToken|Tokens], [Converted|Done])
  end;
convert_tokens([ {white_space = T, Props, V}
               | [{white_space, _, "\n" ++ _}|_] = Tokens], Done) ->
  %% Don't merge newlines
  convert_tokens(Tokens, [convert_token_props(T, Props, V)|Done]);
convert_tokens([{white_space, P1, _}, {white_space, P2, _}|Tokens0], Done) ->
  %% erl_parse splits whitespace longer than 16 characters. We don't like that,
  %% so we merge them together.
  Text = proplists:get_value(text, P1) ++ proplists:get_value(text, P2),
  Tokens = [{white_space, [{text, Text}|P1], Text}|Tokens0],
  convert_tokens(Tokens, Done);
convert_tokens([{T, Props}|Tokens], Done) ->
  convert_tokens(Tokens, [convert_token_props(T, Props)|Done]);
convert_tokens([{T, Props, V}|Tokens], Done) ->
  convert_tokens(Tokens, [convert_token_props(T, Props, V)|Done]).


convert_token_props(Type, Props) ->
  Start = proplists:get_value(location, Props),
  #{ type => Type
   , text  => proplists:get_value(text, Props)
   , start => Start
   , 'end' => token_end(Start, proplists:get_value(text, Props))}.

convert_token_props(Type, Props, Value) ->
  (convert_token_props(Type, Props))#{value => Value}.

token_end(End,             []) -> End;
token_end({Line, _Column}, [$\n|Chars]) -> token_end({Line + 1, 1}, Chars);
token_end({Line,  Column}, [_|Chars])   -> token_end({Line, Column + 1}, Chars).


parseable_token_p(#{type := white_space}) -> false;
parseable_token_p(#{type := comment})     -> false;
parseable_token_p(#{type := '?'})         -> false;
parseable_token_p(_)                      -> true.

parse_forms(Tokens0) ->
  Tokens = lists:filtermap(fun(Token) ->
                               case parseable_token_p(Token) of
                                 true  -> {true, wrap_token_for_parsing(Token)};
                                 false -> false
                               end
                           end,
                           Tokens0),
  parse_forms(Tokens, []).

parse_forms([], ParsedForms) ->
  lists:reverse(ParsedForms);
parse_forms(Tokens, ParsedForms) ->
  {FormTokens, Rest} = take_form_tokens(Tokens),
  case erl_parse:parse_form(FormTokens) of
    {ok,    ParsedForm} -> parse_forms(Rest, [ParsedForm|ParsedForms]);
    {error, {#{start := {Line, Column}}, erl_parse, Reason}}     ->
      Err = io_lib:format( "Line ~p, column ~p: ~s"
                         , [Line, Column, lists:append(Reason)]),
      erlang:exit(lists:flatten(erl_parse:format_error(Err)))
  end.

take_form_tokens(Tokens) ->
  take_form_tokens(Tokens, []).

take_form_tokens([], FormTokens) ->
  {lists:reverse(FormTokens), []};
take_form_tokens([{dot, _} = Token|Tokens], FormTokens) ->
  {lists:reverse([Token|FormTokens]), Tokens};
take_form_tokens([Token|Tokens], FormTokens) ->
  take_form_tokens(Tokens, [Token|FormTokens]).

convert_parse_tree(Tokens, Forms) ->
  ChildList = do_convert_parse_tree(Tokens, Forms, []),
  Tree = #{ type => tree
          , children => ChildList
          , start => maps:get(start, hd(ChildList))
          , 'end' => maps:get('end', lists:last(ChildList))},
  IsBranch = fun(#{children := [_|_]}) -> true;
                (_)                    -> false
             end,
  ChildrenF = fun(#{children := Children}) -> Children end,
  MakeNodeF = fun(Node, Children) -> Node#{children => Children} end,
  zipper:new(IsBranch, ChildrenF, MakeNodeF, Tree).

do_convert_parse_tree(_Tokens, [], Done) ->
  lists:reverse(Done);
do_convert_parse_tree(Tokens, [Node0|Nodes], Done) ->
  % Find last non-comment, non-whitespace token before NextNode
  {Preceeding, Rest} =
    case Nodes of
      []           -> {Tokens, []};
      [NextNode|_] -> split_tokens_at_node(NextNode, Tokens)
    end,
  #{'end' := End} = last_code_token(Preceeding),
  Node = case Node0 of
           {_, Props}                      ->
             Props;
           {_, Props, _}                   ->
             Props;
           {T, Props, As, Children}  = A ->
             io:fwrite(user, <<"~p ~p ~p: A = ~p~n">>, [self(), ?MODULE, ?LINE, A]),
             Props#{ type => T
                   , children => do_convert_parse_tree(Preceeding, [As|Children], [])
                   , 'end' => End};
           {T, Props, _As, _G, Children}  = A->
             io:fwrite(user, <<"~p ~p ~p: A = ~p~n">>, [self(), ?MODULE, ?LINE, A]),
             Props#{ type => T
                   , children => do_convert_parse_tree(Preceeding, Children, [])
                   , 'end' => End}
           %% {'try', Props, _As, _G, Children1, Children2} ->
           %%   Props#{ type => T
           %%         , children => do_convert_parse_tree(Preceeding, Children, [])
           %%         , 'end' => End}
         end,
  do_convert_parse_tree(Rest, Nodes, [Node|Done]).

last_code_token(Tokens) ->
  hd(lists:dropwhile(fun(#{type := Type}) ->
                         Type =:= white_space orelse Type =:= comment
                     end,
                     lists:reverse(Tokens))).

split_tokens_at_node(Node, Tokens) ->
  {Ln, Col} = maps:get(start, element(2, Node)),
  lists:splitwith(fun(#{'end' := {EndLn, EndCol}}) ->
                      EndLn < Ln orelse EndLn =:= Ln andalso EndCol < Col
                  end,
                  Tokens).



%%%_* Tests ====================================================================

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
