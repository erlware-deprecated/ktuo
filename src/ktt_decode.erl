%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
%%%-------------------------------------------------------------------
%%% Permission is hereby granted, free of charge, to any
%%% person obtaining a copy of this software and associated
%%% documentation files (the "Software"), to deal in the
%%% Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute,
%%% sublicense, and/or sell copies of the Software, and to permit
%%% persons to whom the Software is furnished to do so, subject to
%%% the following conditions:
%%%
%%% The above copyright notice and this permission notice shall
%%% be included in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%%% OTHER DEALINGS IN THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @copyright (C) 2006-2010 Erlware
%%% @doc
%%%  Does a safe parsing of erlang tuple syntax. If it finds an atom it
%%%  uses list_to_existing atom to translate it. This will only work
%%%  if that atom is already in the atom table. This means its a bit quirky
%%%  but it works for what I need.
%%% @end
%%% Created : 19 Dec 2006 by Eric Merritt
%%%-------------------------------------------------------------------
-module(ktt_decode).

-include_lib("eunit/include/eunit.hrl").

-export([decode/1,
	 decode/3]).

-export_type([ktt_list/0,
	      ktt_string/0,
	      ktt_number/0,
	      ktt_atom/0,
	      value/0,
	      stream/0,
	      position_indicator/0,
	      result/0]).
%%=============================================================================
%% Types
%%=============================================================================
-type ktt_list() :: list().
-type ktt_string() :: binary().
-type ktt_number() :: number().
-type ktt_atom() :: atom().
-type value() :: ktt_list() | ktt_string() | ktt_number() | ktt_atom().
-type stream() :: binary() | string().
-type position_indicator() :: {integer(), integer()}.
-type result() :: {[value()], string(), position_indicator()}.

%%=============================================================================
%% API
%%=============================================================================
%% @doc
%%  Parses the incoming stream into valid erlang objects.
%%  ``
%%   Tuple   ==   Erlang
%%   List        List
%%   String      binary
%%   Number      Number
%%   Atom        Atom
%%  ''
%%  This decode function parses a subset of erlang data notation.
%% @end
-spec decode(stream()) -> result().
decode(Stream) when is_binary(Stream) ->
   decode(binary_to_list(Stream), 0, 0);
decode(Stream) ->
   decode(Stream, 0, 0).

-spec decode(stream(), integer(), integer()) -> result().
decode(Stream, NewLines, Chars) ->
    value(Stream, NewLines, Chars).


%%=============================================================================
%% Internal Functions
%%=============================================================================
%% @doc
%%  Parse a tuple value.
%% @end
-spec value(string(), integer(), integer()) -> result().
value([$\" | T], NewLines, Chars) ->
    ktuo_parse_utils:stringish_body($\", T, [], NewLines, Chars + 1);
value([$\' | T], NewLines, Chars) ->
    case ktuo_parse_utils:stringish_body($\', T, [], NewLines, Chars + 1) of
        {String, Rest, Pos} ->
            {list_to_existing_atom(String), Rest, Pos};
        Else ->
          Else
    end;
value([$- | T], NewLines, Chars) ->
    ktuo_parse_utils:digit19(T, [$-], NewLines, Chars + 1);
value([$0 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$0], front, NewLines, Chars + 1);
value([$1 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$1], front, NewLines, Chars + 1);
value([$2 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$2], front, NewLines, Chars + 1);
value([$3 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$3], front, NewLines, Chars + 1);
value([$4 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$4], front, NewLines, Chars + 1);
value([$5 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$5], front, NewLines, Chars + 1);
value([$6 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$6], front, NewLines, Chars + 1);
value([$7 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$7], front, NewLines, Chars + 1);
value([$8 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$8], front, NewLines, Chars + 1);
value([$9 | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$9], front, NewLines, Chars + 1);
value([$[ | T], NewLines, Chars) ->
    list_body(T, [], NewLines, Chars + 1);
value([${ | T], NewLines, Chars) ->
    tuple_body(T, [], NewLines, Chars + 1);
value([$\s | T], NewLines, Chars) ->
    value(T, NewLines, Chars + 1);
value([$\t | T], NewLines, Chars) ->
    value(T, NewLines, Chars + 1);
value([$\r | T], NewLines, _Chars) ->
    value(T, NewLines + 1, 0);
value([$\n | T], NewLines, _Chars) ->
    value(T, NewLines + 1, 0);
value(Stream, NewLines, Chars) ->
    bare_atom(Stream, [], NewLines, Chars).


%% @doc
%%  Parse a list body. A list is [ elements, ..].
%% @end
-spec list_body(string(), [value()], integer(), integer()) ->
    result() | {error, {string(), integer(), integer()}}.
list_body([$] | T], Acc, NewLines, Chars) ->
    {lists:reverse(Acc), T, {NewLines, Chars + 1}};
list_body([$, | T], Acc, NewLines, Chars) ->
    list_body(T, Acc, NewLines, Chars + 1);
list_body([$\s | T], Acc, NewLines, Chars) ->
    list_body(T, Acc, NewLines, Chars + 1);
list_body([$\t | T], Acc, NewLines, Chars) ->
    list_body(T, Acc, NewLines, Chars + 1);
list_body([$\n | T], Acc, NewLines, _Chars) ->
    list_body(T, Acc, NewLines + 1, 0);
list_body([$\r | T], Acc, NewLines, _Chars) ->
    list_body(T, Acc, NewLines + 1, 0);
list_body(Stream, Acc, NewLines, Chars) ->
    case value(Stream, NewLines, Chars) of
        {Value, Rest, {N, C}} ->
            list_body(Rest, [Value | Acc], N, C);
        Else ->
            Else
    end.

%% @doc
%%  Parse the tuple body. Tuple bodies are of the form { element, ..}.
%% @end
-spec tuple_body(string(), [value()], integer(), integer()) ->
    result() | {error, {string(), integer(), integer()}}.
tuple_body([$} | T], Acc, NewLines, Chars) ->
    {list_to_tuple(lists:reverse(Acc)), T, {NewLines, Chars + 1}};
tuple_body([$, | T], Acc, NewLines, Chars) ->
    tuple_body(T, Acc, NewLines, Chars + 1);
tuple_body([$\s | T], Acc, NewLines, Chars) ->
    tuple_body(T, Acc, NewLines, Chars + 1);
tuple_body([$\t | T], Acc, NewLines, Chars) ->
    tuple_body(T, Acc, NewLines, Chars + 1);
tuple_body([$\n | T], Acc, NewLines, _Chars) ->
    tuple_body(T, Acc, NewLines + 1, 0);
tuple_body([$\r | T], Acc, NewLines, _Chars) ->
    tuple_body(T, Acc, NewLines + 1, 0);
tuple_body(Stream, Acc, NewLines, Chars) ->
    case value(Stream, NewLines, Chars) of
        {Value, Rest, {N, C}} ->
            tuple_body(Rest, [Value | Acc], N, C);
        Else ->
            Else
    end.


%% @doc
%%  Parse an atom that doesn't have single quote delimeters.
%% @end
-spec bare_atom(string(), string(), integer(), integer()) ->
    result() | {error, {string(), integer(), integer()}}.
bare_atom([H | T], Acc, NewLines, Chars) when H >= $a, H =< $z ->
    bare_atom(T, [H | Acc], NewLines, Chars + 1);
bare_atom([H | T], Acc, NewLines, Chars) when H >= $A, H =< $Z ->
    bare_atom(T, [H | Acc], NewLines, Chars + 1);
bare_atom([$_ | T], Acc, NewLines, Chars) ->
    bare_atom(T, [$_ | Acc], NewLines, Chars + 1);
bare_atom([H | T], Acc, NewLines, Chars) when H >= $0, H =< $9 ->
    bare_atom(T, [ H | Acc], NewLines, Chars + 1);
bare_atom([$\s | T], Acc, NewLines, Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines, Chars + 1}};
bare_atom([$\t | T], Acc, NewLines, Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines, Chars + 1}};
bare_atom([$\r | T], Acc, NewLines, _Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines + 1, 0}};
bare_atom([$\n | T], Acc, NewLines, _Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), T, {NewLines + 1, 0}};
bare_atom([], Acc, NewLines, Chars) ->
    {list_to_existing_atom(lists:reverse(Acc)), [], {NewLines, Chars}};
bare_atom(Else, Acc, NewLines, Chars) when length(Acc) > 0->
    {list_to_existing_atom(lists:reverse(Acc)), Else, {NewLines, Chars}}.


%%=============================================================================
%% Unit tests
%%=============================================================================
decode_tuple_test() ->
    ?assertMatch({{hello, 43}, [], {0, 11}}, decode("{hello, 43}")),
    ?assertMatch({{{zoom}, 321, {32, "oeaueou"}}, [], {1, 2}},
                 decode("{{zoom}, 321, {32, \"oeaueou\"\n}}")).

decode_atom_test() ->
    ?assertMatch({hello, [], {0, 5}}, decode("hello")),
    ?assertMatch({goodbye, [], {0, 7}}, decode("goodbye")),
    ?assertMatch({'ba ba black sheep', [], {0, 19}},
                 decode("'ba ba black sheep'")).

decode_number_test() ->
    ?assertMatch({44, [], {0, 2}}, decode("44")),
    ?assertMatch({-44, [], {0, 3}}, decode("-44")),
    ?assertMatch({44.00, [], {0, 5}}, decode("44.00")),
    ?assertMatch({-44.01, [], {0, 6}}, decode("-44.01")),
    ?assertMatch({44.00e+33, [], {0, 9}}, decode("44.00e+33")),
    ?assertMatch({44.00e33, [], {0, 8}}, decode("44.00e33")),
    ?assertMatch({44.00e-10, [], {0, 9}}, decode("44.00e-10")),
    ?assertMatch({42.44, [], {0, 5}}, decode("42.44")),
    ?assertMatch({41.33, [], {0, 5}}, decode("41.33")),
    ?assertMatch({0, [], {0, 1}}, decode("0")).


decode_string_test() ->
    ?assertMatch({"Hello World", [], {0, 13}},
                 decode("\"Hello World\"")),
    ?assertMatch({"Hello\n World", [], {1, 7}},
                 decode("\"Hello\n World\"")),
    ?assertMatch({"Hello\" World", [], {0, 15}},
                 decode("\"Hello\\\" World\"")),
    ?assertMatch({"Hello\\ World", [], {0, 14}},
                 decode("\"Hello\\ World\"")),
    ?assertMatch({"Hello\/ World", [], {0, 14}},
                 decode("\"Hello\/ World\"")),
    ?assertMatch({"Hello\b World", [], {0, 14}},
                 decode("\"Hello\b World\"")),
    ?assertMatch({"Hello\f World", [], {0, 14}},
                 decode("\"Hello\f World\"")),
    ?assertMatch({"Hello\n World", [], {1, 7}},
                 decode("\"Hello\n World\"")),
    ?assertMatch({"Hello\r World", [], {1, 7}},
                 decode("\"Hello\r World\"")),
    ?assertMatch({"Hello\t World", [], {0, 14}},
                 decode("\"Hello\t World\"")),
    ?assertMatch({"Hello% World", [], {0, 19}},
                 decode("\"Hello\\u0025 World\"")).
