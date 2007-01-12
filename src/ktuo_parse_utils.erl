%%%-------------------------------------------------------------------
%%% Copyright 2006 Eric Merritt
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");  
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%       http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%  Unless required by applicable law or agreed to in writing, software
%%%  distributed under the License is distributed on an "AS IS" BASIS,
%%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or 
%%%  implied. See the License for the specific language governing 
%%%  permissions and limitations under the License.
%%%
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(ktuo_parse_utils).

-include("eunit.hrl").

-export([stringish_body/5, digit/5, digit19/4]).

-define(LOC_1, 1).
-define(LOC_2, 16).
-define(LOC_3, 256).
-define(LOC_4, 4096).

%%=============================================================================
%% API
%%=============================================================================
%%--------------------------------------------------------------------
%% @spec stringish_body(Delim, Stream, Acc) -> {String, Rest}.
%% 
%% @doc 
%%  Parses a string body into a string.
%%  It expects the fact that something is a string to already be 
%%  detected. So strings should be of the form
%%
%%  this is a string body"
%%
%% @end
%%--------------------------------------------------------------------
stringish_body(Delim, [$\\, $\" | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$\" | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $/ | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$/ | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $\\ | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$\\ | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $b | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$\b | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $f | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$\f | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $n | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$\n | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $r | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$\r | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $t | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [$\t | Acc], NewLines, Chars + 2);
stringish_body(Delim, [$\\, $u | T], Acc, NewLines, Chars) ->
    parse_hex_digit(T, Acc, [], Delim, NewLines, Chars + 2);
stringish_body(Delim, [$\n | T], Acc, NewLines, _Chars) ->
    stringish_body(Delim, T, [$\n | Acc], NewLines + 1, 0);
stringish_body(Delim, [$\r | T], Acc, NewLines, _Chars) ->
    stringish_body(Delim, T, [$\r | Acc], NewLines + 1, 0);
stringish_body(Delim, [Delim | T], Acc, NewLines, Chars) ->
    {lists:reverse(Acc), T, {NewLines, Chars + 1}};
stringish_body(Delim, [H | T], Acc, NewLines, Chars) ->
    stringish_body(Delim, T, [H | Acc], NewLines, Chars + 1);
stringish_body(_Delim, [], _Acc, NewLines, Chars) ->
    {error, {"Found end of file while parsing string", NewLines, Chars}}.

%%--------------------------------------------------------------------
%% @spec digit19(Stream, Acc, NewLines, Chars) -> Acc2 | Error.
%% 
%% @doc 
%%  Parse from the stream ensuring that the digits has a length of 
%%  between 1 and 9.
%% @end
%%--------------------------------------------------------------------
digit19([$1 | T], Acc, NewLines, Chars) ->
    digit(T, [$1 | Acc], front, NewLines, Chars + 1);
digit19([$2 | T], Acc, NewLines, Chars) ->
    digit(T, [$2 | Acc], front, NewLines, Chars + 1);
digit19([$3 | T], Acc, NewLines, Chars) ->
    digit(T, [$3 | Acc], front, NewLines, Chars + 1);
digit19([$4 | T], Acc, NewLines, Chars) ->
    digit(T, [$4 | Acc], front, NewLines, Chars + 1);
digit19([$5 | T], Acc, NewLines, Chars) ->
    digit(T, [$5 | Acc], front, NewLines, Chars + 1);
digit19([$6 | T], Acc, NewLines, Chars) ->
    digit(T, [$6 | Acc], front, NewLines, Chars + 1);
digit19([$7 | T], Acc, NewLines, Chars) ->
    digit(T, [$7 | Acc], front, NewLines, Chars + 1);
digit19([$8 | T], Acc, NewLines, Chars) ->
    digit(T, [$8 | Acc], front, NewLines, Chars + 1);
digit19([$9 | T], Acc, NewLines, Chars) ->
    digit(T, [$9 | Acc], front, NewLines, Chars + 1);
digit19(Else, Acc, NewLines, Chars) ->
    decimal(Else, Acc, NewLines, Chars + 1).


%%--------------------------------------------------------------------
%% @spec digit(Stream, Acc, Next) -> {Res, Rest}.
%% 
%% @doc 
%%  Parse out the specified digit set.
%% @end
%%--------------------------------------------------------------------
digit([$0 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$0 | Acc], Next, NewLines, Chars + 1);
digit([$1 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$1 | Acc], Next, NewLines, Chars + 1);
digit([$2 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$2 | Acc], Next, NewLines, Chars + 1);
digit([$3 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$3 | Acc], Next, NewLines, Chars + 1);
digit([$4 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$4 | Acc], Next, NewLines, Chars + 1);
digit([$5 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$5 | Acc], Next, NewLines, Chars + 1);
digit([$6 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$6 | Acc], Next, NewLines, Chars + 1);
digit([$7 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$7 | Acc], Next, NewLines, Chars + 1);
digit([$8 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$8 | Acc], Next, NewLines, Chars + 1);
digit([$9 | T], Acc, Next, NewLines, Chars) ->
    digit(T, [$9 | Acc], Next, NewLines, Chars + 1);
digit(Stream, Acc, Next, NewLines, Chars) ->
    digit_next(Stream, Acc, Next, NewLines, Chars).

%%=============================================================================
%% Internal functions
%%=============================================================================
parse_hex_digit([$0 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$0 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$1 | T], Acc, HexAcc, Delim, NewLines, Chars)  
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$1 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$2 | T], Acc, HexAcc, Delim, NewLines, Chars)  
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$2 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$3 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$3 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$4 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$4 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$5 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$5 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$6 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$6 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$7 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$7 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$8 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$8 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$9 | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$9 | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$A | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$A | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$a | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$A | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$B | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$B | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$b | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$B | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$C | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$C | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$c | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$C | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$D | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$D | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$d | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$D | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$E | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$E | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$e | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$E | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$F | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$F | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit([$f | T], Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$F | HexAcc], Delim, NewLines, Chars + 1);
parse_hex_digit(Stream, Acc, HexAcc, Delim, NewLines, Chars) 
  when length(HexAcc) == 4 ->
    [D1, D2, D3, D4] = HexAcc,
    Char = ((c2n(D1) * ?LOC_1) + 
            (c2n(D2) * ?LOC_2) +
            (c2n(D3) * ?LOC_3) +
            (c2n(D4) * ?LOC_4)),
    stringish_body(Delim, Stream, [Char | Acc], NewLines, Chars).


c2n(Char) when Char < 58 ->
    Char - 48;
c2n(Char) ->
    Char - 54.


decimal([$.| T], Acc, NewLines, Chars) when length(T) > 0 ->
    digit(T, [$. | Acc], decimal, NewLines, Chars + 1);
decimal(Stream, Acc, NewLines, Chars) ->
    integer_end(Stream, Acc, NewLines, Chars).

exponent([$e, $+ | T], Acc,  NewLines, Chars) ->
    digit(T, [$+, $e | Acc], exponent, NewLines, Chars + 2);
exponent([$E, $+ | T], Acc,  NewLines, Chars) ->
    digit(T, [$+, $E | Acc], exponent, NewLines, Chars + 2);
exponent([$e, $- | T], Acc,  NewLines, Chars) ->
    digit(T, [$-, $e | Acc], exponent, NewLines, Chars + 2);
exponent([$E, $- | T], Acc,  NewLines, Chars) ->
    digit(T, [$-, $E | Acc], exponent, NewLines, Chars + 2);
exponent([$E | T], Acc,  NewLines, Chars) ->
    digit(T, [$E | Acc], exponent, NewLines, Chars + 1);
exponent([$e | T], Acc,  NewLines, Chars) ->
    digit(T, [$e | Acc], exponent, NewLines, Chars + 1);
exponent(Stream, Acc,  NewLines, Chars) ->
    float_end(Stream, Acc, NewLines, Chars).

integer_end(Stream, Acc, NewLines, Chars) ->
    {list_to_integer(lists:reverse(Acc)), Stream, {NewLines, Chars}}.


float_end(Stream, Acc, NewLines, Chars) ->
    {list_to_float(lists:reverse(Acc)), Stream, {NewLines, Chars}}.


digit_next(Stream, Acc, front, NewLines, Chars) ->
    decimal(Stream, Acc, NewLines, Chars);
digit_next(Stream, Acc, decimal, NewLines, Chars) ->
    exponent(Stream, Acc, NewLines, Chars);
digit_next(Stream, Acc, exponent, NewLines, Chars) ->
    float_end(Stream, Acc, NewLines, Chars).

%%=============================================================================
%% Unit tests
%%=============================================================================
number_test() ->
    ?assertMatch({44, [], {0, 2}}, digit("44", [], front, 0, 0)),
    ?assertMatch({-44, [], {0, 3}}, digit("44", [$-], front, 0, 1)),
    ?assertMatch({44.00, [], {0, 5}}, digit("44.00", [], front, 0, 0)),
    ?assertMatch({-44.01, [], {0, 6}}, digit("44.01", [$-], front, 0, 1)),
    ?assertMatch({44.00e+33, [], {0, 9}}, digit("44.00e+33", [], front, 0, 0)),
    ?assertMatch({44.00e33, [], {0, 8}}, digit("44.00e33", [], front, 0, 0)),
    ?assertMatch({44.00e-10, [], {0, 9}}, digit("44.00e-10", [], front, 0, 0)),
    ?assertMatch({42.44, [], {0, 5}}, digit("42.44", [], front, 0, 0)),
    ?assertMatch({41.33, [], {0, 5}}, digit("41.33", [], front, 0, 0)),
    ?assertMatch({0, [], {0, 1}}, digit("0", [], front, 0, 0)).


string_test() ->
    ?assertMatch({"Hello World", [], {0, 13}},
                 stringish_body($\", "Hello World\"", [], 0, 1)),
    ?assertMatch({"Hello\n World", [], {1, 7}},
                 stringish_body($\", "Hello\n World\"", [], 0, 1)),
    ?assertMatch({"Hello\" World", [], {0, 15}},
                 stringish_body($\", "Hello\\\" World\"", [], 0, 1)),
    ?assertMatch({"Hello\\ World", [], {0, 14}},
                 stringish_body($\", "Hello\\ World\"", [], 0, 1)),
    ?assertMatch({"Hello\/ World", [], {0, 14}},
                 stringish_body($\", "Hello\/ World\"", [], 0, 1)),
    ?assertMatch({"Hello\b World", [], {0, 14}},
                 stringish_body($\", "Hello\b World\"", [], 0, 1)),
    ?assertMatch({"Hello\f World", [], {0, 14}},
                 stringish_body($\", "Hello\f World\"", [], 0, 1)),
    ?assertMatch({"Hello\n World", [], {1, 7}},
                 stringish_body($\", "Hello\n World\"", [], 0, 1)),
    ?assertMatch({"Hello\r World", [], {1, 7}},
                 stringish_body($\", "Hello\r World\"", [], 0, 1)),
    ?assertMatch({"Hello\t World", [], {0, 14}},
                 stringish_body($\", "Hello\t World\"", [], 0, 1)),
    ?assertMatch({"Hello% World", [], {0, 19}},
                 stringish_body($\", "Hello\\u0025 World\"", [], 0, 1)).


