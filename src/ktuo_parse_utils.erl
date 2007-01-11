%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(ktuo_parse_utils).

%-include("eunit.hrl").

-export([stringish_body/3, digit/3, digit19/2]).

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
stringish_body(Delim, [$\\, $\" | T], Acc) ->
    stringish_body(Delim, T, [$\" | Acc]);
stringish_body(Delim, [$\\, $/ | T], Acc) ->
    stringish_body(Delim, T, [$/ | Acc]);
stringish_body(Delim, [$\\, $\\ | T], Acc) ->
    stringish_body(Delim, T, [$\\ | Acc]);
stringish_body(Delim, [$\\, $b | T], Acc) ->
    stringish_body(Delim, T, [$\b | Acc]);
stringish_body(Delim, [$\\, $f | T], Acc) ->
    stringish_body(Delim, T, [$\f | Acc]);
stringish_body(Delim, [$\\, $n | T], Acc) ->
    stringish_body(Delim, T, [$\n | Acc]);
stringish_body(Delim, [$\\, $r | T], Acc) ->
    stringish_body(Delim, T, [$\r | Acc]);
stringish_body(Delim, [$\\, $t | T], Acc) ->
    stringish_body(Delim, T, [$\t | Acc]);
stringish_body(Delim, [$\\, $u | T], Acc) ->
    parse_hex_digit(T, Acc, [], Delim);
stringish_body(Delim, [Delim | T], Acc) ->
    {lists:reverse(Acc), T};
stringish_body(Delim, [H | T], Acc) ->
    stringish_body(Delim, T, [H | Acc]).

%%--------------------------------------------------------------------
%% @spec digit19(Stream, Acc) -> Acc2.
%% 
%% @doc 
%%  Parse from the stream ensuring that the digits has a length of 
%%  between 1 and 9.
%% @end
%%--------------------------------------------------------------------
digit19([$1 | T], Acc) ->
    digit(T, [$1 | Acc], front);
digit19([$2 | T], Acc) ->
    digit(T, [$2 | Acc], front);
digit19([$3 | T], Acc) ->
    digit(T, [$3 | Acc], front);
digit19([$4 | T], Acc) ->
    digit(T, [$4 | Acc], front);
digit19([$5 | T], Acc) ->
    digit(T, [$5 | Acc], front);
digit19([$6 | T], Acc) ->
    digit(T, [$6 | Acc], front);
digit19([$7 | T], Acc) ->
    digit(T, [$7 | Acc], front);
digit19([$8 | T], Acc) ->
    digit(T, [$8 | Acc], front);
digit19([$9 | T], Acc) ->
    digit(T, [$9 | Acc], front);
digit19(Else, Acc) ->
    decimal(Else, Acc).


%%--------------------------------------------------------------------
%% @spec digit(Stream, Acc, Next) -> {Res, Rest}.
%% 
%% @doc 
%%  Parse out the specified digit set.
%% @end
%%--------------------------------------------------------------------
digit([$0 | T], Acc, Next) ->
    digit(T, [$0 | Acc], Next);
digit([$1 | T], Acc, Next) ->
    digit(T, [$1 | Acc], Next);
digit([$2 | T], Acc, Next) ->
    digit(T, [$2 | Acc], Next);
digit([$3 | T], Acc, Next) ->
    digit(T, [$3 | Acc], Next);
digit([$4 | T], Acc, Next) ->
    digit(T, [$4 | Acc], Next);
digit([$5 | T], Acc, Next) ->
    digit(T, [$5 | Acc], Next);
digit([$6 | T], Acc, Next) ->
    digit(T, [$6 | Acc], Next);
digit([$7 | T], Acc, Next) ->
    digit(T, [$7 | Acc], Next);
digit([$8 | T], Acc, Next) ->
    digit(T, [$8 | Acc], Next);
digit([$9 | T], Acc, Next) ->
    digit(T, [$9 | Acc], Next);
digit(Stream, Acc, Next) ->
    digit_next(Stream, Acc, Next).

%%=============================================================================
%% Internal functions
%%=============================================================================
parse_hex_digit([$0 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$0 | HexAcc], Delim);
parse_hex_digit([$1 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$1 | HexAcc], Delim);
parse_hex_digit([$2 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$2 | HexAcc], Delim);
parse_hex_digit([$3 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$3 | HexAcc], Delim);
parse_hex_digit([$4 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$4 | HexAcc], Delim);
parse_hex_digit([$5 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$5 | HexAcc], Delim);
parse_hex_digit([$6 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$6 | HexAcc], Delim);
parse_hex_digit([$7 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$7 | HexAcc], Delim);
parse_hex_digit([$8 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$8 | HexAcc], Delim);
parse_hex_digit([$9 | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$9 | HexAcc], Delim);
parse_hex_digit([$A | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$A | HexAcc], Delim);
parse_hex_digit([$a | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$A | HexAcc], Delim);
parse_hex_digit([$B | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$B | HexAcc], Delim);
parse_hex_digit([$b | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$B | HexAcc], Delim);
parse_hex_digit([$C | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$C | HexAcc], Delim);
parse_hex_digit([$c | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$C | HexAcc], Delim);
parse_hex_digit([$D | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$D | HexAcc], Delim);
parse_hex_digit([$d | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$D | HexAcc], Delim);
parse_hex_digit([$E | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$E | HexAcc], Delim);
parse_hex_digit([$e | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$E | HexAcc], Delim);
parse_hex_digit([$F | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$F | HexAcc], Delim);
parse_hex_digit([$f | T], Acc, HexAcc, Delim) when length(HexAcc) < 4 ->
    parse_hex_digit(T, Acc, [$F | HexAcc], Delim);
parse_hex_digit(Stream, Acc, HexAcc, Delim) when length(HexAcc) == 4 ->
    [D1, D2, D3, D4] = HexAcc,
    Char = ((c2n(D1) * ?LOC_1) + 
            (c2n(D2) * ?LOC_2) +
            (c2n(D3) * ?LOC_3) +
            (c2n(D4) * ?LOC_4)),
    stringish_body(Delim, Stream, [Char | Acc]).


c2n(Char) when Char < 58 ->
    Char - 48;
c2n(Char) ->
    Char - 54.


decimal([$.| T], Acc) when length(T) > 0 ->
    digit(T, [$. | Acc], decimal);
decimal(Stream, Acc) ->
    integer_end(Stream, Acc).

exponent([$e, $+ | T], Acc) ->
    digit(T, [$+, $e | Acc], exponent);
exponent([$E, $+ | T], Acc) ->
    digit(T, [$+, $E | Acc], exponent);
exponent([$e, $- | T], Acc) ->
    digit(T, [$-, $e | Acc], exponent);
exponent([$E, $- | T], Acc) ->
    digit(T, [$-, $E | Acc], exponent);
exponent([$E | T], Acc) ->
    digit(T, [$E | Acc], exponent);
exponent([$e | T], Acc) ->
    digit(T, [$e | Acc], exponent);
exponent(Stream, Acc) ->
    float_end(Stream, Acc).

integer_end(Stream, Acc) ->
    {list_to_integer(lists:reverse(Acc)), Stream}.


float_end(Stream, Acc) ->
    {list_to_float(lists:reverse(Acc)), Stream}.



digit_next(Stream, Acc, front) ->
    decimal(Stream, Acc);
digit_next(Stream, Acc, decimal) ->
    exponent(Stream, Acc);
digit_next(Stream, Acc, exponent) ->
    float_end(Stream, Acc).

%%=============================================================================
%% Unit tests
%%=============================================================================
%% number_test() ->
%%     ?assertMatch({44, []}, digit("44", [], front)),
%%     ?assertMatch({-44, []}, digit("44", [$-], front)),
%%     ?assertMatch({44.00, []}, digit("44.00", [], front)),
%%     ?assertMatch({-44.01, []}, digit("44.01", [$-], front)),
%%     ?assertMatch({44.00e+33, []}, digit("44.00e+33", [], front)),
%%     ?assertMatch({44.00e33, []}, digit("44.00e33", [], front)),
%%     ?assertMatch({44.00e-10, []}, digit("44.00e-10", [], front)),
%%     ?assertMatch({42.44, []}, digit("42.44", [], front)),
%%     ?assertMatch({41.33, []}, digit("41.33", [], front)),
%%     ?assertMatch({0, []}, digit("0", [], front)).


%% string_test() ->
%%     ?assertMatch({"Hello World", []},
%%                  stringish_body($\", "Hello World\"", [])),
%%     ?assertMatch({"Hello\n World", []},
%%                  stringish_body($\", "Hello\n World\"", [])),
%%     ?assertMatch({"Hello\" World", []},
%%                  stringish_body($\", "Hello\\\" World\"", [])),
%%     ?assertMatch({"Hello\\ World", []},
%%                  stringish_body($\", "Hello\\ World\"", [])),
%%     ?assertMatch({"Hello\/ World", []},
%%                  stringish_body($\", "Hello\/ World\"", [])),
%%     ?assertMatch({"Hello\b World", []},
%%                  stringish_body($\", "Hello\b World\"", [])),
%%     ?assertMatch({"Hello\f World", []},
%%                  stringish_body($\", "Hello\f World\"", [])),
%%     ?assertMatch({"Hello\n World", []},
%%                  stringish_body($\", "Hello\n World\"", [])),
%%     ?assertMatch({"Hello\r World", []},
%%                  stringish_body($\", "Hello\r World\"", [])),
%%     ?assertMatch({"Hello\t World", []},
%%                  stringish_body($\", "Hello\t World\"", [])),
%%     ?assertMatch({"Hello% World", []},
%%                  stringish_body($\", "Hello\\u0025 World\"", [])).


