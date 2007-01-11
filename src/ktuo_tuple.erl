%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(ktuo_tuple).

%-include("eunit.hrl").

-compile(export_all).

-export([decode/1]).

%%--------------------------------------------------------------------
%% @spec decode(Stream) -> {ParsedTuples, UnparsedRemainder}
%% 
%% @doc 
%%  Parses the incoming stream into valid erlang objects. 
%%  ``
%%   Tuple   ==   Erlang
%%   List        List
%%   String      List
%%   Number      Number
%%   Atom        Atom
%%  ''
%%  This decode function parses a subset of erlang data notation.
%% @end
%%--------------------------------------------------------------------
decode(Stream) ->
    value(Stream).


%%=============================================================================
%% Internal Functions
%%=============================================================================
value([$\" | T]) ->
    ktuo_parse_utils:stringish_body($\", T, []);
value([$\' | T]) ->
    {String, Rest} = ktuo_parse_utils:stringish_body($\', T, []),
    {list_to_existing_atom(String), Rest};
value([$- | T]) ->
    ktuo_parse_utils:digit19(T, [$-]);
value([$0 | T]) ->
    ktuo_parse_utils:digit(T, [$0], front); 
value([$1 | T]) ->
    ktuo_parse_utils:digit(T, [$1], front);
value([$2 | T]) ->
    ktuo_parse_utils:digit(T, [$2], front);
value([$3 | T]) ->
    ktuo_parse_utils:digit(T, [$3], front);
value([$4 | T]) ->
    ktuo_parse_utils:digit(T, [$4], front);
value([$5 | T]) ->
    ktuo_parse_utils:digit(T, [$5], front);
value([$6 | T]) ->
    ktuo_parse_utils:digit(T, [$6], front);
value([$7 | T]) ->
    ktuo_parse_utils:digit(T, [$7], front);
value([$8 | T]) ->
    ktuo_parse_utils:digit(T, [$8], front);
value([$9 | T]) ->
    ktuo_parse_utils:digit(T, [$9], front);
value([$[ | T]) ->
    list_body(T, []);
value([${ | T]) ->
    tuple_body(T, []);
value([$\s | T]) ->
    value(T);
value([$\t | T]) ->
    value(T);
value([$\r | T]) ->
    value(T);
value([$\n | T]) ->
    value(T);
value(Stream) ->
    bare_atom(Stream, []).


list_body([$] | T], Acc) ->
    {lists:reverse(Acc), T};
list_body([$, | T], Acc) ->
    list_body(T, Acc);
list_body([$\s | T], Acc) ->
    list_body(T, Acc);
list_body([$\t | T], Acc) ->
    list_body(T, Acc);
list_body([$\n | T], Acc) ->
    list_body(T, Acc);
list_body([$\r | T], Acc) ->
    list_body(T, Acc);
list_body(Stream, Acc) ->
    {Value, Rest} = value(Stream),
    list_body(Rest, [Value | Acc]).


tuple_body([$} | T], Acc) ->
    {list_to_tuple(lists:reverse(Acc)), T};
tuple_body([$, | T], Acc) ->
    tuple_body(T, Acc);
tuple_body([$\s | T], Acc) ->
    tuple_body(T, Acc);
tuple_body([$\t | T], Acc) ->
    tuple_body(T, Acc);
tuple_body([$\n | T], Acc) ->
    tuple_body(T, Acc);
tuple_body([$\r | T], Acc) ->
    tuple_body(T, Acc);
tuple_body(Stream, Acc) ->
    {Value, Rest} = value(Stream),
    tuple_body(Rest, [Value | Acc]).



bare_atom([H | T], Acc) when H >= $a, H =< $z ->
    bare_atom(T, [H | Acc]);
bare_atom([H | T], Acc) when H >= $A, H =< $Z ->
    bare_atom(T, [H | Acc]);
bare_atom([$_ | T], Acc) ->
    bare_atom(T, [$_ | Acc]);
bare_atom([H | T], Acc) when H >= $0, H =< $9 ->
    bare_atom(T, [ H | Acc]);
bare_atom([$\s | T], Acc) ->
    {list_to_existing_atom(lists:reverse(Acc)), T};
bare_atom([$\t | T], Acc) ->
    {list_to_existing_atom(lists:reverse(Acc)), T};
bare_atom([$\r | T], Acc) ->
    {list_to_existing_atom(lists:reverse(Acc)), T};
bare_atom([$\n | T], Acc) ->
    {list_to_existing_atom(lists:reverse(Acc)), T};
bare_atom([], Acc) ->
    {list_to_existing_atom(lists:reverse(Acc)), []};
bare_atom(Else, Acc) when length(Acc) > 0->
    {list_to_existing_atom(lists:reverse(Acc)), Else}.


%%=============================================================================
%% Unit tests
%%=============================================================================
%% decode_tuple_test() ->
%%     ?assertMatch({{hello, 43}, []}, decode("{hello, 43}")),
%%     ?assertMatch({{{zoom}, 321, {32, "oeaueou"}}, []},
%%                  decode("{{zoom}, 321, {32, \"oeaueou\"}}")).

%% decode_atom_test() ->
%%     ?assertMatch({hello, []}, decode("hello")),
%%     ?assertMatch({goodbye, []}, decode("goodbye")),
%%     ?assertMatch({'ba ba black sheep', []}, 
%%                  decode("'ba ba black sheep'")).

%% decode_number_test() ->
%%     ?assertMatch({44, []}, decode("44")),
%%     ?assertMatch({-44, []}, decode("-44")),
%%     ?assertMatch({44.00, []}, decode("44.00")),
%%     ?assertMatch({-44.01, []}, decode("-44.01")),
%%     ?assertMatch({44.00e+33, []}, decode("44.00e+33")),
%%     ?assertMatch({44.00e33, []}, decode("44.00e33")),
%%     ?assertMatch({44.00e-10, []}, decode("44.00e-10")),
%%     ?assertMatch({42.44, []}, decode("42.44")),
%%     ?assertMatch({41.33, []}, decode("41.33")),
%%     ?assertMatch({0, []}, decode("0")).


%% decode_string_test() ->
%%     ?assertMatch({"Hello World", []},
%%                  decode("\"Hello World\"")),
%%     ?assertMatch({"Hello\n World", []},
%%                  decode("\"Hello\n World\"")),
%%     ?assertMatch({"Hello\" World", []},
%%                  decode("\"Hello\\\" World\"")),
%%     ?assertMatch({"Hello\\ World", []},
%%                  decode("\"Hello\\ World\"")),
%%     ?assertMatch({"Hello\/ World", []},
%%                  decode("\"Hello\/ World\"")),
%%     ?assertMatch({"Hello\b World", []},
%%                  decode("\"Hello\b World\"")),
%%     ?assertMatch({"Hello\f World", []},
%%                  decode("\"Hello\f World\"")),
%%     ?assertMatch({"Hello\n World", []},
%%                  decode("\"Hello\n World\"")),
%%     ?assertMatch({"Hello\r World", []},
%%                  decode("\"Hello\r World\"")),
%%     ?assertMatch({"Hello\t World", []},
%%                  decode("\"Hello\t World\"")),
%%     ?assertMatch({"Hello% World", []},
%%                  decode("\"Hello\\u0025 World\"")).
