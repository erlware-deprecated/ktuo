%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @copyright (C) 2006-2010 Erlware
%%% @doc
%%%  Parsing erlang into json
%%% @end
%%% Created : 19 Dec 2006 by Eric Merritt
%%%-------------------------------------------------------------------
-module(ktj_encode).

-include_lib("eunit/include/eunit.hrl").

-export([encode/1]).

-export_type([prop_list/0,
	      ktj_string/0,
	      ktj_array/0,
	      ktj_atom/0,
	      object/0,
	      ktj_number/0,
	      ktj_bool/0,
	      null/0,
	      value/0]).

%%=============================================================================
%% Types
%%=============================================================================

-type prop_list() :: [{ktj_string(), value()}].
-type ktj_string() :: binary().
-type ktj_array() :: [value()].
-type ktj_atom() :: string().
-type object() :: {obj, prop_list()}.
-type ktj_number() :: integer() | float().
-type ktj_bool() :: true | false.
-type null() :: null.
-type value() :: ktj_string() | ktj_array() | ktj_atom() | object()
                     | ktj_number() | ktj_bool() | null().

%%=============================================================================
%% API
%%=============================================================================
%% @doc
%%  Parses a list of data objects into a list. The list is a deeply
%%  nested list and should be flattened if you wish to use it as a
%%  string. Otherwise, io functions will flatten the list for you.
%% @end
-spec encode(value()) -> string().
encode(Data) when is_list(Data) ->
    lists:reverse(encode_array(Data, []));
encode({obj, Value}) ->
    encode_object(Value, []);
encode(Data) when is_binary(Data) ->
    encode_string(Data);
encode(Data) when is_integer(Data) ->
    encode_integer(Data);
encode(Data) when is_float(Data) ->
    encode_float(Data);
encode(true) ->
    atom_to_list(true);
encode(false) ->
    atom_to_list(false);
encode(null) ->
    atom_to_list(null);
encode(Data) when is_atom(Data)->
    encode_string(Data).


%%=============================================================================
%% Internal Functions
%%=============================================================================
%% @doc
%%  Encodes the string according to the rules of json.
%%  ``
%%   string
%%      ""
%%      " chars "
%%   chars
%%     char
%%     char chars
%%   char
%%     any-Unicode-character-
%%         except-"-or-\-or-
%%         control-character
%%     \"
%%     \\
%%     \/
%%     \b
%%     \f
%%     \n
%%     \r
%%     \t
%%     \u four-hex-digits
%%
%% ''
%% @end
-spec encode_string(ktj_string()) -> string().
encode_string(Value) when is_binary(Value) ->
    [$\", escape_string(binary_to_list(Value), []), $\"];
encode_string(Value) when is_atom(Value) ->
    [$\", escape_string(atom_to_list(Value), []), $\"];
encode_string(Value) when is_list(Value) ->
    [$\", escape_string(Value, []), $\"].

%% @doc
%% escapes a string as required for the json
%% @end
-spec escape_string(string(), list()) -> string().
escape_string([$\" | Rest], Acc) ->
    escape_string(Rest, [$\", $\\ | Acc]);
escape_string([$\\ | Rest], Acc) ->
    escape_string(Rest, [$\\, $\\ | Acc]);
escape_string([$\/ | Rest], Acc) ->
    escape_string(Rest, [$\/, $\\ | Acc]);
escape_string([$\b | Rest], Acc) ->
    escape_string(Rest, [$b, $\\ | Acc]);
escape_string([$\f | Rest], Acc) ->
    escape_string(Rest, [$f, $\\ | Acc]);
escape_string([$\n | Rest], Acc) ->
    escape_string(Rest, [$n, $\\ | Acc]);
escape_string([$\r | Rest], Acc) ->
    escape_string(Rest, [$r, $\\ | Acc]);
escape_string([$\t | Rest], Acc) ->
    escape_string(Rest, [$t, $\\ | Acc]);
escape_string([N | Rest], Acc) ->
    escape_string(Rest, [N | Acc]);
escape_string([], Acc) ->
    lists:reverse(Acc).

%% @doc
%%  Encode the integer according to the precepts of json.
%%
%% ``
%%  int
%%    digit
%%    digit1-9 digits
%%    - digit
%%    - digit1-9 digits
%%
%%  digits
%%    digit
%%    digit digits
%% ''
%% @end
-spec encode_integer(integer()) -> string().
encode_integer(Value) when is_integer(Value) ->
    integer_to_list(Value).

%% @doc
%%  Encode the float according to json rules.
%%  ``
%%  number
%%    int
%%    int frac
%%    int exp
%%    int frac exp
%%  int
%%    digit
%%    digit1-9 digits
%%    - digit
%%    - digit1-9 digits
%%  frac
%%    . digits
%%  exp
%%    e digits
%%  digits
%%    digit
%%    digit digits
%%  e
%%    e
%%    e+
%%    e-
%%    E
%%    E+
%%    E-
%% ''
%% @end
-spec encode_float(float()) -> string().
encode_float(Value) when is_float(Value) ->
    float_to_list(Value).

%% @doc
%%  Encode erlang array according to jsonic precepts
%% <pre>
%%  array
%%    []
%%    [ elements ]
%%  elements
%%    value
%%    value , elements
%%  value
%%    string
%%    number
%%    object
%%    array
%%    true
%%    false
%%    null
%% </pre>
%% @end
-spec encode_array(list(), string()) -> string().
encode_array([H | T], []) ->
    encode_array(T, [encode(H), $[]);
encode_array([H | T], TAcc) ->
    encode_array(T, [encode(H), $, | TAcc]);
encode_array([], []) ->
    [$], $[];
encode_array([], TAcc) ->
    [$] | TAcc].


%% @doc
%%  Encode a property list into a json array.
%% ``
%%  object
%%    {}
%%    { members }
%%  members
%%    pair
%%    pair , members
%%  pair
%%    string : value
%%  array
%%    []
%%    [ elements ]
%%  elements
%%    value
%%    value , elements
%%  value
%%    string
%%    number
%%    object
%%    array
%%    true
%%    false
%%    null
%%
%% ''
%% @end
-spec encode_object(prop_list(), string()) -> string().
encode_object([{Key, Value} | T], []) ->
    encode_object(T, [encode_string(Key), $:,
                      encode(Value), $}]);
encode_object([{Key, Value} | T], TAcc) ->
    encode_object(T, [encode_string(Key), $:,
                      encode(Value), $, | TAcc]);
encode_object([], []) ->
    [${, $}];
encode_object([], TAcc) ->
    [${ | TAcc].


%%=============================================================================
%% Unit tests
%%=============================================================================
encode_string_test() ->
    ?assertMatch("\"Hello\"", lists:flatten(encode(<<"Hello">>))),
    ?assertMatch("\"hello\"", lists:flatten(encode('hello'))),

    % Don't be confused by the escapes, think in the bytes behind them ;)
    ?assertMatch("\"\\\\hello\"", lists:flatten(encode(<<"\\hello">>))).

encode_number_test() ->
    ?assertMatch("430", lists:flatten(encode(430))),
    ?assertMatch("4303432", lists:flatten(encode(4303432))),
    ?assertMatch("4.30000000000000000000e+01", lists:flatten(encode(43.00))),
    ?assertMatch("3.22232219999999983884e+02",
                 lists:flatten(encode(322.23222))).

encode_array_test() ->
    ?assertMatch("[33,43,53]", lists:flatten(encode([33, 43, 53]))),
    ?assertMatch("[\"String\",34,\"song\"]", lists:flatten(encode([<<"String">>,
                                                                   34,
                                                                   song]))),
    ?assertMatch("[{\"Goodbye\":true,\"Hello\":44},43,54]",
                 lists:flatten(encode([{obj, [{<<"Hello">>, 44},
                                              {<<"Goodbye">>, true}]},
                                       43, 54]))).



