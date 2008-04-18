%% -*- mode: Erlang; fill-column: 132; comment-column: 118; -*-
%%%-------------------------------------------------------------------
%%% Copyright (c) 2006,2007,2008 Erlware
%%%
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
%%% @doc
%%%  Parsing erlang into json
%%%
%%%  @type in_string() = binary()
%%%  @type in_array() = [in_value()]
%%%  @type in_atom() = string()
%%%  @type in_object() = {obj, [{string(), in_value()}]}
%%%  @type in_number() = int() | float()
%%%  @type in_bool() = true | false
%%%  @type in_null() = null
%%%  @type in_value() = in_string() | in_array() | in_atom() | in_object()
%%%                     | in_number() | in_bool() | in_null()
%%%
%%%
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt
%%%-------------------------------------------------------------------
-module(ktj_encode).

-include("eunit.hrl").

-export([encode/1]).

%%=============================================================================
%% API
%%=============================================================================
%%--------------------------------------------------------------------
%% @doc
%%  Parses a list of data objects into a list. The list is a deeply
%%  nested list and should be flattened if you wish to use it as a
%%  string. Otherwise, io functions will flatten the list for you.
%%
%% @spec encode(DataObjects::in_value()) -> Output::string()
%% @end
%%--------------------------------------------------------------------
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
%%--------------------------------------------------------------------
%%
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
%%
%% @spec encode_string(Value::in_string()) -> EncodedList::string()
%% @private
%% @end
%%--------------------------------------------------------------------
encode_string(Value) when is_binary(Value) ->
    [$\", escape_string(binary_to_list(Value), []), $\"];
encode_string(Value) when is_atom(Value) ->
    [$\", escape_string(atom_to_list(Value), []), $\"];
encode_string(Value) when is_list(Value) ->
    [$\", escape_string(Value, []), $\"].

%%--------------------------------------------------------------------
%% @doc
%% escapes a string as required for the json
%%
%% @spec escape_string(Value::list(), Acc::list()) -> EscapedString::string()
%% @private
%% @end
%%--------------------------------------------------------------------
escape_string([$\\, Char | Rest], Acc) ->
    escape_string(Rest, [Char, $\\ | Acc]);
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

%%--------------------------------------------------------------------
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
%%
%% @spec encode_integer(Value::integer()) -> List::string()
%% @private
%% @end
%%--------------------------------------------------------------------
encode_integer(Value) when is_integer(Value) ->
    integer_to_list(Value).

%%--------------------------------------------------------------------
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
%%
%% @spec encode_float(Value::float()) -> List::string()
%% @private
%% @end
%%--------------------------------------------------------------------
encode_float(Value) when is_float(Value) ->
    float_to_list(Value).


%%--------------------------------------------------------------------
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
%% @spec encode_array(Array::list(), Acc::string()) -> List::string()
%% @private
%% @end
%%--------------------------------------------------------------------
encode_array([H | T], []) ->
    encode_array(T, [encode(H), $[]);
encode_array([H | T], TAcc) ->
    encode_array(T, [encode(H), $, | TAcc]);
encode_array([], []) ->
    [$], $[];
encode_array([], TAcc) ->
    [$] | TAcc].


%%--------------------------------------------------------------------
%%
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
%% @spec encode_object(PropList, Acc::string()) -> List::string()
%% @end
%%--------------------------------------------------------------------
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
    ?assertMatch("\"hello\"", lists:flatten(encode('hello'))).

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



