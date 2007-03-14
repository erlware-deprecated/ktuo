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
%%%  Parses and encodes json for erlang. It expects to have a complete
%%%  json expression available in the stream. If more then
%%%  on json expression is in the stream it will parse one expression
%%%  and return the result and the rest of the stream which you may then
%%%  call parse on again. If a failure occures. it will return a error
%%%  value of the form {error, {Reason, Line, Char}}.
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(ktuo_json).

-include("eunit.hrl").

-export([decode/1, decode/3, encode/1]).

%%--------------------------------------------------------------------
%% @spec decode(Stream) -> {ParsedJson, UnparsedRemainder}
%% 
%% @doc 
%%  Parses the incoming stream into valid json objects. 
%%  ``
%%   JSON   ==   Erlang
%%   Array       List
%%   String      List
%%   Number      Number
%%   Object      PropList
%%   Ident       String
%%  ''
%%  This decode function parses a superset of json, in that single, un
%%  quoted words are parsed into strings. This makes it easer to 
%%  use json as a config language.
%% @end
%%--------------------------------------------------------------------
decode(Stream) when is_binary(Stream) ->
    decode(binary_to_list(Stream), 0, 0);
decode(Stream) ->
    decode(Stream, 0, 0).


%%--------------------------------------------------------------------
%% @spec decode(Stream, NewLines, Chars) -> DecodedValue.
%% 
%% @doc 
%%  Decodes the value with the fixed set of newlines and chars.
%% @end
%%--------------------------------------------------------------------
decode(Stream, NewLines, Chars) ->
    value(Stream, NewLines, Chars).


%%--------------------------------------------------------------------
%% @spec  encode(DataObjects) -> List.
%% 
%% @doc 
%%  Parses a list of data objects into a list. The list is a deeply 
%%  nested list and should be flattened if you wish to use it as a 
%%  string. Otherwise, io functions will flatten the list for you.
%%  ``
%%   Erlang    ==     JSON
%%   {string, Val}    String
%%   List             Array
%%   Atom             String
%%   PropList         Object
%%   Number           Number
%%  ''
%% @end
%%--------------------------------------------------------------------
encode(Data = [{string, _} | _]) ->
    lists:reverse(encode_array(Data, []));
encode(Data = [{_, _} | _]) ->
    encode_object(Data, []);
encode(Data) when is_list(Data) ->
    lists:reverse(encode_array(Data, []));
encode({string, Data}) ->
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
%% @spec encode_string(Value) -> EncodedList.
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
%% @end
%%--------------------------------------------------------------------
encode_string({string, Value}) ->
    [$\", Value, $\"];
encode_string(Value) when is_atom(Value) ->
    [$\", atom_to_list(Value), $\"];
encode_string(Value) when is_list(Value) ->
    [$\", Value, $\"].


%%--------------------------------------------------------------------
%% @spec encode_integer(Value) -> List.
%% 
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
%%--------------------------------------------------------------------
encode_integer(Value) ->
    integer_to_list(Value).


%%--------------------------------------------------------------------
%% @spec encode_float(Value) -> List.
%% 
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
%%--------------------------------------------------------------------
encode_float(Value) ->
    float_to_list(Value).


%%--------------------------------------------------------------------
%% @spec encode_array(Array, Acc) -> List.
%% 
%% @doc 
%%  Encode erlang array according to jsonic precepts 
%% ``
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
%% ``
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
%% @spec encode_object(PropList, Acc) -> List
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


value([$\" | T], NewLines, Chars) ->
    ktuo_parse_utils:stringish_body($\", T, [], NewLines, Chars + 1);
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
    array_body(T, [], NewLines, Chars + 1); 
value([${ | T], NewLines, Chars) ->
    object_body(T, dict:new(), NewLines, Chars + 1); 
value([$t, $r, $u, $e | T], NewLines, Chars) ->
    {true, T, {NewLines, Chars + 4}};
value([$f, $a, $l, $s, $e | T], NewLines, Chars) ->
    {false, T, {NewLines, Chars + 5}};
value([$n, $u, $l, $l | T], NewLines, Chars) ->
    {null, T, {NewLines, Chars + 4}};
value([$\s | T], NewLines, Chars) ->
    value(T, NewLines, Chars + 1); 
value([$\t | T], NewLines, Chars) ->
    value(T, NewLines, Chars + 1); 
value([$\r | T], NewLines, _Chars) ->
    value(T, NewLines + 1, 0); 
value([$\n | T], NewLines, _Chars) ->
    value(T, NewLines + 1, 0);
value(Stream, NewLines, Chars) ->
    ident(Stream, [], NewLines, Chars).


array_body([$] | T], Acc, NewLines, Chars) ->
    {lists:reverse(Acc), T, {NewLines, Chars + 1}};
array_body([$, | T], Acc, NewLines, Chars) ->
    array_body(T, Acc, NewLines, Chars + 1);
array_body([$\s | T], Acc, NewLines, Chars) ->
    array_body(T, Acc, NewLines, Chars + 1);
array_body([$\t | T], Acc, NewLines, Chars) ->
    array_body(T, Acc, NewLines, Chars + 1);
array_body([$\n | T], Acc, NewLines, _Chars) ->
    array_body(T, Acc, NewLines + 1, 0);
array_body([$\r | T], Acc, NewLines, _Chars) ->
    array_body(T, Acc, NewLines + 1, 0);
array_body(Stream, Acc, NewLines, Chars) ->
    {Value, Rest, {NLines, NChars}} = value(Stream, NewLines, Chars),
    array_body(Rest, [Value | Acc], NLines, NChars).

object_body([$} | T], Acc, NewLines, Chars) ->
    {Acc, T, {NewLines, Chars + 1}};
object_body([$, | T], Acc, NewLines, Chars) ->
    object_body(T, Acc, NewLines, Chars + 1);
object_body([$\s | T], Acc, NewLines, Chars) ->
    object_body(T, Acc, NewLines, Chars + 1);
object_body([$\t | T], Acc, NewLines, Chars) ->
    object_body(T, Acc, NewLines, Chars + 1);
object_body([$\r | T], Acc, NewLines, _Chars) ->
    object_body(T, Acc, NewLines + 1, 0);
object_body([$\n | T], Acc, NewLines, _Chars) ->
    object_body(T, Acc, NewLines + 1, 0);
object_body(Else, Acc, NewLines, Chars) ->
    do_key(Else, Acc, NewLines, Chars).


%%--------------------------------------------------------------------
%% @spec do_key(Stream, Acc, NewLines, Chars) -> Error | {Prop, Rest, N, C}.
%% 
%% @doc 
%%  Parse out the key. When that is complete parse out the value.
%% @end
%%--------------------------------------------------------------------
do_key(Stream, Acc, NewLines, Chars) ->
    case key(Stream, NewLines, Chars) of
        {Key, Rest1, {NLines, NChars}} ->
            do_value(Key, Rest1, Acc, NLines, NChars);
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @spec do_value(Key, Stream, NewLines, Chars) -> Error | 
%%   {PropList, Rest, {NewLines, Chars}}.
%% 
%% @doc 
%%  Parse out the value. Then continue with the object.
%% @end
%%--------------------------------------------------------------------
do_value(Key, Stream, Acc, NewLines, Chars) ->
    case find($:, Stream, NewLines, Chars) of
        {Rest, NLines, NChars} ->
            case value(Rest, NLines, NChars) of
                {Value, Rest1, {NLines1, NChars1}} ->
                    object_body(Rest1, dict:store(Key, Value, Acc), 
                                NLines1, NChars1);
                Else ->
                    Else
            end;
        Else1 -> 
            Else1
    end.

%%--------------------------------------------------------------------
%% @spec find(Delim, Stream, NewLines, Chars) -> Error | {Rest, NewLines,
%%          Chars}.
%% 
%% @doc 
%%  Make an effort to run to the next instance of the delimeter. 
%%  Only whitespace and newlines are expected to be between the start
%%  and the delim.
%% @end
%%--------------------------------------------------------------------
find(Delim, [Delim | T], NewLines, Chars) ->
    {T, NewLines, Chars + 1};
find(Delim, [$\s | T], NewLines, Chars) ->
    find(Delim, T, NewLines, Chars + 1);
find(Delim, [$\t | T], NewLines, Chars) ->
    find(Delim, T, NewLines, Chars + 1);
find(Delim, [$\r | T], NewLines, _Chars) ->
    find(Delim, T, NewLines + 1, 0);
find(Delim, [$\n | T], NewLines, _Chars) ->
    find(Delim, T, NewLines + 1, 0);
find(Delim, _Rest, NewLines, Chars) ->
    {error, {"Expected object seperator", Delim, NewLines, Chars}}.


%%--------------------------------------------------------------------
%% @spec key(Stream, NewLines, Chars) -> {Key, NewLine, Chars} | Error.
%% 
%% @doc 
%%  Parse the key as part of the object.
%% @end
%%--------------------------------------------------------------------
key([$\" | T], NewLines, Chars) ->
    ktuo_parse_utils:stringish_body($\", T, [], NewLines, Chars + 1);
key([$\s | T], NewLines, Chars) ->
    key(T, NewLines, Chars + 1);
key([$\t | T], NewLines, Chars) ->
    key(T, NewLines, Chars + 1);
key([$\r | T], NewLines, _Chars) ->
    key(T, NewLines + 1, 0);
key([$\n | T], NewLines, _Chars) ->
    key(T, NewLines + 1, 0);
key(Stream, NewLines, Chars) ->
    ident(Stream, [], NewLines, Chars).


%%--------------------------------------------------------------------
%% @spec ident(Stream, Acc, NewLines, Chars) -> {Ident, Rest, N, L} | 
%%   Error.
%% 
%% @doc 
%%  Parse a single word ident from the stream. Idents may be 
%%  of the form [a-z][a-zA-Z0-9_]*
%% @end
%%--------------------------------------------------------------------
ident([H | T], Acc, NewLines, Chars) when H >= $a, H =< $z ->
    ident(T, [H | Acc], NewLines, Chars + 1);
ident([H | T], Acc, NewLines, Chars) when H >= $A, H =< $Z ->
    ident(T, [H | Acc], NewLines, Chars + 1);
ident([$_ | T], Acc, NewLines, Chars) ->
    ident(T, [$_ | Acc], NewLines, Chars + 1);
ident([H | T], Acc, NewLines, Chars) when H >= $0, H =< $9 ->
    ident(T, [ H | Acc], NewLines, Chars + 1);
ident([$\s | T], Acc, NewLines, Chars) ->
    {lists:reverse(Acc), T, {NewLines, Chars + 1}};
ident([$\t | T], Acc, NewLines, Chars) ->
    {lists:reverse(Acc), T, {NewLines, Chars + 1}};
ident([$\r | T], Acc, NewLines, _Chars) ->
    {lists:reverse(Acc), T, {NewLines + 1, 0}};
ident([$\n | T], Acc, NewLines, _Chars) ->
    {lists:reverse(Acc), T, {NewLines + 1, 0}};
ident([], Acc, NewLines, Chars) ->
    {lists:reverse(Acc), [], {NewLines, Chars}};
ident(Else, Acc, NewLines, Chars) when length(Acc) > 0 ->
    {lists:reverse(Acc), Else, {NewLines, Chars}};
ident(_Else, _Acc, NewLines, Chars)  ->
    {error, {"Unexpected character while parsing ident", NewLines, Chars}}.


%%=============================================================================
%% Unit tests
%%=============================================================================
encode_string_test() ->
    ?assertMatch("\"Hello\"", lists:flatten(encode({string, "Hello"}))),
    ?assertMatch("\"hello\"", lists:flatten(encode('hello'))).

encode_number_test() ->
    ?assertMatch("430", lists:flatten(encode(430))),
    ?assertMatch("4303432", lists:flatten(encode(4303432))),
    ?assertMatch("4.30000000000000000000e+01", lists:flatten(encode(43.00))),
    ?assertMatch("3.22232219999999983884e+02", 
                 lists:flatten(encode(322.23222))).

encode_array_test() ->
    ?assertMatch("[33,43,53]", lists:flatten(encode([33, 43, 53]))),
    ?assertMatch("[\"String\",34,\"song\"]", lists:flatten(encode([{string, 
                                                                    "String"},
                                                                   34, 
                                                                   song]))),
    ?assertMatch("[{\"Goodbye\":true,\"Hello\":44},43,54]",
                 lists:flatten(encode([[{{string, "Hello"}, 44},
                                        {{string, "Goodbye"}, true}],
                                       43, 54]))).

boolean_test() ->
    ?assertMatch({true, [], {0, 4}}, value("true", 0, 0)),
    ?assertMatch({false, [], {0, 5}}, value("false", 0, 0)).

null_test() ->
    ?assertMatch({null, [], {0, 4}}, value("null", 0, 0)).

ident_test() ->
    ?assertMatch({"Hello", [], {0, 5}}, value("Hello", 0, 0)),
    ?assertMatch({"boo88", [], {0, 5}}, value("boo88", 0, 0)),
    ?assertMatch({"bock", [$:], {0, 4}}, value("bock:", 0, 0)),
    ?assertMatch({"bock", [${], {0, 4}}, value("bock{", 0, 0)),
    ?assertMatch({"bock", [$[], {0, 4}}, value("bock[", 0, 0)).

