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
%%% @copyright (C) 2006-2010
%%% @doc
%%% Used for decoding json from a string or binary
%%%  Parsing strings into erlang.
%%% @end
%%% Created : 19 Dec 2006 by Eric Merritt
%%%-------------------------------------------------------------------
-module(ktj_parse).

-include_lib("eunit/include/eunit.hrl").

-export([parse/1, parse/3]).

-export_type([key/0,
	      value/0,
	      object/0,
	      atj_array/0,
	      json_number/0,
	      json_string/0,
	      json_bool/0,
	      null/0,
	      mid_parse_stream/0,
	      stream/0]).


%%=============================================================================
%% Types
%%=============================================================================

-type key() :: json_string().
-type value() :: object() | json_number() | atj_array() | json_string() | json_bool() | null().
-type object() :: {obj, [{key(), value()}]}.
-type atj_array() :: [value()].
-type json_number() :: integer() | float().
-type json_string() :: binary().
-type json_bool() :: true | false.
-type null() :: null.

-opaque mid_parse_stream() :: {value(), string(), {integer(), integer()}}.
-type stream() :: binary() | string() | mid_parse_stream().

%%--------------------------------------------------------------------
%% @doc
%%  Parses the incoming stream into valid json objects.
%%
%%  This decode function parses a superset of json, in that single, un
%%  quoted words are parsed into strings. This makes it easer to
%%  use json as a config language.
%%
%%  It is designed to parse a stream of json objects. It will return
%%  the erlang term representing the first json object along with
%%  continuation information. You may continue to parse the json objects
%%  by passing the return value of decode back to itself. At the end
%%  of the json stream decode will return an 'end_of_stream'.
%%
%%
%% @end
%%--------------------------------------------------------------------
-spec parse(stream()) -> mid_parse_stream() | end_of_stream.
parse({_, [], _}) ->
    end_of_stream;
parse({_, UnparsedRemainder, {NewLines, Chars}}) ->
    parse(UnparsedRemainder, NewLines, Chars);
parse(Stream) when is_list(Stream) ->
    parse(Stream, 0, 0);
parse(Stream) when is_binary(Stream) ->
    parse(binary_to_list(Stream), 0, 0).


%%--------------------------------------------------------------------
%% @doc
%%  Decodes the value with the fixed set of newlines and chars.
%% @end
%%--------------------------------------------------------------------
-spec parse(string(), integer(), integer()) -> mid_parse_stream().
parse(Stream, NewLines, Chars) ->
    value(Stream, NewLines, Chars).



%%=============================================================================
%% Internal Functions
%%=============================================================================
%%--------------------------------------------------------------------
%% @doc
%% Parses a json value out into system
%%
%% @spec (Parsee::string(), NewLines::integer(), Chars::string()) -> value()
%% @private
%% @end
%%--------------------------------------------------------------------
-spec value(string(), integer(), integer()) -> mid_parse_stream().
value([$\" | T], NewLines, Chars) ->
    ktuo_parse_utils:stringish_body($\", T, [], NewLines, Chars + 1);
value([$- | T], NewLines, Chars) ->
    ktuo_parse_utils:digit(T, [$-], front, NewLines, Chars + 1);
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
    object_body(T, [], NewLines, Chars + 1);
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

-spec array_body(string(), [value()], integer(), integer()) -> mid_parse_stream().
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

-spec object_body(string(), [{string(), value()}], integer(), integer()) -> mid_parse_stream().
object_body([$} | T], Acc, NewLines, Chars) ->
    {{obj, Acc}, T, {NewLines, Chars + 1}};
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


%% @doc
%%  Parse out the key. When that is complete parse out the value.
%% @end
-spec do_key(string(), [{string(), value()}], integer(), integer()) -> mid_parse_stream().
do_key(Stream, Acc, NewLines, Chars) ->
    case key(Stream, NewLines, Chars) of
        {Key, Rest1, {NLines, NChars}} ->
            do_value(Key, Rest1, Acc, NLines, NChars);
        Else ->
            Else
    end.

%% @doc
%%  Parse out the value. Then continue with the object.
%% @end
-spec do_value(string(), string(), [{string(), value()}], integer(), integer()) -> mid_parse_stream().
do_value(Key, Stream, Acc, NewLines, Chars) ->
    case find($:, Stream, NewLines, Chars) of
        {Rest, NLines, NChars} ->
            case value(Rest, NLines, NChars) of
                {Value, Rest1, {NLines1, NChars1}} ->
                    object_body(Rest1, [{Key, Value} |  Acc],
                                NLines1, NChars1);
                Else ->
                    Else
            end;
        Else1 ->
            Else1
    end.

%% @doc
%%  Make an effort to run to the next instance of the delimeter.
%%  Only whitespace and newlines are expected to be between the start
%%  and the delim.
%% @end
-spec find(string(), string(), integer(), integer()) -> mid_parse_stream().
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


%% @doc
%%  Parse the key as part of the object.
%% @end
-spec key(string(), integer(), integer()) -> mid_parse_stream().
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


%% @doc
%%  Parse a single word ident from the stream. Idents may be
%%  of the form [a-z][a-zA-Z0-9_]*
%% @end
-spec ident(string(), string(), integer(), integer()) -> mid_parse_stream().
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
boolean_test() ->
    ?assertMatch({true, [], {0, 4}}, value("true", 0, 0)),
    ?assertMatch({false, [], {0, 5}}, value("false", 0, 0)).

%% This triggers a bug fixed after 0.4.0.1
real_test() ->
    ?assertMatch({-0.1, [], {0, 4}}, value("-0.1", 0, 0)).

null_test() ->
    ?assertMatch({null, [], {0, 4}}, value("null", 0, 0)).

ident_test() ->
    ?assertMatch({"Hello", [], {0, 5}}, value("Hello", 0, 0)),
    ?assertMatch({"boo88", [], {0, 5}}, value("boo88", 0, 0)),
    ?assertMatch({"bock", [$:], {0, 4}}, value("bock:", 0, 0)),
    ?assertMatch({"bock", [${], {0, 4}}, value("bock{", 0, 0)),
    ?assertMatch({"bock", [$[], {0, 4}}, value("bock[", 0, 0)).

config_test() ->
    {Value, _, _} = parse("{\"build\": {\"start_dir\": \"\\u002fhome\\u002fsomething\\u002fworkspace\\u002fsinan\\u002fclient\"}}"),
    ?assertMatch({obj,
                  [{<<"build">>,
                    {obj,
                     [{<<"start_dir">>,
                       <<"/home/something/workspace/sinan/client">>}]}}]},
                 Value).

