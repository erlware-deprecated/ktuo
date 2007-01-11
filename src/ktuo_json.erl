%%%-------------------------------------------------------------------
%%% @author Eric Merritt 
%%% @doc 
%%% 
%%% @end
%%% @copyright (C) 2006
%%% Created : 19 Dec 2006 by Eric Merritt 
%%%-------------------------------------------------------------------
-module(ktuo_json).

%-include("eunit.hrl").

-export([decode/1, encode/1]).

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
decode(Stream) ->
    value(Stream).


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
encode_string({string, Value}) ->
    [$\", Value, $\"];
encode_string(Value) when is_atom(Value) ->
    [$\", atom_to_list(Value), $\"];
encode_string(Value) when is_list(Value) ->
    [$\", Value, $\"].

encode_integer(Value) ->
    integer_to_list(Value).

encode_float(Value) ->
    float_to_list(Value).


encode_array([H | T], []) ->
    encode_array(T, [encode(H), $[]);
encode_array([H | T], TAcc) ->
    encode_array(T, [encode(H), $, | TAcc]);
encode_array([], []) ->
    [$], $[];
encode_array([], TAcc) ->
    [$] | TAcc].

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


value([$\" | T]) ->
    ktuo_parse_utils:stringish_body($\", T, []);
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
    array_body(T, []);
value([${ | T]) ->
    object_body(T, []);
value([$t, $r, $u, $e | T]) ->
    {true, T};
value([$f, $a, $l, $s, $e | T]) ->
    {false, T};
value([$n, $u, $l, $l | T]) ->
    {null, T};
value([$\s | T]) ->
    value(T);
value([$\t | T]) ->
    value(T);
value([$\r | T]) ->
    value(T);
value([$\n | T]) ->
    value(T);
value(Stream) ->
    ident(Stream, []).


array_body([$] | T], Acc) ->
    {lists:reverse(Acc), T};
array_body([$, | T], Acc) ->
    array_body(T, Acc);
array_body([$\s | T], Acc) ->
    array_body(T, Acc);
array_body([$\t | T], Acc) ->
    array_body(T, Acc);
array_body([$\n | T], Acc) ->
    array_body(T, Acc);
array_body([$\r | T], Acc) ->
    array_body(T, Acc);
array_body(Stream, Acc) ->
    {Value, Rest} = value(Stream),
    array_body(Rest, [Value | Acc]).

object_body([$} | T], Acc) ->
    {Acc, T};
object_body([$, | T], Acc) ->
    object_body(T, Acc);
object_body([$\s | T], Acc) ->
    object_body(T, Acc);
object_body([$\t | T], Acc) ->
    object_body(T, Acc);
object_body([$\r | T], Acc) ->
    object_body(T, Acc);
object_body([$\n | T], Acc) ->
    object_body(T, Acc);
object_body(Else, Acc) ->
    {Key, Rest1} = key(Else),
    Rest2 = find($:, Rest1),
    {Value, Rest3} = value(Rest2),
    object_body(Rest3, [{Key, Value} | Acc]).

find(Delim, [Delim | T]) ->
    T;
find(Delim, [$\s | T]) ->
    find(Delim, T);
find(Delim, [$\t | T]) ->
    find(Delim, T);
find(Delim, [$\r | T]) ->
    find(Delim, T);
find(Delim, [$\n | T]) ->
    find(Delim, T).

key([$\" | T]) ->
    ktuo_parse_utils:stringish_body($\", T, []);
key([$\s | T]) ->
    key(T);
key([$\t | T]) ->
    key(T);
key([$\r | T]) ->
    key(T);
key([$\n | T]) ->
    key(T);
key(Stream) ->
    ident(Stream, []).



ident([H | T], Acc) when H >= $a, H =< $z ->
    ident(T, [H | Acc]);
ident([H | T], Acc) when H >= $A, H =< $Z ->
    ident(T, [H | Acc]);
ident([$_ | T], Acc) ->
    ident(T, [$_ | Acc]);
ident([H | T], Acc) when H >= $0, H =< $9 ->
    ident(T, [ H | Acc]);
ident([$\s | T], Acc) ->
    {lists:reverse(Acc), T};
ident([$\t | T], Acc) ->
    {lists:reverse(Acc), T};
ident([$\r | T], Acc) ->
    {lists:reverse(Acc), T};
ident([$\n | T], Acc) ->
    {lists:reverse(Acc), T};
ident([], Acc) ->
    {lists:reverse(Acc), []};
ident(Else, Acc) when length(Acc) > 0->
    {lists:reverse(Acc), Else}.


%%=============================================================================
%% Unit tests
%%=============================================================================
%% encode_string_test() ->
%%     ?assertMatch("\"Hello\"", lists:flatten(encode({string, "Hello"}))),
%%     ?assertMatch("\"hello\"", lists:flatten(encode('hello'))).

%% encode_number_test() ->
%%     ?assertMatch("430", lists:flatten(encode(430))),
%%     ?assertMatch("4303432", lists:flatten(encode(4303432))),
%%     ?assertMatch("4.30000000000000000000e+01", lists:flatten(encode(43.00))),
%%     ?assertMatch("3.22232219999999983884e+02", 
%%                  lists:flatten(encode(322.23222))).

%% encode_array_test() ->
%%     ?assertMatch("[33,43,53]", lists:flatten(encode([33, 43, 53]))),
%%     ?assertMatch("[\"String\",34,\"song\"]", lists:flatten(encode([{string, 
%%                                                                     "String"},
%%                                                                    34, 
%%                                                                    song]))),
%%     ?assertMatch("[{\"Goodbye\":true,\"Hello\":44},43,54]",
%%                  lists:flatten(encode([[{{string, "Hello"}, 44},
%%                                         {{string, "Goodbye"}, true}],
%%                                        43, 54]))).

%% encode_object_test() ->
%%     ?assertMatch("{\"Hello\":\"Hel\",\"Super\":421}",
%%                  lists:flatten(encode([{{string, "Super"}, 421},
%%                                 {'Hello','Hel'}]))).



%% boolean_test() ->
%%     ?assertMatch({true, []}, value("true")),
%%     ?assertMatch({false, []}, value("false")).

%% null_test() ->
%%     ?assertMatch({null, []}, value("null")).

%% ident_test() ->
%%     ?assertMatch({"Hello", []}, value("Hello")),
%%     ?assertMatch({"boo88", []}, value("boo88")),
%%     ?assertMatch({"bock", [$:]}, value("bock:")),
%%     ?assertMatch({"bock", [${]}, value("bock{")),
%%     ?assertMatch({"bock", [$[]}, value("bock[")).


%% glossary_test() ->
%%     ?assertMatch({[{"glossary",
%%                    [{"GlossDiv",
%%                      [{"GlossList",
%%                        [{"GlossEntry",
%%                          [{"GlossSee","markup"},
%%                           {"GlossDef",
%%                            [{"GlossSeeAlso",["GML","XML"]},
%%                             {"para","A meta-mars DocBook."}]},
%%                           {"Abbrev","ISO 8879:1986"},
%%                           {"Acronym","SGML"},
%%                           {"GlossTerm","Standareralized"},
%%                           {"SortAs","SGML"},
%%                           {"ID","SGML"}]}]},
%%                       {"title","S"}]},
%%                     {"title","example glossary"}]}],
%%                   []},  
%%                  value("{ "   
%%                       "\"glossary\": { "
%%                       "  \"title\": \"example glossary\","
%%                       " \"GlossDiv\": {"
%%                       "      \"title\": \"S\", "
%%                       "  \"GlossList\": { "
%%                       "          \"GlossEntry\": {"
%%                       "              \"ID\": \"SGML\","
%%                       "            \"SortAs\": \"SGML\","
%%                       "            \"GlossTerm\": \"Standareralized\", "
%%                       "            \"Acronym\": \"SGML\", "
%%                       "            \"Abbrev\": \"ISO 8879:1986\","
%%                       "            \"GlossDef\": { "
%%                       "                  \"para\": \"A meta-mars DocBook.\", "
%%                       "             \"GlossSeeAlso\": [\"GML\", \"XML\"]"
%%                       "             },"
%%                       "            \"GlossSee\": \"markup\" "
%%                       "          }"
%%                       "      }"
%%                       "  }"
%%                       "}}")).

%% menu_test() ->
%%     ?assertMatch({[{"menu",
%%                     [{"popup",
%%                       [{"menuitem",
%%                         [[{"onclick","CreateNewDoc()"},
%%                           {"value","New"}],
%%                          [{"onclick","OpenDoc()"},
%%                           {"value","Open"}],
%%                          [{"onclick","CloseDoc()"},
%%                           {"value","Close"}]]}]},
%%                      {"value","File"},
%%                      {"id","file"}]}],
%%                   []},
%%       value("{\"menu\": {"
%%             "  \"id\": \"file\","
%%             "  \"value\": \"File\","
%%             "  \"popup\": {"
%%             "      \"menuitem\": ["
%%             "          {\"value\": \"New\", \"onclick\": \"CreateNewDoc()\"}, "
%%             "          {\"value\": \"Open\", \"onclick\": \"OpenDoc()\"},"
%%             "          {\"value\": \"Close\", \"onclick\": \"CloseDoc()\"} "
%%             "                    ]"
%%             "   }"
%%             "}}")).

%% widget_test() ->
%%     ?assertMatch({[{"widget",
%%                     [{"text",
%%                       [{"onMouseUp","sun1.opacity = (sun1.opacity / 100) * 90;"},
%%                        {"alignment","center"},
%%                        {"vOffset",100},
%%                        {"hOffset",250},
%%                        {"name","text1"},
%%                        {"style","bold"},
%%                        {"size",36},
%%       {"data","Click Here"}]},
%%                      {"image",
%%                       [{"alignment","center"},
%%                        {"vOffset",250},
%%                        {"hOffset",250},
%%                        {"name","sun1"},
%%                        {"src","Images/Sun.png"}]},
%%                      {"window",
%%                       [{"height",500},
%%                        {"width",500},
%%                        {"name","main_window"},
%%                        {"title","Sample Konfabulator Widget"}]},
%%                      {"debug","on"}]}],
%%                   []},
                 
%%                  value("{\"widget\": {"
%%                        "    debug: on,"
%%                        "    window: {"
%%                        "        title: \"Sample Konfabulator Widget\","
%%                        "        name: \"main_window\","
%%                        "        \"width\": 500,"
%%                        "        \"height\": 500"
%%                        "    },"
%%                        "    \"image\": { "
%%                        "        \"src\": \"Images/Sun.png\","
%%                        "        \"name\": \"sun1\","
%%                        "        \"hOffset\": 250,"
%%                        "        \"vOffset\": 250,"
%%                        "        \"alignment\": \"center\""
%%                        "    },"
%%                        "    \"text\": {"
%%                        "        \"data\": \"Click Here\","
%%                        "        \"size\": 36,"
%%                        "        \"style\": \"bold\","
%%                        "        \"name\": \"text1\","
%%                        "        \"hOffset\": 250,"
%%                        "        \"vOffset\": 100,"
%%                        "        \"alignment\": \"center\","
%%                        "        \"onMouseUp\": \"sun1.opacity = (sun1.opacity / 100) * 90;\""
%%                        "    }"
%%                        "}}")).  

