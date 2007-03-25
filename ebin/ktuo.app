%% -*- mode:Erlang; fill-column:79 -*-
{application, ktuo,
 [{description, "Json/Tuple parser encoder for Erlang"},
  {vsn, "0.2.0"},           
  {modules, [ktuo_json,   
             ktuo_tuple,
             ktuo_parse_utils]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
