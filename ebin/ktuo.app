%% -*- mode:Erlang; fill-column:79 -*-
{application, ktuo,
 [{description, "Json/Tuple parser encoder for Erlang"},
  {vsn, "0.4.0.3"},
  {modules, [ktj_encode,
             ktj_decode,
             ktt_decode,
             ktuo_parse_utils]},
  {registered, []},
  {applications, [kernel, stdlib]}]}.
