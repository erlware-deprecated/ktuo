%% -*- mode: Erlang; fill-column: 80; comment-column: 76; -*-
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt
%%% @copyright (C) 2006-2010
%%% @doc This module has been discontinued, use {@link ktj_parse} instead.
%%%
%%% ktj_parse API is not backwards compatible with former ktj_decode API.
%%% The change, however, is not very dramatic. The only incompatible evolution
%%% is that now JSON strings are parsed to erlang binaries, which is coherent
%%% with kjt_encode API.
%%%
%%% @deprecated Use {@link ktj_parse}.
%%% @end
%%% Created : 19 Dec 2006 by Eric Merritt
%%%-------------------------------------------------------------------
-module(ktj_decode).
-deprecated(module).

-export([decode/1,
	 decode/3]).

decode(_,_,_) ->
    decode(none).

decode(_) ->
    erlang:error(
      {discontinued,
       "ktj_decode no longer exists, use the new ktj_parse instead."
       " Note that now JSON strings will be parsed as erlang  binaries"}).
