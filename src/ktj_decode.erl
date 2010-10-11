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
