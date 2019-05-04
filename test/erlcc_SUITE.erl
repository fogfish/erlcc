%%
%% Copyright (C) 2016 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/erlcc
%%
-module(erlcc_SUITE).

-export([
   all/0
,  test/1
,  fail/1
,  syntax/1
]).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports),
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

test(_) ->
   {ok, Code} = erlcc:compile(test, <<"-module(x). -export([a/0]). a() -> ok.">>),
   {ok, test} = erlcc:inject(test, Code),
   ok = test:a().

fail(_) ->
   {ok, Code} = erlcc:compile(test, <<"-module(x). -export([a/0]). a() -> ok.">>),
   {error, badfile} = erlcc:inject(test, <<"deadbeef", Code/binary>>).

syntax(_) ->
   {error, _} = erlcc:compile(test, <<"-module(x). -export([a/0]). a() ->">>).
