%%
%% Copyright (C) 2016 Dmitry Kolesnikov
%%
%% This Makefile may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/erlcc
%%
-module(erlcc).
-compile({parse_transform, category}).

-export([
   compile/2
,  compile/3
,  inject/2
]).

%%
%% data types
-type mod()    :: atom().
-type src()    :: binary() | string().
-type opts()   :: #{
                     %% working directory (default /tmp)
                     workdir     => binary() | string()

                     %% list of parse transforms (default [category])
                  ,  transform   => [module()]
                  }.

%%
%% compile 
-spec compile(mod(), src()) -> datum:either(binary()).
-spec compile(mod(), src(), opts()) -> datum:either(binary()).

compile(Module, Source) ->
   compile(Module, Source, #{
      workdir   => "/tmp"
   ,  transform => [category]
   }).

compile(Module, Source, Opts) ->
   [either ||
      Filename =< filename(Module, Opts),
      filelib:ensure_dir(Filename),
      file:write_file(Filename, Source),
      Forms <- compile_ast(Filename, Opts),
      file:delete(Filename),
      compile_obj( compile_mod(Module, Forms), Opts )
   ].

%%
%%
-spec inject(mod(), binary()) -> datum:either(mod()).

inject(Module, Code) ->
   code:purge(Module),
   case code:load_binary(Module, undefined, Code) of
      {module, _} ->
         {ok, Module};
      Error ->
         Error
   end.   

%%-----------------------------------------------------------------------------
%%
%% internal
%%
%%-----------------------------------------------------------------------------

%%
%%
filename(Module, #{workdir := WorkDir}) ->
   typecast:c(
      filename:join([WorkDir, typecast:c(Module) ++ ".erl"])
   ).

%%
%%
compile_ast(Filename, #{workdir := WorkDir, transform := Transforms}) ->
   case 
      compile:file(
         Filename, 
         [
            {outdir, WorkDir}
         ,  'P'
         ,  return_errors
         ,  binary
         ,  no_error_module_mismatch
         ] ++ [{parse_transform, X} || X <- Transforms]
      )
   of
      {ok, _, Forms} ->
         {ok, Forms};
      {error, Reason, _} ->
         {error, Reason}
   end.

%%
%%
compile_mod(Id, [{attribute, Ln, module, _}|Tail]) ->
   [{attribute, Ln, module, Id} | compile_mod(Id, Tail)];
compile_mod(Id, [Head|Tail]) ->
   [Head | compile_mod(Id, Tail)];
compile_mod(_, []) ->
   [].

%%
%%
compile_obj(Code, #{transform := Transforms}) ->
   case
      compile:forms(Code, 
         [
            return_errors
         ,  binary
         ] ++ [{parse_transform, X} || X <- Transforms]
      )
   of
      {ok, _, Binary} ->
         {ok, Binary};
      Error ->
         Error
   end.
