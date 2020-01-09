%% Copyright 2020 JobTeaser
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(proto_validator).

-export([main/1]).

-type options() :: list(option()).
-type option() :: help
                | {include, string()}
                | verbose.

-spec main(list(string())) -> no_return().
main(Args) ->
  %% Global options
  io:setopts([{encoding, unicode}]),
  %% Command line
  CommandLineSpec = command_line_spec(),
  case getopt:parse(CommandLineSpec, Args) of
    {ok, {Options, ProtoFiles}} ->
      %% Verbosity
      put(verbose, proplists:is_defined(verbose, Options)),
      %% Help string
      Help = proplists:is_defined(help, Options),
      if
        Help == true ->
          getopt:usage(CommandLineSpec, escript:script_name()),
          erlang:halt(0);
        true -> ok
      end,
      %% Configuration file
      Config = case load_config(Options) of
                 {ok, Config2} ->
                   Config2;
                 {error, ConfigErrReason} ->
                   die("cannot load configuration: ~p", [ConfigErrReason])
               end,
      %% File validation
      Results = validate_proto_files(ProtoFiles, Options),
      info("results: ~p", [Results]); % TODO formatting
    {error, {Reason, Data}} ->
      io:format(standard_error, "error: ~s ~p~n~n", [Reason, Data]),
      getopt:usage(CommandLineSpec, escript:script_name()),
      erlang:halt(1)
  end.

-spec info(string(), list(term())) -> ok.
info(Format, Args) ->
  case get(verbose) of
    true -> io:format(Format ++ "\n", Args), ok;
    _ -> ok
  end.

-spec die(string(), list(term())) -> no_return().
die(Format, Args) ->
  io:format(standard_error, Format ++ "\n", Args),
  erlang:halt(1).

-spec command_line_spec() -> list(getopt:option_spec()).
command_line_spec() ->
    [{help, $h, "help", undefined, "print help and exit"},
     {config, $c, "config", string, "set the configuration file"},
     {include_path, $I, "include", string, "add an include path"},
     {verbose, $v, "verbose", undefined, "enable processing logs"}].

-spec load_config(options()) -> {ok, Config} | {error, Reason} when
    Config :: proto_validator_config:config(),
    Reason :: term().
load_config(Options) ->
  case proplists:get_value(config, Options) of
    undefined ->
      {ok, proto_validator_config:default_config()};
    Path ->
      proto_validator_config:load(Path)
  end.

-spec validate_proto_files(Files, options()) -> list({File, Result}) when
    Files :: list(File),
    File :: file:name_all(),
    Result :: ok | {error, term()}.
validate_proto_files(Files, Options) ->
  info("using options ~p", [Options]),
  info("validating proto files ~p", [Files]),
  IncludePaths = proplists:get_all_values(include_path, Options),
  GpbBaseOpts = [use_packages,
                 to_proto_defs,
                 ignore_wellknown_types_directory],
  GpbIOptions = [{i, Path} || Path <- IncludePaths],
  GpbOptions = GpbBaseOpts ++ GpbIOptions,
  lists:map(fun (File) ->
                {File, validate_proto_file(File, Options, GpbOptions)}
            end, Files).

-spec validate_proto_file(File, options(), gpb:opts()) ->
        ok | {error, Reason} when
    File :: file:name_all(),
    Reason :: term().
validate_proto_file(File, Options, GpbOptions) ->
  info("validating proto file ~p", [File]),
  case gpb_compile:file(File, GpbOptions) of
    {ok, Definitions} ->
      validate_definitions(Definitions, Options);
    {ok, Definitions, Warnings} ->
      lists:foreach(fun (Warning) ->
                        info("warning: ~p", [Warning])
                    end, Warnings),
      validate_definitions(Definitions, Options),
      ok;
    {error, Reason} ->
      {error, {compile_error, Reason}}
  end.

-spec validate_definitions(gpb_defs:defs(), options()) -> ok | {error, reason}.
validate_definitions(Definitions, _Options) ->
  Catalog = proto_validator_catalog:catalog(Definitions),
  ObjectsData = proto_validator_rules:collect_objects_data(Catalog),
  info("XXX CATALOG ~n~p", [Catalog]),
  info("XXX OBJECTS DATA ~n~p", [ObjectsData]),
  ok.
