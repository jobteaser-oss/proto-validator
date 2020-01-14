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

-type validation_result() :: {file:name_all(), ok | {error, term()}}.
-type validation_results() :: list(validation_result()).

-spec main(list(string())) -> no_return().
main(Args) ->
  %% Global options
  io:setopts([{encoding, unicode}]),
  %% Command line
  CommandLineSpec = command_line_spec(),
  case getopt:parse(CommandLineSpec, Args) of
    {ok, {Options, ProtoFiles}} ->
      %% Help string
      Help = proplists:is_defined(help, Options),
      if
        Help == true ->
          getopt:usage(CommandLineSpec, escript:script_name()),
          erlang:halt(0);
        true -> ok
      end,
      %% Verbosity
      put(verbose, proplists:is_defined(verbose, Options)),
      %% Configuration file
      Config = case load_config(Options) of
                 {ok, Config2} ->
                   Config2;
                 {error, ConfigErrReason} ->
                   ConfigErrString = format_error_reason(ConfigErrReason),
                   die("cannot load configuration: ~s", [ConfigErrString])
               end,
      %% Main command
      main_validate(ProtoFiles, Config, Options);
    {error, {Reason, Data}} ->
      log_error("~s ~p", [Reason, Data]),
      getopt:usage(CommandLineSpec, escript:script_name()),
      erlang:halt(1)
  end.

-spec main_validate(Files, Config, options()) -> ok when
    Files :: list(file:name_all()),
    Config :: proto_validator_config:config().
main_validate(Files, Config, Options) ->
  Results = validate_proto_files(Files, Config, Options),
  lists:foreach(fun process_validation_result/1, Results),
  case are_validation_results_success(Results) of
    true ->
      ok;
    false ->
      erlang:halt(1)
  end.

-spec log_info(string(), list(term())) -> ok.
log_info(Format, Args) ->
  case get(verbose) of
    true ->
      io:format(Format ++ "\n", Args),
      ok;
    _ ->
      ok
  end.

-spec log_error(string(), list(term())) -> ok.
log_error(Format, Args) ->
  io:format(standard_error, Format ++ "\n", Args),
  ok.

-spec die(string(), list(term())) -> no_return().
die(Format, Args) ->
  log_error(Format ++ "\n", Args),
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
      case proto_validator_config:load(Path) of
        {ok, Config} ->
          try proto_validator_config:validate(Config) of
              ok ->
              {ok, Config}
          catch error:Error ->
              {error, Error}
          end;
        {error, Reason} ->
          {error, Reason}
      end
  end.

-spec validate_proto_files(Files, Config, options()) ->
        validation_results() when
    Files :: list(file:name_all()),
    Config :: proto_validator_config:config().
validate_proto_files(Files, Config, Options) ->
  IncludePaths = proplists:get_all_values(include_path, Options),
  GpbBaseOpts = [use_packages,
                 to_proto_defs,
                 ignore_wellknown_types_directory],
  GpbIOptions = [{i, Path} || Path <- IncludePaths],
  GpbOptions = GpbBaseOpts ++ GpbIOptions,
  lists:map(fun (File) ->
                {File, validate_proto_file(File, Config, Options, GpbOptions)}
            end, Files).

-spec validate_proto_file(File, Config, options(), gpb:opts()) ->
        ok | {error, Reason} when
    File :: file:name_all(),
    Config :: proto_validator_config:config(),
    Reason :: term().
validate_proto_file(File, Config, _Options, GpbOptions) ->
  case gpb_compile:file(File, GpbOptions) of
    {ok, Definitions} ->
      validate_definitions(Definitions, Config);
    {ok, Definitions, Warnings} ->
      lists:foreach(fun (Warning) ->
                        log_info("warning: ~p", [Warning])
                    end, Warnings),
      validate_definitions(Definitions, Config);
    {error, Reason} ->
      {error, {compile_error, Reason}}
  end.

-spec validate_definitions(gpb_defs:defs(), Config) -> ok | {error, Reason} when
    Config :: proto_validator_config:config(),
    Reason :: term().
validate_definitions(Definitions, Config) ->
  Catalog = proto_validator_catalog:catalog(Definitions),
  ObjectsData = proto_validator_rules:collect_objects_data(Catalog),
  Rules = proto_validator_config:rules(Config),
  RuleFun = fun (Rule) ->
                case proto_validator_rules:evaluate_rule(Rule, ObjectsData) of
                  ok ->
                    false;
                  {error, RuleErrors} ->
                    {true, RuleErrors}
                end
            end,
  case lists:flatten(lists:filtermap(RuleFun, Rules)) of
    [] ->
      ok;
    Errors ->
      {error, Errors}
  end.

-spec are_validation_results_success(validation_results()) -> boolean().
are_validation_results_success(Results) ->
  lists:all(fun ({_, Result}) -> Result =:= ok end, Results).

-spec process_validation_result(validation_result()) -> ok.
process_validation_result({File, Result}) ->
  case Result of
    ok ->
      ok;
    {error, Reasons} when is_list(Reasons) ->
      lists:foreach(fun (Reason) ->
                        report_validation_error(File, Reason)
                    end, Reasons),
      ok;
    {error, Reason} ->
      report_validation_error(File, Reason),
      ok
  end.

-spec report_validation_error(file:name_all(), term()) -> ok.
report_validation_error(File, Reason) ->
  ReasonString = format_error_reason(Reason),
  log_error("~s: ~s", [File, ReasonString]).

-spec format_error_reason(term()) -> iolist().
format_error_reason(Reason = {test_failure, _, _, _}) ->
  format_test_failure(Reason);
format_error_reason({validation_error, Reason, Data}) ->
  format_validation_error(Reason, Data);
format_error_reason(Reason) ->
  io_lib:format("~0p", [Reason]).

-spec format_test_failure(proto_validator_rules:test_failure()) -> string().
format_test_failure({test_failure, _Test, {message, Data}, Message}) ->
  MessageName = maps:get(name, Data),
  io_lib:format("invalid message ~0p: ~s", [MessageName, Message]);
format_test_failure({test_failure, _Test, {field, Data}, Message}) ->
  FieldName = maps:get(name, Data),
  MessageName = maps:get(parent_message_name, Data),
  io_lib:format("invalid field ~0p in message ~0p: ~s",
                [FieldName, MessageName, Message]);
format_test_failure({test_failure, _Test, {service, Data}, Message}) ->
  ServiceName = maps:get(name, Data),
  io_lib:format("invalid service ~0p: ~s", [ServiceName, Message]);
format_test_failure({test_failure, _Test, {rpc, Data}, Message}) ->
  RPCName = maps:get(name, Data),
  ServiceName = maps:get(parent_service_name, Data),
  io_lib:format("invalid rpc ~0p in service ~0p: ~s",
                [RPCName, ServiceName, Message]).

format_validation_error(invalid_config_entry, Entry) ->
  io_lib:format("invalid configuration entry ~0p", [Entry]);
format_validation_error(invalid_rule, Rule) ->
  io_lib:format("invalid rule ~0p", [Rule]);
format_validation_error({invalid_rule, Message}, Data) ->
  io_lib:format("invalid rule ~0p: ~0p", [Message, Data]);
format_validation_error(Reason, Data) ->
  io_lib:format("~0p: ~0p", [Reason, Data]).
