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



-spec main(list(string())) -> no_return().
main(Args) ->
  io:setopts([{encoding, unicode}]),
  CommandLineSpec = command_line_spec(),
  case getopt:parse(CommandLineSpec, Args) of
    {ok, {Options, ProtoFiles}} ->
      put(verbose, proplists:is_defined(verbose, Options)),
      info("using options ~p", [Options]),
      info("processing proto files ~p", [ProtoFiles]),
      erlang:halt(0);
    {error, {Reason, Data}} ->
      io:format("error: ~s ~p~n~n", [Reason, Data]),
      getopt:usage(CommandLineSpec, escript:script_name()),
      erlang:halt(1)
  end.

-spec info(string(), list(term())) -> ok.
info(Format, Args) ->
  case get(verbose) of
    true -> io:format(Format ++ "\n", Args), ok;
    _ -> ok
  end.

-spec command_line_spec() -> list(getopt:option_spec()).
command_line_spec() ->
    [{help, $h, "help", undefined, "print help and exit"},
     {include, $I, "include", string, "add an include path"},
     {verbose, $v, "verbose", undefined, "enable processing logs"}].
