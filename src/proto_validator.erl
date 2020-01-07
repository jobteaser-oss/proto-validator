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
main(_Args) ->
  io:setopts([{encoding, unicode}]),
  die("not implemented").

-spec die(string()) -> no_return().
die(String) ->
  die(String, []).

-spec die(string(), list(term())) -> no_return().
die(Format, Args) ->
  io:format(standard_error, [Format, $\n], Args),
  erlang:halt(1).
