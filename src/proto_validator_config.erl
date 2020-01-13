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

-module(proto_validator_config).

-export([default_config/0, load/1, validate/1, rules/1]).

-type config() :: list(config_entry()).

-type config_entry() :: {rules, proto_validator_rules:rules()}.

-spec default_config() -> config().
default_config() ->
  [].

-spec load(file:name_all()) -> {ok, config()} | {error, term()}.
load(Path) ->
  file:consult(Path).

-spec validate(config()) -> ok | no_return().
validate(Config) ->
  lists:foreach(fun (Entry) ->
                    case Entry of
                      {rules, Rules} ->
                        proto_validator_rules:validate_rules(Rules);
                      _ ->
                        error({validation_error, invalid_config_entry, Entry})
                    end
                end, Config),
  ok.

-spec rules(config()) -> proto_validator_rules:rules().
rules(Config) ->
  lists:flatten(proplists:get_all_values(rules, Config)).
