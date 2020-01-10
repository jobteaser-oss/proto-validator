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

-module(proto_validator_utils).

-export([aggregate_results/1]).

-spec aggregate_results(list(ok | {error, term()})) ->
        ok | {error, list(term())}.
aggregate_results(Results) ->
  aggregate_results(Results, []).

-spec aggregate_results(list(ok | {error, term()}), list(term())) ->
        ok | {error, list(term())}.
aggregate_results([ok | Rest], ErrorReasons) ->
  aggregate_results(Rest, ErrorReasons);
aggregate_results([{error, Reason} | Rest], ErrorReasons) ->
  aggregate_results(Rest, [Reason | ErrorReasons]);
aggregate_results([], []) ->
  ok;
aggregate_results([], ErrorReasons) ->
  {error, ErrorReasons}.
