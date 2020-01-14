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

-module(proto_validator_utils_test).

-include_lib("eunit/include/eunit.hrl").

aggregate_results_test() ->
  Aggregate = fun (Results) ->
                  proto_validator_utils:aggregate_results(Results)
              end,
  ?assertEqual(ok, Aggregate([])),
  ?assertEqual(ok, Aggregate([ok])),
  ?assertEqual(ok, Aggregate([ok, ok, ok])),
  ?assertEqual({error, [e1]}, Aggregate([{error, e1}])),
  ?assertEqual({error, [e1]}, Aggregate([ok, {error, e1}, ok])),
  ?assertEqual({error, [e2, e1]}, Aggregate([{error, e1}, {error, e2}])).
