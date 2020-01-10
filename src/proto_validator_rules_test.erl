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

-module(proto_validator_rules_test).

-include_lib("eunit/include/eunit.hrl").

evaluate_expression_test() ->
  Eval = fun (Expr, Value) ->
             proto_validator_rules:evaluate_expression(Expr, Value)
         end,
  %% is
  ?assert(Eval({is, "foo"}, "foo")),
  ?assert(Eval({is, int32}, int32)),
  ?assert(Eval({is, {enum, "Foo"}}, {enum, "Foo"})),
  %% is_not
  ?assert(Eval({is_not, "foo"}, "bar")),
  ?assert(Eval({is_not, "foo"}, "Foo")),
  ?assert(Eval({is_not, int32}, string)),
  ?assert(Eval({is_not, {enum, "Foo"}}, {enum, "Bar"})),
  ?assert(Eval({is_not, {enum, "Foo"}}, {message, "Foo"})),
  %% is_any_of
  ?assert(Eval({is_any_of, ["hello", "world", "foo"]}, "foo")),
  ?assertNot(Eval({is_any_of, ["a", "b", "c"]}, "foo")),
  ?assertNot(Eval({is_any_of, []}, "foo")),
  ?assert(Eval({is_any_of, [int32, uint32, string]}, int32)),
  ?assert(Eval({is_any_of, [{message, "Bar"}, {message, "Foo"}]},
               {message, "Foo"})),
  %% is_none_of
  ?assert(Eval({is_none_of, ["a", "b", "c"]}, "foo")),
  ?assert(Eval({is_none_of, [string, int32]}, uint64)),
  ?assert(Eval({is_none_of, [{service, "Foo"}, {service, "Bar"}]},
              {service, "Hello"})),
  %% has_prefix
  ?assert(Eval({has_prefix, ""}, "hello")),
  ?assert(Eval({has_prefix, "h"}, "hello")),
  ?assert(Eval({has_prefix, "hello"}, "hello")),
  ?assertNot(Eval({has_prefix, "hello2"}, "hello")),
  %% has_suffix
  ?assert(Eval({has_suffix, ""}, "hello")),
  ?assert(Eval({has_suffix, "o"}, "hello")),
  ?assert(Eval({has_suffix, "hello"}, "hello")),
  ?assertNot(Eval({has_suffix, "2hello"}, "hello")).
