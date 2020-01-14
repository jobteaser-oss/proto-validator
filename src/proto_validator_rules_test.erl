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

collect_objects_data_test() ->
  Catalog = #{enums =>
                [{"E1", ["M1"]}],
              messages =>
                [{"M3", [{oneof, "v", [{field, "i", int64},
                                       {field, "s", string}]}]},
                 {"M2", [{field, "f1", {message, "test.M1"}},
                         {field, "f2", {enum, "test.E1"}}]},
                 {"M1", [{field, "f1", string},
                         {field, "f2", {map, string, int32}}]}],
              services =>
                [{"S1", [{"RPC1",
                          {message, "test.M1"},
                          {message, "test.M2"},
                          false, false}]}]},
  Data = proto_validator_rules:collect_objects_data(Catalog),
  ?assertEqual(#{message =>
                   [#{name => "M3"},
                    #{name => "M2"},
                    #{name => "M1"}],
                 field =>
                   [#{name => "i",
                      parent_message_name => "M3",
                      type => int64},
                    #{name => "s",
                      parent_message_name => "M3",
                      type => string},
                    #{name => "f1",
                      parent_message_name => "M2",
                      type => {message, "test.M1"}},
                    #{name => "f2",
                      parent_message_name => "M2",
                      type => {enum, "test.E1"}},
                    #{name => "f1",
                      parent_message_name => "M1",
                      type => string},
                    #{name => "f2",
                      parent_message_name => "M1",
                      type => {map, string, int32}}],
                 service =>
                   [#{name => "S1"}],
                 rpc =>
                   [#{name => "RPC1",
                      parent_service_name => "S1",
                      input_type => {message, "test.M1"},
                      input_type_name => "test.M1",
                      output_type => {message, "test.M2"},
                      output_type_name => "test.M2"}]
                }, Data).

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
