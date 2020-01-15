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

-module(proto_validator_catalog_test).

-include_lib("eunit/include/eunit.hrl").

-spec catalog(string()) -> proto_validator_catalog:catalog().
catalog(ProtoSource) ->
  GpbOptions = [use_packages, to_proto_defs, ignore_wellknown_types_directory],
  {ok, Defs} = gpb_compile:string(test, ProtoSource, GpbOptions),
  proto_validator_catalog:catalog(Defs).

type_equal_test() ->
  Equal = fun (T1, T2) ->
             proto_validator_catalog:type_equal(T1,  T2)
         end,
  ?assert(Equal(int32, int32)),
  ?assert(Equal({service, "foo"}, {service, "foo"})),
  ?assert(Equal({enum, "foo"}, {enum, "foo"})),
  ?assert(Equal({message, "foo"}, {message, "foo"})),
  ?assert(Equal({map, string, float}, {map, string, float})),
  ?assert(Equal({map, string, {enum, "foo"}}, {map, string, {enum, "foo"}})),
  ?assertNot(Equal(int32, uint32)),
  ?assertNot(Equal({service, "foo"}, {service, "bar"})),
  ?assertNot(Equal({service, "foo"}, {message, "foo"})),
  ?assertNot(Equal({service, "foo"}, {service, "Foo"})),
  ?assertNot(Equal({map, string, float}, {map, string, double})).

validate_type_test() ->
  Validate = fun proto_validator_catalog:validate_type/1,
  ?assertEqual(ok, Validate(int32)),
  ?assertError({validation_error, invalid_type, foo}, Validate(foo)),
  ?assertEqual(ok, Validate({service, "foo"})),
  ?assertEqual(ok, Validate({enum, "foo"})),
  ?assertEqual(ok, Validate({message, "foo"})),
  ?assertEqual(ok, Validate({map, string, int32})),
  ?assertError({validation_error, invalid_service_name, foo},
               Validate({service, foo})),
  ?assertError({validation_error, invalid_enum_name, foo},
               Validate({enum, foo})),
  ?assertError({validation_error, invalid_message_name, foo},
               Validate({message, foo})),
  ?assertError({validation_error, invalid_type, {map, string}},
               Validate({map, string})),
  ?assertError({validation_error, invalid_type, {foo, "a"}},
               Validate({foo, "a"})).

catalog_enum_test() ->
  Source =
    "syntax = \"proto3\";" ++
    "package test;" ++
    "enum E1 { M1 = 1; }" ++
    "enum E2 { M1 = 1; M2 = 2; }",
  Catalog = catalog(Source),
  ?assertEqual(#{package => "test",
                 enums => [{"E2", ["M1", "M2"]},
                           {"E1", ["M1"]}],
                 messages => [],
                 services => []},
               Catalog).

catalog_message_test() ->
  Source =
    "syntax = \"proto3\";" ++
    "package test;" ++
    "message M1 { string f1 = 1; map<string, int32> f2 = 2; }" ++
    "message M2 { M1 f1 = 1; E1 f2 = 2; }" ++
    "message M3 { oneof v { int64 i = 1; string s = 2;} }" ++
    "enum E1 { M1 = 1; }",
  Catalog = catalog(Source),
  ?assertEqual(#{package => "test",
                 enums => [{"E1", ["M1"]}],
                 messages => [{"M3", [{oneof, "v", [{field, "i", int64},
                                                    {field, "s", string}]}]},
                              {"M2", [{field, "f1", {message, "test.M1"}},
                                      {field, "f2", {enum, "test.E1"}}]},
                              {"M1", [{field, "f1", string},
                                      {field, "f2", {map, string, int32}}]}],
                 services => []},
               Catalog).

catalog_service_test() ->
  Source =
    "syntax = \"proto3\";" ++
    "package test;" ++
    "service S1 {" ++
    "  rpc RPC1(Req) returns (Res) {}" ++
    "  rpc RPC2(stream Req) returns (stream Res) {}" ++
    "}" ++
    "message Req { }" ++
    "message Res { }",
  Catalog = catalog(Source),
  ?assertEqual(#{package => "test",
                 enums => [],
                 messages => [{"Res", []},
                              {"Req", []}],
                 services => [{"S1", [{"RPC1",
                                       {message, "test.Req"},
                                       {message, "test.Res"},
                                       false, false},
                                      {"RPC2",
                                       {message, "test.Req"},
                                       {message, "test.Res"},
                                       true, true}]}]},
               Catalog).
