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

-module(proto_validator_rules).

-export_type([rules/0]).

-type proto_object() :: service
                      | rpc
                      | enum
                      | member
                      | message
                      | field
                      | oneof.

-type proto_type_name() :: string().

-type proto_type() :: {service, proto_type_name()}
                    | {enum, proto_type_name()}
                    | {message, proto_type_name()}
                    | proto_type_name().

-type expr_value() :: string() | proto_type().
-type expr_values() :: list(expr_value()).

-type expr() :: {is, expr_value()}
              | {is_not, expr_value()}
              | {is_any_of, expr_values()}
              | {is_none_of, expr_values()}
              | {has_prefix, string()}
              | {has_suffix, string()}.

-type attribute() :: package
                   | name
                   | parent_service_name
                   | parent_message_name
                   | parent_enum_name
                   | parent_oneof_name
                   | type
                   | type_name
                   | input_type
                   | intput_type_name
                   | output_type
                   | output_type_name.

-type test() :: {attribute(), expr()}.
-type tests() :: list(test()).

-type message() :: string().

-type rule() :: {message(),
                 proto_object(),
                 tests(),
                 tests(),
                 message()}.
-type rules() :: list(rule()).
