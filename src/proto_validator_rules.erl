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

-export([collect_objects_data/1]).

-export_type([rules/0]).

-type object() :: message
                | field
                | service
                | rpc.

-type value() :: string() | proto_validator_catalog:type().
-type values() :: list(value()).

-type expr() :: {is, value()}
              | {is_not, value()}
              | {is_any_of, values()}
              | {is_none_of, values()}
              | {has_prefix, string()}
              | {has_suffix, string()}.

-type attribute() :: name
                   | parent_service_name
                   | parent_message_name
                   | type
                   | input_type
                   | input_type_name
                   | output_type
                   | output_type_name.

-type object_data() :: #{attribute() => value()}.
-type objects_data() :: #{messages := list(object_data()),
                          fields := list(object_data()),
                          services := list(object_data()),
                          rpcs := list(object_data())}.

-type predicate() :: {attribute(), expr()}.
-type predicates() :: list(predicate()).

-type message() :: string().

-type rule() :: {message(),
                 object(),
                 predicates(),
                 predicates()}.
-type rules() :: list(rule()).

-spec collect_objects_data(Catalog) -> objects_data() when
    Catalog :: proto_validator_catalog:catalog().
collect_objects_data(#{messages := Messages,
                       services := Services}) ->
  #{messages =>
      lists:map(fun collect_message_data/1, Messages),
    fields =>
      lists:flatten(lists:map(fun collect_message_fields_data/1, Messages)),
    services =>
      lists:map(fun collect_service_data/1, Services),
    rpcs =>
      lists:flatten(lists:map(fun collect_service_rpcs_data/1, Services))}.

-spec collect_message_data(proto_validator_catalog:message()) -> object_data().
collect_message_data({Name, _}) ->
  #{name => Name}.

-spec collect_message_fields_data(proto_validator_catalog:message()) ->
        list(object_data()).
collect_message_fields_data({MessageName, Fields}) ->
  ProcessField = fun F(Field) ->
                     case Field of
                       {field, Name, Type, _Occurrence} ->
                         #{name => Name,
                           parent_message_name => MessageName,
                           type => Type};
                       {oneof, _Name, OneofFields} ->
                         lists:map(F, OneofFields)
                     end
                 end,
  lists:flatten(lists:map(ProcessField, Fields)).

-spec collect_service_data(proto_validator_catalog:service()) -> object_data().
collect_service_data({Name, _}) ->
  #{name => Name}.

-spec collect_service_rpcs_data(proto_validator_catalog:service()) ->
        list(object_data()).
collect_service_rpcs_data({ServiceName, RPCs}) ->
  lists:map(fun (RPC) ->
                {Name,
                 InputType = {message, InputTypeName},
                 OutputType = {message, OutputTypeName},
                 _, _} = RPC,
                #{name => Name,
                  parent_service_name => ServiceName,
                  input_type => InputType,
                  input_type_name => InputTypeName,
                  output_type => OutputType,
                  output_type_name => OutputTypeName}
            end, RPCs).
