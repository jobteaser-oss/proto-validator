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

-export([collect_objects_data/1,
         evaluate_rule/2, evaluate_predicate/2, evaluate_expression/2,
         validate_rules/1, validate_rule/1,
         validate_predicates/1, validate_predicate/1]).

-export_type([rules/0, test_failure/0]).

-type object() :: message
                | field
                | service
                | rpc.

-type value() :: string() | proto_validator_catalog:type().
-type values() :: list(value()).

-type expression() :: {is, value()}
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
-type objects_data() :: #{object() => list(object_data())}.

-type predicate() :: {attribute(), expression()}.
-type predicates() :: list(predicate()).

-type message() :: string().

-type rule() :: {message(),
                 object(),
                 predicates(),
                 predicates()}.
-type rules() :: list(rule()).

-type test_failure() :: {test_failure, predicate(), {object(), object_data()},
                         message()}.

-spec collect_objects_data(Catalog) -> objects_data() when
    Catalog :: proto_validator_catalog:catalog().
collect_objects_data(#{messages := Messages,
                       services := Services}) ->
  #{message =>
      lists:map(fun collect_message_data/1, Messages),
    field =>
      lists:flatten(lists:map(fun collect_message_fields_data/1, Messages)),
    service =>
      lists:map(fun collect_service_data/1, Services),
    rpc =>
      lists:flatten(lists:map(fun collect_service_rpcs_data/1, Services))}.

-spec collect_message_data(proto_validator_catalog:message()) -> object_data().
collect_message_data({Name, _}) ->
  #{name => Name}.

-spec collect_message_fields_data(proto_validator_catalog:message()) ->
        list(object_data()).
collect_message_fields_data({MessageName, Fields}) ->
  ProcessField = fun F(Field) ->
                     case Field of
                       {field, Name, Type} ->
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

-spec matching_object_data(predicates(), list(object_data())) ->
        list(object_data()).
matching_object_data(Predicates, DataList) ->
  lists:filter(fun (Data) ->
                   lists:all(fun (Predicate) ->
                                 evaluate_predicate(Predicate, Data)
                             end, Predicates)
               end, DataList).

-spec evaluate_rule(rule(), objects_data()) -> ok | {error, term()}.
evaluate_rule({Msg, Object, Conditions, Tests}, ObjectsData) ->
  %% Find the list of data sets for this object
  DataList = maps:get(Object, ObjectsData, []),
  %% Only keep data sets which match all conditions
  MatchingDataList = matching_object_data(Conditions, DataList),
  %% Evaluate all tests for each object data set and collect errors
  Cases = [{Test, Data} || Test <- Tests, Data <- MatchingDataList],
  Results = lists:map(
             fun ({Test, Data}) ->
                 case evaluate_predicate(Test, Data) of
                   true ->
                     ok;
                   false ->
                     {error, {test_failure, Test, {Object, Data}, Msg}}
                 end
             end, Cases),
  proto_validator_utils:aggregate_results(Results).

-spec evaluate_predicate(predicate(), object_data()) -> boolean().
evaluate_predicate({Attribute, Expression}, Data) ->
  case maps:get(Attribute, Data, undefined) of
    undefined ->
      false;
    Value ->
      evaluate_expression(Expression, Value)
  end.

-spec evaluate_expression(expression(), value()) -> boolean().
evaluate_expression({is, Value}, ObjValue) ->
  value_equal(Value, ObjValue);
evaluate_expression({is_not, Value}, ObjValue) ->
  not(evaluate_expression({is, Value}, ObjValue));
evaluate_expression({is_any_of, Values}, ObjValue) ->
  lists:any(fun (Value) ->
                evaluate_expression({is, Value}, ObjValue)
            end, Values);
evaluate_expression({is_none_of, Values}, ObjValue) ->
  lists:all(fun (Value) ->
                evaluate_expression({is_not, Value}, ObjValue)
            end, Values);
evaluate_expression({has_prefix, Value}, ObjValue) ->
  lists:prefix(Value, ObjValue);
evaluate_expression({has_suffix, Value}, ObjValue) ->
  lists:suffix(Value, ObjValue).

-spec value_equal(value(), value()) -> boolean().
value_equal(V1, V2) when is_list(V1) andalso is_list(V2) ->
  V1 == V2;
value_equal(V1, V2) when is_atom(V1) andalso is_atom(V2) ->
  V1 == V2;
value_equal(V1, V2) when is_tuple(V1) andalso is_tuple(V2) ->
  proto_validator_catalog:type_equal(V1, V2);
value_equal(_, _) ->
  false.

-spec validate_rules(rules()) -> ok | no_return().
validate_rules(Rules) ->
  lists:foreach(fun validate_rule/1, Rules),
  ok.

-spec validate_rule(rule()) -> ok | no_return().
validate_rule({Message, Object, Conditions, Tests}) ->
  try
    io_lib:printable_unicode_list(Message)
      orelse error({validation_error, invalid_message, Message}),
    lists:member(Object, [message, field, service, rpc])
      orelse error({validation_error, invalid_object, Object}),
    validate_predicates(Conditions),
    validate_predicates(Tests)
  catch error:{validation_error, Error} ->
      error({validation_error, {invalid_rule, Message}, Error})
  end;
validate_rule(Rule) ->
  error({validation_error, invalid_rule, Rule}).

-spec validate_predicates(predicates()) -> ok | no_return().
validate_predicates(Predicates) ->
  lists:foreach(fun validate_predicate/1, Predicates),
  ok.

-spec validate_predicate(predicate()) -> ok | no_return().
validate_predicate({Attribute, Expression}) ->
  lists:member(Attribute, [name,
                           parent_service_name,
                           parent_message_name,
                           type,
                           input_type,
                           input_type_name,
                           output_type,
                           output_type_name])
    orelse error({validation_error, invalid_attribute, Attribute}),
  validate_expression(Expression);
validate_predicate(Predicate) ->
  error({validation_error, invalid_predicate, Predicate}).

-spec validate_expression(expression()) -> ok | no_return().
validate_expression({is, Value}) ->
  validate_value(Value);
validate_expression({is_not, Value}) ->
  validate_value(Value);
validate_expression({is_any_of, Values}) ->
  validate_values(Values);
validate_expression({is_none_of, Values}) ->
  validate_values(Values);
validate_expression({has_prefix, String}) ->
  io_lib:printable_unicode_list(String)
    orelse error({validation_error, invalid_prefix, String}),
  ok;
validate_expression({has_suffix, String}) ->
  io_lib:printable_unicode_list(String)
    orelse error({validation_error, invalid_suffix, String}),
  ok;
validate_expression(Expression) ->
  error({validation_error, invalid_expression, Expression}).

-spec validate_values(values()) -> ok | no_return().
validate_values(Values) ->
  lists:foreach(fun validate_value/1, Values),
  ok.

-spec validate_value(value()) -> ok | no_return().
validate_value(Value) when is_tuple(Value) orelse is_atom(Value) ->
  proto_validator_catalog:validate_type(Value);
validate_value(String) when is_list(String) ->
  io_lib:printable_unicode_list(String)
    orelse error({validation_error, invalid_value, String}),
  ok;
validate_value(Value) ->
  error({validation_error, invalid_value, Value}).
