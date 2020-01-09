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

-module(proto_validator_proto).

-export([messages/1, services/1]).

-export_type([name/0, message/0, messages/0, field/0, fields/0, occurrence/0,
              type/0, service/0, services/0, rpc/0, rpcs/0]).

-type name() :: unicode:chardata().

-type message() :: {Package :: name(), name(), fields()}.
-type messages() :: list(message()).

-type field() :: {field, name(), type(), occurrence()}
               | {oneof, name(), fields()}.
-type fields() :: list(field()).

-type occurrence() :: required | optional | repeated.

-type type() :: {service, name()}
              | {enum, name()}
              | {message, name()}
              | {map, type(), type()}
              | int32 | int64 | uint32 | uint64 | sint32 | sint64
              | fixed32 | fixed64 | sfixed32 | sfixed64
              | bool | float | double | string | bytes.

-type service() :: {Package :: name(), rpcs()}.
-type services() :: list(service()).

-type rpc() :: {name(),
                InputType :: type(), OutputType :: type(),
                InputStream :: boolean(), OutputStream :: boolean()}.
-type rpcs() :: list(rpc()).

-spec messages(gpb_defs:defs()) -> messages().
messages(Defs) ->
  lists:filtermap(fun (Def) ->
                      case Def of
                        {{msg, _}, _} ->
                          {true, extract_message(Def)};
                        _ ->
                          false
                      end
                  end, Defs).

-spec services(gpb_defs:defs()) -> services().
services(Defs) ->
  lists:filtermap(fun (Def) ->
                      case Def of
                        {{service, _}, _} ->
                          {true, extract_service(Def)};
                        _ ->
                          false
                      end
                  end, Defs).

-spec extract_name(atom()) -> name().
extract_name(Name) when is_atom(Name) ->
  atom_to_list(Name);
extract_name(Name) ->
  error({invalid_name, Name}).

-spec extract_message(gpb_defs:def()) -> message().
extract_message({{msg, Name}, Fields}) ->
  [Package, Name2] = split_full_name(extract_name(Name)),
  {Package, Name2, lists:map(fun extract_field/1, Fields)}.

-spec extract_field(gpb_defs:def()) -> field().
extract_field({gpb_oneof, Name, _, Fields}) ->
  {oneof, extract_name(Name), lists:map(fun extract_field/1, Fields)};
extract_field({field, Name, _, _, Type, Occurrence, _Opts}) ->
  {field, extract_name(Name), extract_type(Type), Occurrence}.

-spec extract_type(term()) -> type().
extract_type(Type) when is_atom(Type) ->
  Type;
extract_type({enum, Name}) ->
  {enum, extract_name(Name)};
extract_type({msg, Name}) ->
  {message, extract_name(Name)};
extract_type({map, Key, Value}) ->
  {map, extract_type(Key), extract_type(Value)}.

-spec extract_service(gpb_defs:def()) -> service().
extract_service({{service, Name}, RPCs}) ->
  [Package, Name2] = split_full_name(extract_name(Name)),
  {Package, Name2, lists:map(fun extract_rpc/1, RPCs)}.

-spec extract_rpc(gpb_defs:def()) -> rpc().
extract_rpc({rpc, Name, InputType, OutputType, InputStream, OutputStream, _}) ->
  {extract_name(Name), extract_type(InputType), extract_type(OutputType),
   InputStream, OutputStream}.

-spec split_full_name(name()) -> list(name()).
split_full_name(Name) ->
  string:split(Name, ".", trailing).
