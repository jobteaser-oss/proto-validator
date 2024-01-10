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

-module(proto_validator_catalog).

-export([build_catalog/1,
         type_equal/2,
         validate_type/1]).

-export_type([name/0,
              enum/0, enums/0, member/0, members/0,
              message/0, messages/0, field/0, fields/0,
              type/0,
              service/0, services/0, rpc/0, rpcs/0]).

-type catalog() :: #{package := string(),
                     enums := enums(),
                     messages := messages(),
                     services := services()}.

-type name() :: string().

-type enum() :: {name(), members()}.
-type enums() :: list(enum()).

-type member() :: name().
-type members() :: list(members()).

-type message() :: {name(), fields()}.
-type messages() :: list(message()).

-type field() :: {field, name(), type()}
               | {oneof, name(), fields()}.
-type fields() :: list(field()).

-type type() :: {service, name()}
              | {enum, name()}
              | {message, name()}
              | {map, type(), type()}
              | int32 | int64 | uint32 | uint64 | sint32 | sint64
              | fixed32 | fixed64 | sfixed32 | sfixed64
              | bool | float | double | string | bytes.

-type service() :: {name(), rpcs()}.
-type services() :: list(service()).

-type rpc() :: {name(),
                InputType :: type(), OutputType :: type(),
                InputStream :: boolean(), OutputStream :: boolean()}.
-type rpcs() :: list(rpc()).

-spec build_catalog(gpb_defs:defs()) -> catalog().
build_catalog(Defs) ->
  build_catalog(Defs, empty_catalog()).

-spec build_catalog(gpb_defs:defs(), catalog()) -> catalog().
build_catalog([{file, _} | _], Catalog = #{package := _}) ->
  %% When compiling a file, GPB output the concatenation of the definitions of
  %% the file and the definitions of all its dependencies. Definitions for
  %% each file start with a `{file, Name}' term. Therefore if we already
  %% identified a package, we can stop at the next file.
  Catalog;
build_catalog([{package, Name} | Defs], Catalog) ->
  Catalog2 = Catalog#{package => extract_name(Name)},
  build_catalog(Defs, Catalog2);
build_catalog([Def = {{enum, _}, _} | Defs], Catalog = #{enums := Enums}) ->
  Catalog2 = Catalog#{enums => [extract_enum(Def) | Enums]},
  build_catalog(Defs, Catalog2);
build_catalog([Def = {{msg, _}, _} | Defs], Catalog = #{messages := Messages}) ->
  Catalog2 = Catalog#{messages => [extract_message(Def) | Messages]},
  build_catalog(Defs, Catalog2);
build_catalog([Def = {{service, _}, _} | Defs], Catalog = #{services := Services}) ->
  Catalog2 = Catalog#{services => [extract_service(Def) | Services]},
  build_catalog(Defs, Catalog2);
build_catalog([_ | Defs], Catalog) ->
  build_catalog(Defs, Catalog);
build_catalog([], Catalog) ->
  Catalog.

-spec empty_catalog() -> catalog().
empty_catalog() ->
  #{enums => [],
    messages => [],
    services => []}.

-spec extract_name(atom()) -> name().
extract_name(Name) when is_atom(Name) ->
  atom_to_list(Name);
extract_name(Name) ->
  error({invalid_name, Name}).

-spec extract_enum(gpb_defs:def()) -> enum().
extract_enum({{enum, Name}, Members}) ->
  Name2 = full_name_to_element_name(extract_name(Name)),
  {Name2, lists:map(fun extract_member/1, Members)}.

-spec extract_member({atom(), integer()}) -> member().
extract_member({Name, _Value}) ->
  extract_name(Name).

-spec extract_message(gpb_defs:def()) -> message().
extract_message({{msg, Name}, Fields}) ->
  Name2 = full_name_to_element_name(extract_name(Name)),
  {Name2, lists:map(fun extract_field/1, Fields)}.

-spec extract_field(term()) -> field().
extract_field({gpb_oneof, Name, _, Fields}) ->
  {oneof, extract_name(Name), lists:map(fun extract_field/1, Fields)};
extract_field({gpb_oneof, Name, Value, Fields, []}) ->
    extract_field({gpb_oneof, Name, Value, Fields});
extract_field({field, Name, _, _, Type, _Occurrence, _Opts}) ->
  {field, extract_name(Name), extract_type(Type)}.

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
  Name2 = full_name_to_element_name(extract_name(Name)),
  {Name2, lists:map(fun extract_rpc/1, RPCs)}.

-spec extract_rpc(term()) -> rpc().
extract_rpc({rpc, Name, InputType, OutputType, InputStream, OutputStream, _}) ->
  {extract_name(Name),
   {message, extract_name(InputType)},
   {message, extract_name(OutputType)},
   InputStream, OutputStream}.

-spec full_name_to_element_name(name()) -> name().
full_name_to_element_name(FullName) ->
  [_, Name] = string:split(FullName, ".", trailing),
  Name.

-spec type_equal(type(), type()) -> boolean().
type_equal(T1, T2) when T1 =:= T2 ->
  true;
type_equal(_, _) ->
  false.

-spec validate_type(type()) -> ok | no_return().
validate_type({service, Name}) ->
  io_lib:printable_unicode_list(Name)
    orelse error({validation_error, invalid_service_name, Name}),
  ok;
validate_type({enum, Name}) ->
  io_lib:printable_unicode_list(Name)
    orelse error({validation_error, invalid_enum_name, Name}),
  ok;
validate_type({message, Name}) ->
  io_lib:printable_unicode_list(Name)
    orelse error({validation_error, invalid_message_name, Name}),
  ok;
validate_type({map, KeyType, ValueType}) ->
  validate_type(KeyType),
  validate_type(ValueType);
validate_type(Type) when is_atom(Type) ->
  lists:member(Type, [int32, int64, uint32, uint64, sint32, sint64,
                     fixed32, fixed64, sfixed32, sfixed64,
                     bool, float, double, string, bytes])
    orelse error({validation_error, invalid_type, Type}),
  ok;
validate_type(Type) ->
  error({validation_error, invalid_type, Type}).
