%% ---------------------------------------------------------------------
%% Licensed under the MIT License, (the "License");  See LICENSE.txt,
%% distributed with this file for information regarding your rights
%% related to the use of this file.
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author Phillip Clark <phillip@flitbit.com>
%% @copyright 2014 Phillip Clark

-module(ejson).

-export([
	add_property/3,
	add_property/4,
	add_string/3,
	add_datetime/3,
	get_atom/2,
	get_prop/2,
	get_string/2,
	get_integer/2,
	get_datetime/2,
	get_iso_time/2
	]).
-export([
	get_record/4,
	get_records/4
	]).

add_property(_, undefined, Props) ->
	Props;
add_property(Name, Value, Props) when is_atom(Name) ->
	add_property(atom_to_list(Name), Value, Props);
add_property(Name, Value, Props) when is_list(Name) ->
	add_property(list_to_binary(Name), Value, Props);
add_property(Name, Value, Props)
		when is_binary(Name), is_list(Value) ->
	add_property(Name, unicode:characters_to_binary(Value), Props);
add_property(Name, Value, Props) ->
	[{Name, Value} | Props].

add_property(_, undefined, _, Props) ->
	Props;
add_property(Name, Value, Trans, Props) when is_atom(Name) ->
	add_property(atom_to_list(Name), Value, Trans, Props);
add_property(Name, Value, Trans, Props) when is_list(Name) ->
	add_property(list_to_binary(Name), Value, Trans, Props);
add_property(Name, Value, Trans, Props) ->
	[{Name, Trans(Value)} | Props].

add_string(_, undefined, Props) ->
	Props;
add_string(Name, Value, Props) when is_atom(Name) ->
	add_string(atom_to_list(Name), Value, Props);
add_string(Name, Value, Props) when is_list(Name) ->
	add_string(list_to_binary(Name), Value, Props);
add_string(Name, Value, Props)
		when is_binary(Name), is_list(Value) ->
	add_string(Name, unicode:characters_to_binary(Value), Props);
add_string(Name, Value, Props)
		when is_binary(Name), is_binary(Value) ->
	[{Name, Value} | Props].

add_datetime(_, undefined, Props) ->
	Props;
add_datetime(Name, Value, Props) when is_atom(Name) ->
	add_datetime(atom_to_list(Name), Value, Props);
add_datetime(Name, Value, Props) when is_list(Name) ->
	add_datetime(list_to_binary(Name), Value, Props);
add_datetime(Name, Value, Props) when is_binary(Name) ->
	IsoFormatted = etz:iso_format(Value),
	[{Name, IsoFormatted} | Props].


get_prop(_, []) ->
	undefined;
get_prop(N, L) when is_atom(N) ->
	get_prop(atom_to_list(N), L);
get_prop(N, L) when is_list(N) ->
	get_prop(list_to_binary(N), L);
get_prop(N, L) when is_binary(N), is_list(L) ->
	proplists:get_value(N, L).

get_string(N, L) when is_list(L) ->
	case get_prop(N, L) of
		undefined -> undefined;
		B when is_binary(B) ->
			binary_to_list(B);
		V when is_list(V) -> V;
		E -> { error, {unexpected, E}}
	end.

get_datetime(N, L) when is_list(L) ->
	case get_string(N, L) of
		undefined -> undefined;
		V when is_list(V) ->
			case etz:iso_parse(V) of
				{ok, {DateTime,_,_}} -> DateTime;
				EE -> EE
			end;
		E -> { error, {unexpected, E}}
	end.

get_iso_time(N, L) when is_list(L) ->
	case get_string(N, L) of
		undefined -> undefined;
		V when is_list(V) ->
			case etz:iso_parse(V) of
				{ok, IsoTime} -> IsoTime;
				E -> E
			end;
		E -> { error, {unexpected, E}}
	end.

get_atom(N, L) when is_list(L) ->
	case get_prop(N, L) of
		undefined -> undefined;
		A when is_atom(A) -> A;
		B when is_binary(B) ->
			list_to_existing_atom(binary_to_list(B));
		V when is_list(V) ->
			list_to_existing_atom(V);
		E -> { error, {unexpected, E}}
	end.

get_integer(N, L) when is_list(L) ->
	case get_prop(N, L) of
		undefined -> undefined;
		B when is_binary(B) ->
			list_to_integer(binary_to_list(B));
		I when is_list(I) ->
			list_to_integer(I);
		V when is_integer(V) -> V;
		E -> { error, {unexpected, E}}
	end.

get_record(N, T, Transform, L) when is_atom(T), is_list(L) ->
	case get_prop(N, L) of
		undefined -> undefined;
		Ejson when is_tuple(Ejson) ->
			Transform(T, Ejson);
		Err ->
			{error, {unexpected, Err}}
	end.

get_records(N, T, Transform, L) when is_atom(T), is_list(L) ->
	case get_prop(N, L) of
		undefined -> undefined;
		Ejson when is_list(Ejson) ->
			lists:map(fun(It) -> Transform(T, It) end, Ejson);
		Err ->
			{error, {unexpected, Err}}
	end.

