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

-export([add_property/3, add_property/4, get_atom/2, get_prop/2, get_string/2, get_integer/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

add_property(_, undefined, Props) ->
	Props;
add_property(Name, Value, Props) when is_atom(Name) ->
	add_property(atom_to_list(Name), Value, Props);
add_property(Name, Value, Props) when is_list(Name) ->
	add_property(list_to_binary(Name), Value, Props);
add_property(Name, Value, Props) when is_list(Value) ->
	[{Name, unicode:characters_to_binary(Value)}|Props];
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

get_string(_, []) ->
	undefined;
get_string(N, L) when is_atom(N) ->
	get_string(atom_to_list(N), L);
get_string(N, L) when is_list(N) ->
	get_string(list_to_binary(N), L);
get_string(N, L) when is_binary(N), is_list(L) ->
	case proplists:get_value(N, L) of
		undefined -> undefined;
		B when is_binary(B) ->
			binary_to_list(B);
		V -> V
	end.

get_atom(_, []) ->
	undefined;
get_atom(N, L) when is_atom(N) ->
	get_atom(atom_to_list(N), L);
get_atom(N, L) when is_list(N) ->
	get_atom(list_to_binary(N), L);
get_atom(N, L) when is_binary(N), is_list(L) ->
	case proplists:get_value(N, L) of
		undefined -> undefined;
		B when is_binary(B) ->
			list_to_existing_atom(binary_to_list(B));
		V -> V
	end.


get_integer(_, []) ->
	undefined;
get_integer(N, L) when is_atom(N) ->
	get_integer(atom_to_list(N), L);
get_integer(N, L) when is_list(N) ->
	get_integer(list_to_binary(N), L);
get_integer(N, L) when is_binary(N), is_list(L) ->
	case proplists:get_value(N, L) of
		undefined -> undefined;
		B when is_binary(B) ->
			list_to_integer(binary_to_list(B));
		I when is_list(I) ->
			list_to_integer(I);
		V -> V
	end.

get_prop(_, []) ->
	undefined;
get_prop(N, L) when is_atom(N) ->
	get_prop(atom_to_list(N), L);
get_prop(N, L) when is_list(N) ->
	get_prop(list_to_binary(N), L);
get_prop(N, L) when is_binary(N), is_list(L) ->
	proplists:get_value(N, L).

%% TESTS ----------------------------------------------------------------

-ifdef(TEST).

add_property_is_benign_when_value_is_undefined_test() ->
	?assertEqual([], add_property(unimportant, undefined, [])).

add_property_adds_when_name_is_atom() ->
	?assertEqual([{"atom", "Itsa atom."}], add_property(atom, "Itsa atom.", [])).

add_property_adds_multiple_test() ->
	?assertEqual([
			{<<"name">>, <<"Phillip">>},
			{<<"age">>, <<"old enough">>}
			], add_property(name, "Phillip",
			add_property(age, "old enough", [])
			)).

add_property_with_transform_test() ->
	F = fun({Name, Occ}) ->
			L = add_property(name, Name, []),
			{lists:reverse(add_property(occupation, Occ, L))}
	end,
	Who = {"Wilbur", "Woodsman"},
	?assertEqual([{<<"who">>, {[{<<"name">>, <<"Wilbur">>},{<<"occupation">>,<<"Woodsman">>}]}}],
		add_property("who", Who, F, [])).

-endif.
