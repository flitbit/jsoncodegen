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
%% @doc JSON Code Generation for Erlang records <---> JSON

-module(jsoncodegen).

-export([make/4]).
-export([take_records/1]).
-export([gen_to_json/1, gen_from_json/1]).
-export([gen_to_ejson/1, gen_from_ejson/1]).
-export([to_binary_string_literal/1]).

-include("deps/merl/include/merl.hrl").

-define(ESModule(Name), erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)])).
-define(ESFnA(N, A), erl_syntax:arity_qualifier(erl_syntax:atom(N), erl_syntax:integer(A))).
-define(ESExport(L), erl_syntax:attribute(erl_syntax:atom(export), [erl_syntax:list(L)])).
-define(ESInclude(F), erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(F)])).
-define(ESCall(F, A), erl_syntax:application(erl_syntax:atom(F), A)).
-define(ESCall(M, F, A), erl_syntax:application(erl_syntax:module_qualifier(erl_syntax:atom(M), erl_syntax:atom(F)), A)).

-record(field, {
		name :: atom(),
		type :: term(),
		default :: any()
		}).

-record(rec, {
		name :: atom(),
		fields :: list(#field{})
		}).

-spec make(Infiles, IncludePaths, Outpath, ModuleName) -> ok when
	Infiles :: [FileName],
	IncludePaths :: [string()],
	Outpath :: string(),
	ModuleName :: string(),
	FileName :: string().

make(Infiles, IncludePaths, Outpath, ModuleName) ->
	{ ok, Records, Includes } = take_records_from_input_files(Infiles, IncludePaths),
	Forms = gen_src(Records, Includes, ModuleName),
	ok=file:write_file(
			filename:join(Outpath, ModuleName ++ ".erl"),
			list_to_binary(erl_prettypr:format(Forms))).

take_records_from_input_files(Infiles, IncludePaths) ->
	take_records_from_input_files(Infiles, IncludePaths, { [], [] }).

take_records_from_input_files([], _, { Records, Includes }) ->
	{ok, lists:flatten(Records), lists:reverse(Includes)};
take_records_from_input_files([H|T], IncludePaths, { Records, Includes }) ->
	{ok, Tree} = epp:parse_file(H, IncludePaths, []),
	R = take_records(erl_syntax:form_list(Tree)),
	I = erl_syntax:revert(?ESInclude(H)),
	take_records_from_input_files(T, IncludePaths, {[R|Records], [I|Includes]}).

gen_src(Records, Includes, ModuleName) ->
	ModForm = erl_syntax:revert(?ESModule(ModuleName)),
	Export1Form = erl_syntax:revert(?ESExport([?ESFnA(to_json,1), ?ESFnA(from_json,2)])),
	Export2Form = erl_syntax:revert(?ESExport([?ESFnA(to_ejson,1), ?ESFnA(from_ejson,2)])),
	erl_syntax:form_list(lists:flatten([ModForm, Export1Form, Export2Form,
				Includes,
				gen_to_json(Records),
				gen_from_json(Records),
				gen_to_ejson(Records),
				gen_from_ejson(Records)
				])).

gen_to_json(Records) ->
	gen_to_json(Records, []).

gen_to_json([], Clauses) ->
	Form = erl_syntax:function(
			erl_syntax:atom(to_json),
			lists:reverse(Clauses)),
	erl_syntax:revert(Form);
gen_to_json([H|T], Clauses) ->
	RVar = erl_syntax:variable(
			latin1_title_case(atom_to_list(H#rec.name))
			),
	%
	% to_json(@Entity) when is_record(@Entity, @record) ->
	%     jiffy:encode(to_ejson(@Entity));
	%
	Clause = erl_syntax:clause([RVar],
			gen_guard_var_is_record(RVar, H#rec.name),
			[
				?ESCall(jiffy, encode,[?ESCall(to_ejson, [RVar])])
				]),
	gen_to_json(T, [Clause|Clauses]).

gen_from_json(Records) ->
	gen_from_json(Records, []).

gen_from_json([], Clauses) ->
	Form = erl_syntax:function(
			erl_syntax:atom(from_json),
			lists:reverse(Clauses)),
	erl_syntax:revert(Form);
gen_from_json([H|T], Clauses) ->
	Json = erl_syntax:variable("Json"),
	RAtom = erl_syntax:atom(H#rec.name),
	Clause = erl_syntax:clause([RAtom, Json], none,
			[
				?ESCall(from_ejson, [RAtom, ?ESCall(jiffy, decode, [Json])])
				]),
	gen_from_json(T, [Clause|Clauses]).

gen_to_ejson(Records) ->
	gen_to_ejson(Records, []).


gen_to_ejson([H|T], Clauses) ->
	Rec = H#rec.name,
	R = erl_syntax:variable("R"),
	AddPropertiesAst = encode_as_ejson_props(H),
	Clause = erl_syntax:clause([R], [?Q("is_record(R, '@Rec@')")],
			[?Q(" { _@AddPropertiesAst }")]),
	gen_to_ejson(T, [Clause|Clauses]);
gen_to_ejson([], Clauses) ->
	Form = erl_syntax:function(
			erl_syntax:atom(to_ejson),
			lists:reverse(Clauses)),
	erl_syntax:revert(Form).

encode_as_ejson_props(Rec) when is_record(Rec, rec) ->
	encode_as_ejson_props(Rec#rec.name, lists:reverse(Rec#rec.fields), erl_syntax:list([])).

encode_as_ejson_props(Rec, [H|T], Inner) when is_atom(Rec), is_record(H, field) ->
	encode_as_ejson_props(Rec, T, encode_ejson_prop_from_field(Rec, H, Inner));
encode_as_ejson_props(_, [], Inner) ->
	Inner.

encode_ejson_prop_from_field(R, #field{name=FieldName,type=[undefined,{list, [{record,[_]}]}]}, Inner)
	when is_atom(R) ->
	?Q("ejson:add_property('@FieldName@', R#'@R@'.'@FieldName@', fun(Them) -> lists:map(fun to_ejson/1, Them) end, _@Inner) ");
encode_ejson_prop_from_field(R, #field{name=FieldName,type=[undefined,{list, _}]}, Inner)
	when is_atom(R) ->
	?Q("ejson:add_property('@FieldName@', R#'@R@'.'@FieldName@', _@Inner) ");
encode_ejson_prop_from_field(R, #field{name=FieldName,type=[undefined,{record,[_]}]}, Inner)
	when is_atom(R) ->
	?Q("ejson:add_property('@FieldName@', R#'@R@'.'@FieldName@', fun(It) -> to_ejson(It) end, _@Inner) ");
encode_ejson_prop_from_field(R, #field{name=FieldName,type=[undefined,{string, _}]}, Inner)
	when is_atom(R) ->
	?Q("ejson:add_string('@FieldName@', R#'@R@'.'@FieldName@', _@Inner) ");
encode_ejson_prop_from_field(R, #field{name=FieldName,type=[undefined,{calendar, datetime, _}]}, Inner)
	when is_atom(R) ->
	?Q("ejson:add_datetime('@FieldName@', R#'@R@'.'@FieldName@', _@Inner) ");
encode_ejson_prop_from_field(R, #field{name=FieldName,type=[undefined,{etz, iso_time, _}]}, Inner)
	when is_atom(R) ->
	?Q("ejson:add_datetime('@FieldName@', R#'@R@'.'@FieldName@', _@Inner) ");
encode_ejson_prop_from_field(R, #field{name=FieldName}, Inner)
	when is_atom(R) ->
	?Q("ejson:add_property('@FieldName@', R#'@R@'.'@FieldName@', _@Inner) ").

gen_from_ejson(Records) ->
	gen_from_ejson(Records, []).

gen_from_ejson([H|T], Clauses) ->
	Props = erl_syntax:variable("Props"),
	EJson = ?Q("{Props}"),
	RAtom = erl_syntax:atom(H#rec.name),
	DecodeAst = decode_ejson_ast(H, Props),
	Undef = erl_syntax:atom(undefined),
	UndefClause = erl_syntax:clause([RAtom, Undef], none, [Undef]),
	Clause = erl_syntax:clause([RAtom, EJson], none, [
		DecodeAst
		]),
	gen_from_ejson(T, [Clause,UndefClause|Clauses]);
gen_from_ejson([], Clauses) ->
	Form = erl_syntax:function(
			erl_syntax:atom(from_ejson),
			lists:reverse(Clauses)),
	erl_syntax:revert(Form).

decode_ejson_ast(Rec, PropsVar) ->
	erl_syntax:record_expr(erl_syntax:atom(Rec#rec.name),
		lists:map(fun(F) -> decode_ejson_field_ast(F, PropsVar) end,
			Rec#rec.fields)
		).

decode_ejson_field_ast(F, PropsVar) ->
	erl_syntax:record_field(erl_syntax:atom(F#field.name), gen_decode_ejson_field_value(F, PropsVar)).

gen_decode_ejson_field_value(#field{name=N, type=[undefined,{list, [{record,[R]}]}]}, PropsVar) ->
	RecordName = erl_syntax:atom(R),
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_records(_@FieldName, _@RecordName, fun from_ejson/2, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N, type=[undefined,{record,[R]}]}, PropsVar) ->
	RecordName = erl_syntax:atom(R),
	FieldName = erl_syntax:atom(N),
	?Q("from_ejson(_@RecordName, ejson:get_prop(_@FieldName, _@PropsVar))");
gen_decode_ejson_field_value(#field{name=N, type=[undefined,{atom,[]}]}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_atom(_@FieldName, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N, type=[undefined,{string,[]}]}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_string(_@FieldName, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N, type=[undefined,{integer,[]}]}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_integer(_@FieldName, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N, type=[undefined,{calendar, datetime, []}]}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_datetime(_@FieldName, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N, type=[{atom,[]}]}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_atom(_@FieldName, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N, type=[{string,[]}]}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_string(_@FieldName, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N, type=[{integer,[]}]}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_integer(_@FieldName, _@PropsVar)");
gen_decode_ejson_field_value(#field{name=N}, PropsVar) ->
	FieldName = erl_syntax:atom(N),
	?Q("ejson:get_prop(_@FieldName, _@PropsVar)").

to_binary_string_literal(S) when is_atom(S) ->
	to_binary_string_literal(atom_to_list(S));
to_binary_string_literal(S) when is_list(S) ->
	erl_syntax:binary(lists:map(fun(Ch) ->
		erl_syntax:binary_field(erl_syntax:integer(Ch))
	end, S)).

gen_guard_var_is_record(Var, RecType) ->
	?ESCall(is_record, [Var, erl_syntax:atom(RecType)]).

latin1_title_case(A) when is_atom(A) ->
	latin1_title_case(atom_to_list(A));
latin1_title_case([H|T]) when H >= $a, H =< $z ->
	[H + ($A - $a) | T];
latin1_title_case(Wierd) ->
	Wierd.

take_records(Tree) ->
	[S] = erl_syntax:subtrees(Tree),
	RAsts = [erl_syntax:attribute_arguments(R)
			|| R <- S, erl_syntax:type(R) =:= attribute,
				erl_syntax:atom_value(erl_syntax:attribute_name(R)) =:= record],
	Recs = [to_rec(Rn, erl_syntax:tuple_elements(Rf)) || [Rn, Rf] <- RAsts],
	RecordTypes = to_record_type_info([hd(erl_syntax:attribute_arguments(T))
			|| T <- S, erl_syntax:type(T) =:= attribute,
				erl_syntax:atom_value(erl_syntax:attribute_name(T)) =:= type,
				is_record_type_info_ast(hd(erl_syntax:attribute_arguments(T)))]),
	Dict = lists:foldl(
			fun(R, D) ->
					dict:store(R#rec.name, R, D)
			end,
			dict:new(),
			Recs
			),
	[V || {_, V} <- dict:to_list(annotate_records_with_type_info(RecordTypes, Dict))].

is_record_type_info_ast(It) ->
	case erl_syntax:tuple_elements(It) of
		[PossibleRecordNameTuple, _, _] ->
			case erl_syntax:type(PossibleRecordNameTuple) of
				tuple ->
					case erl_syntax:tuple_elements(PossibleRecordNameTuple) of
						[PossibleRecordAtom, PossibleNameAtom] ->
							erl_syntax:is_atom(PossibleRecordAtom, record)
								andalso erl_syntax:type(PossibleNameAtom) =:= atom;
						_ -> false
					end;
				_ -> false
			end;
		_ -> false
	end.

to_rec(Rec, Fields) ->
	#rec{name=erl_syntax:atom_value(Rec), fields = [to_field(F) || F <- Fields]}.

to_field(Field) ->
	case erl_syntax:record_field_value(Field) of
		none ->
			Type = none,
			Value = none;
		V ->
			Type = erl_syntax:type(V),
			Value = erl_syntax:concrete(V)
	end,
	#field{name=erl_syntax:atom_value(erl_syntax:record_field_name(Field)), type=Type, default=Value}.

to_record_type_info([H|T]) ->
	to_record_type_info([H|T], []).

to_record_type_info([H|T], Acc) ->
	[NameTuple, FieldsTuple, _] = erl_syntax:tuple_elements(H),
	[_, NameAtom] = erl_syntax:tuple_elements(NameTuple),
	Name = erl_syntax:atom_value(NameAtom),
	FieldInfo = to_field_type_info(erl_syntax:list_elements(FieldsTuple)),
	R = #rec{name=Name, fields=FieldInfo},
	to_record_type_info(T, [R|Acc]);
to_record_type_info([], Acc) ->
	Acc.

to_field_type_info(F) ->
	to_field_type_info(F, []).

to_field_type_info([H|T], Acc) ->
	% There doesn't seem to be erl_syntax funs for dealing with these
	% so I'm just going to match out the parts I need.
	% TODO: figure out "proper" erl_syntax for this.
	{Name, Type, Default} = field_type_detail(erl_syntax:concrete(H)),
	to_field_type_info(T, [#field{name=Name, type=Type, default=Default} | Acc]);
to_field_type_info([], Acc) ->
	lists:reverse(Acc).

field_type_detail({record_field, _, Name}) ->
	{erl_syntax:atom_value(Name), none, undefined};
field_type_detail({record_field, _, Name, Default}) ->
	{erl_syntax:atom_value(Name), none, Default};
field_type_detail({typed_record_field, {record_field, _, Name}, Type}) ->
	{erl_syntax:atom_value(Name), take_types(Type), undefined};
field_type_detail({typed_record_field, {record_field, _, Name, Default}, Type}) ->
	{erl_syntax:atom_value(Name), take_types(Type), Default}.

take_types({type, _, union, TypeList}) ->
	take_types(TypeList, []);
take_types(T) ->
	take_types([T], []).

take_types([{type, _, union, U}|Rest], Acc) ->
	% TODO: experiment with records to determine if this is even possible.
	% none of my records result in this structure.
	R = take_types(U, []),
	take_types(Rest, [R|Acc]);
take_types([{type, _, Type, []}|Rest], Acc) ->
	take_types(Rest, [{Type, []}|Acc]);
take_types([{type, _, Type, Args}|Rest], Acc) ->
	A = take_types(Args, []),
	take_types(Rest, [{Type, A}|Acc]);
take_types([{remote_type, _, [M,T,[]]}|Rest], Acc) ->
	take_types(Rest, [{erl_syntax:atom_value(M), erl_syntax:atom_value(T),[]}|Acc]);
take_types([{remote_type, _, [M,T, Args]}|Rest], Acc) ->
	A = take_types(Args, []),
	take_types(Rest, [{erl_syntax:atom_value(M), erl_syntax:atom_value(T), A}|Acc]);
take_types([{atom, _, Atom}|Rest], Acc) ->
	take_types(Rest, [Atom|Acc]);
take_types([], Acc) ->
	lists:reverse(Acc).

annotate_records_with_type_info([#rec{name=Name}=R|T], Dict) ->
	annotate_records_with_type_info(T, dict:store(Name, R, Dict));
annotate_records_with_type_info([], Dict) ->
	Dict.
