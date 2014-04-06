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

-module(json_transform).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
	Tree = erl_syntax:form_list(Forms),
	ModifiedTree = postorder(
			fun(Node, Orig) ->
					case erl_syntax:type(Node) of
						function ->
							case erl_syntax:atom_value(erl_syntax:function_name(Node)) of
								to_json ->
									possibly_gen_to_json(Node, Orig);
								from_json ->
									possibly_gen_from_json(Node, Orig);
								to_ejson ->
									possibly_gen_to_ejson(Node, Orig);
								from_ejson ->
									possibly_gen_from_ejson(Node, Orig);
								_ ->
									Node
							end;
						_ ->
							Node
					end
			end,
			Tree,
			Tree),
	erl_syntax:revert_forms(ModifiedTree).

postorder(F, Tree, Orig) ->
	F(case erl_syntax:subtrees(Tree) of
			[] -> Tree;
			List -> erl_syntax:update_tree(Tree,
					[[postorder(F, Subtree, Orig)
							|| Subtree <- Group]
						|| Group <- List])
		end, Orig).

is_pp_generated(Fn, Arity) ->
	% has arity of Arity
	case erl_syntax:function_arity(Fn) of
		Arity ->
			Clauses = erl_syntax:function_clauses(Fn),
			% has only 1 clause
			case length(Clauses) of
				1 ->
					Clause = hd(Clauses),
					% the clause has no guards
					case erl_syntax:clause_guard(Clause) of
						none ->
							Body = erl_syntax:clause_body(Clause),
							% the body has only one statement
							% 	and the statement is the atom 'pp_generated'
							length(Body) =:= 1
							andalso erl_syntax:is_atom(hd(Body), pp_generated);
						_ -> false
					end;
				_ -> false
			end;
		_ -> false
	end.


possibly_gen_from_json(Node, Orig) ->
	case is_pp_generated(Node,2) of
		true ->
			Records = jsoncodegen:take_records(Orig),
			jsoncodegen:gen_from_json(Records);
		_ -> Node
	end.

possibly_gen_to_json(Node, Orig) ->
	case is_pp_generated(Node,1) of
		true ->
			Records = jsoncodegen:take_records(Orig),
			jsoncodegen:gen_to_json(Records);
		_ -> Node
	end.

possibly_gen_from_ejson(Node, Orig) ->
	case is_pp_generated(Node,2) of
		true ->
			Records = jsoncodegen:take_records(Orig),
			jsoncodegen:gen_from_ejson(Records);
		_ -> Node
	end.

possibly_gen_to_ejson(Node, Orig) ->
	case is_pp_generated(Node,1) of
		true ->
			Records = jsoncodegen:take_records(Orig),
			jsoncodegen:gen_to_ejson(Records);
		_ -> Node
	end.
