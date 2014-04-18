-module(ejson_test).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

ejson_test_() ->
	[ {"add_property: {atom(), undefined} does not get added",
			fun() ->
					?assertEqual([], ejson:add_property(unimportant, undefined, []))
			end},
		{"add_property: {atom(), string()} added as {string(), string()}",
			fun() ->
					?assertEqual([{<<"atom">>, <<"Itsa atom.">>}], ejson:add_property(atom, "Itsa atom.", []))
			end},
		{"add_property: {string(), string()} added as {string(), string()}",
			fun() ->
					?assertEqual([{<<"string">>, <<"Itsa string.">>}], ejson:add_property(<<"string">>, "Itsa string.", []))
			end},
		{"add_property: {atom(), integer()} added as {string(), integer()}",
			fun() ->
					?assertEqual([{<<"itsa_integer">>, 987}], ejson:add_property(itsa_integer, 987, []))
			end},
		{"add_property: multiple properties are added",
			fun() ->
					?assertEqual([
							{<<"name">>, <<"Phillip">>},
							{<<"age">>, <<"old enough">>}
							], ejson:add_property(name, "Phillip",
							ejson:add_property(age, "old enough", [])
							))
			end},
		{"add_property: transforms complex property using specified fun",
			fun() ->
					F = fun({Name, Occ}) ->
							L = ejson:add_property(name, Name, []),
							{lists:reverse(ejson:add_property(occupation, Occ, L))}
					end,
					Who = {"Wilbur", "Woodsman"},
					?assertEqual([{<<"who">>, {[{<<"name">>, <<"Wilbur">>},{<<"occupation">>,<<"Woodsman">>}]}}],
						ejson:add_property("who", Who, F, []))
			end},

		{"get_string: gets undefined when property list empty",
				fun() ->
					?assert(undefined =:= ejson:get_string(<<"not_present">>, []))
			end},
		{"get_string: gets string when present as string",
				fun() ->
					?assertEqual("hiya", ejson:get_string(<<"string_present">>, [{<<"string_present">>, "hiya"}]))
			end},
		{"get_string: gets string when present as string literal",
				fun() ->
					?assertEqual("hiya", ejson:get_string(<<"string_present">>, [{<<"string_present">>, <<"hiya">>}]))
			end},
		{"get_string: errors when present as integer",
				fun() ->
					?assertEqual({error, {unexpected, 134}}, ejson:get_string(<<"string_present">>, [{<<"string_present">>, 134}]))
			end},
		{"get_string: errors when present as tuple",
				fun() ->
					?assertEqual({error, {unexpected, {this, {is, unexpected}}}}, ejson:get_string(<<"unexpected_tuple">>, [{<<"unexpected_tuple">>, {this, {is, unexpected}}}]))
			end},

		{"get_atom: gets undefined when property list empty",
				fun() ->
					?assert(undefined =:= ejson:get_atom(<<"not_present">>, []))
			end},
		{"get_atom: gets atom when present as string",
				fun() ->
					?assertEqual(itsa_atom, ejson:get_atom(<<"string_present">>, [{<<"string_present">>, "itsa_atom"}]))
			end},
		{"get_atom: gets atom when present as atom",
				fun() ->
					?assertEqual(itsa_atom, ejson:get_atom(<<"string_present">>, [{<<"string_present">>, itsa_atom}]))
			end},
		{"get_atom: gets atom when present as string literal",
				fun() ->
					?assertEqual(itsa_atom, ejson:get_atom(<<"string_present">>, [{<<"string_present">>, <<"itsa_atom">>}]))
			end},
		{"get_atom: errors when present as integer",
				fun() ->
					?assertEqual({error, {unexpected, 134}}, ejson:get_atom(<<"string_present">>, [{<<"string_present">>, 134}]))
			end},
		{"get_atom: errors when present as tuple",
				fun() ->
					?assertEqual({error, {unexpected, {this, {is, unexpected}}}}, ejson:get_atom(<<"unexpected_tuple">>, [{<<"unexpected_tuple">>, {this, {is, unexpected}}}]))
			end},


		{"get_integer: gets undefined when property list empty",
				fun() ->
					?assert(undefined =:= ejson:get_integer(<<"not_present">>, []))
			end},
		{"get_integer: errors when present as integer",
				fun() ->
					?assertEqual(134, ejson:get_integer(<<"int_present">>, [{<<"int_present">>, 134}]))
			end},
		{"get_integer: gets integer when present as string",
				fun() ->
					?assertEqual(1349, ejson:get_integer(<<"int_present">>, [{<<"int_present">>, "1349"}]))
			end},
		{"get_integer: gets integer when present as string literal",
				fun() ->
					?assertEqual(45692, ejson:get_integer(<<"int_present">>, [{<<"int_present">>, <<"45692">>}]))
			end},
		{"get_integer: errors when present as atom",
				fun() ->
					?assertEqual({error, {unexpected, itsa_atom}}, ejson:get_integer(<<"atom_present">>, [{<<"atom_present">>, itsa_atom}]))
			end},
		{"get_integer: errors when present as tuple",
				fun() ->
					?assertEqual({error, {unexpected, {this, {is, unexpected}}}}, ejson:get_integer(<<"unexpected_tuple">>, [{<<"unexpected_tuple">>, {this, {is, unexpected}}}]))
			end}


		].
