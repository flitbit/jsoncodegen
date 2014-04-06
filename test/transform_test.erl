-module(transform_test).

-compile({parse_transform, json_transform}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-include("test/test_records.hrl").

to_json(_) -> pp_generated.
from_json(_,_) -> pp_generated.

to_ejson(_) -> pp_generated.
from_ejson(_,_) -> pp_generated.

%% TESTS ----------------------------------------------------------------

-ifdef(TEST).

to_ejson_test_() ->
	[ {"can transform master",
			fun() ->
					Expect = #master{id=random:uniform(99999), name="This is the test master."},
					Ejson = to_ejson(Expect),

					ok=file:write_file(
							filename:join("./", "debug.term"),
							io_lib:fwrite("~p\n", [Ejson])),

					Revived = from_ejson(master, Ejson),
					?assertEqual(Expect, Revived)
			end
			},
		{"can transform term when nested",
			fun() ->
					Expect = #master{
							id = 24896,
							name = "This-is-my-master",
							details = [
								#detail{
									notes = "this is the frist detail",
									detail = #sub_detail{
										ordinal = 1,
										reason = "because we wanted at least one sub-detail"
										}
									},
								#detail{
									notes = "this is another detail",
									detail = #sub_detail{
										ordinal = 2,
										reason = "something should be here",
										description = "another detail / ordinal 2"
										}
									}
								]
							},

					ExpectEjson = {[{<<"id">>,24896},
								{<<"name">>,<<"This-is-my-master">>},
								{<<"details">>,
									[{[{<<"notes">>,<<"this is the frist detail">>},
												{<<"detail">>,
													{[{<<"ordinal">>,1},
															{<<"reason">>,<<"because we wanted at least one sub-detail">>}]}}]},
										{[{<<"notes">>,<<"this is another detail">>},
												{<<"detail">>,
													{[{<<"ordinal">>,2},
															{<<"reason">>,<<"something should be here">>},
															{<<"description">>,<<"another detail / ordinal 2">>}]}}]}]}]},
					Ejson = to_ejson(Expect),

					?assertEqual(ExpectEjson, Ejson),
					Revived = from_ejson(master, Ejson),
					?assertEqual(Expect, Revived)
			end
			}
		].

-endif.
