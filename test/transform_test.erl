-module(transform_test).

-compile({parse_transform, json_transform}).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-include("test/test_records.hrl").

to_json(_) -> pp_generated.
from_json(_,_) -> pp_generated.

to_ejson(_) -> pp_generated.
from_ejson(_,_) -> pp_generated.

to_ejson_test_() ->
	{
		foreach,
		fun setup/0,
		fun cleanup/1,

		[ {"can transform master",
				fun() ->
						Expect = #master{id=random:uniform(99999), name="This is the test master."},
						Ejson = to_ejson(Expect),

						Revived = from_ejson(master, Ejson),
						?assertEqual(Expect, Revived)
				end
				},
			{"can transform term when nested",
				fun() ->

						%% Set the timezone to EST so ISO formatted datetimes are
						%% predictable.
						ok = etz:use_timezone({'-',4,0}),

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
											description = "another detail / ordinal 2",
											date = {{1999,12,31},{11,59,59}}
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
																{<<"description">>,<<"another detail / ordinal 2">>},
																{<<"date">>,<<"1999-12-31T11:59:59-04">>}
																]}}]}]}]},
						Ejson = to_ejson(Expect),

						ok=file:write_file(
								filename:join("./", "transform_test.term"),
								io_lib:fwrite("~p\n", [Ejson])),

						?assertEqual(ExpectEjson, Ejson),
						Revived = from_ejson(master, Ejson),
						?assertEqual(Expect, Revived)
				end
				}
			]}.

setup() ->
	{ok, Pid} = etz:start_link(),
	Pid.

cleanup(Pid) ->
	MRef = erlang:monitor(process, Pid),
	gen_server:call(Pid, stop),
	receive {'DOWN', MRef, _, _, _} -> ok end.
