
-record(sub_detail, {
		ordinal :: integer(),
		reason :: string(),
		description :: string()
		}).

-record(detail, {
		notes :: string(),
		detail :: #sub_detail{}
		}).

-record(master, {
		id :: integer(),
		name :: string(),
		details :: list(#detail{})
		}).
