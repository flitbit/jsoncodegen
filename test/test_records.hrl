
-record(sub_detail, {
		ordinal :: integer(),
		reason :: string(),
		description :: string(),
		date :: calendar:datetime()
		}).

-record(detail, {
		notes :: string(),
		detail :: #sub_detail{},
		timestamp :: etz:iso_time()
		}).

-record(master, {
		id :: integer(),
		name :: string(),
		details :: list(#detail{})
		}).
