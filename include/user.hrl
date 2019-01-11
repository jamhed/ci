-ifndef(CI_DB_USER).
-define(CI_DB_USER, 1).
-include_lib("include/types.hrl").

-record(user, {
	id :: id(),
	name :: t_binary(),
	password :: t_binary()
}).

-endif.