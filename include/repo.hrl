-ifndef(CI_DB_REPO).
-define(CI_DB_REPO, 1).
-include_lib("include/types.hrl").

-record(repo, {
	id :: id(),
	name :: t_binary(),
	url :: t_binary()
}).

-endif.