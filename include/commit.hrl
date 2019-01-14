-ifndef(CI_DB_COMMIT).
-define(CI_DB_COMMIT, 1).
-include_lib("include/types.hrl").

-record(commit, {
	sha :: t_binary(),
	repo_id :: id(),
	branch :: t_binary(),
	ts
}).

-endif.