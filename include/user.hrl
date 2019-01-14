-ifndef(CI_DB_USER).
-define(CI_DB_USER, 1).
-include_lib("include/types.hrl").

-record(user, {
	id :: id(),
	name :: t_binary(),
	password :: t_binary(),
	login :: t_binary(),
	email :: t_binary(),
	department :: t_binary(),
	ts
}).

-record(user_repo, {
	user_id :: id(),
	repo_id :: id(),
	access :: t_binary()
}).

-endif.