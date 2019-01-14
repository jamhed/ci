-ifndef(CI_DB).
-define(CI_DB, 1).

-record(id_seq, {name, id}).

-include_lib("include/user.hrl").
-include_lib("include/repo.hrl").
-include_lib("include/commit.hrl").

-endif.