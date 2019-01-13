-ifndef(CI_EVENTS).
-define(CI_EVENTS, 1).

-record('user-state', {
	user_id,
	state
}).

-endif.