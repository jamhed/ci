-ifndef(CI_DB_TYPES).
-define(CI_DB_TYPES, 1).

-type(t_binary() :: binary() | undefined).
-type(t_boolean() :: boolean() | undefined).
-type(t_integer() :: integer() | undefined).
-type(t_float() :: float() | undefined).
-type(t_non_neg_integer() :: non_neg_integer() | undefined).
-type(t_pos_integer() :: pos_integer() | undefined).
-type(id() :: pos_integer() | undefined).

-endif.