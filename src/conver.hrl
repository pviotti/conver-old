
-type op_type() :: read | write.

-record(op, {proc :: atom(),
  type :: op_type(),
  start_time :: integer(),
  end_time :: integer(),
  arg :: pos_integer(),
  notes = [] :: [atom()]
}).

-type op() :: #op{}.

-record(state, {id="", store, t0, num_op, ops=[] :: [op()]}).

-define(ETS_TABLE, ops_db).

-define(MAX_OP_INTERVAL, 600).  % max inter-operation interval
-define(MEAN_OPS, 6).           % mean of uniformly distributed number of operations
-define(SIGMA_OPS, 2).          % sigma of uniformly distributed number of operations
-define(READ_PROBABILITY, 2).   % 1 out of X is a read
