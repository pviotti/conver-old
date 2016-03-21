
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
