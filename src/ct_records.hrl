
-type op_type() :: read | write.
-record(op, {op_type :: op_type(),
  proc :: atom(),
  start_time :: integer(),
  end_time :: integer(),
  arg :: pos_integer()
}).
-type op() :: #op{}.
-record(state, {id="", store, t0, num_op, ops=[] :: [op()]}).
