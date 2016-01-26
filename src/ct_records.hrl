
-type op_type() :: read | write.
-record(op, {op_type :: op_type(),
  start_time :: integer(),
  end_time :: integer(),
  result
}).
-type op() :: #op{}.
-record(state, {id="", store, num_op, ops=[] :: [op()]}).