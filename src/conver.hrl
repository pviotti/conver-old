%% ETS table to store execution results of all client processes
-define(ETS_TABLE, ops_db).

%% Parameters of execution
-define(MAX_OP_INTERVAL, 50).   % max inter-operation interval
-define(MEAN_OPS, 10).          % mean of uniformly distributed number of operations
-define(SIGMA_OPS, 2).          % sigma of uniformly distributed number of operations
-define(READ_PROBABILITY, 2).   % 1 out of X is a read operation


-type op_type() :: read | write.

%% Single operation
-record(op, {
  proc :: atom(),           % process id
  type :: op_type(),        % operation type
  start_time :: integer(),  % operation start time
  end_time :: integer(),    % operation end time
  arg :: non_neg_integer(), % read output value / write input value
  notes = [] :: [atom()]    % list of atoms marking consistency violations
}).

-type op() :: #op{}.

%% Single client process state
-record(state, {
  proc :: atom(),               % process id
  store :: atom(),              % name of datastore client module
  t0 :: integer(),              % execution start time
  num_op :: non_neg_integer(),  % number of operations still to be issued
  ops = [] :: [op()]            % list of operations already issued
}).
