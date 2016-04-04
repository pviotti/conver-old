-module(conver_client).


-callback init(atom(), [term()]) -> pid().
-callback read(pid(), atom()) -> integer().
-callback write(pid(), atom(), integer()) -> term().
-callback delete(pid(), atom()) -> term().
-callback terminate(pid()) -> term().