-module(conver_client).


-callback initialize([term()]) -> term().
-callback read(atom()) -> integer().
-callback write(atom(), integer()) -> term().
-callback delete(atom()) -> term().
-callback terminate() -> term().