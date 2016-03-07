-module(cv_client).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{initialize,1}, {read,1}, {write,2}, {delete,1}, {terminate,0}];
behaviour_info(_) -> undefined.