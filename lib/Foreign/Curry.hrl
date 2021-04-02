-ifndef(CURRY_HRL).
-define(CURRY_HRL, true).

-include("../Foreign.hrl").

curryIO(Fun) -> ?IO(curry(Fun)).

%% Curry a function
curry(Fun) -> curry(Fun, arity(Fun)).

curry(Fun, 0) -> Fun; %% ignore
curry(Fun, 1) -> Fun; %% ignore
curry(Fun, N) -> curry(Fun, [], N).

curry(Fun, Args, 0) ->
  erlang:apply(Fun, lists:reverse(Args));
curry(Fun, Args, N) ->
  fun(X) -> curry(Fun, [X|Args], N-1) end.

arity(Fun) ->
  element(2, erlang:fun_info(Fun, arity)).

-endif.
