
-define(IO(Expr), fun() -> (Expr) end).

-define(RunIO(IO), (IO)()).

-define(Nothing, {'Nothing'}).

-define(Just(A), {'Just', A}).

