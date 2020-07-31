
-define(Nothing, {'Nothing'}).

-define(Just(A), {'Just', A}).

-type(maybe(A) :: {'Nothing'} | {'Just', A}).
