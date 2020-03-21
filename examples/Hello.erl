-module('Hello').

-export([add/2]).
-export([sub/2]).

-compile({inline, [sub/2]}).

-import('X', [hi/1]).

add(X,Y) -> X + Y.

sub(A,B) -> A - B.
