-module('Read').

-export([
    readIntImpl/1, 
    readFloatImpl/1, 
    readCharImpl/1, 
    readAtomImpl/1,
    readPidImpl/1
]).

readIntImpl(X) -> case string:to_integer(X) of
    {error, R} -> error(R);
    {I, _} -> I
end.

readFloatImpl(X) -> case string:to_float(X) of
    {error, R} -> error(R);
    {F, _} -> F
end.

readCharImpl([$', $\\, X, $']) -> escape(X);
readCharImpl([$', X, $']) -> X.

escape($n) -> $\n;
escape($t) -> $\t;
escape($b) -> $\b;
escape($v) -> $\v;
escape($f) -> $\f;
escape($r) -> $\r;
escape($e) -> $\e;
escape(X) -> X.

readAtomImpl(X) -> atom:list_to_atom(X).

readPidImpl(S) -> list_to_pid(S).
