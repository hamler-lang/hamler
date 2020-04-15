-module('IO').

-export([print/1]).

print(S) -> io:format("~s", [S]), S.
