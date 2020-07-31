
-module('Regex').
-export([compile/2
        , inspect/1
        , replace/4
        ]).

match_newline(X) -> case X of
  'Cr' -> cr;
  'CrLf' -> crlf;
  'Lf' -> lf;
  'AnyCrLf' -> anycrlf;
  'Any' -> any
end.

match_compile({'Newline', {X}}) -> {newline, match_newline(X)};
match_compile({X}) -> case X of
  'Unicode' -> unicode;
  'Anchored' -> anchored;
  'Caseless' -> caseless;
  'DollarEndOnly' -> dollar_endonly;
  'DotAll' -> dotall;
  'Extended' -> extended;
  'Firstline' -> firstline;
  'Multiline' -> multiline;
  'NoAutoCapture' -> no_auto_capture;
  'Dupname' -> dupname;
  'Ungreedy' -> ungreedy;
  'BsrAnyCrLf' -> bsr_anycrlf;
  'BsrUnicode' -> bsr_unicode;
  'NoStartOptimize' -> no_start_optimize;
  'UCP' -> ucp;
  'NeverUTF' -> never_utf
end.

trans_compile([]) -> [];
trans_compile([X | Xs]) ->
  [match_compile(X) | trans_compile(Xs)].

match_runtime({X, Y}) -> case X of
  'Offset' -> {offset, Y};
  'MatchLimit' -> {match_limit, Y};
  'MatchLimitRecursion' -> {match_limit_recursion, Y}
end;
match_runtime({X}) -> case X of
  'Global' -> global;
  'NotBOL' -> notbol;
  'NotEOL' -> noteol;
  'NotEmpty' -> notempty;
  'NotEmptyStart' -> notempty_start
end.

trans_runtime([]) -> [];
trans_runtime([ X | Xs ]) -> 
  [match_runtime(X) | trans_runtime(Xs)].

compile(S, Opt) -> case re:compile(S, trans_compile(Opt)) of
  {ok, X} -> {'Ok', X};
  {error, X} -> {'Error', X}
end.

inspect(P) -> case re:inspect(P, namelist) of
  {namelist, X} -> X
end.

trans_replace([{'ReplaceCompileOption', X} | Xs]) ->
  [match_compile(X) | trans_replace(Xs)];
trans_replace([{'ReplaceRuntimeOption', X} | Xs]) ->
  [match_runtime(X) | trans_replace(Xs)].

replace(Sub, RE, Rep, Opt) ->
  re:replace(Sub, RE, Rep, trans_replace([{return, list} | Opt])).
