
-module('Regex').
-export([compile/2
        , inspect/1
        , replace/4
        , run/3
        , split/3
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

compile(S, Opt) -> case re:compile(S, trans_compile(Opt)) of
  {ok, X} -> {'Ok', {X}};
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

trans_value_list([]) -> [];
trans_value_list([{_, {X}} | Xs]) ->
  [X | trans_value_list(Xs)].

match_value_spec({'ValueList', {X}}) ->
  trans_value_list(X);
match_value_spec({X}) -> case X of
  'All' -> all;
  'AllButFirst' -> all_but_first;
  'AllNames' -> all_name;
  'First' -> first;
  'None' -> none
end.

match_capture_type({X}) -> case X of
  'CaptureIndex' -> index;
  'CaptureList' -> list;
  'CaptureBinary' -> binary
end.

trans_run([]) -> [];
trans_run([{'RunCompileOption', X} | Xs]) ->
  [match_compile(X) | trans_run(Xs)];
trans_run([{'RunRuntimeOption', X} | Xs]) ->
  [match_runtime(X) | trans_run(Xs)];
trans_run([{'ReportErrors'} | Xs]) ->
  [report_errors | trans_run(Xs)];
trans_run([{'Capture', {X, Y}} | Xs]) ->
  [{capture, match_value_spec(X), match_capture_type(Y)} | trans_run(Xs)].

match_run_err(X) -> case X of
  match_limit -> {'MatchLimit'};
  match_limit_recursion -> {'MatchLimitRecursion'};
  {compile, X} -> {'CompileErr', X}
end.

trans_captured([]) -> [];
trans_captured([{X, Y} | Xs]) 
  when is_integer(X) and is_integer(Y) ->
    [{'DataIndex', {X, Y}} | trans_captured(Xs)];
trans_captured([X | Xs]) when is_binary(X) ->
  [{'DataBinary', {X}} | trans_captured(Xs)];
trans_captured([[C | Cs] | Xs]) when is_integer(C) ->
  [{'DataString', {[C | Cs]}} | trans_captured(Xs)];
trans_captured([X | Xs]) when is_list(X) ->
  [{'Datas', {trans_captured(X)}} | trans_captured(Xs)];
trans_captured([{error, X, Y} | Xs]) ->
  [{'DataErr', {X, Y}} | trans_captured(Xs)];
trans_captured([{incomplete, X, Y} | Xs]) ->
  [{'DataIncomplete', {X, Y}} | trans_captured(Xs)].

run(Sub, RE, Opt) -> case re:run(Sub, RE, trans_run(Opt)) of
    {match, X} -> {'Match', {trans_captured(X)}};
    match -> {'Match', {[]}};
    nomatch -> {'NoMatch'};
    {error, X} -> match_run_err(X)
end.

trans_split([]) -> [];
trans_split([{'SplitCompileOption', X} | Xs]) ->
  [match_compile(X) | trans_split(Xs)];
trans_split([{'SplitRuntimeOption', X} | Xs]) ->
  [match_runtime(X) | trans_split(Xs)];
trans_split([X | Xs]) -> [ case X of
  {'Parts', {Y}} -> {parts, Y};
  {'Group'} -> group;
  {'Trim'} -> trim
end | trans_split(Xs)].

split(Sub, RE, Opt) -> 
  re:split(Sub, RE, trans_split([{return, list} | Opt])).