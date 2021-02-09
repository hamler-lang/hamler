-ifndef(E_VAL_HRL).
-define(E_VAL_HRL, true).

curry(AnonymousFun) ->
    {arity, Arity} =
        erlang:fun_info(AnonymousFun, arity),

    do_curry(AnonymousFun, Arity, [[], [], []]).

do_curry(Fun, 0, [_Fronts, _Middle, _Ends] = X) ->
    % Fronts ++ Middle ++ ")" ++ Ends;
    [F, M, E] =
        lists:map(fun(L) -> string:join(L, "") end, X),
    Fstring =
        F ++ "Run(" ++ string:trim(M, trailing, ",") ++ ")" ++ E,

    {ok, Tokens, _} =
        erl_scan:string(Fstring ++ "."),
    {ok, Parsed} =
        erl_parse:parse_exprs(Tokens),

    FunBinding =
        erl_eval:add_binding(
          'Run',
          Fun,
          erl_eval:new_bindings()
        ),
    {value ,CurriedFun, _} =
        erl_eval:exprs(Parsed, FunBinding),

    CurriedFun;

do_curry(Fun, Arity, [Fronts, Middle, Ends]) ->
    VarName = [64 + Arity],
    NewFronts = ["fun(" ++ VarName ++ ") -> " | Fronts] ,
    NewMiddle = [VarName ++ ","|Middle],
    NewEnds = [" end"|Ends],
    do_curry(Fun, Arity-1, [NewFronts, NewMiddle, NewEnds]).

toEVal(X) -> if
    is_integer(X) -> {'EInt', X};
    is_boolean(X) -> {'EBool', X};
    is_float(X) -> {'EFloat', X};
    is_atom(X) -> {'EAtom', X};
    is_bitstring(X) -> {'EBin', X};
    is_pid(X) -> {'EPid', X};
    is_port(X) -> {'EPort', X};
    is_reference(X) -> {'ERef', X};
    is_list(X) -> {'EList', list:map(fun (I) -> toEVal(I) end, X)};
    is_tuple(X) -> {'ETuple', list:map(fun(I) -> toEVal(I) end, erlang:tuple_to_list(X))};
    is_function(X) -> {'EFun', fun (V) -> toEVal((curry(X))(fromEVal(V))) end} end.

fromEVal(X) -> case X of
    {'EList', V} -> list:map(fun (I) -> fromEVal(I) end, V);
    {'ETuple', V} -> erlang:list_to_tuple(list:map(fun (I) -> fromEVal(I) end, V));
    {'EFun', V} -> fun(I) -> fromEVal(V((toEVal(I)))) end;
    {_, V} -> V end.

-endif.
