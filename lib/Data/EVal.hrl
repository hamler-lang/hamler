-ifndef(E_VAL_HRL).
-define(E_VAL_HRL, true).

-include("../Foreign/Curry.hrl").

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
