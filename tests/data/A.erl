-module('A').

-export([ 'boolean'/0
        , 'integer'/0
        , 'number'/0
        , 'character'/0
        , 'string'/0
        , 'array'/0
        , 'tuple'/0
        , 'nonEmpty'/0
        , 'getName'/1
        , 'switch'/1
        , 'not'/1
        ]).

-record(person, {name}).

%% Boolean literal
-spec boolean() -> boolean().
boolean() -> true.

%% Int literal
-spec integer() -> integer().
integer() -> 7.

%% Number literal
-spec number() -> number().
number() -> 1.2.

%% Char literal
-spec character() -> char().
character() -> $t.

%% String literal
-spec 'string'() -> string().
'string'() -> "thran".

%% Array literal
-spec array() -> [integer()].
array() -> [1, 2, 3].

%% empty Record literal
-spec 'tuple'() -> tuple().
'tuple'() -> {}.

%% non-empty Record literal
-spec nonEmpty() -> #person{}.
nonEmpty() -> #person{name = "thran"}.

%% record access
-spec getName(#person{}) -> string().
getName(Person) -> Person#person.name.

%% case expression
-spec switch(integer()) -> integer().
switch(X) ->
   case {X, X} of
       {0, 0} -> 0;
       {1, Z} -> Z;
       {Y, 1} -> Y;
       {_, _} -> X
   end.

%% conditional expression
-spec 'not'(boolean()) -> boolean().
'not'(X) -> if X -> false; true -> true end.
