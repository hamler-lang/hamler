
## Hamler Data Types mapping to Erlang Types

| Hamler Data Type  | Erlang Data Type               | Mapping Description               |
| ----------------- | ------------------------------ | --------------------------------- |
| None              | 'None' \| none()               | null                              |
| Bool              | boolean()                      | True -> true <br />False -> false |
| Atom(Symbol)      | atom()                         |                                   |
| Char              | char() ??                      |                                   |
| Int(Integer)      | integer()                      | Integer type                      |
| Num(Number)       | number(), integer() \| float() |                                   |
| Float(Double)     | float()                        | Float type                        |
| String            | "hello"                        | String is a list of character     |
| Tuple             | tuple()                        |                                   |
| List              | list()                         |                                   |
| Enum, Range       |                                |                                   |
| Binary/Bitstrings | binary() \| bitstring()        |                                   |
| Map(Dict)         | map()                          |                                   |
| Record            |                                |                                   |
| Fun               | fun()                          | Function                          |
| Port              | port()                         | Erlang Port                       |
| Pid               | pid()                          | Erlang Pid                        |
| Ref               | reference()                    | Erlang Reference                  |

## Mapping Examples

True  -> true
False -> false
:atom -> atom
(:error, "Reason") -> {error, "Reason"}

## Records

{x=1, hello="world"}

