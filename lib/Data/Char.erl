%%---------------------------------------------------------------------------
%% |
%% Module      :  Char
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Char FFI module.
%%
%%---------------------------------------------------------------------------
-module('Char').

-export([ isControl/1
        , isSpace/1
        , isLower/1
        , isUpper/1
        , isAlpha/1
        , isAlphaNum/1
        , isPrint/1
        , isDigit/1
        , isOctDigit/1
        , isHexDigit/1
        ]).

-export([ isAscii/1 ]).

-export([ toUpper/1
        , toLower/1
        , toTitle/1
        ]).

-export([ digitToInt/1
        , intToDigit/1
        ]).

-export([ord/1, chr/1]).

%% isblank(int c);
%% isgraph(int c);
%% isprint(int c);
%% ispunct(int c);

-spec(isControl(char()) -> boolean()).
isControl(16#7F) -> true;
isControl(C) when 0 =< C, C =< 16#1F ->
    true;
isControl(_) -> false.

%% ' '  (0x20) space (SPC)
%% '\t' (0x09) horizontal tab (TAB)
%% '\n' (0x0a) newline (LF)
%% '\v' (0x0b) vertical tab (VT)
%% '\f' (0x0c) feed (FF)
%% '\r' (0x0d) carriage return (CR)
-spec(isSpace(char()) -> boolean()).
isSpace($\ ) -> true;
isSpace($\t) -> true;
isSpace($\n) -> true;
isSpace($\v) -> true;
isSpace($\f) -> true;
isSpace($\r) -> true;
isSpace(_)   -> false.

-spec(isLower(char()) -> boolean()).
isLower(C) when $a =< C, C =< $z -> true;
isLower(_) -> false.

-spec(isUpper(char()) -> boolean()).
isUpper(C) when $A =< C, C =< $Z -> true;
isUpper(_) -> false.

-spec(isAlpha(char()) -> boolean()).
isAlpha(C) -> isLower(C) orelse isUpper(C).

-spec(isAlphaNum(char()) -> boolean()).
isAlphaNum(C) -> isAlpha(C) orelse isDigit(C).

-spec(isPrint(char()) -> boolean()).
isPrint(C) -> not isControl(C).

-spec(isDigit(char()) -> boolean()).
isDigit(C) when $0 =< C, C =< $9 -> true;
isDigit(_) -> false.

-spec(isOctDigit(char()) -> boolean()).
isOctDigit(C) when $0 =< C, C =< $7 -> true;
isOctDigit(_) -> false.

-spec(isHexDigit(char()) -> boolean()).
isHexDigit(C) when $a =< C, C =< $f -> true;
isHexDigit(C) when $A =< C, C =< $F -> true;
isHexDigit(C) -> isDigit(C).

-spec(isAscii(char()) -> boolean()).
isAscii(C) when 0 =< C, C =< 127 -> true;
isAscii(_) -> false.

-spec(toUpper(char()) -> char()).
toUpper(C) when $a =< C, C =< $z -> C - $a + $A;
toUpper(C) -> C.

-spec(toLower(char()) -> char()).
toLower(C) when $A =< C, C =< $Z -> C - $A + $a;
toLower(C) -> C.

-spec(digitToInt(char()) -> integer()).
digitToInt(C) when $0 =< C, C =< $9 -> C - $0;
digitToInt(C) when $a =< C, C =< $f -> (C - $a) + 10;
digitToInt(C) when $A =< C, C =< $F -> (C - $A) + 10;
digitToInt(_) -> error("Not a digit char").

-spec(intToDigit(integer()) -> char()).
intToDigit(I) when 0 =< I, I =< 9 -> I + $0;
intToDigit(I) when 10 =< I, I =< 15 -> (I - 10) + $a;
intToDigit(_) -> error("Not in range 0..15").

%% TODO: right?
%%
-spec(toTitle(char()) -> char()).
toTitle(C) -> toUpper(C).

-spec(ord(char()) -> integer()).
ord(C) -> C.

-spec(chr(integer()) -> char()).
chr(I) -> I.
