%%---------------------------------------------------------------------------
%% |
%% Module      :  String
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The UTF-8 String FFI module.
%%
%%---------------------------------------------------------------------------
-module('String').

-include("Maybe.hrl").

-export([ concat/2
        , reverse/1
        , replicate/2
        , strlen/1
        , equalIgnoreCase/2
        , hasPrefix/2
        , hasSuffix/2
        , indexOf/2
        , lastIndexOf/2
        , find/2
        , findLast/2
        , replace/3
        , replaceFirst/3
        , replaceLast/3
        , split/2
        , lines/1
        , words/1
        , sliceTo/3
        , padLeft/2
        , padRight/2
        , trimChars/2
        , trimLeft/1
        , trimLeftChars/2
        , trimRight/1
        , trimRightChars/2
        ]).

-define(Whitespace, "\x{0009}\x{000B}\x{000C}\x{0020}\x{00A0}").

-define(LineFeed, "\x{000A}\x{000D}\x{2028}\x{2029}").

-type(prefix() :: string()).
-type(suffix() :: string()).
-type(pattern() :: string()).
-type(replacement() :: string()).

-spec(concat(string(), string()) -> string()).
concat(S1, S2) -> string:concat(S1, S2).

-spec(reverse(string()) -> string()).
reverse(String) -> lists:reverse(String).

-spec(replicate(pos_integer(), string()) -> string()).
replicate(N, S) ->
    lists:flatten(lists:duplicate(N, S)).

-spec(strlen(string()) -> integer()).
strlen(String) -> string:length(String).

-spec(equalIgnoreCase(string(), string()) -> boolean()).
equalIgnoreCase(S1, S2) -> string:equal(S1, S2, true).

-spec(hasPrefix(string(), prefix()) -> boolean()).
hasPrefix(String, Prefix) ->
    string:prefix(String, Prefix) /= nomatch.

-spec(hasSuffix(string(), suffix()) -> boolean()).
hasSuffix(String, Suffix) ->
    Pos = (strlen(String) - strlen(Suffix)) + 1,
    string:equal(string:slice(String, Pos), Suffix).

-spec(indexOf(char(), string()) -> integer()).
indexOf(Char, String) -> string:chr(String, Char).

-spec(lastIndexOf(char(), string()) -> integer()).
lastIndexOf(Char, String) -> string:rchr(String, Char).

-spec(find(string(), pattern()) -> maybe(string())).
find(String, Pattern) -> doFind(String, Pattern, leading).

-spec(findLast(string(), pattern()) -> maybe(string())).
findLast(String, Pattern) -> doFind(String, Pattern, trailing).

doFind(String, Pattern, Dir) ->
    case string:find(String, Pattern, Dir) of
        nomatch -> {'Nothing'};
        SubStr  -> {'Just', SubStr}
    end.

-spec(replace(string(), pattern(), replacement()) -> string()).
replace(String, Pattern, Replacement) ->
    string:replace(String, Pattern, Replacement, all).

-spec(replaceFirst(string(), pattern(), replacement()) -> string()).
replaceFirst(String, Pattern, Replacement) ->
    string:replace(String, Pattern, Replacement, leading).

-spec(replaceLast(string(), pattern(), replacement()) -> string()).
replaceLast(String, Pattern, Replacement) ->
    string:replace(String, Pattern, Replacement, trailing).

-spec(split(string(), Sep :: string()) -> [string()]).
split(String, Sep) -> string:split(String, Sep, all).

-spec(lines(string()) -> [string()]).
lines(String) -> string:split(String, "\n", all).

-spec(words(string()) -> [string()]).
words(String) -> string:tokens(String, ?Whitespace ++ ?LineFeed).

-spec(sliceTo(string(), pos_integer(), pos_integer()) -> string()).
sliceTo(String, Start, End) ->
    string:slice(String, Start, (End - Start)).

-spec(padLeft(string(), pos_integer()) -> string()).
padLeft(String, Len) ->
    string:pad(String, Len, leading).

-spec(padRight(string(), pos_integer()) -> string()).
padRight(String, Len) ->
    string:pad(String, Len, trailing).

-spec(trimChars(string(), string()) -> string()).
trimChars(String, Chars) -> string:trim(String, both, Chars).

-spec(trimLeft(string()) -> string()).
trimLeft(String) -> string:trim(String, leading).

-spec(trimLeftChars(string(), string()) -> string()).
trimLeftChars(String, Chars) -> string:trim(String, leading, Chars).

-spec(trimRight(string()) -> string()).
trimRight(String) -> string:trim(String, trailing).

-spec(trimRightChars(string(), string()) -> string()).
trimRightChars(String, Chars) -> string:trim(String, trailing, Chars).

