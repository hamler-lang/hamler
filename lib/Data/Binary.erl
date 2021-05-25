%%---------------------------------------------------------------------------
%% |
%% Module      :  Binary
%% Copyright   :  (c) 2020-2021 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Binary FFI module.
%%
%%---------------------------------------------------------------------------
-module('Binary').

-export([
    split/3,
    replace/4,
    matches/3
]).

splitOptions(Options) -> case Options of
    { 'SplitGlobal' } -> global;
    { 'SplitTrim' } -> trim;
    { 'SplitTrimAll' } -> trim_all;
    { 'SplitScope', S, L } -> {scope, {S, L}}
end.

split(Subject, Pattern, Options) -> 
    binary:split(Subject, Pattern, list:map(fun splitOptions/1, Options)).

replaceOptions(Options) -> case Options of
    { 'ReplaceGlobal' } -> global;
    { 'ReplaceScope', S, L } -> {scope, {S, L}};
    { 'InsertReplaced', InsPos } -> {insert_replaced, InsPos}
end.

replace(Subject, Pattern, Replacement, Options) ->
    binary:split(Subject, Pattern, Replacement, list:map(fun replaceOptions/1, Options)).

matches(Subject, Pattern, Options) ->
    binary:matches(Subject, Pattern, list:map(fun (X) -> {scope, X} end, Options)).
