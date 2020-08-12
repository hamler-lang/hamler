%%---------------------------------------------------------------------------
%% |
%% Header      :  Foreign
%% Copyright   :  (c) 2020 EMQ Technologies Co., Ltd.
%% License     :  BSD-style (see the LICENSE file)
%%
%% Maintainer  :  Feng Lee, feng@emqx.io
%%                Yang M, yangm@emqx.io
%% Stability   :  experimental
%% Portability :  portable
%%
%% The Foreign Header File
%%
%%---------------------------------------------------------------------------
-ifndef(FOREIGN_HRL).
-define(FOREIGN_HRL, true).

-define(IO(Expr), fun() -> (Expr) end).

-define(RunIO(IO), (IO)()).

-include("./Foreign/Maybe.hrl").

-endif.
