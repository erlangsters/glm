%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_raw).

-compile({inline, [
    foo/0
]}).
-export([
    foo/0
]).
-nifs([
    foo/0
]).

-on_load(glm_init/0).

glm_init() ->
    LibName = "beam-glm",
    LibPath = case code:priv_dir(glm) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, LibName]);
                _ ->
                    filename:join([priv, LibName])
            end;
        PrivDir ->
            filename:join(PrivDir, LibName)
    end,
    erlang:load_nif(LibPath, undefined).

-doc("""
To be written.
""").
-spec foo() -> ok.
foo() ->
    ok.
