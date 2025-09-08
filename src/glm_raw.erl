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
    bool/0, bool/1,
    bool_value/1
]}).
-compile({inline, [
    int8/0, int8/1,
    int8_value/1,
    int16/0, int16/1,
    int16_value/1,
    int32/0, int32/1,
    int32_value/1,
    int64/0, int64/1,
    int64_value/1,
    int64/0, int64/1,
    int64_value/1
]}).
-compile({inline, [
    uint8/0, uint8/1,
    uint8_value/1,
    uint16/0, uint16/1,
    uint16_value/1,
    uint32/0, uint32/1,
    uint32_value/1,
    uint64/0, uint64/1,
    uint64_value/1
]}).
-compile({inline, [
    float/0, float/1,
    float_value/1,
    double/0, double/1,
    double_value/1
]}).
-compile({inline, [
    foo/0
]}).

-export([
    bool/0, bool/1,
    bool_value/1
]).
-export([
    int8/0, int8/1,
    int8_value/1,
    int16/0, int16/1,
    int16_value/1,
    int32/0, int32/1,
    int32_value/1,
    int64/0, int64/1,
    int64_value/1
]).
-export([
    uint8/0, uint8/1,
    uint8_value/1,
    uint16/0, uint16/1,
    uint16_value/1,
    uint32/0, uint32/1,
    uint32_value/1,
    uint64/0, uint64/1,
    uint64_value/1
]).
-export([
    float/0, float/1,
    float_value/1,
    double/0, double/1,
    double_value/1
]).
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
-spec bool() -> binary().
bool() ->
    <<0:8/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec bool(boolean()) -> binary().
bool(V) ->
    <<(case V of true -> 1; false -> 0 end):8/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec bool_value(binary()) -> boolean().
bool_value(D) ->
    <<V:8/little-unsigned-integer>> = D,
    V =/= 0.

-doc("""
To be written.
""").
-spec int8() -> binary().
int8() ->
    <<0:8/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int8(integer()) -> binary().
int8(V) ->
    <<V:8/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int8_value(binary()) -> integer().
int8_value(D) ->
    <<V:8/little-signed-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec int16() -> binary().
int16() ->
    <<0:16/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int16(integer()) -> binary().
int16(V) ->
    <<V:16/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int16_value(binary()) -> integer().
int16_value(D) ->
    <<V:16/little-signed-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec int32() -> binary().
int32() ->
    <<0:32/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int32(integer()) -> binary().
int32(V) ->
    <<V:32/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int32_value(binary()) -> integer().
int32_value(D) ->
    <<V:32/little-signed-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec int64() -> binary().
int64() ->
    <<0:64/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int64(integer()) -> binary().
int64(V) ->
    <<V:64/little-signed-integer>>.

-doc("""
To be written.
""").
-spec int64_value(binary()) -> integer().
int64_value(D) ->
    <<V:64/little-signed-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec uint8() -> binary().
uint8() ->
    <<0:8/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint8(non_neg_integer()) -> binary().
uint8(V) ->
    <<V:8/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint8_value(binary()) -> non_neg_integer().
uint8_value(D) ->
    <<V:8/little-unsigned-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec uint16() -> binary().
uint16() ->
    <<0:16/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint16(non_neg_integer()) -> binary().
uint16(V) ->
    <<V:16/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint16_value(binary()) -> non_neg_integer().
uint16_value(D) ->
    <<V:16/little-unsigned-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec uint32() -> binary().
uint32() ->
    <<0:32/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint32(non_neg_integer()) -> binary().
uint32(V) ->
    <<V:32/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint32_value(binary()) -> non_neg_integer().
uint32_value(D) ->
    <<V:32/little-unsigned-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec uint64() -> binary().
uint64() ->
    <<0:64/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint64(non_neg_integer()) -> binary().
uint64(V) ->
    <<V:64/little-unsigned-integer>>.

-doc("""
To be written.
""").
-spec uint64_value(binary()) -> non_neg_integer().
uint64_value(D) ->
    <<V:64/little-unsigned-integer>> = D,
    V.

-doc("""
To be written.
""").
-spec float() -> binary().
float() ->
    <<0.0:32/little-float>>.

-doc("""
To be written.
""").
-spec float(float() | not_a_number | infinity) -> binary().
float(V) ->
    <<V:32/little-float>>.

-doc("""
To be written.
""").
-spec float_value(binary()) -> float().
float_value(D) ->
    <<V:32/little-float>> = D,
    V.

-doc("""
To be written.
""").
-spec double() -> binary().
double() ->
    <<0.0:64/little-float>>.

-doc("""
To be written.
""").
-spec double(float()) -> binary().
double(V) ->
    <<V:64/little-float>>.

-doc("""
To be written.
""").
-spec double_value(binary()) -> float().
double_value(D) ->
    <<V:64/little-float>> = D,
    V.

-doc("""
To be written.
""").
-spec foo() -> ok.
foo() ->
    ok.
