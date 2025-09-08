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
-moduledoc """
OpenGL Mathematics (GLM) for the BEAM (raw).
""".

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
    vec2/1, vec2/2, vec2/3,
    vec2_x/2, vec2_set_x/3,
    vec2_y/2, vec2_set_y/3
]}).
-compile({inline, [
    vec3/1, vec3/2, vec3/4,
    vec3_x/2, vec3_set_x/3,
    vec3_y/2, vec3_set_y/3,
    vec3_z/2, vec3_set_z/3
]}).
-compile({inline, [
    vec4/1, vec4/2, vec4/5,
    vec4_x/2, vec4_set_x/3,
    vec4_y/2, vec4_set_y/3,
    vec4_z/2, vec4_set_z/3,
    vec4_w/2, vec4_set_w/3
]}).
-compile({inline, [
    clamp/6
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
    vec2/1, vec2/2, vec2/3,
    vec2_x/2, vec2_set_x/3,
    vec2_y/2, vec2_set_y/3
]).
-export([
    vec3/1, vec3/2, vec3/4,
    vec3_x/2, vec3_set_x/3,
    vec3_y/2, vec3_set_y/3,
    vec3_z/2, vec3_set_z/3
]).
-export([
    vec4/1, vec4/2, vec4/5,
    vec4_x/2, vec4_set_x/3,
    vec4_y/2, vec4_set_y/3,
    vec4_z/2, vec4_set_z/3,
    vec4_w/2, vec4_set_w/3
]).
-export([
    clamp/6
]).

-nifs([
    clamp_raw/6
]).

-on_load(glm_init/0).

-define(GLM_BOOL, 1).
-define(GLM_INT8, 2).
-define(GLM_INT16, 3).
-define(GLM_INT32, 4).
-define(GLM_INT64, 5).
-define(GLM_UINT8, 6).
-define(GLM_UINT16, 7).
-define(GLM_UINT32, 8).
-define(GLM_UINT64, 9).
-define(GLM_FLOAT, 10).
-define(GLM_DOUBLE, 11).

-define(GLM_TYPE(T),
    begin
        case T of
            bool -> ?GLM_BOOL;
            {int, 8} -> ?GLM_INT8;
            {int, 16} -> ?GLM_INT16;
            {int, 32} -> ?GLM_INT32;
            {int, 64} -> ?GLM_INT64;
            {uint, 8} -> ?GLM_UINT8;
            {uint, 16} -> ?GLM_UINT16;
            {uint, 32} -> ?GLM_UINT32;
            {uint, 64} -> ?GLM_UINT64;
            float -> ?GLM_FLOAT;
            double -> ?GLM_DOUBLE
        end
    end
).

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
-spec vec2(glm:type()) -> binary().
vec2(bool) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec2({int, 8}) ->
    <<
        0:8/little-signed-integer,
        0:8/little-signed-integer
    >>;
vec2({int, 16}) ->
    <<
        0:16/little-signed-integer,
        0:16/little-signed-integer
    >>;
vec2({int, 32}) ->
    <<
        0:32/little-signed-integer,
        0:32/little-signed-integer
    >>;
vec2({int, 64}) ->
    <<
        0:64/little-signed-integer,
        0:64/little-signed-integer
    >>;
vec2({uint, 8}) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec2({uint, 16}) ->
    <<
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer
    >>;
vec2({uint, 32}) ->
    <<
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer
    >>;
vec2({uint, 64}) ->
    <<
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer
    >>;
vec2(float) ->
    <<
        0:32/little-float,
        0:32/little-float
    >>;
vec2(double) ->
    <<
        0:64/little-float,
        0:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec2(glm:type(), term()) -> binary().
vec2(bool, V) ->
    <<
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec2({int, 8}, V) ->
    <<
        V:8/little-signed-integer,
        V:8/little-signed-integer
    >>;
vec2({int, 16}, V) ->
    <<
        V:16/little-signed-integer,
        V:16/little-signed-integer
    >>;
vec2({int, 32}, V) ->
    <<
        V:32/little-signed-integer,
        V:32/little-signed-integer
    >>;
vec2({int, 64}, V) ->
    <<
        V:64/little-signed-integer,
        V:64/little-signed-integer
    >>;
vec2({uint, 8}, V) ->
    <<
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer
    >>;
vec2({uint, 16}, V) ->
    <<
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer
    >>;
vec2({uint, 32}, V) ->
    <<
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer
    >>;
vec2({uint, 64}, V) ->
    <<
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer
    >>;
vec2(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float
    >>;
vec2(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec2(glm:type(), term(), term()) -> binary().
vec2(bool, X, Y) ->
    <<
        (case X of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec2({int, 8}, X, Y) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer
    >>;
vec2({int, 16}, X, Y) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer
    >>;
vec2({int, 32}, X, Y) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer
    >>;
vec2({int, 64}, X, Y) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer
    >>;
vec2({uint, 8}, X, Y) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >>;
vec2({uint, 16}, X, Y) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer
    >>;
vec2({uint, 32}, X, Y) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer
    >>;
vec2({uint, 64}, X, Y) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer
    >>;
vec2(float, X, Y) ->
    <<
        X:32/little-float,
        Y:32/little-float
    >>;
vec2(double, X, Y) ->
    <<
        X:64/little-float,
        Y:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec2_x(glm:type(), binary()) -> term().
vec2_x(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    X =/= 0;
vec2_x({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer
    >> = D,
    X;
vec2_x({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer
    >> = D,
    X;
vec2_x({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer
    >> = D,
    X;
vec2_x({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer
    >> = D,
    X;
vec2_x({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    X;
vec2_x({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer
    >> = D,
    X;
vec2_x({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer
    >> = D,
    X;
vec2_x({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer
    >> = D,
    X;
vec2_x(float, D) ->
    <<
        X:32/little-float,
        _Y:32/little-float
    >> = D,
    X;
vec2_x(double, D) ->
    <<
        X:64/little-float,
        _Y:64/little-float
    >> = D,
    X.

-doc("""
To be written.
""").
-spec vec2_set_x(glm:type(), binary(), term()) -> binary().
vec2_set_x(bool, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    <<(case X of true -> 1; false -> 0 end):8/little-unsigned-integer, Y:8/little-unsigned-integer>>;
vec2_set_x({int, 8}, D, X) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer>>;
vec2_set_x({int, 16}, D, X) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer>>;
vec2_set_x({int, 32}, D, X) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer>>;
vec2_set_x({int, 64}, D, X) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer>>;
vec2_set_x({uint, 8}, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer>>;
vec2_set_x({uint, 16}, D, X) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer>>;
vec2_set_x({uint, 32}, D, X) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer>>;
vec2_set_x({uint, 64}, D, X) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer>>;
vec2_set_x(float, D, X) ->
    <<
        _X:32/little-float,
        Y:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float>>;
vec2_set_x(double, D, X) ->
    <<
        _X:64/little-float,
        Y:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float>>.

-doc("""
To be written.
""").
-spec vec2_y(glm:type(), binary()) -> term().
vec2_y(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    Y =/= 0;
vec2_y({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer
    >> = D,
    Y;
vec2_y({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer
    >> = D,
    Y;
vec2_y({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer
    >> = D,
    Y;
vec2_y({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer
    >> = D,
    Y;
vec2_y({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    Y;
vec2_y({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer
    >> = D,
    Y;
vec2_y({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer
    >> = D,
    Y;
vec2_y({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer
    >> = D,
    Y;
vec2_y(float, D) ->
    <<
        _X:32/little-float,
        Y:32/little-float
    >> = D,
    Y;
vec2_y(double, D) ->
    <<
        _X:64/little-float,
        Y:64/little-float
    >> = D,
    Y.

-doc("""
To be written.
""").
-spec vec2_set_y(glm:type(), binary(), term()) -> binary().
vec2_set_y(bool, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer>>;
vec2_set_y({int, 8}, D, Y) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer>>;
vec2_set_y({int, 16}, D, Y) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer>>;
vec2_set_y({int, 32}, D, Y) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer>>;
vec2_set_y({int, 64}, D, Y) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer>>;
vec2_set_y({uint, 8}, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer>>;
vec2_set_y({uint, 16}, D, Y) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer>>;
vec2_set_y({uint, 32}, D, Y) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer>>;
vec2_set_y({uint, 64}, D, Y) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer>>;
vec2_set_y(float, D, Y) ->
    <<
        X:32/little-float,
        _Y:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float>>;
vec2_set_y(double, D, Y) ->
    <<
        X:64/little-float,
        _Y:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float>>.

-doc("""
To be written.
""").
-spec vec3(glm:type()) -> binary().
vec3(bool) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec3({int, 8}) ->
    <<
        0:8/little-signed-integer,
        0:8/little-signed-integer,
        0:8/little-signed-integer
    >>;
vec3({int, 16}) ->
    <<
        0:16/little-signed-integer,
        0:16/little-signed-integer,
        0:16/little-signed-integer
    >>;
vec3({int, 32}) ->
    <<
        0:32/little-signed-integer,
        0:32/little-signed-integer,
        0:32/little-signed-integer
    >>;
vec3({int, 64}) ->
    <<
        0:64/little-signed-integer,
        0:64/little-signed-integer,
        0:64/little-signed-integer
    >>;
vec3({uint, 8}) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec3({uint, 16}) ->
    <<
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer
    >>;
vec3({uint, 32}) ->
    <<
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer
    >>;
vec3({uint, 64}) ->
    <<
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer
    >>;
vec3(float) ->
    <<
        0:32/little-float,
        0:32/little-float,
        0:32/little-float
    >>;
vec3(double) ->
    <<
        0:64/little-float,
        0:64/little-float,
        0:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec3(glm:type(), term()) -> binary().
vec3(bool, V) ->
    <<
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec3({int, 8}, V) ->
    <<
        V:8/little-signed-integer,
        V:8/little-signed-integer,
        V:8/little-signed-integer
    >>;
vec3({int, 16}, V) ->
    <<
        V:16/little-signed-integer,
        V:16/little-signed-integer,
        V:16/little-signed-integer
    >>;
vec3({int, 32}, V) ->
    <<
        V:32/little-signed-integer,
        V:32/little-signed-integer,
        V:32/little-signed-integer
    >>;
vec3({int, 64}, V) ->
    <<
        V:64/little-signed-integer,
        V:64/little-signed-integer,
        V:64/little-signed-integer
    >>;
vec3({uint, 8}, V) ->
    <<
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer
    >>;
vec3({uint, 16}, V) ->
    <<
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer
    >>;
vec3({uint, 32}, V) ->
    <<
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer
    >>;
vec3({uint, 64}, V) ->
    <<
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer
    >>;
vec3(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
vec3(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec3(glm:type(), term(), term(), term()) -> binary().
vec3(bool, X, Y, Z) ->
    <<
        (case X of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec3({int, 8}, X, Y, Z) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >>;
vec3({int, 16}, X, Y, Z) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >>;
vec3({int, 32}, X, Y, Z) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >>;
vec3({int, 64}, X, Y, Z) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >>;
vec3({uint, 8}, X, Y, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >>;
vec3({uint, 16}, X, Y, Z) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >>;
vec3({uint, 32}, X, Y, Z) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >>;
vec3({uint, 64}, X, Y, Z) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >>;
vec3(float, X, Y, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float
    >>;
vec3(double, X, Y, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec3_x(glm:type(), binary()) -> term().
vec3_x(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    X =/= 0;
vec3_x({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        _Z:8/little-signed-integer
    >> = D,
    X;
vec3_x({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        _Z:16/little-signed-integer
    >> = D,
    X;
vec3_x({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        _Z:32/little-signed-integer
    >> = D,
    X;
vec3_x({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        _Z:64/little-signed-integer
    >> = D,
    X;
vec3_x({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    X;
vec3_x({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer
    >> = D,
    X;
vec3_x({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer
    >> = D,
    X;
vec3_x({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer
    >> = D,
    X;
vec3_x(float, D) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        _Z:32/little-float
    >> = D,
    X;
vec3_x(double, D) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        _Z:64/little-float
    >> = D,
    X.

-doc("""
To be written.
""").
-spec vec3_set_x(glm:type(), binary(), term()) -> binary().
vec3_set_x(bool, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<(case X of true -> 1; false -> 0 end):8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_x({int, 8}, D, X) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer>>;
vec3_set_x({int, 16}, D, X) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer>>;
vec3_set_x({int, 32}, D, X) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer>>;
vec3_set_x({int, 64}, D, X) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer>>;
vec3_set_x({uint, 8}, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_x({uint, 16}, D, X) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer>>;
vec3_set_x({uint, 32}, D, X) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer>>;
vec3_set_x({uint, 64}, D, X) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer>>;
vec3_set_x(float, D, X) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        Z:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float>>;
vec3_set_x(double, D, X) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        Z:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float>>.

-doc("""
To be written.
""").
-spec vec3_y(glm:type(), binary()) -> term().
vec3_y(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    Y =/= 0;
vec3_y({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer
    >> = D,
    Y;
vec3_y({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer
    >> = D,
    Y;
vec3_y({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer
    >> = D,
    Y;
vec3_y({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer
    >> = D,
    Y;
vec3_y({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    Y;
vec3_y({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer
    >> = D,
    Y;
vec3_y({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer
    >> = D,
    Y;
vec3_y({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer
    >> = D,
    Y;
vec3_y(float, D) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float
    >> = D,
    Y;
vec3_y(double, D) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float
    >> = D,
    Y.

-doc("""
To be written.
""").
-spec vec3_set_y(glm:type(), binary(), term()) -> binary().
vec3_set_y(bool, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_y({int, 8}, D, Y) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer>>;
vec3_set_y({int, 16}, D, Y) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer>>;
vec3_set_y({int, 32}, D, Y) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer>>;
vec3_set_y({int, 64}, D, Y) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer>>;
vec3_set_y({uint, 8}, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_y({uint, 16}, D, Y) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer>>;
vec3_set_y({uint, 32}, D, Y) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer>>;
vec3_set_y({uint, 64}, D, Y) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer>>;
vec3_set_y(float, D, Y) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float>>;
vec3_set_y(double, D, Y) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float>>.

-doc("""
To be written.
""").
-spec vec3_z(glm:type(), binary()) -> term().
vec3_z(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    Z =/= 0;
vec3_z({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >> = D,
    Z;
vec3_z({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >> = D,
    Z;
vec3_z({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >> = D,
    Z;
vec3_z({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >> = D,
    Z;
vec3_z({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    Z;
vec3_z({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >> = D,
    Z;
vec3_z({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >> = D,
    Z;
vec3_z({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >> = D,
    Z;
vec3_z(float, D) ->
    <<
        _X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float
    >> = D,
    Z;
vec3_z(double, D) ->
    <<
        _X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float
    >> = D,
    Z.

-doc("""
To be written.
""").
-spec vec3_set_z(glm:type(), binary(), term()) -> binary().
vec3_set_z(bool, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer>>;
vec3_set_z({int, 8}, D, Z) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer>>;
vec3_set_z({int, 16}, D, Z) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer>>;
vec3_set_z({int, 32}, D, Z) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer>>;
vec3_set_z({int, 64}, D, Z) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer>>;
vec3_set_z({uint, 8}, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_z({uint, 16}, D, Z) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer>>;
vec3_set_z({uint, 32}, D, Z) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer>>;
vec3_set_z({uint, 64}, D, Z) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer>>;
vec3_set_z(float, D, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float>>;
vec3_set_z(double, D, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float>>.

-doc("""
To be written.
""").
-spec vec4(glm:type()) -> binary().
vec4(bool) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec4({int, 8}) ->
    <<
        0:8/little-signed-integer,
        0:8/little-signed-integer,
        0:8/little-signed-integer,
        0:8/little-signed-integer
    >>;
vec4({int, 16}) ->
    <<
        0:16/little-signed-integer,
        0:16/little-signed-integer,
        0:16/little-signed-integer,
        0:16/little-signed-integer
    >>;
vec4({int, 32}) ->
    <<
        0:32/little-signed-integer,
        0:32/little-signed-integer,
        0:32/little-signed-integer,
        0:32/little-signed-integer
    >>;
vec4({int, 64}) ->
    <<
        0:64/little-signed-integer,
        0:64/little-signed-integer,
        0:64/little-signed-integer,
        0:64/little-signed-integer
    >>;
vec4({uint, 8}) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec4({uint, 16}) ->
    <<
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer
    >>;
vec4({uint, 32}) ->
    <<
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer
    >>;
vec4({uint, 64}) ->
    <<
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer
    >>;
vec4(float) ->
    <<
        0:32/little-float,
        0:32/little-float,
        0:32/little-float,
        0:32/little-float
    >>;
vec4(double) ->
    <<
        0:64/little-float,
        0:64/little-float,
        0:64/little-float,
        0:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec4(glm:type(), term()) -> binary().
vec4(bool, V) ->
    <<
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec4({int, 8}, V) ->
    <<
        V:8/little-signed-integer,
        V:8/little-signed-integer,
        V:8/little-signed-integer,
        V:8/little-signed-integer
    >>;
vec4({int, 16}, V) ->
    <<
        V:16/little-signed-integer,
        V:16/little-signed-integer,
        V:16/little-signed-integer,
        V:16/little-signed-integer
    >>;
vec4({int, 32}, V) ->
    <<
        V:32/little-signed-integer,
        V:32/little-signed-integer,
        V:32/little-signed-integer,
        V:32/little-signed-integer
    >>;
vec4({int, 64}, V) ->
    <<
        V:64/little-signed-integer,
        V:64/little-signed-integer,
        V:64/little-signed-integer,
        V:64/little-signed-integer
    >>;
vec4({uint, 8}, V) ->
    <<
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer
    >>;
vec4({uint, 16}, V) ->
    <<
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer
    >>;
vec4({uint, 32}, V) ->
    <<
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer
    >>;
vec4({uint, 64}, V) ->
    <<
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer
    >>;
vec4(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
vec4(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec4(glm:type(), term(), term(), term(), term()) -> binary().
vec4(bool, X, Y, Z, W) ->
    <<
        (case X of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case W of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec4({int, 8}, X, Y, Z, W) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        W:8/little-signed-integer
    >>;
vec4({int, 16}, X, Y, Z, W) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        W:16/little-signed-integer
    >>;
vec4({int, 32}, X, Y, Z, W) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        W:32/little-signed-integer
    >>;
vec4({int, 64}, X, Y, Z, W) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        W:64/little-signed-integer
    >>;
vec4({uint, 8}, X, Y, Z, W) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >>;
vec4({uint, 16}, X, Y, Z, W) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >>;
vec4({uint, 32}, X, Y, Z, W) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >>;
vec4({uint, 64}, X, Y, Z, W) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >>;
vec4(float, X, Y, Z, W) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >>;
vec4(double, X, Y, Z, W) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >>.

-doc("""
To be written.
""").
-spec vec4_x(glm:type(), binary()) -> term().
vec4_x(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    X =/= 0;
vec4_x({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    X;
vec4_x({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    X;
vec4_x({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    X;
vec4_x({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    X;
vec4_x({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    X;
vec4_x({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    X;
vec4_x({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    X;
vec4_x({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    X;
vec4_x(float, D) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        _Z:32/little-float,
        _W:32/little-float
    >> = D,
    X;
vec4_x(double, D) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        _Z:64/little-float,
        _W:64/little-float
    >> = D,
    X.

-doc("""
To be written.
""").
-spec vec4_set_x(glm:type(), binary(), term()) -> binary().
vec4_set_x(bool, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<(case X of true -> 1; false -> 0 end):8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_x({int, 8}, D, X) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_x({int, 16}, D, X) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_x({int, 32}, D, X) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_x({int, 64}, D, X) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_x({uint, 8}, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_x({uint, 16}, D, X) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_x({uint, 32}, D, X) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_x({uint, 64}, D, X) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_x(float, D, X) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_x(double, D, X) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.

-doc("""
To be written.
""").
-spec vec4_y(glm:type(), binary()) -> term().
vec4_y(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Y =/= 0;
vec4_y({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    Y;
vec4_y({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    Y;
vec4_y({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    Y;
vec4_y({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    Y;
vec4_y({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Y;
vec4_y({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    Y;
vec4_y({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    Y;
vec4_y({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    Y;
vec4_y(float, D) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float,
        _W:32/little-float
    >> = D,
    Y;
vec4_y(double, D) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float,
        _W:64/little-float
    >> = D,
    Y.

-doc("""
To be written.
""").
-spec vec4_set_y(glm:type(), binary(), term()) -> binary().
vec4_set_y(bool, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_y({int, 8}, D, Y) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_y({int, 16}, D, Y) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_y({int, 32}, D, Y) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_y({int, 64}, D, Y) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_y({uint, 8}, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_y({uint, 16}, D, Y) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_y({uint, 32}, D, Y) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_y({uint, 64}, D, Y) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_y(float, D, Y) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_y(double, D, Y) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.

-doc("""
To be written.
""").
-spec vec4_z(glm:type(), binary()) -> term().
vec4_z(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Z =/= 0;
vec4_z({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    Z;
vec4_z({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    Z;
vec4_z({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    Z;
vec4_z({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    Z;
vec4_z({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Z;
vec4_z({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    Z;
vec4_z({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    Z;
vec4_z({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    Z;
vec4_z(float, D) ->
    <<
        _X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float,
        _W:32/little-float
    >> = D,
    Z;
vec4_z(double, D) ->
    <<
        _X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float,
        _W:64/little-float
    >> = D,
    Z.

-doc("""
To be written.
""").
-spec vec4_set_z(glm:type(), binary(), term()) -> binary().
vec4_set_z(bool, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_z({int, 8}, D, Z) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_z({int, 16}, D, Z) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_z({int, 32}, D, Z) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_z({int, 64}, D, Z) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_z({uint, 8}, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_z({uint, 16}, D, Z) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_z({uint, 32}, D, Z) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_z({uint, 64}, D, Z) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_z(float, D, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float,
        W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_z(double, D, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float,
        W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.

-doc("""
To be written.
""").
-spec vec4_w(glm:type(), binary()) -> term().
vec4_w(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    W =/= 0;
vec4_w({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    W;
vec4_w({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    W;
vec4_w({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    W;
vec4_w({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    W;
vec4_w({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    W;
vec4_w({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    W;
vec4_w({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    W;
vec4_w({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    W;
vec4_w(float, D) ->
    <<
        _X:32/little-float,
        _Y:32/little-float,
        _Z:32/little-float,
        W:32/little-float
    >> = D,
    W;
vec4_w(double, D) ->
    <<
        _X:64/little-float,
        _Y:64/little-float,
        _Z:64/little-float,
        W:64/little-float
    >> = D,
    W.

-doc("""
To be written.
""").
-spec vec4_set_w(glm:type(), binary(), term()) -> binary().
vec4_set_w(bool, D, W) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, (case W of true -> 1; false -> 0 end):8/little-unsigned-integer>>;
vec4_set_w({int, 8}, D, W) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_w({int, 16}, D, W) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_w({int, 32}, D, W) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_w({int, 64}, D, W) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_w({uint, 8}, D, W) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_w({uint, 16}, D, W) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_w({uint, 32}, D, W) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_w({uint, 64}, D, W) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_w(float, D, W) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        _W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_w(double, D, W) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        _W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.

-doc("""
To be written.
""").
-spec clamp(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar, scalar} | {vector, scalar, scalar} | {vector, vector, vector},
    X :: binary(), MinVal :: binary(), MaxVal :: binary()
) -> binary().
clamp(T, L, Pattern, X, MinVal, MaxVal) ->
    clamp_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar, scalar} -> 0;
            {vector, scalar, scalar} -> 1;
            {vector, vector, vector} -> 2
        end,
        X, MinVal, MaxVal
    ).

-doc("""
To be written.
""").
-spec clamp_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), MinVal :: binary(), MaxVal :: binary()
) -> binary().
clamp_raw(_T, _L, _Pattern, _X, _MinVal, _MaxVal) ->
    erlang:nif_error(beam_glm_not_loaded).
