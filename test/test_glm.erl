%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(test_glm).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("glm/include/glm.hrl").

-export([
    gen_type/0,
    gen_vec_length/0
]).
-export([
    gen_scalar/1,
    gen_vec/2
]).
-export([
    valid_value/1,
    invalid_value/1
]).
-export([
    assert_bool/2,
    assert_int8/2,
    assert_int16/2,
    assert_int32/2,
    assert_int64/2,
    assert_uint8/2,
    assert_uint16/2,
    assert_uint32/2,
    assert_uint64/2,
    assert_float/2,
    assert_double/2
]).
-export([
    assert_scalar/2,
    assert_scalar/3
]).
-export([
    assert_vec2/3,
    assert_vec3/3,
    assert_vec4/3
]).
-export([
    assert_quat/3
]).
-export([
    assert_vec/3,
    assert_vec/4
]).
-export([
    assert_mat2/3,
    assert_mat3/3,
    assert_mat4/3
]).
-export([
    assert_mat2x3/3,
    assert_mat2x4/3,
    assert_mat3x2/3,
    assert_mat3x4/3,
    assert_mat4x2/3,
    assert_mat4x3/3
]).

gen_type() ->
    oneof([
        bool,
        {int, 8}, {int, 16}, {int, 32}, {int, 64},
        {uint, 8}, {uint, 16}, {uint, 32}, {uint, 64},
        float,
        double
    ]).

gen_vec_length() ->
    oneof([2, 3, 4]).

gen_scalar(T) ->
    ?LET(
        V,
        valid_value(T),
        glm:scalar(T, V)
    ).

gen_vec(T, L) ->
    case L of
        2 ->
            ?LET(
                {X, Y},
                {valid_value(T), valid_value(T)},
                glm:vec2(T, X, Y)
            );
        3 ->
            ?LET(
                {X, Y, Z},
                {valid_value(T), valid_value(T), valid_value(T)},
                glm:vec3(T, X, Y, Z)
            );
        4 ->
            ?LET(
                {X, Y, Z, W},
                {valid_value(T), valid_value(T), valid_value(T), valid_value(T)},
                glm:vec4(T, X, Y, Z, W)
            )
    end.

valid_value(bool) ->
    boolean();
valid_value({int, 8}) ->
    integer(-128, 127);
valid_value({int, 16}) ->
    integer(-32768, 32767);
valid_value({int, 32}) ->
    integer(-2147483648, 2147483647);
valid_value({int, 64}) ->
    integer(-9223372036854775808, 9223372036854775807);
valid_value({uint, 8}) ->
    integer(0, 255);
valid_value({uint, 16}) ->
    integer(0, 65535);
valid_value({uint, 32}) ->
    integer(0, 4294967295);
valid_value({uint, 64}) ->
    integer(0, 18446744073709551615);
valid_value(float) ->
    oneof([
        0.0,
        -0.0,
        1.0,
        -1.0,
        1.175494e-38,
        -1.175494e-38
    ]);
valid_value(double) ->
    oneof([
        0.0,
        -0.0,
        1.0,
        -1.0,
        3.4028235e+38,
        -3.4028235e+38,
        1.7976931348623157e+308,
        -1.7976931348623157e+308
    ]).

invalid_value(bool) ->
    oneof([integer(), float()]);
invalid_value({int, 8}) ->
    oneof([
        boolean(),
        integer(128, 1000),
        integer(-1000, -129),
        float()
    ]);
invalid_value({int, 16}) ->
    oneof([
        boolean(),
        integer(32768, 100000),
        integer(-100000, -32769),
        float()
    ]);
invalid_value({int, 32}) ->
    oneof([
        boolean(),
        integer(2147483648, 10000000000),
        integer(-10000000000, -2147483649),
        float()
    ]);
invalid_value({int, 64}) ->
    oneof([
        boolean(),
        -9223372036854775809, % Just below min (-2^63 - 1)
        9223372036854775808,  % Just above max (2^63)
        float()
    ]);
invalid_value({uint, 8}) ->
    oneof([
        boolean(),
        integer(-1000, -1),
        integer(256, 1000),
        float()
    ]);
invalid_value({uint, 16}) ->
    oneof([
        boolean(),
        integer(-1000, -1),
        integer(65536, 100000),
        float()
    ]);
invalid_value({uint, 32}) ->
    oneof([
        boolean(),
        integer(-10000000000, -1),
        integer(4294967296, 10000000000),
        float()
    ]);
invalid_value({uint, 64}) ->
    oneof([
        boolean(),
        -1,                   % Just below min
        18446744073709551616, % Just above max (2^64)
        float()
    ]);
invalid_value(float) ->
    oneof([
        boolean(),
        integer()
    ]);
invalid_value(double) ->
    oneof([
        boolean(),
        integer()
    ]).

assert_bool(S, V) ->
    {scalar, bool, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(bool, 1, D),
    ?assertEqual(V, glm:bool_value(S)),
    ok.

assert_int8(S, V) ->
    {scalar, {int, 8}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({int, 8}, 1, D),
    ?assertEqual(V, glm:int8_value(S)),
    ok.

assert_int16(S, V) ->
    {scalar, {int, 16}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({int, 16}, 1, D),
    ?assertEqual(V, glm:int16_value(S)),
    ok.

assert_int32(S, V) ->
    {scalar, {int, 32}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({int, 32}, 1, D),
    ?assertEqual(V, glm:int32_value(S)),
    ok.

assert_int64(S, V) ->
    {scalar, {int, 64}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({int, 64}, 1, D),
    ?assertEqual(V, glm:int64_value(S)),
    ok.

assert_uint8(S, V) ->
    {scalar, {uint, 8}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({uint, 8}, 1, D),
    ?assertEqual(V, glm:uint8_value(S)),
    ok.

assert_uint16(S, V) ->
    {scalar, {uint, 16}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({uint, 16}, 1, D),
    ?assertEqual(V, glm:uint16_value(S)),
    ok.

assert_uint32(S, V) ->
    {scalar, {uint, 32}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({uint, 32}, 1, D),
    ?assertEqual(V, glm:uint32_value(S)),
    ok.

assert_uint64(S, V) ->
    {scalar, {uint, 64}, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size({uint, 64}, 1, D),
    ?assertEqual(V, glm:uint64_value(S)),
    ok.

assert_float(S, V) ->
    {scalar, float, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(float, 1, D),

    % ?assertEqual(V, glm:float_value(S)),
    ?assert(erlang:abs(glm:float_value(S) - V) < 1.0e-9),

    % 1.0e-7

    ok.

assert_double(S, V) ->
    {scalar, double, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(double, 1, D),
    % ?assertEqual(V, glm:double_value(S)),

    ?assert(erlang:abs(glm:double_value(S) - V) < 1.0e-12),

    % 1.0e-12

    ok.

assert_scalar(T, V) ->
    {scalar, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 1, D),

    ok.

assert_scalar(T, V, Expected) ->
    {scalar, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 1, D),
    case T of
        float ->
            ?assert(erlang:abs(glm:float_value(V) - Expected) < 1.0e-9);
        % double ->
        %     ?assert(erlang:abs(glm:double_value(V) - Expected) < 1.0e-12);
        _ ->
            ?assertEqual(Expected, glm:scalar_value(V))
    end,

    ok.

assert_vec2(T, V) ->
    {vec, 2, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 2, D),
    ok.

assert_vec2(T, V, {X, Y}) ->
    {vec, 2, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 2, D),
    case T of
        float ->
            ?assert(erlang:abs(glm:vec2_x(V) - X) < 1.0e-9),
            ?assert(erlang:abs(glm:vec2_y(V) - Y) < 1.0e-9);
        double ->
            ?assert(erlang:abs(glm:vec2_x(V) - X) < 1.0e-12),
            ?assert(erlang:abs(glm:vec2_y(V) - Y) < 1.0e-12);
        _ ->
            ?assertEqual(X, glm:vec2_x(V)),
            ?assertEqual(Y, glm:vec2_y(V))
    end,

    ok.

assert_vec3(T, V) ->
    {vec, 3, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 3, D),
    ok.

assert_vec3(T, V, {X, Y, Z}) ->
    {vec, 3, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 3, D),
    case T of
        float ->
            ?assert(erlang:abs(glm:vec3_x(V) - X) < 1.0e-9),
            ?assert(erlang:abs(glm:vec3_y(V) - Y) < 1.0e-9),
            ?assert(erlang:abs(glm:vec3_z(V) - Z) < 1.0e-9);
        double ->
            ?assert(erlang:abs(glm:vec3_x(V) - X) < 1.0e-12),
            ?assert(erlang:abs(glm:vec3_y(V) - Y) < 1.0e-12),
            ?assert(erlang:abs(glm:vec3_z(V) - Z) < 1.0e-12);
        _ ->
            ?assertEqual(X, glm:vec3_x(V)),
            ?assertEqual(Y, glm:vec3_y(V)),
            ?assertEqual(Z, glm:vec3_z(V))
    end,

    ok.

assert_vec4(T, V) ->
    {vec, 4, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 4, D),
    ok.

assert_vec4(T, V, {X, Y, Z, W}) ->
    {vec, 4, T, D} = V,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 4, D),
    case T of
        float ->
            ?assert(erlang:abs(glm:vec4_x(V) - X) < 1.0e-9),
            ?assert(erlang:abs(glm:vec4_y(V) - Y) < 1.0e-9),
            ?assert(erlang:abs(glm:vec4_z(V) - Z) < 1.0e-9),
            ?assert(erlang:abs(glm:vec4_w(V) - W) < 1.0e-9);
        double ->
            ?assert(erlang:abs(glm:vec4_x(V) - X) < 1.0e-12),
            ?assert(erlang:abs(glm:vec4_y(V) - Y) < 1.0e-12),
            ?assert(erlang:abs(glm:vec4_z(V) - Z) < 1.0e-12),
            ?assert(erlang:abs(glm:vec4_w(V) - W) < 1.0e-12);
        _ ->
            ?assertEqual(X, glm:vec4_x(V)),
            ?assertEqual(Y, glm:vec4_y(V)),
            ?assertEqual(Z, glm:vec4_z(V)),
            ?assertEqual(W, glm:vec4_w(V))
    end,
    ok.

assert_vec(T, L, V) ->
    case L of
        2 -> assert_vec2(T, V);
        3 -> assert_vec3(T, V);
        4 -> assert_vec4(T, V)
    end.

assert_vec(T, L, V, Expected) ->
    case L of
        2 -> assert_vec2(T, V, Expected);
        3 -> assert_vec3(T, V, Expected);
        4 -> assert_vec4(T, V, Expected)
    end.

assert_quat(T, Quaternion, {W, X, Y, Z}) ->
    {quat, T, D} = Quaternion,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 4, D),
    case T of
        float ->
            ?assert(erlang:abs(glm:quat_w(Quaternion) - W) < 1.0e-9),
            ?assert(erlang:abs(glm:quat_x(Quaternion) - X) < 1.0e-9),
            ?assert(erlang:abs(glm:quat_y(Quaternion) - Y) < 1.0e-9),
            ?assert(erlang:abs(glm:quat_z(Quaternion) - Z) < 1.0e-9);
        double ->
            ?assert(erlang:abs(glm:quat_w(Quaternion) - W) < 1.0e-12),
            ?assert(erlang:abs(glm:quat_x(Quaternion) - X) < 1.0e-12),
            ?assert(erlang:abs(glm:quat_y(Quaternion) - Y) < 1.0e-12),
            ?assert(erlang:abs(glm:quat_z(Quaternion) - Z) < 1.0e-12)
    end,

    ok.

assert_mat2(T, Matrix, Elements) ->
    {
        M11, M12,
        M21, M22
    } = Elements,
    {mat, 2, 2, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 4, D),

    E11 = glm:mat2_element(Matrix, 1, 1),
    E12 = glm:mat2_element(Matrix, 2, 1),
    E21 = glm:mat2_element(Matrix, 1, 2),
    E22 = glm:mat2_element(Matrix, 2, 2),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12)
    end,

    ok.

assert_mat3(T, Matrix, Elements) ->
    {
        M11, M12, M13,
        M21, M22, M23,
        M31, M32, M33
    } = Elements,
    {mat, 3, 3, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 9, D),

    E11 = glm:mat3_element(Matrix, 1, 1),
    E12 = glm:mat3_element(Matrix, 2, 1),
    E13 = glm:mat3_element(Matrix, 3, 1),
    E21 = glm:mat3_element(Matrix, 1, 2),
    E22 = glm:mat3_element(Matrix, 2, 2),
    E23 = glm:mat3_element(Matrix, 3, 2),
    E31 = glm:mat3_element(Matrix, 1, 3),
    E32 = glm:mat3_element(Matrix, 2, 3),
    E33 = glm:mat3_element(Matrix, 3, 3),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E13 - M13) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E23 - M23) < 1.0e-9),
            ?assert(erlang:abs(E31 - M31) < 1.0e-9),
            ?assert(erlang:abs(E32 - M32) < 1.0e-9),
            ?assert(erlang:abs(E33 - M33) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E13 - M13) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E23 - M23) < 1.0e-12),
            ?assert(erlang:abs(E31 - M31) < 1.0e-12),
            ?assert(erlang:abs(E32 - M32) < 1.0e-12),
            ?assert(erlang:abs(E33 - M33) < 1.0e-12)
    end,

    ok.

assert_mat4(T, Matrix, Elements) ->
    {
        M11, M12, M13, M14,
        M21, M22, M23, M24,
        M31, M32, M33, M34,
        M41, M42, M43, M44
    } = Elements,
    {mat, 4, 4, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 16, D),

    E11 = glm:mat4_element(Matrix, 1, 1),
    E12 = glm:mat4_element(Matrix, 2, 1),
    E13 = glm:mat4_element(Matrix, 3, 1),
    E14 = glm:mat4_element(Matrix, 4, 1),
    E21 = glm:mat4_element(Matrix, 1, 2),
    E22 = glm:mat4_element(Matrix, 2, 2),
    E23 = glm:mat4_element(Matrix, 3, 2),
    E24 = glm:mat4_element(Matrix, 4, 2),
    E31 = glm:mat4_element(Matrix, 1, 3),
    E32 = glm:mat4_element(Matrix, 2, 3),
    E33 = glm:mat4_element(Matrix, 3, 3),
    E34 = glm:mat4_element(Matrix, 4, 3),
    E41 = glm:mat4_element(Matrix, 1, 4),
    E42 = glm:mat4_element(Matrix, 2, 4),
    E43 = glm:mat4_element(Matrix, 3, 4),
    E44 = glm:mat4_element(Matrix, 4, 4),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E13 - M13) < 1.0e-9),
            ?assert(erlang:abs(E14 - M14) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E23 - M23) < 1.0e-9),
            ?assert(erlang:abs(E24 - M24) < 1.0e-9),
            ?assert(erlang:abs(E31 - M31) < 1.0e-9),
            ?assert(erlang:abs(E32 - M32) < 1.0e-9),
            ?assert(erlang:abs(E33 - M33) < 1.0e-9),
            ?assert(erlang:abs(E34 - M34) < 1.0e-9),
            ?assert(erlang:abs(E41 - M41) < 1.0e-9),
            ?assert(erlang:abs(E42 - M42) < 1.0e-9),
            ?assert(erlang:abs(E43 - M43) < 1.0e-9),
            ?assert(erlang:abs(E44 - M44) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E13 - M13) < 1.0e-12),
            ?assert(erlang:abs(E14 - M14) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E23 - M23) < 1.0e-12),
            ?assert(erlang:abs(E24 - M24) < 1.0e-12),
            ?assert(erlang:abs(E31 - M31) < 1.0e-12),
            ?assert(erlang:abs(E32 - M32) < 1.0e-12),
            ?assert(erlang:abs(E33 - M33) < 1.0e-12),
            ?assert(erlang:abs(E34 - M34) < 1.0e-12),
            ?assert(erlang:abs(E41 - M41) < 1.0e-12),
            ?assert(erlang:abs(E42 - M42) < 1.0e-12),
            ?assert(erlang:abs(E43 - M43) < 1.0e-12),
            ?assert(erlang:abs(E44 - M44) < 1.0e-12)
    end,

    ok.

assert_mat2x3(T, Matrix, Elements) ->
    {
        M11, M12,
        M21, M22,
        M31, M32
    } = Elements,
    {mat, 2, 3, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 6, D),

    E11 = glm:mat2x3_element(Matrix, 1, 1),
    E12 = glm:mat2x3_element(Matrix, 2, 1),
    E21 = glm:mat2x3_element(Matrix, 1, 2),
    E22 = glm:mat2x3_element(Matrix, 2, 2),
    E31 = glm:mat2x3_element(Matrix, 1, 3),
    E32 = glm:mat2x3_element(Matrix, 2, 3),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E31 - M31) < 1.0e-9),
            ?assert(erlang:abs(E32 - M32) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E31 - M31) < 1.0e-12),
            ?assert(erlang:abs(E32 - M32) < 1.0e-12)
    end,

    ok.

assert_mat2x4(T, Matrix, Elements) ->
    {
        M11, M12,
        M21, M22,
        M31, M32,
        M41, M42
    } = Elements,
    {mat, 2, 4, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 8, D),

    E11 = glm:mat2x4_element(Matrix, 1, 1),
    E12 = glm:mat2x4_element(Matrix, 2, 1),
    E21 = glm:mat2x4_element(Matrix, 1, 2),
    E22 = glm:mat2x4_element(Matrix, 2, 2),
    E31 = glm:mat2x4_element(Matrix, 1, 3),
    E32 = glm:mat2x4_element(Matrix, 2, 3),
    E41 = glm:mat2x4_element(Matrix, 1, 4),
    E42 = glm:mat2x4_element(Matrix, 2, 4),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E31 - M31) < 1.0e-9),
            ?assert(erlang:abs(E32 - M32) < 1.0e-9),
            ?assert(erlang:abs(E41 - M41) < 1.0e-9),
            ?assert(erlang:abs(E42 - M42) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E31 - M31) < 1.0e-12),
            ?assert(erlang:abs(E32 - M32) < 1.0e-12),
            ?assert(erlang:abs(E41 - M41) < 1.0e-12),
            ?assert(erlang:abs(E42 - M42) < 1.0e-12)
    end,

    ok.

assert_mat3x2(T, Matrix, Elements) ->
    {
        M11, M12, M13,
        M21, M22, M23
    } = Elements,
    {mat, 3, 2, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 6, D),

    E11 = glm:mat3x2_element(Matrix, 1, 1),
    E12 = glm:mat3x2_element(Matrix, 2, 1),
    E13 = glm:mat3x2_element(Matrix, 3, 1),
    E21 = glm:mat3x2_element(Matrix, 1, 2),
    E22 = glm:mat3x2_element(Matrix, 2, 2),
    E23 = glm:mat3x2_element(Matrix, 3, 2),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E13 - M13) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E23 - M23) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E13 - M13) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E23 - M23) < 1.0e-12)
    end,

    ok.

assert_mat3x4(T, Matrix, Elements) ->
    {
        M11, M12, M13,
        M21, M22, M23,
        M31, M32, M33,
        M41, M42, M43
    } = Elements,
    {mat, 3, 4, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 12, D),

    E11 = glm:mat3x4_element(Matrix, 1, 1),
    E12 = glm:mat3x4_element(Matrix, 2, 1),
    E13 = glm:mat3x4_element(Matrix, 3, 1),
    E21 = glm:mat3x4_element(Matrix, 1, 2),
    E22 = glm:mat3x4_element(Matrix, 2, 2),
    E23 = glm:mat3x4_element(Matrix, 3, 2),
    E31 = glm:mat3x4_element(Matrix, 1, 3),
    E32 = glm:mat3x4_element(Matrix, 2, 3),
    E33 = glm:mat3x4_element(Matrix, 3, 3),
    E41 = glm:mat3x4_element(Matrix, 1, 4),
    E42 = glm:mat3x4_element(Matrix, 2, 4),
    E43 = glm:mat3x4_element(Matrix, 3, 4),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E13 - M13) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E23 - M23) < 1.0e-9),
            ?assert(erlang:abs(E31 - M31) < 1.0e-9),
            ?assert(erlang:abs(E32 - M32) < 1.0e-9),
            ?assert(erlang:abs(E33 - M33) < 1.0e-9),
            ?assert(erlang:abs(E41 - M41) < 1.0e-9),
            ?assert(erlang:abs(E42 - M42) < 1.0e-9),
            ?assert(erlang:abs(E43 - M43) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E13 - M13) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E23 - M23) < 1.0e-12),
            ?assert(erlang:abs(E31 - M31) < 1.0e-12),
            ?assert(erlang:abs(E32 - M32) < 1.0e-12),
            ?assert(erlang:abs(E33 - M33) < 1.0e-12),
            ?assert(erlang:abs(E41 - M41) < 1.0e-12),
            ?assert(erlang:abs(E42 - M42) < 1.0e-12),
            ?assert(erlang:abs(E43 - M43) < 1.0e-12)
    end,

    ok.

assert_mat4x2(T, Matrix, Elements) ->
    {
        M11, M12, M13, M14,
        M21, M22, M23, M24
    } = Elements,
    {mat, 4, 2, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 8, D),

    E11 = glm:mat4x2_element(Matrix, 1, 1),
    E12 = glm:mat4x2_element(Matrix, 2, 1),
    E13 = glm:mat4x2_element(Matrix, 3, 1),
    E14 = glm:mat4x2_element(Matrix, 4, 1),
    E21 = glm:mat4x2_element(Matrix, 1, 2),
    E22 = glm:mat4x2_element(Matrix, 2, 2),
    E23 = glm:mat4x2_element(Matrix, 3, 2),
    E24 = glm:mat4x2_element(Matrix, 4, 2),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E13 - M13) < 1.0e-9),
            ?assert(erlang:abs(E14 - M14) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E23 - M23) < 1.0e-9),
            ?assert(erlang:abs(E24 - M24) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E13 - M13) < 1.0e-12),
            ?assert(erlang:abs(E14 - M14) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E23 - M23) < 1.0e-12),
            ?assert(erlang:abs(E24 - M24) < 1.0e-12)
    end,

    ok.

assert_mat4x3(T, Matrix, Elements) ->
    {
        M11, M12, M13, M14,
        M21, M22, M23, M24,
        M31, M32, M33, M34
    } = Elements,
    {mat, 4, 3, T, D} = Matrix,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(T, 12, D),

    E11 = glm:mat4x3_element(Matrix, 1, 1),
    E12 = glm:mat4x3_element(Matrix, 2, 1),
    E13 = glm:mat4x3_element(Matrix, 3, 1),
    E14 = glm:mat4x3_element(Matrix, 4, 1),
    E21 = glm:mat4x3_element(Matrix, 1, 2),
    E22 = glm:mat4x3_element(Matrix, 2, 2),
    E23 = glm:mat4x3_element(Matrix, 3, 2),
    E24 = glm:mat4x3_element(Matrix, 4, 2),
    E31 = glm:mat4x3_element(Matrix, 1, 3),
    E32 = glm:mat4x3_element(Matrix, 2, 3),
    E33 = glm:mat4x3_element(Matrix, 3, 3),
    E34 = glm:mat4x3_element(Matrix, 4, 3),

    case T of
        float ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-9),
            ?assert(erlang:abs(E12 - M12) < 1.0e-9),
            ?assert(erlang:abs(E13 - M13) < 1.0e-9),
            ?assert(erlang:abs(E14 - M14) < 1.0e-9),
            ?assert(erlang:abs(E21 - M21) < 1.0e-9),
            ?assert(erlang:abs(E22 - M22) < 1.0e-9),
            ?assert(erlang:abs(E23 - M23) < 1.0e-9),
            ?assert(erlang:abs(E24 - M24) < 1.0e-9),
            ?assert(erlang:abs(E31 - M31) < 1.0e-9),
            ?assert(erlang:abs(E32 - M32) < 1.0e-9),
            ?assert(erlang:abs(E33 - M33) < 1.0e-9),
            ?assert(erlang:abs(E34 - M34) < 1.0e-9);
        double ->
            ?assert(erlang:abs(E11 - M11) < 1.0e-12),
            ?assert(erlang:abs(E12 - M12) < 1.0e-12),
            ?assert(erlang:abs(E13 - M13) < 1.0e-12),
            ?assert(erlang:abs(E14 - M14) < 1.0e-12),
            ?assert(erlang:abs(E21 - M21) < 1.0e-12),
            ?assert(erlang:abs(E22 - M22) < 1.0e-12),
            ?assert(erlang:abs(E23 - M23) < 1.0e-12),
            ?assert(erlang:abs(E24 - M24) < 1.0e-12),
            ?assert(erlang:abs(E31 - M31) < 1.0e-12),
            ?assert(erlang:abs(E32 - M32) < 1.0e-12),
            ?assert(erlang:abs(E33 - M33) < 1.0e-12),
            ?assert(erlang:abs(E34 - M34) < 1.0e-12)
    end,

    ok.

assert_byte_size(T, N, D) ->
    ByteSize = case T of
        bool -> ?BOOL_BYTE_SIZE;
        {int, 8} -> ?INT8_BYTE_SIZE;
        {int, 16} -> ?INT16_BYTE_SIZE;
        {int, 32} -> ?INT32_BYTE_SIZE;
        {int, 64} -> ?INT64_BYTE_SIZE;
        {uint, 8} -> ?UINT8_BYTE_SIZE;
        {uint, 16} -> ?UINT16_BYTE_SIZE;
        {uint, 32} -> ?UINT32_BYTE_SIZE;
        {uint, 64} -> ?UINT64_BYTE_SIZE;
        float -> ?FLOAT_BYTE_SIZE;
        double -> ?DOUBLE_BYTE_SIZE
    end * N,
    ?assertEqual(ByteSize, erlang:byte_size(D)),
    ok.