%%
%% Copyright (c) 2025, Byteplug LLC.
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
    assert_vec/3,
    assert_vec/4
]).

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
    ?assert(erlang:abs(glm:float_value(S) - V) < 1.0e-9),
    ok.

assert_double(S, V) ->
    {scalar, double, D} = S,
    ?assert(erlang:is_binary(D)),
    ok = assert_byte_size(double, 1, D),
    ?assert(erlang:abs(glm:double_value(S) - V) < 1.0e-12),
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
        double ->
            ?assert(erlang:abs(glm:double_value(V) - Expected) < 1.0e-12);
        _ ->
            ?assertEqual(Expected, glm:scalar_value(V))
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
