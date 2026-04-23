-module(glm_packing_test).
-include_lib("eunit/include/eunit.hrl").

pack_double_2x32_test() ->
    Packed = glm_packing:pack_double_2x32(glm:vec2({uint, 32}, 16#01234567, 16#89ABCDEF)),
    Roundtrip = glm_packing:unpack_double_2x32(Packed),

    ok = test_glm:assert_double(Packed, glm:double_value(Packed)),
    ok = test_glm:assert_vec2({uint, 32}, Roundtrip, {16#01234567, 16#89ABCDEF}),

    ok.

pack_half_1x16_test() ->
    ok = test_glm:assert_uint16(
        glm_packing:pack_half_1x16(glm:float(1.0)),
        16#3C00),

    ok.

pack_half_2x16_test() ->
    ok = test_glm:assert_uint32(
        glm_packing:pack_half_2x16(glm:vec2(float, 1.0, 2.0)),
        16#40003C00),

    ok.

pack_half_4x16_test() ->
    ok = test_glm:assert_uint64(
        glm_packing:pack_half_4x16(glm:vec4(float, 1.0, 2.0, 0.0, -2.0)),
        16#C000000040003C00),

    ok.

pack_snorm_1x8_test() ->
    ok = test_glm:assert_uint8(
        glm_packing:pack_snorm_1x8(glm:float(1.0)),
        16#7F),

    ok.

pack_snorm_2x8_test() ->
    ok = test_glm:assert_uint16(
        glm_packing:pack_snorm_2x8(glm:vec2(float, 1.0, 0.0)),
        16#007F),

    ok.

pack_snorm_1x16_test() ->
    ok = test_glm:assert_uint16(
        glm_packing:pack_snorm_1x16(glm:float(1.0)),
        16#7FFF),

    ok.

pack_snorm_2x16_test() ->
    ok = test_glm:assert_uint32(
        glm_packing:pack_snorm_2x16(glm:vec2(float, 1.0, 0.0)),
        16#00007FFF),

    ok.

pack_snorm_4x8_test() ->
    ok = test_glm:assert_uint32(
        glm_packing:pack_snorm_4x8(glm:vec4(float, 1.0, 0.0, -0.5, -1.0)),
        16#81C0007F),

    ok.

pack_snorm_4x16_test() ->
    ok = test_glm:assert_uint64(
        glm_packing:pack_snorm_4x16(glm:vec4(float, 1.0, 0.0, -0.5, -1.0)),
        16#8001C00000007FFF),

    ok.

pack_unorm_1x8_test() ->
    ok = test_glm:assert_uint8(
        glm_packing:pack_unorm_1x8(glm:float(1.0)),
        16#FF),

    ok.

pack_unorm_2x8_test() ->
    ok = test_glm:assert_uint16(
        glm_packing:pack_unorm_2x8(glm:vec2(float, 1.0, 0.5)),
        16#80FF),

    ok.

pack_unorm_1x16_test() ->
    ok = test_glm:assert_uint16(
        glm_packing:pack_unorm_1x16(glm:float(1.0)),
        16#FFFF),

    ok.

pack_unorm_2x16_test() ->
    ok = test_glm:assert_uint32(
        glm_packing:pack_unorm_2x16(glm:vec2(float, 1.0, 0.0)),
        16#0000FFFF),

    ok.

pack_unorm_4x8_test() ->
    ok = test_glm:assert_uint32(
        glm_packing:pack_unorm_4x8(glm:vec4(float, 1.0, 0.5, 0.0, 1.0)),
        16#FF0080FF),

    ok.

pack_unorm_4x16_test() ->
    ok = test_glm:assert_uint64(
        glm_packing:pack_unorm_4x16(glm:vec4(float, 1.0, 0.5, 0.0, 1.0)),
        16#FFFF00008000FFFF),

    ok.

unpack_double_2x32_test() ->
    ok = test_glm:assert_vec2(
        {uint, 32},
        glm_packing:unpack_double_2x32(glm_packing:pack_double_2x32(glm:vec2({uint, 32}, 1, 2))),
        {1, 2}),

    ok.

unpack_half_1x16_test() ->
    ok = test_glm:assert_float(
        glm_packing:unpack_half_1x16(glm:uint16(16#3C00)),
        1.0),

    ok.

unpack_half_2x16_test() ->
    ok = test_glm:assert_vec2(float,
        glm_packing:unpack_half_2x16(glm:uint32(16#40003C00)),
        {1.0, 2.0}),

    ok.

unpack_half_4x16_test() ->
    ok = test_glm:assert_vec4(float,
        glm_packing:unpack_half_4x16(glm:uint64(16#C000000040003C00)),
        {1.0, 2.0, 0.0, -2.0}),

    ok.

unpack_snorm_1x8_test() ->
    V = glm_packing:unpack_snorm_1x8(glm:uint8(16#C0)),

    assert_float_close(glm:float_value(V), -64.0 / 127.0, 1.0e-7),

    ok.

unpack_snorm_2x8_test() ->
    V = glm_packing:unpack_snorm_2x8(glm:uint16(16#C07F)),

    assert_vec2_close(V, {1.0, -64.0 / 127.0}, 1.0e-7),

    ok.

unpack_snorm_1x16_test() ->
    V = glm_packing:unpack_snorm_1x16(glm:uint16(16#C000)),

    assert_float_close(glm:float_value(V), -16384.0 / 32767.0, 1.0e-6),

    ok.

unpack_snorm_2x16_test() ->
    ok = test_glm:assert_vec2(float,
        glm_packing:unpack_snorm_2x16(glm:uint32(16#00007FFF)),
        {1.0, 0.0}),

    ok.

unpack_snorm_4x8_test() ->
    V = glm_packing:unpack_snorm_4x8(glm:uint32(16#81C0007F)),

    ?assert(erlang:abs(glm:vec4_x(V) - 1.0) < 1.0e-9),
    ?assert(erlang:abs(glm:vec4_y(V) - 0.0) < 1.0e-9),
    ?assert(erlang:abs(glm:vec4_z(V) - (-64.0 / 127.0)) < 1.0e-7),
    ?assert(erlang:abs(glm:vec4_w(V) - (-1.0)) < 1.0e-9),

    ok.

unpack_snorm_4x16_test() ->
    V = glm_packing:unpack_snorm_4x16(glm:uint64(16#8001C00000007FFF)),

    assert_vec4_close(V, {1.0, 0.0, -16384.0 / 32767.0, -1.0}, 1.0e-6),

    ok.

unpack_unorm_1x8_test() ->
    V = glm_packing:unpack_unorm_1x8(glm:uint8(16#80)),

    assert_float_close(glm:float_value(V), 128.0 / 255.0, 1.0e-7),

    ok.

unpack_unorm_2x8_test() ->
    V = glm_packing:unpack_unorm_2x8(glm:uint16(16#80FF)),

    assert_vec2_close(V, {1.0, 128.0 / 255.0}, 1.0e-7),

    ok.

unpack_unorm_1x16_test() ->
    V = glm_packing:unpack_unorm_1x16(glm:uint16(16#8000)),

    assert_float_close(glm:float_value(V), 32768.0 / 65535.0, 1.0e-7),

    ok.

unpack_unorm_2x16_test() ->
    ok = test_glm:assert_vec2(float,
        glm_packing:unpack_unorm_2x16(glm:uint32(16#0000FFFF)),
        {1.0, 0.0}),

    ok.

unpack_unorm_4x8_test() ->
    V = glm_packing:unpack_unorm_4x8(glm:uint32(16#FF0080FF)),

    ?assert(erlang:abs(glm:vec4_x(V) - 1.0) < 1.0e-9),
    ?assert(erlang:abs(glm:vec4_y(V) - (128.0 / 255.0)) < 1.0e-7),
    ?assert(erlang:abs(glm:vec4_z(V) - 0.0) < 1.0e-9),
    ?assert(erlang:abs(glm:vec4_w(V) - 1.0) < 1.0e-9),

    ok.

unpack_unorm_4x16_test() ->
    V = glm_packing:unpack_unorm_4x16(glm:uint64(16#FFFF00008000FFFF)),

    assert_vec4_close(V, {1.0, 32768.0 / 65535.0, 0.0, 1.0}, 1.0e-7),

    ok.

assert_float_close(Actual, Expected, Tolerance) ->
    ?assert(erlang:abs(Actual - Expected) < Tolerance).

assert_vec2_close(V, {X, Y}, Tolerance) ->
    assert_float_close(glm:vec2_x(V), X, Tolerance),
    assert_float_close(glm:vec2_y(V), Y, Tolerance).

assert_vec4_close(V, {X, Y, Z, W}, Tolerance) ->
    assert_float_close(glm:vec4_x(V), X, Tolerance),
    assert_float_close(glm:vec4_y(V), Y, Tolerance),
    assert_float_close(glm:vec4_z(V), Z, Tolerance),
    assert_float_close(glm:vec4_w(V), W, Tolerance).