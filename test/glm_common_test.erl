-module(glm_common_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

abs_test() ->
    ok = test_glm:assert_int32(glm_common:abs(glm:int32(-1)), 1),
    ok = test_glm:assert_float(glm_common:abs(glm:float(-1.0)), 1.0),
    ok = test_glm:assert_double(glm_common:abs(glm:double(-1.0)), 1.0),

    ok = test_glm:assert_vec2({int, 32}, glm_common:abs(glm:vec2({int, 32}, -1, -2)), {1, 2}),
    ok = test_glm:assert_vec3(float, glm_common:abs(glm:vec3(float, -1.0, -2.0, -3.0)), {1.0, 2.0, 3.0}),
    ok = test_glm:assert_vec4(double, glm_common:abs(glm:vec4(double, -1.0, -2.0, -3.0, -4.0)), {1.0, 2.0, 3.0, 4.0}),

    ?assertException(error, function_clause, glm_common:abs(glm:bool(true))),
    ?assertException(error, function_clause, glm_common:abs(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:abs(glm:vec2(bool, true, false))),
    ?assertException(error, function_clause, glm_common:abs(glm:vec2({uint, 32}, 1, 2))),

    ok.

ceil_test() ->
    ok = test_glm:assert_float(glm_common:ceil(glm:float(1.2)), 2.0),
    ok = test_glm:assert_double(glm_common:ceil(glm:double(-2.8)), -2.0),
    ok = test_glm:assert_vec2(float, glm_common:ceil(glm:vec2(float, 1.2, -2.8)), {2.0, -2.0}),
    ok = test_glm:assert_vec3(double, glm_common:ceil(glm:vec3(double, 2.1, -1.9, 0.0)), {3.0, -1.0, +0.0}),

    ?assertException(error, function_clause, glm_common:ceil(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:ceil(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:ceil(glm:vec2({int, 32}, 1, 2))),
    ?assertException(error, function_clause, glm_common:ceil(glm:vec3({uint, 32}, 1, 2, 3))),

    ok.

clamp_test() ->
    R1 = glm_common:clamp(glm:float(0.5), glm:float(0.0), glm:float(1.0)),
    0.5 = glm:float_value(R1),
    R2 = glm_common:clamp(glm:float(1.1), glm:float(0.0), glm:float(1.0)),
    1.0 = glm:float_value(R2),
    R3 = glm_common:clamp(glm:vec2(float, -0.1, 1.1), glm:float(0.0), glm:float(1.0)),
    +0.0 = glm:vec2_x(R3),
    1.0 = glm:vec2_y(R3),
    R4 = glm_common:clamp(
        glm:vec2(float, -0.1, 1.1),
        glm:vec2(float, 0.0, 0.0),
        glm:vec2(float, 1.0, 1.0)
    ),
    +0.0 = glm:vec2_x(R4),
    1.0 = glm:vec2_y(R4),

    ?assert(proper:quickcheck(prop_clamp())),

    ok.

float_bits_to_int_test() ->
    ok = test_glm:assert_int32(glm_common:float_bits_to_int(glm:float(1.0)), 1065353216),
    ok = test_glm:assert_vec2({int, 32}, glm_common:float_bits_to_int(glm:vec2(float, 0.0, -1.0)), {0, -1082130432}),

    ?assertException(error, function_clause, glm_common:float_bits_to_int(glm:double(1.0))),
    ?assertException(error, function_clause, glm_common:float_bits_to_int(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:float_bits_to_int(glm:vec2(double, 1.0, 2.0))),

    ok.

float_bits_to_uint_test() ->
    ok = test_glm:assert_uint32(glm_common:float_bits_to_uint(glm:float(1.0)), 16#3F800000),
    ok = test_glm:assert_vec2({uint, 32}, glm_common:float_bits_to_uint(glm:vec2(float, 0.0, -1.0)), {16#00000000, 16#BF800000}),

    ?assertException(error, function_clause, glm_common:float_bits_to_uint(glm:double(1.0))),
    ?assertException(error, function_clause, glm_common:float_bits_to_uint(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:float_bits_to_uint(glm:vec2(double, 1.0, 2.0))),

    ok.

floor_test() ->
    ok = test_glm:assert_float(glm_common:floor(glm:float(1.7)), 1.0),
    ok = test_glm:assert_double(glm_common:floor(glm:double(-2.3)), -3.0),
    ok = test_glm:assert_vec2(float, glm_common:floor(glm:vec2(float, 1.7, -2.3)), {1.0, -3.0}),
    ok = test_glm:assert_vec3(double, glm_common:floor(glm:vec3(double, 2.9, -1.1, 0.5)), {2.0, -2.0, +0.0}),

    ?assertException(error, function_clause, glm_common:floor(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:floor(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:floor(glm:vec2({int, 32}, 1, 2))),
    ?assertException(error, function_clause, glm_common:floor(glm:vec3({uint, 32}, 1, 2, 3))),

    ok.

fma_test() ->
    ok = test_glm:assert_float(glm_common:fma(glm:float(2.0), glm:float(3.0), glm:float(4.0)), 10.0),
    ok = test_glm:assert_double(glm_common:fma(glm:double(0.5), glm:double(8.0), glm:double(1.0)), 5.0),
    ok = test_glm:assert_vec2(float, glm_common:fma(glm:vec2(float, 1.0, 2.0), glm:vec2(float, 3.0, 4.0), glm:vec2(float, 5.0, 6.0)), {8.0, 14.0}),

    ?assertException(error, function_clause, glm_common:fma(glm:int32(1), glm:int32(2), glm:int32(3))),
    ?assertException(error, _, glm_common:fma(glm:float(1.0), glm:double(2.0), glm:float(3.0))),
    ?assertException(error, _, glm_common:fma(glm:vec2(float, 1.0, 2.0), glm:vec3(float, 1.0, 2.0, 3.0), glm:vec2(float, 4.0, 5.0))),

    ok.

fract_test() ->
    ok = test_glm:assert_float(glm_common:fract(glm:float(1.25)), 0.25),
    ok = test_glm:assert_double(glm_common:fract(glm:double(-1.25)), 0.75),
    ok = test_glm:assert_vec2(float, glm_common:fract(glm:vec2(float, 1.25, -1.25)), {0.25, 0.75}),
    ok = test_glm:assert_vec3(double, glm_common:fract(glm:vec3(double, 2.5, -2.25, 0.0)), {0.5, 0.75, +0.0}),

    ?assertException(error, function_clause, glm_common:fract(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:fract(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:fract(glm:vec2({int, 32}, 1, 2))),
    ?assertException(error, function_clause, glm_common:fract(glm:vec3({uint, 32}, 1, 2, 3))),

    ok.

frexp_test() ->
    {Significand1, Exponent1} = glm_common:frexp(glm:float(8.0)),
    ok = test_glm:assert_float(Significand1, 0.5),
    ok = test_glm:assert_int32(Exponent1, 4),
    {Significand2, Exponent2} = glm_common:frexp(glm:vec2(double, 8.0, -0.75)),
    ok = test_glm:assert_vec2(double, Significand2, {0.5, -0.75}),
    ok = test_glm:assert_vec2({int, 32}, Exponent2, {4, 0}),

    ?assertException(error, function_clause, glm_common:frexp(glm:int32(8))),
    ?assertException(error, function_clause, glm_common:frexp(glm:vec2({int, 32}, 8, 4))),

    ok.

int_bits_to_float_test() ->
    ok = test_glm:assert_float(glm_common:int_bits_to_float(glm:int32(16#3F800000)), 1.0),
    ok = test_glm:assert_vec2(float, glm_common:int_bits_to_float(glm:vec2({int, 32}, 0, -1082130432)), {0.0, -1.0}),

    ?assertException(error, function_clause, glm_common:int_bits_to_float(glm:uint32(16#3F800000))),
    ?assertException(error, function_clause, glm_common:int_bits_to_float(glm:float(1.0))),
    ?assertException(error, function_clause, glm_common:int_bits_to_float(glm:vec2({uint, 32}, 0, 1))),

    ok.

is_inf_test() ->
    FloatVector = {vec, 2, float, <<0, 0, 128, 63, 0, 0, 128, 127>>},
    DoubleVector = {vec, 3, double, <<0, 0, 0, 0, 0, 0, 240, 255, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 240, 63>>},
    ok = test_glm:assert_vec2(bool, glm_common:is_inf(FloatVector), {false, true}),
    ok = test_glm:assert_vec3(bool, glm_common:is_inf(DoubleVector), {true, false, false}),

    ?assertException(error, function_clause, glm_common:is_inf(glm:float(1.0))),
    ?assertException(error, function_clause, glm_common:is_inf(glm:vec2({int, 32}, 1, 2))),

    ok.

is_nan_test() ->
    FloatVector = {vec, 2, float, <<0, 0, 192, 127, 0, 0, 128, 63>>},
    DoubleVector = {vec, 3, double, <<0, 0, 0, 0, 0, 0, 240, 63, 0, 0, 0, 0, 0, 0, 248, 127, 0, 0, 0, 0, 0, 0, 0, 0>>},
    ok = test_glm:assert_vec2(bool, glm_common:is_nan(FloatVector), {true, false}),
    ok = test_glm:assert_vec3(bool, glm_common:is_nan(DoubleVector), {false, true, false}),

    ?assertException(error, function_clause, glm_common:is_nan(glm:float(1.0))),
    ?assertException(error, function_clause, glm_common:is_nan(glm:vec2({int, 32}, 1, 2))),

    ok.

ldexp_test() ->
    ok = test_glm:assert_float(glm_common:ldexp(glm:float(0.5), glm:int32(4)), 8.0),
    ok = test_glm:assert_double(glm_common:ldexp(glm:double(-0.75), glm:int32(0)), -0.75),
    ok = test_glm:assert_vec2(float, glm_common:ldexp(glm:vec2(float, 0.5, -0.75), glm:vec2({int, 32}, 4, 0)), {8.0, -0.75}),

    ?assertException(error, function_clause, glm_common:ldexp(glm:int32(1), glm:int32(2))),
    ?assertException(error, function_clause, glm_common:ldexp(glm:float(1.0), glm:uint32(2))),
    ?assertException(error, _, glm_common:ldexp(glm:vec2(float, 1.0, 2.0), glm:vec3({int, 32}, 1, 2, 3))),

    ok.

max_test() ->
    ok = test_glm:assert_int32(glm_common:max(glm:int32(3), glm:int32(1)), 3),
    ok = test_glm:assert_float(glm_common:max(glm:float(1.5), glm:float(2.5)), 2.5),
    ok = test_glm:assert_vec2({int, 32}, glm_common:max(glm:vec2({int, 32}, 3, -1), glm:int32(1)), {3, 1}),
    ok = test_glm:assert_vec3(float, glm_common:max(glm:vec3(float, 1.0, -2.0, 0.5), glm:vec3(float, 0.0, 3.0, 0.25)), {1.0, 3.0, 0.5}),

    ?assertException(error, function_clause, glm_common:max(glm:bool(true), glm:bool(false))),
    ?assertException(error, _, glm_common:max(glm:int32(1), glm:uint32(1))),
    ?assertException(error, _, glm_common:max(glm:vec2(float, 1.0, 2.0), glm:vec3(float, 1.0, 2.0, 3.0))),

    ok.

min_test() ->
    ok = test_glm:assert_int32(glm_common:min(glm:int32(3), glm:int32(1)), 1),
    ok = test_glm:assert_float(glm_common:min(glm:float(1.5), glm:float(2.5)), 1.5),
    ok = test_glm:assert_vec2({int, 32}, glm_common:min(glm:vec2({int, 32}, 3, -1), glm:int32(1)), {1, -1}),
    ok = test_glm:assert_vec3(float, glm_common:min(glm:vec3(float, 1.0, -2.0, 0.5), glm:vec3(float, 0.0, 3.0, 0.25)), {0.0, -2.0, 0.25}),

    ?assertException(error, function_clause, glm_common:min(glm:bool(true), glm:bool(false))),
    ?assertException(error, _, glm_common:min(glm:int32(1), glm:uint32(1))),
    ?assertException(error, _, glm_common:min(glm:vec2(float, 1.0, 2.0), glm:vec3(float, 1.0, 2.0, 3.0))),

    ok.

mix_test() ->
    ok = test_glm:assert_float(glm_common:mix(glm:float(0.0), glm:float(10.0), glm:float(0.25)), 2.5),
    ok = test_glm:assert_double(glm_common:mix(glm:double(2.0), glm:double(10.0), glm:double(0.5)), 6.0),
    ok = test_glm:assert_vec2(float, glm_common:mix(glm:vec2(float, 0.0, 10.0), glm:vec2(float, 10.0, 20.0), glm:float(0.5)), {5.0, 15.0}),
    ok = test_glm:assert_vec2(double, glm_common:mix(glm:vec2(double, 0.0, 10.0), glm:vec2(double, 10.0, 20.0), glm:vec2(double, 0.25, 0.5)), {2.5, 15.0}),

    ?assertException(error, function_clause, glm_common:mix(glm:int32(0), glm:int32(10), glm:int32(1))),
    ?assertException(error, function_clause, glm_common:mix(glm:vec2({int, 32}, 0, 10), glm:vec2({int, 32}, 10, 20), glm:int32(1))),
    ?assertException(error, _, glm_common:mix(glm:vec2(float, 0.0, 1.0), glm:vec2(float, 1.0, 2.0), glm:vec3(float, 0.1, 0.2, 0.3))),

    ok.

mod_test() ->
    ok = test_glm:assert_float(glm_common:mod(glm:float(5.5), glm:float(2.0)), 1.5),
    ok = test_glm:assert_double(glm_common:mod(glm:double(6.5), glm:double(4.0)), 2.5),
    ok = test_glm:assert_vec2(float, glm_common:mod(glm:vec2(float, 5.5, 6.5), glm:float(2.0)), {1.5, 0.5}),
    ok = test_glm:assert_vec2(double, glm_common:mod(glm:vec2(double, 5.5, 6.5), glm:vec2(double, 2.0, 4.0)), {1.5, 2.5}),

    ?assertException(error, function_clause, glm_common:mod(glm:int32(5), glm:int32(2))),
    ?assertException(error, function_clause, glm_common:mod(glm:vec2({int, 32}, 5, 6), glm:int32(2))),
    ?assertException(error, _, glm_common:mod(glm:vec2(float, 1.0, 2.0), glm:vec3(float, 1.0, 2.0, 3.0))),

    ok.

modf_test() ->
    {Fractional1, Integral1} = glm_common:modf(glm:float(1.25)),
    ok = test_glm:assert_float(Fractional1, 0.25),
    ok = test_glm:assert_float(Integral1, 1.0),
    {Fractional2, Integral2} = glm_common:modf(glm:double(-1.25)),
    ok = test_glm:assert_double(Fractional2, -0.25),
    ok = test_glm:assert_double(Integral2, -1.0),
    {Fractional3, Integral3} = glm_common:modf(glm:vec2(float, 1.25, -1.25)),
    ok = test_glm:assert_vec2(float, Fractional3, {0.25, -0.25}),
    ok = test_glm:assert_vec2(float, Integral3, {1.0, -1.0}),

    ?assertException(error, function_clause, glm_common:modf(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:modf(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:modf(glm:vec2({int, 32}, 1, 2))),
    ?assertException(error, function_clause, glm_common:modf(glm:vec3({uint, 32}, 1, 2, 3))),

    ok.

round_test() ->
    R1 = glm_common:round(glm:vec2(float, 0.1, 0.9)),
    +0.0 = glm:vec2_x(R1),
    1.0 = glm:vec2_y(R1),
    R2 = glm_common:round(glm:vec2(double, 0.1, 0.9)),
    +0.0 = glm:vec2_x(R2),
    1.0 = glm:vec2_y(R2),

    R3 = glm_common:round(glm:vec3(float, 0.1, 0.9, 0.1)),
    +0.0 = glm:vec3_x(R3),
    1.0 = glm:vec3_y(R3),
    +0.0 = glm:vec3_z(R3),
    R4 = glm_common:round(glm:vec3(double, 0.1, 0.9, 0.1)),
    +0.0 = glm:vec3_x(R4),
    1.0 = glm:vec3_y(R4),
    +0.0 = glm:vec3_z(R4),

    R5 = glm_common:round(glm:vec4(float, 0.1, 0.9, 0.1, 0.9)),
    +0.0 = glm:vec4_x(R5),
    1.0 = glm:vec4_y(R5),
    +0.0 = glm:vec4_z(R5),
    1.0 = glm:vec4_w(R5),
    R6 = glm_common:round(glm:vec4(double, 0.1, 0.9, 0.1, 0.9)),
    +0.0 = glm:vec4_x(R6),
    1.0 = glm:vec4_y(R6),
    +0.0 = glm:vec4_z(R6),
    1.0 = glm:vec4_w(R6),

    ?assertException(error, function_clause, glm_common:round(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:round(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:round(glm:vec2({int, 32}, 1, 2))),
    ?assertException(error, function_clause, glm_common:round(glm:vec3({uint, 32}, 1, 2, 3))),

    ok.

round_even_test() ->
    R1 = glm_common:round_even(glm:vec2(float, 3.5, 4.5)),
    4.0 = glm:vec2_x(R1),
    4.0 = glm:vec2_y(R1),
    R2 = glm_common:round_even(glm:vec2(double, 3.5, 4.5)),
    4.0 = glm:vec2_x(R2),
    4.0 = glm:vec2_y(R2),
    R3 = glm_common:round_even(glm:vec3(float, 3.5, 4.5, 5.5)),
    4.0 = glm:vec3_x(R3),
    4.0 = glm:vec3_y(R3),
    6.0 = glm:vec3_z(R3),
    R4 = glm_common:round_even(glm:vec3(double, 3.5, 4.5, 5.5)),
    4.0 = glm:vec3_x(R4),
    4.0 = glm:vec3_y(R4),
    6.0 = glm:vec3_z(R4),
    R5 = glm_common:round_even(glm:vec4(float, 3.5, 4.5, 5.5, 6.5)),
    4.0 = glm:vec4_x(R5),
    4.0 = glm:vec4_y(R5),
    6.0 = glm:vec4_z(R5),
    6.0 = glm:vec4_w(R5),
    R6 = glm_common:round_even(glm:vec4(double, 3.5, 4.5, 5.5, 6.5)),
    4.0 = glm:vec4_x(R6),
    4.0 = glm:vec4_y(R6),
    6.0 = glm:vec4_z(R6),
    6.0 = glm:vec4_w(R6),

    ?assertException(error, function_clause, glm_common:round_even(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:round_even(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:round_even(glm:vec2({int, 32}, 1, 2))),
    ?assertException(error, function_clause, glm_common:round_even(glm:vec3({uint, 32}, 1, 2, 3))),

    ok.

sign_test() ->
    ok = test_glm:assert_int32(glm_common:sign(glm:int32(-7)), -1),
    ok = test_glm:assert_float(glm_common:sign(glm:float(0.0)), 0.0),
    ok = test_glm:assert_double(glm_common:sign(glm:double(3.5)), 1.0),
    ok = test_glm:assert_vec2({int, 32}, glm_common:sign(glm:vec2({int, 32}, -4, 0)), {-1, 0}),
    ok = test_glm:assert_vec3(float, glm_common:sign(glm:vec3(float, -1.0, 0.0, 2.0)), {-1.0, 0.0, 1.0}),

    ?assertException(error, function_clause, glm_common:sign(glm:bool(true))),
    ?assertException(error, function_clause, glm_common:sign(glm:uint32(1))),
    ?assertException(error, function_clause, glm_common:sign(glm:vec2({uint, 32}, 1, 2))),

    ok.

smoothstep_test() ->
    ok = test_glm:assert_float(glm_common:smoothstep(glm:float(0.0), glm:float(1.0), glm:float(0.5)), 0.5),
    ok = test_glm:assert_vec2(float, glm_common:smoothstep(glm:float(0.0), glm:float(1.0), glm:vec2(float, -1.0, 2.0)), {0.0, 1.0}),
    ok = test_glm:assert_vec2(double, glm_common:smoothstep(glm:vec2(double, 0.0, 0.0), glm:vec2(double, 1.0, 2.0), glm:vec2(double, 0.5, 1.0)), {0.5, 0.5}),

    ?assertException(error, function_clause, glm_common:smoothstep(glm:int32(0), glm:int32(1), glm:int32(0))),
    ?assertException(error, _, glm_common:smoothstep(glm:float(0.0), glm:double(1.0), glm:vec3(float, 0.0, 0.5, 1.0))),

    ok.

step_test() ->
    ok = test_glm:assert_float(glm_common:step(glm:float(0.5), glm:float(0.25)), 0.0),
    ok = test_glm:assert_float(glm_common:step(glm:float(0.5), glm:float(0.75)), 1.0),
    ok = test_glm:assert_vec2(float, glm_common:step(glm:float(0.5), glm:vec2(float, 0.25, 0.75)), {0.0, 1.0}),
    ok = test_glm:assert_vec2(double, glm_common:step(glm:vec2(double, 0.5, 0.5), glm:vec2(double, 0.25, 0.75)), {0.0, 1.0}),

    ?assertException(error, function_clause, glm_common:step(glm:int32(1), glm:int32(2))),
    ?assertException(error, _, glm_common:step(glm:vec2(float, 0.0, 1.0), glm:vec3(float, 0.0, 1.0, 2.0))),

    ok.

trunc_test() ->
    ok = test_glm:assert_float(glm_common:trunc(glm:float(1.9)), 1.0),
    ok = test_glm:assert_double(glm_common:trunc(glm:double(-1.9)), -1.0),
    ok = test_glm:assert_vec2(float, glm_common:trunc(glm:vec2(float, 1.9, -1.9)), {1.0, -1.0}),
    ok = test_glm:assert_vec3(double, glm_common:trunc(glm:vec3(double, 2.9, -2.9, 0.0)), {2.0, -2.0, 0.0}),

    ?assertException(error, function_clause, glm_common:trunc(glm:int32(1))),
    ?assertException(error, function_clause, glm_common:trunc(glm:vec2({int, 32}, 1, 2))),

    ok.

uint_bits_to_float_test() ->
    ok = test_glm:assert_float(glm_common:uint_bits_to_float(glm:uint32(16#3F800000)), 1.0),
    ok = test_glm:assert_vec2(float, glm_common:uint_bits_to_float(glm:vec2({uint, 32}, 0, 16#BF800000)), {0.0, -1.0}),

    ?assertException(error, function_clause, glm_common:uint_bits_to_float(glm:int32(16#3F800000))),
    ?assertException(error, function_clause, glm_common:uint_bits_to_float(glm:float(1.0))),
    ?assertException(error, function_clause, glm_common:uint_bits_to_float(glm:vec2({int, 32}, 0, 1))),

    ok.

prop_clamp() ->
    ?FORALL(
        {T, L, T_Diff, V1, V2, V3, V_Diff},
        ?LET(
            {T, T_Diff, L},
            ?SUCHTHAT(
                {T1, T2, _Length},
                {test_glm:gen_type(), test_glm:gen_type(), test_glm:gen_vec_length()},
                T1 =/= T2
            ),
            ?LET(
                {V1, V2, V3, V_Diff},
                {test_glm:gen_vec(T, L), test_glm:gen_vec(T, L), test_glm:gen_vec(T, L), test_glm:gen_vec(T_Diff, L)},
                {T, L, T_Diff, V1, V2, V3, V_Diff}
            )
        ),
        begin
            {S1, S2, S3, S_Diff} = case L of
                2 ->
                    {
                        glm:scalar(T, glm:vec2_x(V1)),
                        glm:scalar(T, glm:vec2_x(V2)),
                        glm:scalar(T, glm:vec2_x(V3)),
                        glm:scalar(T_Diff, glm:vec2_x(V_Diff))
                    };
                3 ->
                    {
                        glm:scalar(T, glm:vec3_x(V1)),
                        glm:scalar(T, glm:vec3_x(V2)),
                        glm:scalar(T, glm:vec3_x(V3)),
                        glm:scalar(T_Diff, glm:vec3_x(V_Diff))
                    };
                4 ->
                    {
                        glm:scalar(T, glm:vec4_x(V1)),
                        glm:scalar(T, glm:vec4_x(V2)),
                        glm:scalar(T, glm:vec4_x(V3)),
                        glm:scalar(T_Diff, glm:vec4_x(V_Diff))
                    }
            end,

            ok = test_glm:assert_scalar(T, glm_common:clamp(S1, S2, S3)),
            ?assertException(error, _, glm_common:clamp(S_Diff, S2, S3)),
            ?assertException(error, _, glm_common:clamp(S1, S_Diff, S3)),
            ?assertException(error, _, glm_common:clamp(S1, S2, S_Diff)),

            ok = test_glm:assert_vec(T, L, glm_common:clamp(V1, S2, S3)),
            ?assertException(error, _, glm_common:clamp(V_Diff, S2, S3)),
            ?assertException(error, _, glm_common:clamp(V1, S_Diff, S3)),
            ?assertException(error, _, glm_common:clamp(V1, S2, S_Diff)),

            ok = test_glm:assert_vec(T, L, glm_common:clamp(V1, V2, V3)),
            ?assertException(error, _, glm_common:clamp(V_Diff, V2, V3)),
            ?assertException(error, _, glm_common:clamp(V1, V_Diff, V3)),
            ?assertException(error, _, glm_common:clamp(V1, V2, V_Diff)),

            true
        end
    ).