-module(glm_angle_test).
-include("test_glm.hrl").
-include_lib("eunit/include/eunit.hrl").

acos_test() ->
    HalfPi = math:pi() / 2.0,
    ok = test_glm:assert_double(glm_angle:acos(glm:double(1.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:acos(glm:vec2(double, 1.0, 0.0)), {0.0, HalfPi}),

    ?assertException(error, function_clause, glm_angle:acos(glm:int32(1))),
    ?assertException(error, function_clause, glm_angle:acos(glm:vec2({int, 32}, 1, 0))),

    ok.

acosh_test() ->
    Acosh2 = math:log(2.0 + math:sqrt(3.0)),
    ok = test_glm:assert_double(glm_angle:acosh(glm:double(1.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:acosh(glm:vec2(double, 1.0, 2.0)), {0.0, Acosh2}),

    ?assertException(error, function_clause, glm_angle:acosh(glm:uint32(1))),
    ?assertException(error, function_clause, glm_angle:acosh(glm:vec2({uint, 32}, 1, 2))),

    ok.

asin_test() ->
    HalfPi = math:pi() / 2.0,
    ok = test_glm:assert_double(glm_angle:asin(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:asin(glm:vec2(double, 0.0, 1.0)), {0.0, HalfPi}),

    ?assertException(error, function_clause, glm_angle:asin(glm:int32(0))),
    ?assertException(error, function_clause, glm_angle:asin(glm:vec2({int, 32}, 0, 1))),

    ok.

asinh_test() ->
    Asinh1 = math:log(1.0 + math:sqrt(2.0)),
    ok = test_glm:assert_double(glm_angle:asinh(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:asinh(glm:vec2(double, 0.0, 1.0)), {0.0, Asinh1}),

    ?assertException(error, function_clause, glm_angle:asinh(glm:int32(0))),
    ?assertException(error, function_clause, glm_angle:asinh(glm:vec2({int, 32}, 0, 1))),

    ok.

atan_test() ->
    QuarterPi = math:pi() / 4.0,
    ok = test_glm:assert_double(glm_angle:atan(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:atan(glm:vec2(double, 0.0, 1.0)), {0.0, QuarterPi}),

    ?assertException(error, function_clause, glm_angle:atan(glm:uint32(0))),
    ?assertException(error, function_clause, glm_angle:atan(glm:vec2({uint, 32}, 0, 1))),

    ok.

atanh_test() ->
    AtanhHalf = math:log(3.0) / 2.0,
    ok = test_glm:assert_double(glm_angle:atanh(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:atanh(glm:vec2(double, 0.0, 0.5)), {0.0, AtanhHalf}),

    ?assertException(error, function_clause, glm_angle:atanh(glm:int32(0))),
    ?assertException(error, function_clause, glm_angle:atanh(glm:vec2({int, 32}, 0, 1))),

    ok.

cos_test() ->
    ok = test_glm:assert_float(glm_angle:cos(glm:float(0.0)), 1.0),
    ok = test_glm:assert_vec2(double, glm_angle:cos(glm:vec2(double, 0.0, math:pi())), {1.0, -1.0}),

    ?assertException(error, function_clause, glm_angle:cos(glm:int32(0))),
    ?assertException(error, function_clause, glm_angle:cos(glm:vec2({int, 32}, 0, 1))),

    ok.

cosh_test() ->
    Cosh1 = (math:exp(1.0) + math:exp(-1.0)) / 2.0,
    ok = test_glm:assert_double(glm_angle:cosh(glm:double(0.0)), 1.0),
    ok = test_glm:assert_vec2(double, glm_angle:cosh(glm:vec2(double, 0.0, 1.0)), {1.0, Cosh1}),

    ?assertException(error, function_clause, glm_angle:cosh(glm:uint32(0))),
    ?assertException(error, function_clause, glm_angle:cosh(glm:vec2({uint, 32}, 0, 1))),

    ok.

degrees_test() ->
    ok = test_glm:assert_double(glm_angle:degrees(glm:double(math:pi())), 180.0),
    ok = test_glm:assert_vec2(double, glm_angle:degrees(glm:vec2(double, math:pi() / 2.0, math:pi())), {90.0, 180.0}),

    ?assertException(error, function_clause, glm_angle:degrees(glm:int32(180))),
    ?assertException(error, function_clause, glm_angle:degrees(glm:vec2({int, 32}, 90, 180))),

    ok.

radians_test() ->
    Pi = math:pi(),
    ok = test_glm:assert_double(glm_angle:radians(glm:double(180.0)), Pi),
    ok = test_glm:assert_vec2(double, glm_angle:radians(glm:vec2(double, 90.0, 180.0)), {Pi / 2.0, Pi}),

    ?assertException(error, function_clause, glm_angle:radians(glm:uint32(180))),
    ?assertException(error, function_clause, glm_angle:radians(glm:vec2({uint, 32}, 90, 180))),

    ok.

sin_test() ->
    ok = test_glm:assert_double(glm_angle:sin(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:sin(glm:vec2(double, 0.0, math:pi() / 2.0)), {0.0, 1.0}),

    ?assertException(error, function_clause, glm_angle:sin(glm:int32(0))),
    ?assertException(error, function_clause, glm_angle:sin(glm:vec2({int, 32}, 0, 1))),

    ok.

sinh_test() ->
    Sinh1 = (math:exp(1.0) - math:exp(-1.0)) / 2.0,
    ok = test_glm:assert_double(glm_angle:sinh(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:sinh(glm:vec2(double, 0.0, 1.0)), {0.0, Sinh1}),

    ?assertException(error, function_clause, glm_angle:sinh(glm:uint32(0))),
    ?assertException(error, function_clause, glm_angle:sinh(glm:vec2({uint, 32}, 0, 1))),

    ok.

tan_test() ->
    ok = test_glm:assert_double(glm_angle:tan(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:tan(glm:vec2(double, 0.0, math:pi() / 4.0)), {0.0, 1.0}),

    ?assertException(error, function_clause, glm_angle:tan(glm:int32(0))),
    ?assertException(error, function_clause, glm_angle:tan(glm:vec2({int, 32}, 0, 1))),

    ok.

tanh_test() ->
    Sinh1 = (math:exp(1.0) - math:exp(-1.0)) / 2.0,
    Cosh1 = (math:exp(1.0) + math:exp(-1.0)) / 2.0,
    ok = test_glm:assert_double(glm_angle:tanh(glm:double(0.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_angle:tanh(glm:vec2(double, 0.0, 1.0)), {0.0, Sinh1 / Cosh1}),

    ?assertException(error, function_clause, glm_angle:tanh(glm:uint32(0))),
    ?assertException(error, function_clause, glm_angle:tanh(glm:vec2({uint, 32}, 0, 1))),

    ok.