-module(glm_exponential_test).
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

exp_test() ->
    ok = test_glm:assert_float(glm_exponential:exp(glm:float(0.0)), 1.0),
    ok = test_glm:assert_double(glm_exponential:exp(glm:double(1.0)), 2.718281828459045),
    ok = test_glm:assert_vec2(double, glm_exponential:exp(glm:vec2(double, 0.0, 1.0)), {1.0, 2.718281828459045}),

    ?assertException(error, function_clause, glm_exponential:exp(glm:int32(1))),
    ?assertException(error, function_clause, glm_exponential:exp(glm:vec2({int, 32}, 1, 2))),

    ok.

exp2_test() ->
    ok = test_glm:assert_float(glm_exponential:exp2(glm:float(3.0)), 8.0),
    ok = test_glm:assert_double(glm_exponential:exp2(glm:double(1.0)), 2.0),
    ok = test_glm:assert_vec3(double, glm_exponential:exp2(glm:vec3(double, 0.0, 1.0, 2.0)), {1.0, 2.0, 4.0}),

    ?assertException(error, function_clause, glm_exponential:exp2(glm:uint32(1))),
    ?assertException(error, function_clause, glm_exponential:exp2(glm:vec3({uint, 32}, 1, 2, 3))),

    ok.

inverse_sqrt_test() ->
    ok = test_glm:assert_float(glm_exponential:inverse_sqrt(glm:float(4.0)), 0.5),
    ok = test_glm:assert_double(glm_exponential:inverse_sqrt(glm:double(0.25)), 2.0),
    ok = test_glm:assert_vec2(float, glm_exponential:inverse_sqrt(glm:vec2(float, 1.0, 4.0)), {1.0, 0.5}),

    ?assertException(error, function_clause, glm_exponential:inverse_sqrt(glm:int32(4))),
    ?assertException(error, function_clause, glm_exponential:inverse_sqrt(glm:vec2({int, 32}, 1, 4))),

    ok.

log_test() ->
    ok = test_glm:assert_float(glm_exponential:log(glm:float(1.0)), 0.0),
    ok = test_glm:assert_double(glm_exponential:log(glm:double(7.38905609893065)), 2.0),
    ok = test_glm:assert_vec2(double, glm_exponential:log(glm:vec2(double, 1.0, 2.718281828459045)), {0.0, 1.0}),

    ?assertException(error, function_clause, glm_exponential:log(glm:int32(1))),
    ?assertException(error, function_clause, glm_exponential:log(glm:vec2({int, 32}, 1, 2))),

    ok.

log2_test() ->
    ok = test_glm:assert_float(glm_exponential:log2(glm:float(8.0)), 3.0),
    ok = test_glm:assert_double(glm_exponential:log2(glm:double(1.0)), 0.0),
    ok = test_glm:assert_vec2(double, glm_exponential:log2(glm:vec2(double, 1.0, 4.0)), {0.0, 2.0}),

    ?assertException(error, function_clause, glm_exponential:log2(glm:uint32(8))),
    ?assertException(error, function_clause, glm_exponential:log2(glm:vec2({uint, 32}, 1, 4))),

    ok.

pow_test() ->
    ok = test_glm:assert_float(glm_exponential:pow(glm:float(2.0), glm:float(3.0)), 8.0),
    ok = test_glm:assert_double(glm_exponential:pow(glm:double(9.0), glm:double(0.5)), 3.0),
    ok = test_glm:assert_vec2(float, glm_exponential:pow(glm:vec2(float, 2.0, 9.0), glm:vec2(float, 3.0, 0.5)), {8.0, 3.0}),

    ?assertException(error, function_clause, glm_exponential:pow(glm:int32(2), glm:int32(3))),
    ?assertException(error, _, glm_exponential:pow(glm:float(2.0), glm:double(3.0))),
    ?assertException(error, function_clause, glm_exponential:pow(glm:vec2(float, 2.0, 3.0), glm:float(2.0))),

    ok.

sqrt_test() ->
    ok = test_glm:assert_float(glm_exponential:sqrt(glm:float(9.0)), 3.0),
    ok = test_glm:assert_double(glm_exponential:sqrt(glm:double(0.25)), 0.5),
    ok = test_glm:assert_vec3(double, glm_exponential:sqrt(glm:vec3(double, 1.0, 4.0, 9.0)), {1.0, 2.0, 3.0}),

    ?assertException(error, function_clause, glm_exponential:sqrt(glm:int32(9))),
    ?assertException(error, function_clause, glm_exponential:sqrt(glm:vec3({int, 32}, 1, 4, 9))),

    ok.