-module(glm_relational_test).
-include("test_glm.hrl").
-include_lib("eunit/include/eunit.hrl").

all_test() ->
    ok = test_glm:assert_bool(glm_relational:all(glm:vec3(bool, true, true, true)), true),
    ok = test_glm:assert_bool(glm_relational:all(glm:vec3(bool, true, false, true)), false),

    ?assertException(error, function_clause, glm_relational:all(glm:vec3(double, 1.0, 0.0, 1.0))),

    ok.

any_test() ->
    ok = test_glm:assert_bool(glm_relational:any(glm:vec4(bool, false, false, false, false)), false),
    ok = test_glm:assert_bool(glm_relational:any(glm:vec4(bool, false, true, false, false)), true),

    ?assertException(error, function_clause, glm_relational:any(glm:vec2({int, 32}, 0, 1))),

    ok.

equal_test() ->
    ok = test_glm:assert_vec3(bool,
        glm_relational:equal(
            glm:vec3(double, 1.0, 2.0, 3.0),
            glm:vec3(double, 1.0, 0.0, 3.0)
        ),
        {true, false, true}
    ),
    ok = test_glm:assert_vec2(bool,
        glm_relational:equal(
            glm:vec2(bool, true, false),
            glm:vec2(bool, true, true)
        ),
        {true, false}
    ),

    ?assertException(error, _, glm_relational:equal(glm:vec2(double, 1.0, 2.0), glm:vec3(double, 1.0, 2.0, 3.0))),

    ok.

greater_than_test() ->
    ok = test_glm:assert_vec3(bool,
        glm_relational:greater_than(
            glm:vec3({int, 32}, 3, 1, 2),
            glm:vec3({int, 32}, 2, 1, 5)
        ),
        {true, false, false}
    ),

    ?assertException(error, function_clause, glm_relational:greater_than(glm:vec2(bool, true, false), glm:vec2(bool, false, false))),
    ?assertException(error, _, glm_relational:greater_than(glm:vec2(double, 1.0, 2.0), glm:vec2(float, 1.0, 2.0))),

    ok.

greater_than_equal_test() ->
    ok = test_glm:assert_vec2(bool,
        glm_relational:greater_than_equal(
            glm:vec2(double, 2.0, 1.0),
            glm:vec2(double, 1.0, 1.0)
        ),
        {true, true}
    ),

    ?assertException(error, function_clause, glm_relational:greater_than_equal(glm:vec2(bool, true, false), glm:vec2(bool, true, false))),

    ok.

less_than_test() ->
    ok = test_glm:assert_vec3(bool,
        glm_relational:less_than(
            glm:vec3({uint, 32}, 1, 5, 2),
            glm:vec3({uint, 32}, 2, 5, 1)
        ),
        {true, false, false}
    ),

    ?assertException(error, function_clause, glm_relational:less_than(glm:vec2(bool, false, true), glm:vec2(bool, true, true))),

    ok.

less_than_equal_test() ->
    ok = test_glm:assert_vec4(bool,
        glm_relational:less_than_equal(
            glm:vec4(double, 1.0, 2.0, 3.0, 4.0),
            glm:vec4(double, 1.0, 1.0, 3.0, 5.0)
        ),
        {true, false, true, true}
    ),

    ?assertException(error, function_clause, glm_relational:less_than_equal(glm:vec2(bool, true, false), glm:vec2(bool, true, true))),

    ok.

not_test() ->
    ok = test_glm:assert_vec3(bool, glm_relational:'not'(glm:vec3(bool, true, false, true)), {false, true, false}),

    ?assertException(error, function_clause, glm_relational:'not'(glm:vec3(double, 1.0, 0.0, 1.0))),

    ok.

not_equal_test() ->
    ok = test_glm:assert_vec3(bool,
        glm_relational:not_equal(
            glm:vec3({int, 16}, 1, 2, 3),
            glm:vec3({int, 16}, 0, 2, 4)
        ),
        {true, false, true}
    ),
    ok = test_glm:assert_vec2(bool,
        glm_relational:not_equal(
            glm:vec2(bool, true, false),
            glm:vec2(bool, false, false)
        ),
        {true, false}
    ),

    ?assertException(error, _, glm_relational:not_equal(glm:vec2(double, 1.0, 2.0), glm:vec2(float, 1.0, 2.0))),

    ok.