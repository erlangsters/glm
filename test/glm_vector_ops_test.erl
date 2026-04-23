-module(glm_vector_ops_test).
-include("test_glm.hrl").
-include_lib("eunit/include/eunit.hrl").

cross_test() ->
    ok = test_glm:assert_vec3(double,
        glm_vector:cross(
            glm:vec3(double, 1.0, 0.0, 0.0),
            glm:vec3(double, 0.0, 1.0, 0.0)
        ),
        {0.0, 0.0, 1.0}
    ),

    ?assertException(error, function_clause, glm_vector:cross(glm:vec2(double, 1.0, 0.0), glm:vec2(double, 0.0, 1.0))),
    ?assertException(error, function_clause, glm_vector:cross(glm:vec3({int, 32}, 1, 0, 0), glm:vec3({int, 32}, 0, 1, 0))),

    ok.

distance_test() ->
    ok = test_glm:assert_double(glm_vector:distance(glm:vec2(double, 0.0, 0.0), glm:vec2(double, 3.0, 4.0)), 5.0),
    ok = test_glm:assert_double(glm_vector:distance(glm:vec3(double, 1.0, 2.0, 2.0), glm:vec3(double, 0.0, 0.0, 0.0)), 3.0),

    ?assertException(error, function_clause, glm_vector:distance(glm:vec2({int, 32}, 0, 0), glm:vec2({int, 32}, 3, 4))),
    ?assertException(error, _, glm_vector:distance(glm:vec2(double, 0.0, 0.0), glm:vec3(double, 3.0, 4.0, 0.0))),

    ok.

dot_test() ->
    ok = test_glm:assert_double(glm_vector:dot(glm:vec2(double, 1.0, 2.0), glm:vec2(double, 3.0, 4.0)), 11.0),
    ok = test_glm:assert_double(glm_vector:dot(glm:vec3(double, 1.0, 2.0, 3.0), glm:vec3(double, 4.0, 5.0, 6.0)), 32.0),

    ?assertException(error, function_clause, glm_vector:dot(glm:vec2({int, 32}, 1, 2), glm:vec2({int, 32}, 3, 4))),
    ?assertException(error, _, glm_vector:dot(glm:vec2(double, 1.0, 2.0), glm:vec2(float, 3.0, 4.0))),

    ok.

face_forward_test() ->
    ok = test_glm:assert_vec2(double,
        glm_vector:face_forward(
            glm:vec2(double, 0.0, 1.0),
            glm:vec2(double, 0.0, -1.0),
            glm:vec2(double, 0.0, 1.0)
        ),
        {0.0, 1.0}
    ),
    ok = test_glm:assert_vec2(double,
        glm_vector:face_forward(
            glm:vec2(double, 0.0, 1.0),
            glm:vec2(double, 0.0, 1.0),
            glm:vec2(double, 0.0, 1.0)
        ),
        {0.0, -1.0}
    ),

    ?assertException(error, function_clause, glm_vector:face_forward(
        glm:vec2({int, 32}, 0, 1),
        glm:vec2({int, 32}, 0, -1),
        glm:vec2({int, 32}, 0, 1)
    )),
    ?assertException(error, _, glm_vector:face_forward(
        glm:vec2(double, 0.0, 1.0),
        glm:vec3(double, 0.0, -1.0, 0.0),
        glm:vec2(double, 0.0, 1.0)
    )),

    ok.

length_test() ->
    ok = test_glm:assert_double(glm_vector:length(glm:vec2(double, 3.0, 4.0)), 5.0),
    ok = test_glm:assert_double(glm_vector:length(glm:vec3(double, 1.0, 2.0, 2.0)), 3.0),

    ?assertException(error, function_clause, glm_vector:length(glm:vec2({int, 32}, 3, 4))),

    ok.

normalize_test() ->
    ok = test_glm:assert_vec2(double, glm_vector:normalize(glm:vec2(double, 3.0, 4.0)), {0.6, 0.8}),
    ok = test_glm:assert_vec3(double, glm_vector:normalize(glm:vec3(double, 0.0, 0.0, 2.0)), {0.0, 0.0, 1.0}),

    ?assertException(error, function_clause, glm_vector:normalize(glm:vec2({int, 32}, 3, 4))),

    ok.

reflect_test() ->
    ok = test_glm:assert_vec2(double,
        glm_vector:reflect(
            glm:vec2(double, 1.0, -1.0),
            glm:vec2(double, 0.0, 1.0)
        ),
        {1.0, 1.0}
    ),

    ?assertException(error, function_clause, glm_vector:reflect(glm:vec2({int, 32}, 1, -1), glm:vec2({int, 32}, 0, 1))),
    ?assertException(error, _, glm_vector:reflect(glm:vec2(double, 1.0, -1.0), glm:vec3(double, 0.0, 1.0, 0.0))),

    ok.

refract_test() ->
    ok = test_glm:assert_vec2(double,
        glm_vector:refract(
            glm:vec2(double, 0.0, -1.0),
            glm:vec2(double, 0.0, 1.0),
            glm:double(0.5)
        ),
        {0.0, -1.0}
    ),

    ?assertException(error, function_clause, glm_vector:refract(
        glm:vec2({int, 32}, 0, -1),
        glm:vec2({int, 32}, 0, 1),
        glm:int32(1)
    )),
    ?assertException(error, _, glm_vector:refract(
        glm:vec2(double, 0.0, -1.0),
        glm:vec2(double, 0.0, 1.0),
        glm:float(0.5)
    )),

    ok.