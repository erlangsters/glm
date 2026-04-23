-module(glm_quat_test).
-include("test_glm.hrl").
-include_lib("eunit/include/eunit.hrl").

angle_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_double(glm_quat:angle(QuarterTurn), math:pi() / 2.0),

    ?assertException(error, function_clause, glm_quat:angle(glm:double(1.0))),

    ok.

angle_axis_test() ->
    Half = math:pi() / 4.0,
    ok = test_glm:assert_quat(
        double,
        glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
        {math:cos(Half), 0.0, 0.0, math:sin(Half)}
    ),

    ?assertException(error, function_clause, glm_quat:angle_axis(glm:int32(1), glm:vec3({int, 32}, 0, 0, 1))),
    ?assertException(error, function_clause, glm_quat:angle_axis(glm:double(1.0), glm:vec2(double, 0.0, 1.0))),

    ok.

axis_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_vec3(double, glm_quat:axis(QuarterTurn), {0.0, 0.0, 1.0}),

    ?assertException(error, function_clause, glm_quat:axis(glm:double(1.0))),

    ok.

conjugate_test() ->
    ok = test_glm:assert_quat(double, glm_quat:conjugate(glm:quat(double, 1.0, 2.0, 3.0, 4.0)), {1.0, -2.0, -3.0, -4.0}),

    ?assertException(error, function_clause, glm_quat:conjugate(glm:float(1.0))),

    ok.

dot_test() ->
    ok = test_glm:assert_double(
        glm_quat:dot(glm:quat(double, 1.0, 2.0, 3.0, 4.0), glm:quat(double, 5.0, 6.0, 7.0, 8.0)),
        70.0
    ),

    ?assertException(error, function_clause, glm_quat:dot(glm:quat(float, 1.0, 2.0, 3.0, 4.0), glm:double(1.0))),

    ok.

euler_angles_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_vec3(double, glm_quat:euler_angles(QuarterTurn), {0.0, 0.0, math:pi() / 2.0}),

    ?assertException(error, function_clause, glm_quat:euler_angles(glm:double(1.0))),

    ok.

inverse_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_quat(double, glm_quat:inverse(QuarterTurn), {math:cos(math:pi() / 4.0), 0.0, 0.0, -math:sin(math:pi() / 4.0)}),

    ?assertException(error, function_clause, glm_quat:inverse(glm:double(1.0))),

    ok.

length_test() ->
    ok = test_glm:assert_double(glm_quat:length(glm:quat(double, 1.0, 2.0, 2.0, 1.0)), math:sqrt(10.0)),

    ?assertException(error, function_clause, glm_quat:length(glm:double(1.0))),

    ok.

mat3_cast_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_mat3(double, glm_quat:mat3_cast(QuarterTurn), {0.0, -1.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0}),

    ?assertException(error, function_clause, glm_quat:mat3_cast(glm:double(1.0))),

    ok.

mat4_cast_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_mat4(double, glm_quat:mat4_cast(QuarterTurn), {0.0, -1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 1.0}),

    ?assertException(error, function_clause, glm_quat:mat4_cast(glm:double(1.0))),

    ok.

mix_test() ->
    Identity = glm:quat(double),
    HalfTurn = glm_quat:angle_axis(glm:double(math:pi()), glm:vec3(double, 0.0, 0.0, 1.0)),
    Mixed = glm_quat:mix(Identity, HalfTurn, glm:double(0.5)),
    ok = test_glm:assert_double(glm_quat:angle(Mixed), math:pi() / 2.0),

    ?assertException(error, _, glm_quat:mix(glm:quat(double), glm:quat(double), glm:float(0.5))),

    ok.

normalize_test() ->
    Scale = math:sqrt(10.0),
    ok = test_glm:assert_quat(double, glm_quat:normalize(glm:quat(double, 1.0, 2.0, 2.0, 1.0)), {1.0 / Scale, 2.0 / Scale, 2.0 / Scale, 1.0 / Scale}),

    ?assertException(error, function_clause, glm_quat:normalize(glm:double(1.0))),

    ok.

pitch_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 1.0, 0.0, 0.0)),
    ok = test_glm:assert_double(glm_quat:pitch(QuarterTurn), math:pi() / 2.0),

    ?assertException(error, function_clause, glm_quat:pitch(glm:double(1.0))),

    ok.

quat_cast_test() ->
    Matrix3 = glm:mat3(double, 0.0, 1.0, 0.0, -1.0, 0.0, 0.0, 0.0, 0.0, 1.0),
    Matrix4 = glm:mat4(double,
        0.0, 1.0, 0.0, 0.0,
        -1.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    ),
    ok = test_glm:assert_vec3(double, glm_quat:rotate(glm_quat:quat_cast(Matrix3), glm:vec3(double, 1.0, 0.0, 0.0)), {0.0, 1.0, 0.0}),
    ok = test_glm:assert_vec3(double, glm_quat:rotate(glm_quat:quat_cast(Matrix4), glm:vec3(double, 1.0, 0.0, 0.0)), {0.0, 1.0, 0.0}),

    ?assertException(error, function_clause, glm_quat:quat_cast(glm:mat2(double, 1.0, 0.0, 0.0, 1.0))),

    ok.

roll_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_double(glm_quat:roll(QuarterTurn), math:pi() / 2.0),

    ?assertException(error, function_clause, glm_quat:roll(glm:double(1.0))),

    ok.

rotate_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 0.0, 1.0)),
    ok = test_glm:assert_vec3(double, glm_quat:rotate(QuarterTurn, glm:vec3(double, 1.0, 0.0, 0.0)), {0.0, 1.0, 0.0}),
    ok = test_glm:assert_vec4(double, glm_quat:rotate(QuarterTurn, glm:vec4(double, 1.0, 0.0, 0.0, 1.0)), {0.0, 1.0, 0.0, 1.0}),

    ?assertException(error, function_clause, glm_quat:rotate(QuarterTurn, glm:vec2(double, 1.0, 0.0))),

    ok.

slerp_test() ->
    Identity = glm:quat(double),
    HalfTurn = glm_quat:angle_axis(glm:double(math:pi()), glm:vec3(double, 0.0, 0.0, 1.0)),
    Interpolated = glm_quat:slerp(Identity, HalfTurn, glm:double(0.5)),
    ok = test_glm:assert_vec3(double, glm_quat:rotate(Interpolated, glm:vec3(double, 1.0, 0.0, 0.0)), {0.0, 1.0, 0.0}),

    ?assertException(error, _, glm_quat:slerp(glm:quat(double), glm:quat(double), glm:float(0.5))),

    ok.

yaw_test() ->
    QuarterTurn = glm_quat:angle_axis(glm:double(math:pi() / 2.0), glm:vec3(double, 0.0, 1.0, 0.0)),
    ok = test_glm:assert_double(glm_quat:yaw(QuarterTurn), math:pi() / 2.0),

    ?assertException(error, function_clause, glm_quat:yaw(glm:double(1.0))),

    ok.