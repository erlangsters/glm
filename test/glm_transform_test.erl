-module(glm_transform_test).
-include_lib("eunit/include/eunit.hrl").

frustum_test() ->
    Matrix = glm_transform:frustum(
        glm:double(-1.0),
        glm:double(1.0),
        glm:double(-1.0),
        glm:double(1.0),
        glm:double(1.0),
        glm:double(10.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, -11.0 / 9.0, -20.0 / 9.0,
        0.0, 0.0, -1.0, 0.0
    }),

    ?assertException(error, function_clause, glm_transform:frustum(glm:float(-1.0), glm:double(1.0), glm:double(-1.0), glm:double(1.0), glm:double(1.0), glm:double(10.0))),

    ok.

infinite_perspective_test() ->
    Matrix = glm_transform:infinite_perspective(
        glm:double(math:pi() / 2.0),
        glm:double(1.0),
        glm:double(1.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, -1.0, -2.0,
        0.0, 0.0, -1.0, 0.0
    }),

    ok.

look_at_test() ->
    Matrix = glm_transform:look_at(
        glm:vec3(double, 0.0, 0.0, 1.0),
        glm:vec3(double, 0.0, 0.0, 0.0),
        glm:vec3(double, 0.0, 1.0, 0.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, -1.0,
        0.0, 0.0, 0.0, 1.0
    }),

    ok.

ortho_test() ->
    Matrix = glm_transform:ortho(
        glm:double(-1.0),
        glm:double(1.0),
        glm:double(-2.0),
        glm:double(2.0),
        glm:double(1.0),
        glm:double(11.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        1.0, 0.0, 0.0, 0.0,
        0.0, 0.5, 0.0, 0.0,
        0.0, 0.0, -0.2, -1.2,
        0.0, 0.0, 0.0, 1.0
    }),

    ok.

perspective_test() ->
    Matrix = glm_transform:perspective(
        glm:double(math:pi() / 2.0),
        glm:double(1.0),
        glm:double(1.0),
        glm:double(10.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, -11.0 / 9.0, -20.0 / 9.0,
        0.0, 0.0, -1.0, 0.0
    }),

    ok.

perspective_fov_test() ->
    Matrix = glm_transform:perspective_fov(
        glm:double(math:pi() / 2.0),
        glm:double(2.0),
        glm:double(2.0),
        glm:double(1.0),
        glm:double(10.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, -11.0 / 9.0, -20.0 / 9.0,
        0.0, 0.0, -1.0, 0.0
    }),

    ?assertException(error, function_clause, glm_transform:perspective_fov(glm:double(math:pi() / 2.0), glm:double(2.0), glm:float(2.0), glm:double(1.0), glm:double(10.0))),

    ok.

project_test() ->
    Object = glm:vec3(double, 0.25, -0.5, 0.0),
    Viewport = glm:vec4(double, 10.0, 20.0, 800.0, 600.0),
    Window = glm_transform:project(Object, identity(), identity(), Viewport),
    ok = test_glm:assert_vec3(double, Window, {510.0, 170.0, 0.5}),

    ?assertException(error, function_clause, glm_transform:project(Object, identity(), identity(), glm:vec4(float, 10.0, 20.0, 800.0, 600.0))),

    ok.

rotate_test() ->
    Matrix = glm_transform:rotate(
        identity(),
        glm:double(math:pi() / 2.0),
        glm:vec3(double, 0.0, 0.0, 1.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        0.0, -1.0, 0.0, 0.0,
        1.0, 0.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    }),

    ?assertException(error, function_clause, glm_transform:rotate(identity(), glm:double(math:pi() / 2.0), glm:vec2(double, 0.0, 1.0))),

    ok.

scale_test() ->
    Matrix = glm_transform:scale(
        identity(),
        glm:vec3(double, 2.0, 3.0, 4.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        2.0, 0.0, 0.0, 0.0,
        0.0, 3.0, 0.0, 0.0,
        0.0, 0.0, 4.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    }),

    ok.

translate_test() ->
    Matrix = glm_transform:translate(
        identity(),
        glm:vec3(double, 1.0, 2.0, 3.0)
    ),
    ok = test_glm:assert_mat4(double, Matrix, {
        1.0, 0.0, 0.0, 1.0,
        0.0, 1.0, 0.0, 2.0,
        0.0, 0.0, 1.0, 3.0,
        0.0, 0.0, 0.0, 1.0
    }),

    ok.

un_project_test() ->
    Object = glm:vec3(double, 0.25, -0.5, 0.0),
    Viewport = glm:vec4(double, 10.0, 20.0, 800.0, 600.0),
    Window = glm_transform:project(Object, identity(), identity(), Viewport),
    Unprojected = glm_transform:un_project(Window, identity(), identity(), Viewport),
    ok = test_glm:assert_vec3(double, Unprojected, {0.25, -0.5, 0.0}),

    ?assertException(error, function_clause, glm_transform:un_project(Window, identity(), identity(), glm:vec4(float, 10.0, 20.0, 800.0, 600.0))),

    ok.

identity() ->
    glm:mat4(double,
        1.0, 0.0, 0.0, 0.0,
        0.0, 1.0, 0.0, 0.0,
        0.0, 0.0, 1.0, 0.0,
        0.0, 0.0, 0.0, 1.0
    ).