-module(glm_matrix_ops_test).
-include("test_glm.hrl").
-include_lib("eunit/include/eunit.hrl").

affine_inverse_test() ->
    ok = test_glm:assert_mat4(double,
        glm_matrix:affine_inverse(
            glm:mat4(double,
                2.0, 0.0, 0.0, 0.0,
                0.0, 3.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                4.0, 5.0, 6.0, 1.0
            )
        ),
        {
            0.5, 0.0, 0.0, -2.0,
            0.0, 1.0 / 3.0, 0.0, -(5.0 / 3.0),
            0.0, 0.0, 1.0, -6.0,
            0.0, 0.0, 0.0, 1.0
        }
    ),

    ?assertException(error, function_clause, glm_matrix:affine_inverse(glm:mat2(double, 1.0, 0.0, 0.0, 1.0))),

    ok.

column_test() ->
    Matrix = glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0),
    ok = test_glm:assert_vec3(double, glm_matrix:column(Matrix, 1), {1.0, 3.0, 5.0}),
    ok = test_glm:assert_vec3(double, glm_matrix:column(Matrix, 2), {2.0, 4.0, 6.0}),

    ?assertException(error, function_clause, glm_matrix:column(Matrix, 3)),

    ok.

determinant_test() ->
    ok = test_glm:assert_double(glm_matrix:determinant(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)), -2.0),
    ok = test_glm:assert_double(
        glm_matrix:determinant(glm:mat3(double,
            1.0, 0.0, 0.0,
            0.0, 1.0, 0.0,
            0.0, 0.0, 1.0
        )),
        1.0
    ),

    ?assertException(error, function_clause, glm_matrix:determinant(glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0))),

    ok.

inverse_test() ->
    ok = test_glm:assert_mat2(double,
        glm_matrix:inverse(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)),
        {-2.0, 1.0, 1.5, -0.5}
    ),

    ?assertException(error, function_clause, glm_matrix:inverse(glm:mat3x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0))),

    ok.

inverse_transpose_test() ->
    ok = test_glm:assert_mat2(double,
        glm_matrix:inverse_transpose(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)),
        {-2.0, 1.5, 1.0, -0.5}
    ),

    ?assertException(error, function_clause, glm_matrix:inverse_transpose(glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0))),

    ok.

matrix_comp_mult_test() ->
    ok = test_glm:assert_mat2x3(double,
        glm_matrix:matrix_comp_mult(
            glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0),
            glm:mat2x3(double, 6.0, 4.0, 2.0, 5.0, 3.0, 1.0)
        ),
        {6.0, 10.0, 12.0, 12.0, 10.0, 6.0}
    ),

    ?assertException(error, _, glm_matrix:matrix_comp_mult(
        glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0),
        glm:mat3x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)
    )),

    ok.

outer_product_test() ->
    ok = test_glm:assert_mat2x3(double,
        glm_matrix:outer_product(
            glm:vec3(double, 1.0, 2.0, 3.0),
            glm:vec2(double, 4.0, 5.0)
        ),
        {4.0, 5.0, 8.0, 10.0, 12.0, 15.0}
    ),

    ?assertException(error, _, glm_matrix:outer_product(glm:vec2(double, 1.0, 2.0), glm:vec2(float, 3.0, 4.0))),

    ok.

row_test() ->
    Matrix = glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0),
    ok = test_glm:assert_vec2(double, glm_matrix:row(Matrix, 1), {1.0, 2.0}),
    ok = test_glm:assert_vec2(double, glm_matrix:row(Matrix, 2), {3.0, 4.0}),
    ok = test_glm:assert_vec2(double, glm_matrix:row(Matrix, 3), {5.0, 6.0}),

    ?assertException(error, function_clause, glm_matrix:row(Matrix, 4)),

    ok.

transpose_test() ->
    ok = test_glm:assert_mat3x2(double,
        glm_matrix:transpose(glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0)),
        {1.0, 3.0, 5.0, 2.0, 4.0, 6.0}
    ),
    ok = test_glm:assert_mat2(double,
        glm_matrix:transpose(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)),
        {1.0, 3.0, 2.0, 4.0}
    ),

    ok.