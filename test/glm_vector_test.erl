%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_vector_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

glm_vec2_test() ->
    test_glm:assert_vec2(bool, glm:vec2(bool), {false, false}),
    ?assert(proper:quickcheck(prop_vec2(bool))),

    test_glm:assert_vec2({int, 8}, glm:vec2({int, 8}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({int, 8}))),

    test_glm:assert_vec2({int, 16}, glm:vec2({int, 16}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({int, 16}))),

    test_glm:assert_vec2({int, 32}, glm:vec2({int, 32}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({int, 32}))),

    test_glm:assert_vec2({int, 64}, glm:vec2({int, 64}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({int, 64}))),

    test_glm:assert_vec2({uint, 8}, glm:vec2({uint, 8}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({uint, 8}))),

    test_glm:assert_vec2({uint, 16}, glm:vec2({uint, 16}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({uint, 16}))),

    test_glm:assert_vec2({uint, 32}, glm:vec2({uint, 32}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({uint, 32}))),

    test_glm:assert_vec2({uint, 64}, glm:vec2({uint, 64}), {0, 0}),
    ?assert(proper:quickcheck(prop_vec2({uint, 64}))),

    test_glm:assert_vec2(float, glm:vec2(float), {0.0, 0.0}),
    ?assert(proper:quickcheck(prop_vec2(float))),

    test_glm:assert_vec2(double, glm:vec2(double), {0.0, 0.0}),
    ?assert(proper:quickcheck(prop_vec2(double))),

    ok.

glm_vec3_test() ->
    test_glm:assert_vec3(bool, glm:vec3(bool), {false, false, false}),
    ?assert(proper:quickcheck(prop_vec3(bool))),

    test_glm:assert_vec3({int, 8}, glm:vec3({int, 8}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({int, 8}))),

    test_glm:assert_vec3({int, 16}, glm:vec3({int, 16}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({int, 16}))),

    test_glm:assert_vec3({int, 32}, glm:vec3({int, 32}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({int, 32}))),

    test_glm:assert_vec3({int, 64}, glm:vec3({int, 64}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({int, 64}))),

    test_glm:assert_vec3({uint, 8}, glm:vec3({uint, 8}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({uint, 8}))),

    test_glm:assert_vec3({uint, 16}, glm:vec3({uint, 16}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({uint, 16}))),

    test_glm:assert_vec3({uint, 32}, glm:vec3({uint, 32}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({uint, 32}))),

    test_glm:assert_vec3({uint, 64}, glm:vec3({uint, 64}), {0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec3({uint, 64}))),

    test_glm:assert_vec3(float, glm:vec3(float), {0.0, 0.0, 0.0}),
    ?assert(proper:quickcheck(prop_vec3(float))),

    test_glm:assert_vec3(double, glm:vec3(double), {0.0, 0.0, 0.0}),
    ?assert(proper:quickcheck(prop_vec3(double))),

    ok.

glm_vec4_test() ->
    test_glm:assert_vec4(bool, glm:vec4(bool), {false, false, false, false}),
    ?assert(proper:quickcheck(prop_vec4(bool))),

    test_glm:assert_vec4({int, 8}, glm:vec4({int, 8}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({int, 8}))),

    test_glm:assert_vec4({int, 16}, glm:vec4({int, 16}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({int, 16}))),

    test_glm:assert_vec4({int, 32}, glm:vec4({int, 32}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({int, 32}))),

    test_glm:assert_vec4({int, 64}, glm:vec4({int, 64}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({int, 64}))),

    test_glm:assert_vec4({uint, 8}, glm:vec4({uint, 8}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({uint, 8}))),

    test_glm:assert_vec4({uint, 16}, glm:vec4({uint, 16}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({uint, 16}))),

    test_glm:assert_vec4({uint, 32}, glm:vec4({uint, 32}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({uint, 32}))),

    test_glm:assert_vec4({uint, 64}, glm:vec4({uint, 64}), {0, 0, 0, 0}),
    ?assert(proper:quickcheck(prop_vec4({uint, 64}))),

    test_glm:assert_vec4(float, glm:vec4(float), {0.0, 0.0, 0.0, 0.0}),
    ?assert(proper:quickcheck(prop_vec4(float))),

    test_glm:assert_vec4(double, glm:vec4(double), {0.0, 0.0, 0.0, 0.0}),
    ?assert(proper:quickcheck(prop_vec4(double))),

    ok.

prop_vec2(T) ->
    ?FORALL(
        {X = V = V1, Y = V2, InvalidValue},
        {
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:invalid_value(T)
        },
        begin
            ok = test_glm:assert_vec2(T, glm:vec2(T, V), {V, V}),
            ok = test_glm:assert_vec2(T, glm:vec2(T, X, Y), {X, Y}),
            ?assertInvalidValue(T, InvalidValue, glm:vec2(T, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec2(T, X, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec2(T, InvalidValue, Y)),

            A = glm:vec2(T, X, Y),
            case T of
                float ->
                    ?assert(erlang:abs(glm:vec2_x(A) - X) < 1.0e-9),
                    ?assert(erlang:abs(glm:vec2_y(A) - Y) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(1, glm:vec2_values(A)) - X) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(2, glm:vec2_values(A)) - Y) < 1.0e-9);
                double ->
                    ?assert(erlang:abs(glm:vec2_x(A) - X) < 1.0e-12),
                    ?assert(erlang:abs(glm:vec2_y(A) - Y) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(1, glm:vec2_values(A)) - X) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(2, glm:vec2_values(A)) - Y) < 1.0e-12);
                _ ->
                    X = glm:vec2_x(A),
                    Y = glm:vec2_y(A),
                    {X, Y} = glm:vec2_values(A)
            end,

            B = glm:vec2(T, V1),
            ok = test_glm:assert_vec2(T, glm:vec2_set_x(B, V2), {V2, V1}),
            ok = test_glm:assert_vec2(T, glm:vec2_set_y(B, V2), {V1, V2}),
            ?assertInvalidValue(T, InvalidValue, glm:vec2_set_x(B, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec2_set_y(B, InvalidValue)),
            true
        end
    ).

prop_vec3(T) ->
    ?FORALL(
        {X = V = V1, Y = V2, Z, InvalidValue},
        {
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:invalid_value(T)
        },
        begin
            ok = test_glm:assert_vec3(T, glm:vec3(T, V), {V, V, V}),
            ok = test_glm:assert_vec3(T, glm:vec3(T, X, Y, Z), {X, Y, Z}),
            ?assertInvalidValue(T, InvalidValue, glm:vec3(T, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec3(T, X, Y, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec3(T, X, InvalidValue, Z)),
            ?assertInvalidValue(T, InvalidValue, glm:vec3(T, InvalidValue, Y, Z)),

            A = glm:vec3(T, X, Y, Z),
            case T of
                float ->
                    ?assert(erlang:abs(glm:vec3_x(A) - X) < 1.0e-9),
                    ?assert(erlang:abs(glm:vec3_y(A) - Y) < 1.0e-9),
                    ?assert(erlang:abs(glm:vec3_z(A) - Z) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(1, glm:vec3_values(A)) - X) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(2, glm:vec3_values(A)) - Y) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(3, glm:vec3_values(A)) - Z) < 1.0e-9);
                double ->
                    ?assert(erlang:abs(glm:vec3_x(A) - X) < 1.0e-12),
                    ?assert(erlang:abs(glm:vec3_y(A) - Y) < 1.0e-12),
                    ?assert(erlang:abs(glm:vec3_z(A) - Z) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(1, glm:vec3_values(A)) - X) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(2, glm:vec3_values(A)) - Y) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(3, glm:vec3_values(A)) - Z) < 1.0e-12);
                _ ->
                    X = glm:vec3_x(A),
                    Y = glm:vec3_y(A),
                    Z = glm:vec3_z(A),
                    {X, Y, Z} = glm:vec3_values(A)
            end,

            B = glm:vec3(T, V1),
            ok = test_glm:assert_vec3(T, glm:vec3_set_x(B, V2), {V2, V1, V1}),
            ok = test_glm:assert_vec3(T, glm:vec3_set_y(B, V2), {V1, V2, V1}),
            ok = test_glm:assert_vec3(T, glm:vec3_set_z(B, V2), {V1, V1, V2}),
            ?assertInvalidValue(T, InvalidValue, glm:vec3_set_x(B, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec3_set_y(B, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec3_set_z(B, InvalidValue)),
            true
        end
    ).

prop_vec4(T) ->
    ?FORALL(
        {X = V = V1, Y = V2, Z, W, InvalidValue},
        {
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:invalid_value(T)
        },
        begin
            ok = test_glm:assert_vec4(T, glm:vec4(T, V), {V, V, V, V}),
            ok = test_glm:assert_vec4(T, glm:vec4(T, X, Y, Z, W), {X, Y, Z, W}),
            ?assertInvalidValue(T, InvalidValue, glm:vec4(T, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec4(T, X, Y, Z, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec4(T, X, Y, InvalidValue, W)),
            ?assertInvalidValue(T, InvalidValue, glm:vec4(T, X, InvalidValue, Z, W)),
            ?assertInvalidValue(T, InvalidValue, glm:vec4(T, InvalidValue, Y, Z, W)),

            A = glm:vec4(T, X, Y, Z, W),
            case T of
                float ->
                    ?assert(erlang:abs(glm:vec4_x(A) - X) < 1.0e-9),
                    ?assert(erlang:abs(glm:vec4_y(A) - Y) < 1.0e-9),
                    ?assert(erlang:abs(glm:vec4_z(A) - Z) < 1.0e-9),
                    ?assert(erlang:abs(glm:vec4_w(A) - W) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(1, glm:vec4_values(A)) - X) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(2, glm:vec4_values(A)) - Y) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(3, glm:vec4_values(A)) - Z) < 1.0e-9),
                    ?assert(erlang:abs(erlang:element(4, glm:vec4_values(A)) - W) < 1.0e-9);
                double ->
                    ?assert(erlang:abs(glm:vec4_x(A) - X) < 1.0e-12),
                    ?assert(erlang:abs(glm:vec4_y(A) - Y) < 1.0e-12),
                    ?assert(erlang:abs(glm:vec4_z(A) - Z) < 1.0e-12),
                    ?assert(erlang:abs(glm:vec4_w(A) - W) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(1, glm:vec4_values(A)) - X) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(2, glm:vec4_values(A)) - Y) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(3, glm:vec4_values(A)) - Z) < 1.0e-12),
                    ?assert(erlang:abs(erlang:element(4, glm:vec4_values(A)) - W) < 1.0e-12);
                _ ->
                    X = glm:vec4_x(A),
                    Y = glm:vec4_y(A),
                    Z = glm:vec4_z(A),
                    W = glm:vec4_w(A),
                    {X, Y, Z, W} = glm:vec4_values(A)
            end,

            B = glm:vec4(T, V1),
            ok = test_glm:assert_vec4(T, glm:vec4_set_x(B, V2), {V2, V1, V1, V1}),
            ok = test_glm:assert_vec4(T, glm:vec4_set_y(B, V2), {V1, V2, V1, V1}),
            ok = test_glm:assert_vec4(T, glm:vec4_set_z(B, V2), {V1, V1, V2, V1}),
            ok = test_glm:assert_vec4(T, glm:vec4_set_w(B, V2), {V1, V1, V1, V2}),
            ?assertInvalidValue(T, InvalidValue, glm:vec4_set_x(B, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec4_set_y(B, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec4_set_z(B, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:vec4_set_w(B, InvalidValue)),
            true
        end
    ).
