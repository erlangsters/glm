%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_easing_test).
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

back_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:back_ease_in/1),
    ok = assert_back_ease(fun glm_easing:back_ease_in/2).

back_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:back_ease_in_out/1),
    ok = assert_back_ease(fun glm_easing:back_ease_in_out/2).

back_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:back_ease_out/1),
    ok = assert_back_ease(fun glm_easing:back_ease_out/2).

bounce_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:bounce_ease_in/1).

bounce_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:bounce_ease_in_out/1).

bounce_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:bounce_ease_out/1).

circular_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:circular_ease_in/1).

circular_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:circular_ease_in_out/1).

circular_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:circular_ease_out/1).

cubic_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:cubic_ease_in/1).

cubic_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:cubic_ease_in_out/1).

cubic_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:cubic_ease_out/1).

elastic_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:elastic_ease_in/1).

elastic_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:elastic_ease_in_out/1).

elastic_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:elastic_ease_out/1).

exponential_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:exponential_ease_in/1).

exponential_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:exponential_ease_in_out/1).

exponential_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:exponential_ease_out/1).

linear_interpolation_test() ->
    ok = assert_unary_ease(fun glm_easing:linear_interpolation/1).

quadratic_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:quadratic_ease_in/1).

quadratic_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:quadratic_ease_in_out/1).

quadratic_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:quadratic_ease_out/1).

quartic_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:quartic_ease_in/1).

quartic_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:quartic_ease_in_out/1).

quartic_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:quartic_ease_out/1).

quintic_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:quintic_ease_in/1).

quintic_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:quintic_ease_in_out/1).

quintic_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:quintic_ease_out/1).

sine_ease_in_test() ->
    ok = assert_unary_ease(fun glm_easing:sine_ease_in/1).

sine_ease_in_out_test() ->
    ok = assert_unary_ease(fun glm_easing:sine_ease_in_out/1).

sine_ease_out_test() ->
    ok = assert_unary_ease(fun glm_easing:sine_ease_out/1).

out_ease_duality_test() ->
    lists:foreach(
        fun({InFun, OutFun}) ->
            ok = assert_out_duality(InFun, OutFun)
        end,
        [
            {fun glm_easing:bounce_ease_in/1, fun glm_easing:bounce_ease_out/1},
            {fun glm_easing:circular_ease_in/1, fun glm_easing:circular_ease_out/1},
            {fun glm_easing:cubic_ease_in/1, fun glm_easing:cubic_ease_out/1},
            {fun glm_easing:elastic_ease_in/1, fun glm_easing:elastic_ease_out/1},
            {fun glm_easing:exponential_ease_in/1, fun glm_easing:exponential_ease_out/1},
            {fun glm_easing:quadratic_ease_in/1, fun glm_easing:quadratic_ease_out/1},
            {fun glm_easing:quartic_ease_in/1, fun glm_easing:quartic_ease_out/1},
            {fun glm_easing:quintic_ease_in/1, fun glm_easing:quintic_ease_out/1},
            {fun glm_easing:sine_ease_in/1, fun glm_easing:sine_ease_out/1}
        ]
    ),
    ok.

back_ease_duality_test() ->
    ok = assert_back_out_duality(float),
    ok = assert_back_out_duality(double).

invalid_easing_input_test() ->
    ?assertError(
        {invalid_value, {expected_unit_interval, 1.5}},
        glm_easing:back_ease_in(glm:float(1.5))
    ),
    ?assertError(
        {invalid_value, {expected_unit_interval, -0.25}},
        glm_easing:sine_ease_out(glm:double(-0.25))
    ),
    ok.

assert_unary_ease(Fun) ->
    FloatResult = Fun(glm:float(0.25)),
    ok = test_glm:assert_scalar(float, FloatResult),
    DoubleResult = Fun(glm:double(0.25)),
    ok = test_glm:assert_scalar(double, DoubleResult),
    ok.

assert_back_ease(Fun) ->
    FloatResult = Fun(glm:float(0.25), glm:float(1.0)),
    ok = test_glm:assert_scalar(float, FloatResult),
    DoubleResult = Fun(glm:double(0.25), glm:double(1.0)),
    ok = test_glm:assert_scalar(double, DoubleResult),
    ok.

assert_out_duality(InFun, OutFun) ->
    assert_close(
        glm:scalar_value(OutFun(glm:float(0.25))),
        1.0 - glm:scalar_value(InFun(glm:float(0.75))),
        1.0e-6
    ),
    assert_close(
        glm:scalar_value(OutFun(glm:double(0.25))),
        1.0 - glm:scalar_value(InFun(glm:double(0.75))),
        1.0e-12
    ),
    ok.

assert_back_out_duality(Type) ->
    A1 = glm:scalar(Type, 0.25),
    A2 = glm:scalar(Type, 0.75),
    Overshoot = glm:scalar(Type, 1.0),
    assert_close(
        glm:scalar_value(glm_easing:back_ease_out(A1, Overshoot)),
        1.0 - glm:scalar_value(glm_easing:back_ease_in(A2, Overshoot)),
        tolerance(Type)
    ),
    ok.

assert_close(Left, Right, Tolerance) ->
    ?assert(erlang:abs(Left - Right) =< Tolerance).

tolerance(float) ->
    1.0e-6;
tolerance(double) ->
    1.0e-12.
