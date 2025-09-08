%%
%% Copyright (c) 2025, Byteplug LLC.
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
    R1 = glm_easing:back_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),
    R2 = glm_easing:back_ease_in(glm:float(42.0), glm:float(1.0)),
    ok = test_glm:assert_scalar(float, R2),

    R3 = glm_easing:back_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R3),
    R4 = glm_easing:back_ease_in(glm:double(42.0), glm:double(1.0)),
    ok = test_glm:assert_scalar(double, R4),

    ok.

back_ease_in_out_test() ->
    R1 = glm_easing:back_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),
    R2 = glm_easing:back_ease_in_out(glm:float(42.0), glm:float(1.0)),
    ok = test_glm:assert_scalar(float, R2),

    R3 = glm_easing:back_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R3),
    R4 = glm_easing:back_ease_in_out(glm:double(42.0), glm:double(1.0)),
    ok = test_glm:assert_scalar(double, R4),

    ok.

back_ease_out_test() ->
    R1 = glm_easing:back_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),
    R2 = glm_easing:back_ease_out(glm:float(42.0), glm:float(1.0)),
    ok = test_glm:assert_scalar(float, R2),

    R3 = glm_easing:back_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R3),
    R4 = glm_easing:back_ease_out(glm:double(42.0), glm:double(1.0)),
    ok = test_glm:assert_scalar(double, R4),

    ok.

bounce_ease_in_test() ->
    R1 = glm_easing:bounce_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:bounce_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

bounce_ease_in_out_test() ->
    R1 = glm_easing:bounce_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:bounce_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

bounce_ease_out_test() ->
    R1 = glm_easing:bounce_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:bounce_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

circular_ease_in_test() ->
    R1 = glm_easing:circular_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:circular_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

circular_ease_in_out_test() ->
    R1 = glm_easing:circular_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:circular_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

circular_ease_out_test() ->
    R1 = glm_easing:circular_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:circular_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

cubic_ease_in_test() ->
    R1 = glm_easing:cubic_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:cubic_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

cubic_ease_in_out_test() ->
    R1 = glm_easing:cubic_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:cubic_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

cubic_ease_out_test() ->
    R1 = glm_easing:cubic_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:cubic_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

elastic_ease_in_test() ->
    R1 = glm_easing:elastic_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:elastic_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

elastic_ease_in_out_test() ->
    R1 = glm_easing:elastic_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:elastic_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

elastic_ease_out_test() ->
    R1 = glm_easing:elastic_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:elastic_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

exponential_ease_in_test() ->
    R1 = glm_easing:exponential_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:exponential_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

exponential_ease_in_out_test() ->
    R1 = glm_easing:exponential_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:exponential_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

exponential_ease_out_test() ->
    R1 = glm_easing:exponential_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:exponential_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

linear_interpolation_test() ->
    R1 = glm_easing:linear_interpolation(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:linear_interpolation(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quadratic_ease_in_test() ->
    R1 = glm_easing:quadratic_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quadratic_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quadratic_ease_in_out_test() ->
    R1 = glm_easing:quadratic_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quadratic_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quadratic_ease_out_test() ->
    R1 = glm_easing:quadratic_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quadratic_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quartic_ease_in_test() ->
    R1 = glm_easing:quartic_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quartic_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quartic_ease_in_out_test() ->
    R1 = glm_easing:quartic_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quartic_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quartic_ease_out_test() ->
    R1 = glm_easing:quartic_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quartic_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quintic_ease_in_test() ->
    R1 = glm_easing:quintic_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quintic_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quintic_ease_in_out_test() ->
    R1 = glm_easing:quintic_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quintic_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

quintic_ease_out_test() ->
    R1 = glm_easing:quintic_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:quintic_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

sine_ease_in_test() ->
    R1 = glm_easing:sine_ease_in(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:sine_ease_in(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

sine_ease_in_out_test() ->
    R1 = glm_easing:sine_ease_in_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:sine_ease_in_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.

sine_ease_out_test() ->
    R1 = glm_easing:sine_ease_out(glm:float(42.0)),
    ok = test_glm:assert_scalar(float, R1),

    R2 = glm_easing:sine_ease_out(glm:double(42.0)),
    ok = test_glm:assert_scalar(double, R2),

    ok.
