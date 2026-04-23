%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_quaternion_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

glm_quaternion_test() ->
    ok = quat_test(float),
    ok = quat_test(double).

quat_test(T) ->
    ok = test_glm:assert_quat(T, glm:quat(T), {1.0, 0.0, 0.0, 0.0}),
    ?assert(proper:quickcheck(prop_quat(T))).

prop_quat(T) ->
    ?FORALL(
        {W, X, Y, Z, InvalidValue},
        {
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:valid_value(T),
            test_glm:invalid_value(T)
        },
        begin
            Quaternion = glm:quat(T, W, X, Y, Z),
            ok = test_glm:assert_quat(T, Quaternion, {W, X, Y, Z}),
            assert_values(T, Quaternion, {W, X, Y, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_w(Quaternion, W), {W, X, Y, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_x(Quaternion, X), {W, X, Y, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_y(Quaternion, Y), {W, X, Y, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_z(Quaternion, Z), {W, X, Y, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_w(Quaternion, X), {X, X, Y, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_x(Quaternion, Y), {W, Y, Y, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_y(Quaternion, Z), {W, X, Z, Z}),
            ok = test_glm:assert_quat(T, glm:quat_set_z(Quaternion, W), {W, X, Y, W}),
            ?assertInvalidValue(T, InvalidValue, glm:quat(T, InvalidValue, X, Y, Z)),
            ?assertInvalidValue(T, InvalidValue, glm:quat(T, W, InvalidValue, Y, Z)),
            ?assertInvalidValue(T, InvalidValue, glm:quat(T, W, X, InvalidValue, Z)),
            ?assertInvalidValue(T, InvalidValue, glm:quat(T, W, X, Y, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:quat_set_w(Quaternion, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:quat_set_x(Quaternion, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:quat_set_y(Quaternion, InvalidValue)),
            ?assertInvalidValue(T, InvalidValue, glm:quat_set_z(Quaternion, InvalidValue)),
            true
        end
    ).

assert_values(float, Quaternion, {W, X, Y, Z}) ->
    {ActualW, ActualX, ActualY, ActualZ} = glm:quat_values(Quaternion),
    ?assert(erlang:abs(ActualW - W) < 1.0e-9),
    ?assert(erlang:abs(ActualX - X) < 1.0e-9),
    ?assert(erlang:abs(ActualY - Y) < 1.0e-9),
    ?assert(erlang:abs(ActualZ - Z) < 1.0e-9),
    ok;
assert_values(double, Quaternion, {W, X, Y, Z}) ->
    {ActualW, ActualX, ActualY, ActualZ} = glm:quat_values(Quaternion),
    ?assert(erlang:abs(ActualW - W) < 1.0e-12),
    ?assert(erlang:abs(ActualX - X) < 1.0e-12),
    ?assert(erlang:abs(ActualY - Y) < 1.0e-12),
    ?assert(erlang:abs(ActualZ - Z) < 1.0e-12),
    ok.