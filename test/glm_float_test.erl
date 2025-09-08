%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_float_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

float_test() ->
    ok = test_glm:assert_float(glm:float(), 0.0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value(float), test_glm:invalid_value(float)},
        begin
            Scalar = glm:float(Value),
            ok = test_glm:assert_float(Scalar, Value),
            ?assertInvalidValue({expected_float, InvalidValue}, glm:float(InvalidValue)),
            true
        end
    ))),

    ok.

double_test() ->
    ok = test_glm:assert_double(glm:double(), 0.0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value(double), test_glm:invalid_value(double)},
        begin
            Scalar = glm:double(Value),
            ok = test_glm:assert_double(Scalar, Value),
            ?assertInvalidValue({expected_double, InvalidValue}, glm:double(InvalidValue)),
            true
        end
    ))),

    ok.
