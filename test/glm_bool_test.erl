%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_bool_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

bool_test() ->
    ok = test_glm:assert_bool(glm:bool(), false),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value(bool), test_glm:invalid_value(bool)},
        begin
            Scalar = glm:bool(Value),
            ok = test_glm:assert_bool(Scalar, Value),
            ?assertInvalidValue({expected_boolean, InvalidValue}, glm:bool(InvalidValue)),
            true
        end
    ))),

    ok.
