%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_scalar_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

glm_scalar_test() ->
    test_glm:assert_bool(glm:scalar(bool), false),
    ?assert(proper:quickcheck(prop_scalar(bool))),

    test_glm:assert_int8(glm:scalar({int, 8}), 0),
    ?assert(proper:quickcheck(prop_scalar({int, 8}))),

    test_glm:assert_int16(glm:scalar({int, 16}), 0),
    ?assert(proper:quickcheck(prop_scalar({int, 16}))),

    test_glm:assert_int32(glm:scalar({int, 32}), 0),
    ?assert(proper:quickcheck(prop_scalar({int, 32}))),

    test_glm:assert_int64(glm:scalar({int, 64}), 0),
    ?assert(proper:quickcheck(prop_scalar({int, 64}))),

    test_glm:assert_uint8(glm:scalar({uint, 8}), 0),
    ?assert(proper:quickcheck(prop_scalar({uint, 8}))),

    test_glm:assert_uint16(glm:scalar({uint, 16}), 0),
    ?assert(proper:quickcheck(prop_scalar({uint, 16}))),

    test_glm:assert_uint32(glm:scalar({uint, 32}), 0),
    ?assert(proper:quickcheck(prop_scalar({uint, 32}))),

    test_glm:assert_uint64(glm:scalar({uint, 64}), 0),
    ?assert(proper:quickcheck(prop_scalar({uint, 64}))),

    test_glm:assert_float(glm:scalar(float), 0.0),
    ?assert(proper:quickcheck(prop_scalar(float))),

    test_glm:assert_double(glm:scalar(double), 0.0),
    ?assert(proper:quickcheck(prop_scalar(double))),

    ok.

prop_scalar(T) ->
    ?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value(T), test_glm:invalid_value(T)},
        begin
            Scalar = glm:scalar(T, Value),
            ok = test_glm:assert_scalar(T, Scalar, Value),
            ?assertInvalidValue(T, InvalidValue, glm:scalar(T, InvalidValue)),
            true
        end
    ).
