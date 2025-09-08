%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_integer_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

int8_test() ->
    ok = test_glm:assert_int8(glm:int8(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({int, 8}), test_glm:invalid_value({int, 8})},
        begin
            Scalar = glm:int8(Value),
            ok = test_glm:assert_int8(Scalar, Value),
            ?assertInvalidValue({expected_int8, InvalidValue}, glm:int8(InvalidValue)),
            true
        end
    ))),

    ok.

int16_test() ->
    ok = test_glm:assert_int16(glm:int16(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({int, 16}), test_glm:invalid_value({int, 16})},
        begin
            Scalar = glm:int16(Value),
            ok = test_glm:assert_int16(Scalar, Value),
            ?assertInvalidValue({expected_int16, InvalidValue}, glm:int16(InvalidValue)),
            true
        end
    ))),

    ok.

int32_test() ->
    ok = test_glm:assert_int32(glm:int32(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({int, 32}), test_glm:invalid_value({int, 32})},
        begin
            Scalar = glm:int32(Value),
            ok = test_glm:assert_int32(Scalar, Value),
            ?assertInvalidValue({expected_int32, InvalidValue}, glm:int32(InvalidValue)),
            true
        end
    ))),

    ok.

int64_test() ->
    ok = test_glm:assert_int64(glm:int64(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({int, 64}), test_glm:invalid_value({int, 64})},
        begin
            Scalar = glm:int64(Value),
            ok = test_glm:assert_int64(Scalar, Value),
            ?assertInvalidValue({expected_int64, InvalidValue}, glm:int64(InvalidValue)),
            true
        end
    ))),

    ok.

uint8_test() ->
    ok = test_glm:assert_uint8(glm:uint8(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({uint, 8}), test_glm:invalid_value({uint, 8})},
        begin
            Scalar = glm:uint8(Value),
            ok = test_glm:assert_uint8(Scalar, Value),
            ?assertInvalidValue({expected_uint8, InvalidValue}, glm:uint8(InvalidValue)),
            true
        end
    ))),

    ok.

uint16_test() ->
    ok = test_glm:assert_uint16(glm:uint16(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({uint, 16}), test_glm:invalid_value({uint, 16})},
        begin
            Scalar = glm:uint16(Value),
            ok = test_glm:assert_uint16(Scalar, Value),
            ?assertInvalidValue({expected_uint16, InvalidValue}, glm:uint16(InvalidValue)),
            true
        end
    ))),

    ok.

uint32_test() ->
    ok = test_glm:assert_uint32(glm:uint32(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({uint, 32}), test_glm:invalid_value({uint, 32})},
        begin
            Scalar = glm:uint32(Value),
            ok = test_glm:assert_uint32(Scalar, Value),
            ?assertInvalidValue({expected_uint32, InvalidValue}, glm:uint32(InvalidValue)),
            true
        end
    ))),

    ok.

uint64_test() ->
    ok = test_glm:assert_uint64(glm:uint64(), 0),
    ?assert(proper:quickcheck(?FORALL(
        {Value, InvalidValue},
        {test_glm:valid_value({uint, 64}), test_glm:invalid_value({uint, 64})},
        begin
            Scalar = glm:uint64(Value),
            ok = test_glm:assert_uint64(Scalar, Value),
            ?assertInvalidValue({expected_uint64, InvalidValue}, glm:uint64(InvalidValue)),
            true
        end
    ))),

    ok.
