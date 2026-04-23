-module(glm_integer_test).
-include_lib("eunit/include/eunit.hrl").

bit_count_test() ->
    ok = test_glm:assert_int32(glm_integer:bit_count(glm:uint32(0)), 0),
    ok = test_glm:assert_int32(glm_integer:bit_count(glm:uint32(3)), 2),

    ok = test_glm:assert_vec3({int, 32},
        glm_integer:bit_count(glm:vec3({uint, 32}, 1, 3, 7)),
        {1, 2, 3}),

    ok.

bitfield_extract_test() ->
    ok = test_glm:assert_uint32(
        glm_integer:bitfield_extract(glm:uint32(16#80000000), glm:int32(31), glm:int32(1)),
        1),
    ok = test_glm:assert_uint32(
        glm_integer:bitfield_extract(glm:uint32(16#F0000000), glm:int32(28), glm:int32(4)),
        16#0000000F),

    ok = test_glm:assert_vec3({uint, 32},
        glm_integer:bitfield_extract(glm:vec3({uint, 32}, 16#80000000, 16#000000F0, 16#FFFFFFFF), glm:int32(4), glm:int32(4)),
        {0, 16#0000000F, 16#0000000F}),

    ok.

bitfield_insert_test() ->
    ok = test_glm:assert_uint32(
        glm_integer:bitfield_insert(glm:uint32(16#00000000), glm:uint32(16#FFFFFFFF), glm:int32(0), glm:int32(32)),
        16#FFFFFFFF),
    ok = test_glm:assert_uint32(
        glm_integer:bitfield_insert(glm:uint32(16#FF000000), glm:uint32(16#000000FF), glm:int32(8), glm:int32(8)),
        16#FF00FF00),

    ok = test_glm:assert_vec2({uint, 32},
        glm_integer:bitfield_insert(
            glm:vec2({uint, 32}, 16#00000000, 16#FFFF0000),
            glm:vec2({uint, 32}, 16#FFFFFFFF, 16#000000FF),
            glm:int32(8),
            glm:int32(8)
        ),
        {16#0000FF00, 16#FFFFFF00}),

    ok.

bitfield_reverse_test() ->
    ok = test_glm:assert_uint32(glm_integer:bitfield_reverse(glm:uint32(16#00000001)), 16#80000000),

    ok = test_glm:assert_vec2({uint, 32},
        glm_integer:bitfield_reverse(glm:vec2({uint, 32}, 16#00000001, 16#0000000F)),
        {16#80000000, 16#F0000000}),

    ok.

find_lsb_test() ->
    ok = test_glm:assert_int32(glm_integer:find_lsb(glm:uint32(0)), -1),
    ok = test_glm:assert_int32(glm_integer:find_lsb(glm:uint32(6)), 1),

    ok = test_glm:assert_vec3({int, 32},
        glm_integer:find_lsb(glm:vec3({uint, 32}, 1, 2, 12)),
        {0, 1, 2}),

    ok.

find_msb_test() ->
    ok = test_glm:assert_int32(glm_integer:find_msb(glm:uint32(0)), -1),
    ok = test_glm:assert_int32(glm_integer:find_msb(glm:uint32(4)), 2),

    ok = test_glm:assert_vec3({int, 32},
        glm_integer:find_msb(glm:vec3({uint, 32}, 1, 2, 12)),
        {0, 1, 3}),

    ok.

imul_extended_test() ->
    {ScalarMsb, ScalarLsb} = glm_integer:imul_extended(glm:int32(2), glm:int32(3)),
    ok = test_glm:assert_int32(ScalarMsb, 0),
    ok = test_glm:assert_int32(ScalarLsb, 6),

    {VectorMsb, VectorLsb} = glm_integer:imul_extended(
        glm:vec2({int, 32}, 2, 2),
        glm:vec2({int, 32}, 3, 3)
    ),
    ok = test_glm:assert_vec2({int, 32}, VectorMsb, {0, 0}),
    ok = test_glm:assert_vec2({int, 32}, VectorLsb, {6, 6}),

    ok.

is_multiple_test() ->
    ok = test_glm:assert_bool(glm_integer:is_multiple(glm:uint32(9), glm:uint32(3)), true),
    ok = test_glm:assert_bool(glm_integer:is_multiple(glm:uint32(8), glm:uint32(3)), false),

    ok = test_glm:assert_vec3(bool,
        glm_integer:is_multiple(glm:vec3({int, 32}, 6, 7, 9), glm:int32(3)),
        {true, false, true}),
    ok = test_glm:assert_vec3(bool,
        glm_integer:is_multiple(glm:vec3({uint, 32}, 8, 7, 9), glm:vec3({uint, 32}, 3, 7, 4)),
        {false, true, false}),

    ?assertException(error, function_clause, glm_integer:is_multiple(glm:vec3({int, 32}, 1, 2, 3), glm:float(2.0))),

    ok.

is_power_of_two_test() ->
    ok = test_glm:assert_bool(glm_integer:is_power_of_two(glm:uint32(8)), true),
    ok = test_glm:assert_bool(glm_integer:is_power_of_two(glm:uint32(0)), true),
    ok = test_glm:assert_bool(glm_integer:is_power_of_two(glm:uint32(3)), false),

    ok = test_glm:assert_vec3(bool,
        glm_integer:is_power_of_two(glm:vec3({int, 32}, 1, 0, 3)),
        {true, true, false}),

    ?assertException(error, function_clause, glm_integer:is_power_of_two(glm:double(8.0))),

    ok.

next_multiple_test() ->
    ok = test_glm:assert_uint32(glm_integer:next_multiple(glm:uint32(8), glm:uint32(3)), 9),
    ok = test_glm:assert_uint32(glm_integer:next_multiple(glm:uint32(7), glm:uint32(7)), 7),

    ok = test_glm:assert_vec3({uint, 32},
        glm_integer:next_multiple(glm:vec3({uint, 32}, 3, 4, 5), glm:uint32(4)),
        {4, 4, 8}),
    ok = test_glm:assert_vec3({uint, 32},
        glm_integer:next_multiple(glm:vec3({uint, 32}, 3, 6, 8), glm:vec3({uint, 32}, 4, 3, 3)),
        {4, 6, 9}),

    ok.

next_power_of_two_test() ->
    ok = test_glm:assert_int32(glm_integer:next_power_of_two(glm:int32(-3)), -4),
    ok = test_glm:assert_uint32(glm_integer:next_power_of_two(glm:uint32(15)), 16),

    ok = test_glm:assert_vec3({uint, 32},
        glm_integer:next_power_of_two(glm:vec3({uint, 32}, 7, 15, 32)),
        {8, 16, 32}),

    ok.

uadd_carry_test() ->
    {ScalarResult, ScalarCarry} = glm_integer:uadd_carry(glm:uint32(16#FFFFFFFF), glm:uint32(1)),
    ok = test_glm:assert_uint32(ScalarResult, 0),
    ok = test_glm:assert_uint32(ScalarCarry, 1),

    {VectorResult, VectorCarry} = glm_integer:uadd_carry(
        glm:vec2({uint, 32}, 16#FFFFFFFF, 1),
        glm:vec2({uint, 32}, 1, 16#FFFFFFFF)
    ),
    ok = test_glm:assert_vec2({uint, 32}, VectorResult, {0, 0}),
    ok = test_glm:assert_vec2({uint, 32}, VectorCarry, {1, 1}),

    ok.

umul_extended_test() ->
    {ScalarMsb, ScalarLsb} = glm_integer:umul_extended(glm:uint32(2), glm:uint32(3)),
    ok = test_glm:assert_uint32(ScalarMsb, 0),
    ok = test_glm:assert_uint32(ScalarLsb, 6),

    {VectorMsb, VectorLsb} = glm_integer:umul_extended(
        glm:vec2({uint, 32}, 2, 2),
        glm:vec2({uint, 32}, 3, 3)
    ),
    ok = test_glm:assert_vec2({uint, 32}, VectorMsb, {0, 0}),
    ok = test_glm:assert_vec2({uint, 32}, VectorLsb, {6, 6}),

    ok.

prev_multiple_test() ->
    ok = test_glm:assert_uint32(glm_integer:prev_multiple(glm:uint32(8), glm:uint32(3)), 6),
    ok = test_glm:assert_uint32(glm_integer:prev_multiple(glm:uint32(7), glm:uint32(7)), 7),

    ok = test_glm:assert_vec3({uint, 32},
        glm_integer:prev_multiple(glm:vec3({uint, 32}, 8, 7, 9), glm:uint32(7)),
        {7, 7, 7}),
    ok = test_glm:assert_vec3({uint, 32},
        glm_integer:prev_multiple(glm:vec3({uint, 32}, 8, 7, 9), glm:vec3({uint, 32}, 3, 7, 4)),
        {6, 7, 8}),

    ok.

prev_power_of_two_test() ->
    ok = test_glm:assert_uint32(glm_integer:prev_power_of_two(glm:uint32(15)), 8),
    ok = test_glm:assert_uint32(glm_integer:prev_power_of_two(glm:uint32(32)), 32),

    ok = test_glm:assert_vec3({uint, 32},
        glm_integer:prev_power_of_two(glm:vec3({uint, 32}, 7, 15, 32)),
        {4, 8, 32}),

    ok.

usub_borrow_test() ->
    {ScalarResult, ScalarBorrow} = glm_integer:usub_borrow(glm:uint32(16), glm:uint32(17)),
    ok = test_glm:assert_uint32(ScalarResult, 1),
    ok = test_glm:assert_uint32(ScalarBorrow, 1),

    {VectorResult, VectorBorrow} = glm_integer:usub_borrow(
        glm:vec2({uint, 32}, 16, 16),
        glm:vec2({uint, 32}, 17, 17)
    ),
    ok = test_glm:assert_vec2({uint, 32}, VectorResult, {1, 1}),
    ok = test_glm:assert_vec2({uint, 32}, VectorBorrow, {1, 1}),

    ok.