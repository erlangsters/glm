%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_raw).
-moduledoc """
OpenGL Mathematics (GLM) raw binary helpers for the BEAM.

This module is the thin binary-oriented layer beneath the public wrapper
modules. Its functions operate on compact type and shape metadata together with
binary payloads, and either encode values directly or forward to the NIF layer.

Most callers should use the safe API exposed by `glm` and the `glm_*` family
modules instead of calling this module directly.
""".

-compile({inline, [
    bool/0, bool/1,
    bool_value/1
]}).
-compile({inline, [
    int8/0, int8/1,
    int8_value/1,
    int16/0, int16/1,
    int16_value/1,
    int32/0, int32/1,
    int32_value/1,
    int64/0, int64/1,
    int64_value/1,
    int64/0, int64/1,
    int64_value/1
]}).
-compile({inline, [
    uint8/0, uint8/1,
    uint8_value/1,
    uint16/0, uint16/1,
    uint16_value/1,
    uint32/0, uint32/1,
    uint32_value/1,
    uint64/0, uint64/1,
    uint64_value/1
]}).
-compile({inline, [
    float/0, float/1,
    float_value/1,
    double/0, double/1,
    double_value/1
]}).
-compile({inline, [
    vec2/1, vec2/2, vec2/3,
    vec2_x/2, vec2_set_x/3,
    vec2_y/2, vec2_set_y/3,
    vec2_values/2
]}).
-compile({inline, [
    vec3/1, vec3/2, vec3/4,
    vec3_x/2, vec3_set_x/3,
    vec3_y/2, vec3_set_y/3,
    vec3_z/2, vec3_set_z/3,
    vec3_values/2
]}).
-compile({inline, [
    vec4/1, vec4/2, vec4/5,
    vec4_x/2, vec4_set_x/3,
    vec4_y/2, vec4_set_y/3,
    vec4_z/2, vec4_set_z/3,
    vec4_w/2, vec4_set_w/3,
    vec4_values/2
]}).
-compile({inline, [
    mat2/1, mat2/2, mat2/5,
    mat2_element/3, mat2_element/4,
    mat2_set_element/4, mat2_set_element/5,
    mat2_values/2
]}).
-compile({inline, [
    mat3/1, mat3/2, mat3/10,
    mat3_element/3, mat3_element/4,
    mat3_set_element/4, mat3_set_element/5,
    mat3_values/2
]}).
-compile({inline, [
    mat4/1, mat4/2, mat4/17,
    mat4_element/3, mat4_element/4,
    mat4_set_element/4, mat4_set_element/5,
    mat4_values/2
]}).
-compile({inline, [
    mat2x3/1, mat2x3/2, mat2x3/7,
    mat2x3_element/3, mat2x3_element/4,
    mat2x3_set_element/4, mat2x3_set_element/5,
    mat2x3_values/2
]}).
-compile({inline, [
    mat2x4/1, mat2x4/2, mat2x4/9,
    mat2x4_element/3, mat2x4_element/4,
    mat2x4_set_element/4, mat2x4_set_element/5,
    mat2x4_values/2
]}).
-compile({inline, [
    mat3x2/1, mat3x2/2, mat3x2/7,
    mat3x2_element/3, mat3x2_element/4,
    mat3x2_set_element/4, mat3x2_set_element/5,
    mat3x2_values/2
]}).
-compile({inline, [
    mat3x4/1, mat3x4/2, mat3x4/13,
    mat3x4_element/3, mat3x4_element/4,
    mat3x4_set_element/4, mat3x4_set_element/5,
    mat3x4_values/2
]}).
-compile({inline, [
    mat4x2/1, mat4x2/2, mat4x2/9,
    mat4x2_element/3, mat4x2_element/4,
    mat4x2_set_element/4, mat4x2_set_element/5,
    mat4x2_values/2
]}).
-compile({inline, [
    mat4x3/1, mat4x3/2, mat4x3/13,
    mat4x3_element/3, mat4x3_element/4,
    mat4x3_set_element/4, mat4x3_set_element/5,
    mat4x3_values/2
]}).
-compile({inline, [
    quat/1, quat/5,
    quat_w/2, quat_set_w/3,
    quat_x/2, quat_set_x/3,
    quat_y/2, quat_set_y/3,
    quat_z/2, quat_set_z/3,
    quat_values/2
]}).
-compile({inline, [
    frustum/7,
    all/2,
    any/2,
    angle/2,
    angle_axis/3,
    axis/2,
    affine_inverse/3,
    acos/3,
    acosh/3,
    asin/3,
    asinh/3,
    atan/3,
    atanh/3,
    abs/3,
    ceil/3,
    column/4,
    conjugate/2,
    cross/4,
    cos/3,
    cosh/3,
    determinant/3,
    distance/4,
    degrees/3,
    dot/3,
    dot/4,
    bit_count/3,
    bitfield_extract/5,
    bitfield_insert/6,
    bitfield_reverse/3,
    equal/4,
    euler_angles/2,
    exp/3,
    exp2/3,
    face_forward/5,
    find_lsb/3,
    find_msb/3,
    clamp/6,
    float_bits_to_int/3,
    float_bits_to_uint/3,
    fma/5,
    fract/3,
    frexp/3,
    floor/3,
    infinite_perspective/4,
    greater_than/4,
    greater_than_equal/4,
    inverse_sqrt/3,
    inverse/2,
    inverse/3,
    inverse_transpose/3,
    int_bits_to_float/3,
    imul_extended/4,
    is_inf/3,
    is_nan/3,
    is_multiple/5,
    is_power_of_two/3,
    ldexp/4,
    length/2,
    length/3,
    look_at/4,
    log/3,
    log2/3,
    max/5,
    mat3_cast/2,
    mat4_cast/2,
    matrix_comp_mult/4,
    min/5,
    mix/4,
    mix/6,
    mod/5,
    modf/3,
    'not'/2,
    not_equal/4,
    normalize/2,
    normalize/3,
    outer_product/4,
    ortho/7,
    next_multiple/5,
    next_power_of_two/3,
    pack_double_2x32/2,
    pack_half_1x16/2,
    pack_half_2x16/2,
    pack_half_4x16/2,
    pack_snorm_1x8/2,
    pack_snorm_2x8/2,
    pack_snorm_1x16/2,
    pack_snorm_2x16/2,
    pack_snorm_4x8/2,
    pack_snorm_4x16/2,
    pack_unorm_1x8/2,
    pack_unorm_2x8/2,
    pack_unorm_1x16/2,
    pack_unorm_2x16/2,
    pack_unorm_4x8/2,
    pack_unorm_4x16/2,
    perspective/5,
    perspective_fov/6,
    project/5,
    pitch/2,
    pow/4,
    prev_multiple/5,
    prev_power_of_two/3,
    quat_cast/3,
    radians/3,
    reflect/4,
    refract/5,
    less_than/4,
    less_than_equal/4,
    roll/2,
    rotate/3,
    rotate/4,
    row/4,
    round/3,
    round_even/3,
    scale/3,
    sin/3,
    sinh/3,
    sign/3,
    slerp/4,
    smoothstep/6,
    unpack_double_2x32/2,
    unpack_half_1x16/2,
    unpack_half_2x16/2,
    unpack_half_4x16/2,
    unpack_snorm_1x8/2,
    unpack_snorm_2x8/2,
    unpack_snorm_1x16/2,
    unpack_snorm_2x16/2,
    unpack_snorm_4x8/2,
    unpack_snorm_4x16/2,
    unpack_unorm_1x8/2,
    unpack_unorm_2x8/2,
    unpack_unorm_1x16/2,
    unpack_unorm_2x16/2,
    unpack_unorm_4x8/2,
    unpack_unorm_4x16/2,
    step/5,
    sqrt/3,
    tan/3,
    tanh/3,
    translate/3,
    transpose/3,
    trunc/3,
    uint_bits_to_float/3,
    yaw/2
]}).
-export([
    bool/0, bool/1,
    bool_value/1
]).
-export([
    int8/0, int8/1,
    int8_value/1,
    int16/0, int16/1,
    int16_value/1,
    int32/0, int32/1,
    int32_value/1,
    int64/0, int64/1,
    int64_value/1
]).
-export([
    uint8/0, uint8/1,
    uint8_value/1,
    uint16/0, uint16/1,
    uint16_value/1,
    uint32/0, uint32/1,
    uint32_value/1,
    uint64/0, uint64/1,
    uint64_value/1
]).
-export([
    float/0, float/1,
    float_value/1,
    double/0, double/1,
    double_value/1
]).
-export([
    vec2/1, vec2/2, vec2/3,
    vec2_x/2, vec2_set_x/3,
    vec2_y/2, vec2_set_y/3,
    vec2_values/2
]).
-export([
    vec3/1, vec3/2, vec3/4,
    vec3_x/2, vec3_set_x/3,
    vec3_y/2, vec3_set_y/3,
    vec3_z/2, vec3_set_z/3,
    vec3_values/2
]).
-export([
    vec4/1, vec4/2, vec4/5,
    vec4_x/2, vec4_set_x/3,
    vec4_y/2, vec4_set_y/3,
    vec4_z/2, vec4_set_z/3,
    vec4_w/2, vec4_set_w/3,
    vec4_values/2
]).
-export([
    mat2/1, mat2/2, mat2/5,
    mat2_element/3, mat2_element/4,
    mat2_set_element/4, mat2_set_element/5,
    mat2_values/2
]).
-export([
    mat3/1, mat3/2, mat3/10,
    mat3_element/3, mat3_element/4,
    mat3_set_element/4, mat3_set_element/5,
    mat3_values/2
]).
-export([
    mat4/1, mat4/2, mat4/17,
    mat4_element/3, mat4_element/4,
    mat4_set_element/4, mat4_set_element/5,
    mat4_values/2
]).
-export([
    mat2x3/1, mat2x3/2, mat2x3/7,
    mat2x3_element/3, mat2x3_element/4,
    mat2x3_set_element/4, mat2x3_set_element/5,
    mat2x3_values/2
]).
-export([
    mat2x4/1, mat2x4/2, mat2x4/9,
    mat2x4_element/3, mat2x4_element/4,
    mat2x4_set_element/4, mat2x4_set_element/5,
    mat2x4_values/2
]).
-export([
    mat3x2/1, mat3x2/2, mat3x2/7,
    mat3x2_element/3, mat3x2_element/4,
    mat3x2_set_element/4, mat3x2_set_element/5,
    mat3x2_values/2
]).
-export([
    mat3x4/1, mat3x4/2, mat3x4/13,
    mat3x4_element/3, mat3x4_element/4,
    mat3x4_set_element/4, mat3x4_set_element/5,
    mat3x4_values/2
]).
-export([
    mat4x2/1, mat4x2/2, mat4x2/9,
    mat4x2_element/3, mat4x2_element/4,
    mat4x2_set_element/4, mat4x2_set_element/5,
    mat4x2_values/2
]).
-export([
    mat4x3/1, mat4x3/2, mat4x3/13,
    mat4x3_element/3, mat4x3_element/4,
    mat4x3_set_element/4, mat4x3_set_element/5,
    mat4x3_values/2
]).
-export([
    quat/1, quat/5,
    quat_w/2, quat_set_w/3,
    quat_x/2, quat_set_x/3,
    quat_y/2, quat_set_y/3,
    quat_z/2, quat_set_z/3,
    quat_values/2
]).
-export([
    frustum/7,
    all/2,
    any/2,
    angle/2,
    angle_axis/3,
    axis/2,
    affine_inverse/3,
    acos/3,
    acosh/3,
    asin/3,
    asinh/3,
    atan/3,
    atanh/3,
    abs/3,
    ceil/3,
    column/4,
    conjugate/2,
    cross/4,
    cos/3,
    cosh/3,
    determinant/3,
    distance/4,
    degrees/3,
    dot/3,
    dot/4,
    bit_count/3,
    bitfield_extract/5,
    bitfield_insert/6,
    bitfield_reverse/3,
    equal/4,
    euler_angles/2,
    exp/3,
    exp2/3,
    face_forward/5,
    find_lsb/3,
    find_msb/3,
    clamp/6,
    float_bits_to_int/3,
    float_bits_to_uint/3,
    fma/5,
    fract/3,
    frexp/3,
    floor/3,
    infinite_perspective/4,
    greater_than/4,
    greater_than_equal/4,
    inverse_sqrt/3,
    inverse/2,
    inverse/3,
    inverse_transpose/3,
    int_bits_to_float/3,
    imul_extended/4,
    is_inf/3,
    is_nan/3,
    is_multiple/5,
    is_power_of_two/3,
    ldexp/4,
    length/2,
    length/3,
    look_at/4,
    log/3,
    log2/3,
    max/5,
    mat3_cast/2,
    mat4_cast/2,
    matrix_comp_mult/4,
    min/5,
    mix/4,
    mix/6,
    mod/5,
    modf/3,
    'not'/2,
    not_equal/4,
    normalize/2,
    normalize/3,
    next_multiple/5,
    next_power_of_two/3,
    outer_product/4,
    ortho/7,
    perspective/5,
    perspective_fov/6,
    project/5,
    pitch/2,
    pow/4,
    prev_multiple/5,
    prev_power_of_two/3,
    quat_cast/3,
    radians/3,
    reflect/4,
    refract/5,
    less_than/4,
    less_than_equal/4,
    roll/2,
    rotate/3,
    rotate/4,
    row/4,
    round/3,
    round_even/3,
    scale/3,
    sin/3,
    sinh/3,
    sign/3,
    slerp/4,
    smoothstep/6,
    pack_double_2x32/2,
    pack_half_1x16/2,
    pack_half_2x16/2,
    pack_half_4x16/2,
    pack_snorm_1x8/2,
    pack_snorm_2x8/2,
    pack_snorm_1x16/2,
    pack_snorm_2x16/2,
    pack_snorm_4x8/2,
    pack_snorm_4x16/2,
    pack_unorm_1x8/2,
    pack_unorm_2x8/2,
    pack_unorm_1x16/2,
    pack_unorm_2x16/2,
    pack_unorm_4x8/2,
    pack_unorm_4x16/2,
    unpack_double_2x32/2,
    unpack_half_1x16/2,
    unpack_half_2x16/2,
    unpack_half_4x16/2,
    unpack_snorm_1x8/2,
    unpack_snorm_2x8/2,
    unpack_snorm_1x16/2,
    unpack_snorm_2x16/2,
    unpack_snorm_4x8/2,
    unpack_snorm_4x16/2,
    unpack_unorm_1x8/2,
    unpack_unorm_2x8/2,
    unpack_unorm_1x16/2,
    unpack_unorm_2x16/2,
    unpack_unorm_4x8/2,
    unpack_unorm_4x16/2,
    step/5,
    sqrt/3,
    uadd_carry/4,
    umul_extended/4,
    tan/3,
    usub_borrow/4,
    tanh/3,
    translate/3,
    transpose/3,
    trunc/3,
    un_project/5,
    uint_bits_to_float/3,
    yaw/2
]).
-export([
    back_ease_in/2, back_ease_in/3,
    back_ease_in_out/2, back_ease_in_out/3,
    back_ease_out/2, back_ease_out/3,
    bounce_ease_in/2,
    bounce_ease_in_out/2,
    bounce_ease_out/2,
    circular_ease_in/2,
    circular_ease_in_out/2,
    circular_ease_out/2,
    cubic_ease_in/2,
    cubic_ease_in_out/2,
    cubic_ease_out/2,
    elastic_ease_in/2,
    elastic_ease_in_out/2,
    elastic_ease_out/2,
    exponential_ease_in/2,
    exponential_ease_in_out/2,
    exponential_ease_out/2,
    linear_interpolation/2,
    quadratic_ease_in/2,
    quadratic_ease_in_out/2,
    quadratic_ease_out/2,
    quartic_ease_in/2,
    quartic_ease_in_out/2,
    quartic_ease_out/2,
    quintic_ease_in/2,
    quintic_ease_in_out/2,
    quintic_ease_out/2,
    sine_ease_in/2,
    sine_ease_in_out/2,
    sine_ease_out/2
]).
-nifs([
    frustum_raw/7,
    all_raw/2,
    any_raw/2,
    angle_raw/2,
    angle_axis_raw/3,
    axis_raw/2,
    affine_inverse_raw/3,
    acos_raw/3,
    acosh_raw/3,
    asin_raw/3,
    asinh_raw/3,
    atan_raw/3,
    atanh_raw/3,
    abs_raw/3,
    ceil_raw/3,
    column_raw/4,
    conjugate_raw/2,
    cross_raw/4,
    cos_raw/3,
    cosh_raw/3,
    determinant_raw/3,
    distance_raw/4,
    degrees_raw/3,
    dot_raw/3,
    dot_raw/4,
    bit_count_raw/3,
    bitfield_extract_raw/5,
    bitfield_insert_raw/6,
    bitfield_reverse_raw/3,
    equal_raw/4,
    euler_angles_raw/2,
    exp_raw/3,
    exp2_raw/3,
    face_forward_raw/5,
    find_lsb_raw/3,
    find_msb_raw/3,
    clamp_raw/6,
    float_bits_to_int_raw/3,
    float_bits_to_uint_raw/3,
    fma_raw/5,
    fract_raw/3,
    frexp_raw/3,
    floor_raw/3,
    infinite_perspective_raw/4,
    greater_than_raw/4,
    greater_than_equal_raw/4,
    inverse_sqrt_raw/3,
    inverse_raw/2,
    inverse_raw/3,
    inverse_transpose_raw/3,
    int_bits_to_float_raw/3,
    imul_extended_raw/4,
    is_inf_raw/3,
    is_nan_raw/3,
    is_multiple_raw/5,
    is_power_of_two_raw/3,
    ldexp_raw/4,
    length_raw/2,
    length_raw/3,
    look_at_raw/4,
    log_raw/3,
    log2_raw/3,
    max_raw/5,
    mat3_cast_raw/2,
    mat4_cast_raw/2,
    matrix_comp_mult_raw/4,
    min_raw/5,
    mix_raw/4,
    mix_raw/6,
    mod_raw/5,
    modf_raw/3,
    not_raw/2,
    not_equal_raw/4,
    normalize_raw/2,
    normalize_raw/3,
    next_multiple_raw/5,
    next_power_of_two_raw/3,
    outer_product_raw/4,
    ortho_raw/7,
    pack_double_2x32_raw/2,
    pack_half_1x16_raw/2,
    pack_half_2x16_raw/2,
    pack_half_4x16_raw/2,
    pack_snorm_1x8_raw/2,
    pack_snorm_2x8_raw/2,
    pack_snorm_1x16_raw/2,
    pack_snorm_2x16_raw/2,
    pack_snorm_4x8_raw/2,
    pack_snorm_4x16_raw/2,
    pack_unorm_1x8_raw/2,
    pack_unorm_2x8_raw/2,
    pack_unorm_1x16_raw/2,
    pack_unorm_2x16_raw/2,
    pack_unorm_4x8_raw/2,
    pack_unorm_4x16_raw/2,
    perspective_raw/5,
    perspective_fov_raw/6,
    project_raw/5,
    pitch_raw/2,
    pow_raw/4,
    prev_multiple_raw/5,
    prev_power_of_two_raw/3,
    quat_cast_raw/3,
    radians_raw/3,
    reflect_raw/4,
    refract_raw/5,
    less_than_raw/4,
    less_than_equal_raw/4,
    roll_raw/2,
    rotate_raw/3,
    rotate_raw/4,
    row_raw/4,
    round_raw/3,
    round_even_raw/3,
    scale_raw/3,
    sin_raw/3,
    sinh_raw/3,
    sign_raw/3,
    slerp_raw/4,
    smoothstep_raw/6,
    step_raw/5,
    sqrt_raw/3,
    tan_raw/3,
    tanh_raw/3,
    translate_raw/3,
    transpose_raw/3,
    trunc_raw/3,
    unpack_double_2x32_raw/2,
    unpack_half_1x16_raw/2,
    unpack_half_2x16_raw/2,
    unpack_half_4x16_raw/2,
    unpack_snorm_1x8_raw/2,
    unpack_snorm_2x8_raw/2,
    unpack_snorm_1x16_raw/2,
    unpack_snorm_2x16_raw/2,
    unpack_snorm_4x8_raw/2,
    unpack_snorm_4x16_raw/2,
    unpack_unorm_1x8_raw/2,
    unpack_unorm_2x8_raw/2,
    unpack_unorm_1x16_raw/2,
    unpack_unorm_2x16_raw/2,
    unpack_unorm_4x8_raw/2,
    unpack_unorm_4x16_raw/2,
    un_project_raw/5,
    uadd_carry_raw/4,
    umul_extended_raw/4,
    uint_bits_to_float_raw/3,
    usub_borrow_raw/4,
    yaw_raw/2,
    back_ease_in_raw/2, back_ease_in_raw/3,
    back_ease_in_out_raw/2, back_ease_in_out_raw/3,
    back_ease_out_raw/2, back_ease_out_raw/3,
    bounce_ease_in_raw/2,
    bounce_ease_in_out_raw/2,
    bounce_ease_out_raw/2,
    circular_ease_in_raw/2,
    circular_ease_in_out_raw/2,
    circular_ease_out_raw/2,
    cubic_ease_in_raw/2,
    cubic_ease_in_out_raw/2,
    cubic_ease_out_raw/2,
    elastic_ease_in_raw/2,
    elastic_ease_in_out_raw/2,
    elastic_ease_out_raw/2,
    exponential_ease_in_raw/2,
    exponential_ease_in_out_raw/2,
    exponential_ease_out_raw/2,
    linear_interpolation_raw/2,
    quadratic_ease_in_raw/2,
    quadratic_ease_in_out_raw/2,
    quadratic_ease_out_raw/2,
    quartic_ease_in_raw/2,
    quartic_ease_in_out_raw/2,
    quartic_ease_out_raw/2,
    quintic_ease_in_raw/2,
    quintic_ease_in_out_raw/2,
    quintic_ease_out_raw/2,
    sine_ease_in_raw/2,
    sine_ease_in_out_raw/2,
    sine_ease_out_raw/2
]).

%% Placeholder raw inventory for upcoming operational families.
%%
%% These entrypoints are intentionally documented here before implementation so
%% each family can be filled in incrementally without revisiting the raw layer
%% structure.
%%
%% glm_common
%% abs_raw/2
%% ceil_raw/2
%% clamp_raw/6
%% float_bits_to_int_raw/2
%% float_bits_to_uint_raw/2
%% floor_raw/2
%% fma_raw/4
%% fract_raw/2
%% frexp_raw/2
%% int_bits_to_float_raw/2
%% is_inf_raw/2
%% is_nan_raw/2
%% ldexp_raw/3
%% max_raw/3
%% min_raw/3
%% mix_raw/4
%% mod_raw/3
%% modf_raw/2
%% round_raw/3
%% round_even_raw/3
%% sign_raw/2
%% smoothstep_raw/4
%% step_raw/3
%% trunc_raw/2
%% uint_bits_to_float_raw/2
%%
%% glm_angle
%% acos_raw/2
%% acosh_raw/2
%% asin_raw/2
%% asinh_raw/2
%% atan_raw/2
%% atanh_raw/2
%% cos_raw/2
%% cosh_raw/2
%% degrees_raw/2
%% radians_raw/2
%% sin_raw/2
%% sinh_raw/2
%% tan_raw/2
%% tanh_raw/2
%%
%% glm_integer
%% bit_count_raw/3
%% bitfield_extract_raw/5
%% bitfield_insert_raw/6
%% bitfield_reverse_raw/3
%% find_lsb_raw/3
%% find_msb_raw/3
%% imul_extended_raw/4
%% is_multiple_raw/5
%% is_power_of_two_raw/3
%% next_multiple_raw/5
%% next_power_of_two_raw/3
%% prev_multiple_raw/5
%% prev_power_of_two_raw/3
%% uadd_carry_raw/4
%% umul_extended_raw/4
%% usub_borrow_raw/4
%%
%% glm_matrix
%% affine_inverse_raw/3
%% column_raw/4
%% determinant_raw/3
%% inverse_raw/3
%% inverse_transpose_raw/3
%% matrix_comp_mult_raw/4
%% outer_product_raw/4
%% row_raw/4
%% transpose_raw/3
%%
%% glm_packing
%% pack_double_2x32_raw/2
%% pack_half_1x16_raw/2
%% pack_half_2x16_raw/2
%% pack_half_4x16_raw/2
%% pack_snorm_1x8_raw/2
%% pack_snorm_2x8_raw/2
%% pack_snorm_1x16_raw/2
%% pack_snorm_2x16_raw/2
%% pack_snorm_4x8_raw/2
%% pack_snorm_4x16_raw/2
%% pack_unorm_1x8_raw/2
%% pack_unorm_2x8_raw/2
%% pack_unorm_1x16_raw/2
%% pack_unorm_2x16_raw/2
%% pack_unorm_4x8_raw/2
%% pack_unorm_4x16_raw/2
%% unpack_double_2x32_raw/2
%% unpack_half_1x16_raw/2
%% unpack_half_2x16_raw/2
%% unpack_half_4x16_raw/2
%% unpack_snorm_1x8_raw/2
%% unpack_snorm_2x8_raw/2
%% unpack_snorm_1x16_raw/2
%% unpack_snorm_2x16_raw/2
%% unpack_snorm_4x8_raw/2
%% unpack_snorm_4x16_raw/2
%% unpack_unorm_1x8_raw/2
%% unpack_unorm_2x8_raw/2
%% unpack_unorm_1x16_raw/2
%% unpack_unorm_2x16_raw/2
%% unpack_unorm_4x8_raw/2
%% unpack_unorm_4x16_raw/2
%%
%% glm_quat
%% angle_raw/2
%% angle_axis_raw/3
%% axis_raw/2
%% conjugate_raw/2
%% dot_raw/3
%% euler_angles_raw/2
%% inverse_raw/2
%% length_raw/2
%% mat3_cast_raw/2
%% mat4_cast_raw/2
%% mix_raw/4
%% normalize_raw/2
%% pitch_raw/2
%% quat_cast_raw/3
%% roll_raw/2
%% rotate_raw/3
%% slerp_raw/4
%% yaw_raw/2
%%
%% glm_relational
%% all_raw/2
%% any_raw/2
%% equal_raw/3
%% greater_than_raw/3
%% greater_than_equal_raw/3
%% less_than_raw/3
%% less_than_equal_raw/3
%% not_raw/2
%% not_equal_raw/3
%%
%% glm_transform
%% frustum_raw/7
%% infinite_perspective_raw/4
%% look_at_raw/4
%% ortho_raw/7
%% perspective_raw/5
%% perspective_fov_raw/6
%% project_raw/5
%% rotate_raw/4
%% scale_raw/3
%% translate_raw/3
%% un_project_raw/5
%%
%% glm_vector
%% cross_raw/4
%% distance_raw/4
%% dot_raw/4
%% face_forward_raw/5
%% length_raw/3
%% normalize_raw/3
%% reflect_raw/4
%% refract_raw/5

-on_load(glm_init/0).

-define(GLM_BOOL, 1).
-define(GLM_INT8, 2).
-define(GLM_INT16, 3).
-define(GLM_INT32, 4).
-define(GLM_INT64, 5).
-define(GLM_UINT8, 6).
-define(GLM_UINT16, 7).
-define(GLM_UINT32, 8).
-define(GLM_UINT64, 9).
-define(GLM_FLOAT, 10).
-define(GLM_DOUBLE, 11).

-define(GLM_TYPE(T),
    begin
        case T of
            bool -> ?GLM_BOOL;
            {int, 8} -> ?GLM_INT8;
            {int, 16} -> ?GLM_INT16;
            {int, 32} -> ?GLM_INT32;
            {int, 64} -> ?GLM_INT64;
            {uint, 8} -> ?GLM_UINT8;
            {uint, 16} -> ?GLM_UINT16;
            {uint, 32} -> ?GLM_UINT32;
            {uint, 64} -> ?GLM_UINT64;
            float -> ?GLM_FLOAT;
            double -> ?GLM_DOUBLE
        end
    end
).

glm_matrix_shape({2, 2}) -> 0;
glm_matrix_shape({3, 3}) -> 1;
glm_matrix_shape({4, 4}) -> 2;
glm_matrix_shape({2, 3}) -> 3;
glm_matrix_shape({2, 4}) -> 4;
glm_matrix_shape({3, 2}) -> 5;
glm_matrix_shape({3, 4}) -> 6;
glm_matrix_shape({4, 2}) -> 7;
glm_matrix_shape({4, 3}) -> 8.

glm_init() ->
    LibName = "beam-glm",
    LibPath = case code:priv_dir(glm) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, LibName]);
                _ ->
                    filename:join([priv, LibName])
            end;
        PrivDir ->
            filename:join(PrivDir, LibName)
    end,
    erlang:load_nif(LibPath, undefined).

-doc("""
Returns the raw binary representation of the boolean value `false`.
""").
-spec bool() -> binary().
bool() ->
    <<0:8/little-unsigned-integer>>.

-doc("""
Encodes a BEAM boolean as the raw one-byte GLM boolean payload.
""").
-spec bool(boolean()) -> binary().
bool(V) ->
    <<(case V of true -> 1; false -> 0 end):8/little-unsigned-integer>>.

-doc("""
Decodes a raw GLM boolean payload into a BEAM boolean.
""").
-spec bool_value(binary()) -> boolean().
bool_value(D) ->
    <<V:8/little-unsigned-integer>> = D,
    V =/= 0.

-doc("""
Returns the raw binary representation of the signed 8-bit value `0`.
""").
-spec int8() -> binary().
int8() ->
    <<0:8/little-signed-integer>>.

-doc("""
Encodes a BEAM integer as a signed 8-bit raw payload.
""").
-spec int8(integer()) -> binary().
int8(V) ->
    <<V:8/little-signed-integer>>.

-doc("""
Decodes a signed 8-bit raw payload into a BEAM integer.
""").
-spec int8_value(binary()) -> integer().
int8_value(D) ->
    <<V:8/little-signed-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the signed 16-bit value `0`.
""").
-spec int16() -> binary().
int16() ->
    <<0:16/little-signed-integer>>.

-doc("""
Encodes a BEAM integer as a signed 16-bit raw payload.
""").
-spec int16(integer()) -> binary().
int16(V) ->
    <<V:16/little-signed-integer>>.

-doc("""
Decodes a signed 16-bit raw payload into a BEAM integer.
""").
-spec int16_value(binary()) -> integer().
int16_value(D) ->
    <<V:16/little-signed-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the signed 32-bit value `0`.
""").
-spec int32() -> binary().
int32() ->
    <<0:32/little-signed-integer>>.

-doc("""
Encodes a BEAM integer as a signed 32-bit raw payload.
""").
-spec int32(integer()) -> binary().
int32(V) ->
    <<V:32/little-signed-integer>>.

-doc("""
Decodes a signed 32-bit raw payload into a BEAM integer.
""").
-spec int32_value(binary()) -> integer().
int32_value(D) ->
    <<V:32/little-signed-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the signed 64-bit value `0`.
""").
-spec int64() -> binary().
int64() ->
    <<0:64/little-signed-integer>>.

-doc("""
Encodes a BEAM integer as a signed 64-bit raw payload.
""").
-spec int64(integer()) -> binary().
int64(V) ->
    <<V:64/little-signed-integer>>.

-doc("""
Decodes a signed 64-bit raw payload into a BEAM integer.
""").
-spec int64_value(binary()) -> integer().
int64_value(D) ->
    <<V:64/little-signed-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the unsigned 8-bit value `0`.
""").
-spec uint8() -> binary().
uint8() ->
    <<0:8/little-unsigned-integer>>.

-doc("""
Encodes a BEAM integer as an unsigned 8-bit raw payload.
""").
-spec uint8(non_neg_integer()) -> binary().
uint8(V) ->
    <<V:8/little-unsigned-integer>>.

-doc("""
Decodes an unsigned 8-bit raw payload into a BEAM integer.
""").
-spec uint8_value(binary()) -> non_neg_integer().
uint8_value(D) ->
    <<V:8/little-unsigned-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the unsigned 16-bit value `0`.
""").
-spec uint16() -> binary().
uint16() ->
    <<0:16/little-unsigned-integer>>.

-doc("""
Encodes a BEAM integer as an unsigned 16-bit raw payload.
""").
-spec uint16(non_neg_integer()) -> binary().
uint16(V) ->
    <<V:16/little-unsigned-integer>>.

-doc("""
Decodes an unsigned 16-bit raw payload into a BEAM integer.
""").
-spec uint16_value(binary()) -> non_neg_integer().
uint16_value(D) ->
    <<V:16/little-unsigned-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the unsigned 32-bit value `0`.
""").
-spec uint32() -> binary().
uint32() ->
    <<0:32/little-unsigned-integer>>.

-doc("""
Encodes a BEAM integer as an unsigned 32-bit raw payload.
""").
-spec uint32(non_neg_integer()) -> binary().
uint32(V) ->
    <<V:32/little-unsigned-integer>>.

-doc("""
Decodes an unsigned 32-bit raw payload into a BEAM integer.
""").
-spec uint32_value(binary()) -> non_neg_integer().
uint32_value(D) ->
    <<V:32/little-unsigned-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the unsigned 64-bit value `0`.
""").
-spec uint64() -> binary().
uint64() ->
    <<0:64/little-unsigned-integer>>.

-doc("""
Encodes a BEAM integer as an unsigned 64-bit raw payload.
""").
-spec uint64(non_neg_integer()) -> binary().
uint64(V) ->
    <<V:64/little-unsigned-integer>>.

-doc("""
Decodes an unsigned 64-bit raw payload into a BEAM integer.
""").
-spec uint64_value(binary()) -> non_neg_integer().
uint64_value(D) ->
    <<V:64/little-unsigned-integer>> = D,
    V.

-doc("""
Returns the raw binary representation of the 32-bit floating-point value `0.0`.
""").
-spec float() -> binary().
float() ->
    <<0.0:32/little-float>>.

-doc("""
Encodes a BEAM floating-point value as a 32-bit raw payload.
""").
-spec float(float() | not_a_number | infinity) -> binary().
float(V) ->
    <<V:32/little-float>>.

-doc("""
Decodes a 32-bit raw payload into a BEAM float.
""").
-spec float_value(binary()) -> float().
float_value(D) ->
    <<V:32/little-float>> = D,
    V.

-doc("""
Returns the raw binary representation of the 64-bit floating-point value `0.0`.
""").
-spec double() -> binary().
double() ->
    <<0.0:64/little-float>>.

-doc("""
Encodes a BEAM floating-point value as a 64-bit raw payload.
""").
-spec double(float()) -> binary().
double(V) ->
    <<V:64/little-float>>.

-doc("""
Decodes a 64-bit raw payload into a BEAM float.
""").
-spec double_value(binary()) -> float().
double_value(D) ->
    <<V:64/little-float>> = D,
    V.

-doc("""
Returns the zero-filled raw binary representation of a 2-component vector with element type `T`.
""").
-spec vec2(glm:type()) -> binary().
vec2(bool) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec2({int, 8}) ->
    <<
        0:8/little-signed-integer,
        0:8/little-signed-integer
    >>;
vec2({int, 16}) ->
    <<
        0:16/little-signed-integer,
        0:16/little-signed-integer
    >>;
vec2({int, 32}) ->
    <<
        0:32/little-signed-integer,
        0:32/little-signed-integer
    >>;
vec2({int, 64}) ->
    <<
        0:64/little-signed-integer,
        0:64/little-signed-integer
    >>;
vec2({uint, 8}) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec2({uint, 16}) ->
    <<
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer
    >>;
vec2({uint, 32}) ->
    <<
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer
    >>;
vec2({uint, 64}) ->
    <<
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer
    >>;
vec2(float) ->
    <<
        0:32/little-float,
        0:32/little-float
    >>;
vec2(double) ->
    <<
        0:64/little-float,
        0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2-component vector with both components set to `V`.
""").
-spec vec2(glm:type(), term()) -> binary().
vec2(bool, V) ->
    <<
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec2({int, 8}, V) ->
    <<
        V:8/little-signed-integer,
        V:8/little-signed-integer
    >>;
vec2({int, 16}, V) ->
    <<
        V:16/little-signed-integer,
        V:16/little-signed-integer
    >>;
vec2({int, 32}, V) ->
    <<
        V:32/little-signed-integer,
        V:32/little-signed-integer
    >>;
vec2({int, 64}, V) ->
    <<
        V:64/little-signed-integer,
        V:64/little-signed-integer
    >>;
vec2({uint, 8}, V) ->
    <<
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer
    >>;
vec2({uint, 16}, V) ->
    <<
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer
    >>;
vec2({uint, 32}, V) ->
    <<
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer
    >>;
vec2({uint, 64}, V) ->
    <<
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer
    >>;
vec2(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float
    >>;
vec2(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2-component vector with explicit `x` and `y` component values.
""").
-spec vec2(glm:type(), term(), term()) -> binary().
vec2(bool, X, Y) ->
    <<
        (case X of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec2({int, 8}, X, Y) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer
    >>;
vec2({int, 16}, X, Y) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer
    >>;
vec2({int, 32}, X, Y) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer
    >>;
vec2({int, 64}, X, Y) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer
    >>;
vec2({uint, 8}, X, Y) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >>;
vec2({uint, 16}, X, Y) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer
    >>;
vec2({uint, 32}, X, Y) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer
    >>;
vec2({uint, 64}, X, Y) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer
    >>;
vec2(float, X, Y) ->
    <<
        X:32/little-float,
        Y:32/little-float
    >>;
vec2(double, X, Y) ->
    <<
        X:64/little-float,
        Y:64/little-float
    >>.

-doc("""
Decodes the `x` component from a raw 2-component vector payload.
""").
-spec vec2_x(glm:type(), binary()) -> term().
vec2_x(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    X =/= 0;
vec2_x({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer
    >> = D,
    X;
vec2_x({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer
    >> = D,
    X;
vec2_x({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer
    >> = D,
    X;
vec2_x({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer
    >> = D,
    X;
vec2_x({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    X;
vec2_x({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer
    >> = D,
    X;
vec2_x({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer
    >> = D,
    X;
vec2_x({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer
    >> = D,
    X;
vec2_x(float, D) ->
    <<
        X:32/little-float,
        _Y:32/little-float
    >> = D,
    X;
vec2_x(double, D) ->
    <<
        X:64/little-float,
        _Y:64/little-float
    >> = D,
    X.

-doc("""
Returns `D` with the `x` component of a raw 2-component vector payload replaced by `V`.
""").
-spec vec2_set_x(glm:type(), binary(), term()) -> binary().
vec2_set_x(bool, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    <<(case X of true -> 1; false -> 0 end):8/little-unsigned-integer, Y:8/little-unsigned-integer>>;
vec2_set_x({int, 8}, D, X) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer>>;
vec2_set_x({int, 16}, D, X) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer>>;
vec2_set_x({int, 32}, D, X) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer>>;
vec2_set_x({int, 64}, D, X) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer>>;
vec2_set_x({uint, 8}, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer>>;
vec2_set_x({uint, 16}, D, X) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer>>;
vec2_set_x({uint, 32}, D, X) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer>>;
vec2_set_x({uint, 64}, D, X) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer>>;
vec2_set_x(float, D, X) ->
    <<
        _X:32/little-float,
        Y:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float>>;
vec2_set_x(double, D, X) ->
    <<
        _X:64/little-float,
        Y:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float>>.

-doc("""
Decodes the `y` component from a raw 2-component vector payload.
""").
-spec vec2_y(glm:type(), binary()) -> term().
vec2_y(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    Y =/= 0;
vec2_y({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer
    >> = D,
    Y;
vec2_y({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer
    >> = D,
    Y;
vec2_y({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer
    >> = D,
    Y;
vec2_y({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer
    >> = D,
    Y;
vec2_y({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    Y;
vec2_y({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer
    >> = D,
    Y;
vec2_y({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer
    >> = D,
    Y;
vec2_y({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer
    >> = D,
    Y;
vec2_y(float, D) ->
    <<
        _X:32/little-float,
        Y:32/little-float
    >> = D,
    Y;
vec2_y(double, D) ->
    <<
        _X:64/little-float,
        Y:64/little-float
    >> = D,
    Y.

-doc("""
Returns `D` with the `y` component of a raw 2-component vector payload replaced by `V`.
""").
-spec vec2_set_y(glm:type(), binary(), term()) -> binary().
vec2_set_y(bool, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer>>;
vec2_set_y({int, 8}, D, Y) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer>>;
vec2_set_y({int, 16}, D, Y) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer>>;
vec2_set_y({int, 32}, D, Y) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer>>;
vec2_set_y({int, 64}, D, Y) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer>>;
vec2_set_y({uint, 8}, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer>>;
vec2_set_y({uint, 16}, D, Y) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer>>;
vec2_set_y({uint, 32}, D, Y) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer>>;
vec2_set_y({uint, 64}, D, Y) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer>>;
vec2_set_y(float, D, Y) ->
    <<
        X:32/little-float,
        _Y:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float>>;
vec2_set_y(double, D, Y) ->
    <<
        X:64/little-float,
        _Y:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float>>.

-doc("""
Decodes a raw 2-component vector payload into a BEAM tuple.
""").
-spec vec2_values(glm:type(), binary()) -> {term(), term()}.
vec2_values(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    {X =/= 0, Y =/= 0};
vec2_values({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer
    >> = D,
    {X, Y};
vec2_values({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer
    >> = D,
    {X, Y};
vec2_values({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer
    >> = D,
    {X, Y};
vec2_values({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer
    >> = D,
    {X, Y};
vec2_values({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer
    >> = D,
    {X, Y};
vec2_values({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer
    >> = D,
    {X, Y};
vec2_values({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer
    >> = D,
    {X, Y};
vec2_values({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer
    >> = D,
    {X, Y};
vec2_values(float, D) ->
    <<
        X:32/little-float,
        Y:32/little-float
    >> = D,
    {X, Y};
vec2_values(double, D) ->
    <<
        X:64/little-float,
        Y:64/little-float
    >> = D,
    {X, Y}.

-doc("""
Returns the zero-filled raw binary representation of a 3-component vector with element type `T`.
""").
-spec vec3(glm:type()) -> binary().
vec3(bool) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec3({int, 8}) ->
    <<
        0:8/little-signed-integer,
        0:8/little-signed-integer,
        0:8/little-signed-integer
    >>;
vec3({int, 16}) ->
    <<
        0:16/little-signed-integer,
        0:16/little-signed-integer,
        0:16/little-signed-integer
    >>;
vec3({int, 32}) ->
    <<
        0:32/little-signed-integer,
        0:32/little-signed-integer,
        0:32/little-signed-integer
    >>;
vec3({int, 64}) ->
    <<
        0:64/little-signed-integer,
        0:64/little-signed-integer,
        0:64/little-signed-integer
    >>;
vec3({uint, 8}) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec3({uint, 16}) ->
    <<
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer
    >>;
vec3({uint, 32}) ->
    <<
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer
    >>;
vec3({uint, 64}) ->
    <<
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer
    >>;
vec3(float) ->
    <<
        0:32/little-float,
        0:32/little-float,
        0:32/little-float
    >>;
vec3(double) ->
    <<
        0:64/little-float,
        0:64/little-float,
        0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3-component vector with all components set to `V`.
""").
-spec vec3(glm:type(), term()) -> binary().
vec3(bool, V) ->
    <<
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec3({int, 8}, V) ->
    <<
        V:8/little-signed-integer,
        V:8/little-signed-integer,
        V:8/little-signed-integer
    >>;
vec3({int, 16}, V) ->
    <<
        V:16/little-signed-integer,
        V:16/little-signed-integer,
        V:16/little-signed-integer
    >>;
vec3({int, 32}, V) ->
    <<
        V:32/little-signed-integer,
        V:32/little-signed-integer,
        V:32/little-signed-integer
    >>;
vec3({int, 64}, V) ->
    <<
        V:64/little-signed-integer,
        V:64/little-signed-integer,
        V:64/little-signed-integer
    >>;
vec3({uint, 8}, V) ->
    <<
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer
    >>;
vec3({uint, 16}, V) ->
    <<
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer
    >>;
vec3({uint, 32}, V) ->
    <<
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer
    >>;
vec3({uint, 64}, V) ->
    <<
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer
    >>;
vec3(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
vec3(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3-component vector with explicit `x`, `y`, and `z` component values.
""").
-spec vec3(glm:type(), term(), term(), term()) -> binary().
vec3(bool, X, Y, Z) ->
    <<
        (case X of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec3({int, 8}, X, Y, Z) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >>;
vec3({int, 16}, X, Y, Z) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >>;
vec3({int, 32}, X, Y, Z) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >>;
vec3({int, 64}, X, Y, Z) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >>;
vec3({uint, 8}, X, Y, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >>;
vec3({uint, 16}, X, Y, Z) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >>;
vec3({uint, 32}, X, Y, Z) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >>;
vec3({uint, 64}, X, Y, Z) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >>;
vec3(float, X, Y, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float
    >>;
vec3(double, X, Y, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float
    >>.

-doc("""
Decodes the `x` component from a raw 3-component vector payload.
""").
-spec vec3_x(glm:type(), binary()) -> term().
vec3_x(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    X =/= 0;
vec3_x({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        _Z:8/little-signed-integer
    >> = D,
    X;
vec3_x({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        _Z:16/little-signed-integer
    >> = D,
    X;
vec3_x({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        _Z:32/little-signed-integer
    >> = D,
    X;
vec3_x({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        _Z:64/little-signed-integer
    >> = D,
    X;
vec3_x({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    X;
vec3_x({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer
    >> = D,
    X;
vec3_x({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer
    >> = D,
    X;
vec3_x({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer
    >> = D,
    X;
vec3_x(float, D) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        _Z:32/little-float
    >> = D,
    X;
vec3_x(double, D) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        _Z:64/little-float
    >> = D,
    X.

-doc("""
Returns `D` with the `x` component of a raw 3-component vector payload replaced by `V`.
""").
-spec vec3_set_x(glm:type(), binary(), term()) -> binary().
vec3_set_x(bool, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<(case X of true -> 1; false -> 0 end):8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_x({int, 8}, D, X) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer>>;
vec3_set_x({int, 16}, D, X) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer>>;
vec3_set_x({int, 32}, D, X) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer>>;
vec3_set_x({int, 64}, D, X) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer>>;
vec3_set_x({uint, 8}, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_x({uint, 16}, D, X) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer>>;
vec3_set_x({uint, 32}, D, X) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer>>;
vec3_set_x({uint, 64}, D, X) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer>>;
vec3_set_x(float, D, X) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        Z:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float>>;
vec3_set_x(double, D, X) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        Z:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float>>.

-doc("""
Decodes the `y` component from a raw 3-component vector payload.
""").
-spec vec3_y(glm:type(), binary()) -> term().
vec3_y(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    Y =/= 0;
vec3_y({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer
    >> = D,
    Y;
vec3_y({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer
    >> = D,
    Y;
vec3_y({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer
    >> = D,
    Y;
vec3_y({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer
    >> = D,
    Y;
vec3_y({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    Y;
vec3_y({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer
    >> = D,
    Y;
vec3_y({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer
    >> = D,
    Y;
vec3_y({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer
    >> = D,
    Y;
vec3_y(float, D) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float
    >> = D,
    Y;
vec3_y(double, D) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float
    >> = D,
    Y.

-doc("""
Returns `D` with the `y` component of a raw 3-component vector payload replaced by `V`.
""").
-spec vec3_set_y(glm:type(), binary(), term()) -> binary().
vec3_set_y(bool, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_y({int, 8}, D, Y) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer>>;
vec3_set_y({int, 16}, D, Y) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer>>;
vec3_set_y({int, 32}, D, Y) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer>>;
vec3_set_y({int, 64}, D, Y) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer>>;
vec3_set_y({uint, 8}, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_y({uint, 16}, D, Y) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer>>;
vec3_set_y({uint, 32}, D, Y) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer>>;
vec3_set_y({uint, 64}, D, Y) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer>>;
vec3_set_y(float, D, Y) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float>>;
vec3_set_y(double, D, Y) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float>>.

-doc("""
Decodes the `z` component from a raw 3-component vector payload.
""").
-spec vec3_z(glm:type(), binary()) -> term().
vec3_z(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    Z =/= 0;
vec3_z({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >> = D,
    Z;
vec3_z({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >> = D,
    Z;
vec3_z({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >> = D,
    Z;
vec3_z({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >> = D,
    Z;
vec3_z({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    Z;
vec3_z({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >> = D,
    Z;
vec3_z({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >> = D,
    Z;
vec3_z({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >> = D,
    Z;
vec3_z(float, D) ->
    <<
        _X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float
    >> = D,
    Z;
vec3_z(double, D) ->
    <<
        _X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float
    >> = D,
    Z.

-doc("""
Returns `D` with the `z` component of a raw 3-component vector payload replaced by `V`.
""").
-spec vec3_set_z(glm:type(), binary(), term()) -> binary().
vec3_set_z(bool, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer>>;
vec3_set_z({int, 8}, D, Z) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer>>;
vec3_set_z({int, 16}, D, Z) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer>>;
vec3_set_z({int, 32}, D, Z) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer>>;
vec3_set_z({int, 64}, D, Z) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer>>;
vec3_set_z({uint, 8}, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer>>;
vec3_set_z({uint, 16}, D, Z) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer>>;
vec3_set_z({uint, 32}, D, Z) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer>>;
vec3_set_z({uint, 64}, D, Z) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer>>;
vec3_set_z(float, D, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float>>;
vec3_set_z(double, D, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float>>.

-doc("""
Decodes a raw 3-component vector payload into a BEAM tuple.
""").
-spec vec3_values(glm:type(), binary()) -> {term(), term(), term()}.
vec3_values(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    {X =/= 0, Y =/= 0, Z =/= 0};
vec3_values({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer
    >> = D,
    {X, Y, Z};
vec3_values({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer
    >> = D,
    {X, Y, Z};
vec3_values({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer
    >> = D,
    {X, Y, Z};
vec3_values({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer
    >> = D,
    {X, Y, Z};
vec3_values({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer
    >> = D,
    {X, Y, Z};
vec3_values({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer
    >> = D,
    {X, Y, Z};
vec3_values({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer
    >> = D,
    {X, Y, Z};
vec3_values({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer
    >> = D,
    {X, Y, Z};
vec3_values(float, D) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float
    >> = D,
    {X, Y, Z};
vec3_values(double, D) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float
    >> = D,
    {X, Y, Z}.

-doc("""
Returns the zero-filled raw binary representation of a 4-component vector with element type `T`.
""").
-spec vec4(glm:type()) -> binary().
vec4(bool) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec4({int, 8}) ->
    <<
        0:8/little-signed-integer,
        0:8/little-signed-integer,
        0:8/little-signed-integer,
        0:8/little-signed-integer
    >>;
vec4({int, 16}) ->
    <<
        0:16/little-signed-integer,
        0:16/little-signed-integer,
        0:16/little-signed-integer,
        0:16/little-signed-integer
    >>;
vec4({int, 32}) ->
    <<
        0:32/little-signed-integer,
        0:32/little-signed-integer,
        0:32/little-signed-integer,
        0:32/little-signed-integer
    >>;
vec4({int, 64}) ->
    <<
        0:64/little-signed-integer,
        0:64/little-signed-integer,
        0:64/little-signed-integer,
        0:64/little-signed-integer
    >>;
vec4({uint, 8}) ->
    <<
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer,
        0:8/little-unsigned-integer
    >>;
vec4({uint, 16}) ->
    <<
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer,
        0:16/little-unsigned-integer
    >>;
vec4({uint, 32}) ->
    <<
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer,
        0:32/little-unsigned-integer
    >>;
vec4({uint, 64}) ->
    <<
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer,
        0:64/little-unsigned-integer
    >>;
vec4(float) ->
    <<
        0:32/little-float,
        0:32/little-float,
        0:32/little-float,
        0:32/little-float
    >>;
vec4(double) ->
    <<
        0:64/little-float,
        0:64/little-float,
        0:64/little-float,
        0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4-component vector with all components set to `V`.
""").
-spec vec4(glm:type(), term()) -> binary().
vec4(bool, V) ->
    <<
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case V of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec4({int, 8}, V) ->
    <<
        V:8/little-signed-integer,
        V:8/little-signed-integer,
        V:8/little-signed-integer,
        V:8/little-signed-integer
    >>;
vec4({int, 16}, V) ->
    <<
        V:16/little-signed-integer,
        V:16/little-signed-integer,
        V:16/little-signed-integer,
        V:16/little-signed-integer
    >>;
vec4({int, 32}, V) ->
    <<
        V:32/little-signed-integer,
        V:32/little-signed-integer,
        V:32/little-signed-integer,
        V:32/little-signed-integer
    >>;
vec4({int, 64}, V) ->
    <<
        V:64/little-signed-integer,
        V:64/little-signed-integer,
        V:64/little-signed-integer,
        V:64/little-signed-integer
    >>;
vec4({uint, 8}, V) ->
    <<
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer,
        V:8/little-unsigned-integer
    >>;
vec4({uint, 16}, V) ->
    <<
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer,
        V:16/little-unsigned-integer
    >>;
vec4({uint, 32}, V) ->
    <<
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer,
        V:32/little-unsigned-integer
    >>;
vec4({uint, 64}, V) ->
    <<
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer,
        V:64/little-unsigned-integer
    >>;
vec4(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
vec4(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4-component vector with explicit `x`, `y`, `z`, and `w` component values.
""").
-spec vec4(glm:type(), term(), term(), term(), term()) -> binary().
vec4(bool, X, Y, Z, W) ->
    <<
        (case X of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer,
        (case W of true -> 1; false -> 0 end):8/little-unsigned-integer
    >>;
vec4({int, 8}, X, Y, Z, W) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        W:8/little-signed-integer
    >>;
vec4({int, 16}, X, Y, Z, W) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        W:16/little-signed-integer
    >>;
vec4({int, 32}, X, Y, Z, W) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        W:32/little-signed-integer
    >>;
vec4({int, 64}, X, Y, Z, W) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        W:64/little-signed-integer
    >>;
vec4({uint, 8}, X, Y, Z, W) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >>;
vec4({uint, 16}, X, Y, Z, W) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >>;
vec4({uint, 32}, X, Y, Z, W) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >>;
vec4({uint, 64}, X, Y, Z, W) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >>;
vec4(float, X, Y, Z, W) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >>;
vec4(double, X, Y, Z, W) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >>.

-doc("""
Decodes the `x` component from a raw 4-component vector payload.
""").
-spec vec4_x(glm:type(), binary()) -> term().
vec4_x(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    X =/= 0;
vec4_x({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    X;
vec4_x({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    X;
vec4_x({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    X;
vec4_x({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    X;
vec4_x({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    X;
vec4_x({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    X;
vec4_x({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    X;
vec4_x({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    X;
vec4_x(float, D) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        _Z:32/little-float,
        _W:32/little-float
    >> = D,
    X;
vec4_x(double, D) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        _Z:64/little-float,
        _W:64/little-float
    >> = D,
    X.

-doc("""
Returns `D` with the `x` component of a raw 4-component vector payload replaced by `V`.
""").
-spec vec4_set_x(glm:type(), binary(), term()) -> binary().
vec4_set_x(bool, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<(case X of true -> 1; false -> 0 end):8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_x({int, 8}, D, X) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_x({int, 16}, D, X) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_x({int, 32}, D, X) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_x({int, 64}, D, X) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_x({uint, 8}, D, X) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_x({uint, 16}, D, X) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_x({uint, 32}, D, X) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_x({uint, 64}, D, X) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_x(float, D, X) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_x(double, D, X) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.

-doc("""
Decodes the `y` component from a raw 4-component vector payload.
""").
-spec vec4_y(glm:type(), binary()) -> term().
vec4_y(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Y =/= 0;
vec4_y({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    Y;
vec4_y({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    Y;
vec4_y({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    Y;
vec4_y({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    Y;
vec4_y({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Y;
vec4_y({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    Y;
vec4_y({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    Y;
vec4_y({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    Y;
vec4_y(float, D) ->
    <<
        _X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float,
        _W:32/little-float
    >> = D,
    Y;
vec4_y(double, D) ->
    <<
        _X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float,
        _W:64/little-float
    >> = D,
    Y.

-doc("""
Returns `D` with the `y` component of a raw 4-component vector payload replaced by `V`.
""").
-spec vec4_set_y(glm:type(), binary(), term()) -> binary().
vec4_set_y(bool, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, (case Y of true -> 1; false -> 0 end):8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_y({int, 8}, D, Y) ->
    <<
        X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_y({int, 16}, D, Y) ->
    <<
        X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_y({int, 32}, D, Y) ->
    <<
        X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_y({int, 64}, D, Y) ->
    <<
        X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_y({uint, 8}, D, Y) ->
    <<
        X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_y({uint, 16}, D, Y) ->
    <<
        X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_y({uint, 32}, D, Y) ->
    <<
        X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_y({uint, 64}, D, Y) ->
    <<
        X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_y(float, D, Y) ->
    <<
        X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_y(double, D, Y) ->
    <<
        X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.

-doc("""
Decodes the `z` component from a raw 4-component vector payload.
""").
-spec vec4_z(glm:type(), binary()) -> term().
vec4_z(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Z =/= 0;
vec4_z({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    Z;
vec4_z({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    Z;
vec4_z({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    Z;
vec4_z({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    Z;
vec4_z({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    Z;
vec4_z({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    Z;
vec4_z({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    Z;
vec4_z({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    Z;
vec4_z(float, D) ->
    <<
        _X:32/little-float,
        _Y:32/little-float,
        Z:32/little-float,
        _W:32/little-float
    >> = D,
    Z;
vec4_z(double, D) ->
    <<
        _X:64/little-float,
        _Y:64/little-float,
        Z:64/little-float,
        _W:64/little-float
    >> = D,
    Z.

-doc("""
Returns `D` with the `z` component of a raw 4-component vector payload replaced by `V`.
""").
-spec vec4_set_z(glm:type(), binary(), term()) -> binary().
vec4_set_z(bool, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, (case Z of true -> 1; false -> 0 end):8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_z({int, 8}, D, Z) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_z({int, 16}, D, Z) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_z({int, 32}, D, Z) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_z({int, 64}, D, Z) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_z({uint, 8}, D, Z) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_z({uint, 16}, D, Z) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_z({uint, 32}, D, Z) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_z({uint, 64}, D, Z) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_z(float, D, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        _Z:32/little-float,
        W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_z(double, D, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        _Z:64/little-float,
        W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.

-doc("""
Decodes the `w` component from a raw 4-component vector payload.
""").
-spec vec4_w(glm:type(), binary()) -> term().
vec4_w(bool, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    W =/= 0;
vec4_w({int, 8}, D) ->
    <<
        _X:8/little-signed-integer,
        _Y:8/little-signed-integer,
        _Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    W;
vec4_w({int, 16}, D) ->
    <<
        _X:16/little-signed-integer,
        _Y:16/little-signed-integer,
        _Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    W;
vec4_w({int, 32}, D) ->
    <<
        _X:32/little-signed-integer,
        _Y:32/little-signed-integer,
        _Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    W;
vec4_w({int, 64}, D) ->
    <<
        _X:64/little-signed-integer,
        _Y:64/little-signed-integer,
        _Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    W;
vec4_w({uint, 8}, D) ->
    <<
        _X:8/little-unsigned-integer,
        _Y:8/little-unsigned-integer,
        _Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    W;
vec4_w({uint, 16}, D) ->
    <<
        _X:16/little-unsigned-integer,
        _Y:16/little-unsigned-integer,
        _Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    W;
vec4_w({uint, 32}, D) ->
    <<
        _X:32/little-unsigned-integer,
        _Y:32/little-unsigned-integer,
        _Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    W;
vec4_w({uint, 64}, D) ->
    <<
        _X:64/little-unsigned-integer,
        _Y:64/little-unsigned-integer,
        _Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    W;
vec4_w(float, D) ->
    <<
        _X:32/little-float,
        _Y:32/little-float,
        _Z:32/little-float,
        W:32/little-float
    >> = D,
    W;
vec4_w(double, D) ->
    <<
        _X:64/little-float,
        _Y:64/little-float,
        _Z:64/little-float,
        W:64/little-float
    >> = D,
    W.

-doc("""
Returns `D` with the `w` component of a raw 4-component vector payload replaced by `V`.
""").
-spec vec4_set_w(glm:type(), binary(), term()) -> binary().
vec4_set_w(bool, D, W) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, (case W of true -> 1; false -> 0 end):8/little-unsigned-integer>>;
vec4_set_w({int, 8}, D, W) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        _W:8/little-signed-integer
    >> = D,
    <<X:8/little-signed-integer, Y:8/little-signed-integer, Z:8/little-signed-integer, W:8/little-signed-integer>>;
vec4_set_w({int, 16}, D, W) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        _W:16/little-signed-integer
    >> = D,
    <<X:16/little-signed-integer, Y:16/little-signed-integer, Z:16/little-signed-integer, W:16/little-signed-integer>>;
vec4_set_w({int, 32}, D, W) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        _W:32/little-signed-integer
    >> = D,
    <<X:32/little-signed-integer, Y:32/little-signed-integer, Z:32/little-signed-integer, W:32/little-signed-integer>>;
vec4_set_w({int, 64}, D, W) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        _W:64/little-signed-integer
    >> = D,
    <<X:64/little-signed-integer, Y:64/little-signed-integer, Z:64/little-signed-integer, W:64/little-signed-integer>>;
vec4_set_w({uint, 8}, D, W) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        _W:8/little-unsigned-integer
    >> = D,
    <<X:8/little-unsigned-integer, Y:8/little-unsigned-integer, Z:8/little-unsigned-integer, W:8/little-unsigned-integer>>;
vec4_set_w({uint, 16}, D, W) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        _W:16/little-unsigned-integer
    >> = D,
    <<X:16/little-unsigned-integer, Y:16/little-unsigned-integer, Z:16/little-unsigned-integer, W:16/little-unsigned-integer>>;
vec4_set_w({uint, 32}, D, W) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        _W:32/little-unsigned-integer
    >> = D,
    <<X:32/little-unsigned-integer, Y:32/little-unsigned-integer, Z:32/little-unsigned-integer, W:32/little-unsigned-integer>>;
vec4_set_w({uint, 64}, D, W) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        _W:64/little-unsigned-integer
    >> = D,
    <<X:64/little-unsigned-integer, Y:64/little-unsigned-integer, Z:64/little-unsigned-integer, W:64/little-unsigned-integer>>;
vec4_set_w(float, D, W) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        _W:32/little-float
    >> = D,
    <<X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;
vec4_set_w(double, D, W) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        _W:64/little-float
    >> = D,
    <<X:64/little-float, Y:64/little-float, Z:64/little-float, W:64/little-float>>.



-doc("""
Decodes a raw 4-component vector payload into a BEAM tuple.
""").
-spec vec4_values(glm:type(), binary()) -> {term(), term(), term(), term()}.
vec4_values(bool, D) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    {(X =/= 0), (Y =/= 0), (Z =/= 0), (W =/= 0)};
vec4_values({int, 8}, D) ->
    <<
        X:8/little-signed-integer,
        Y:8/little-signed-integer,
        Z:8/little-signed-integer,
        W:8/little-signed-integer
    >> = D,
    {X, Y, Z, W};
vec4_values({int, 16}, D) ->
    <<
        X:16/little-signed-integer,
        Y:16/little-signed-integer,
        Z:16/little-signed-integer,
        W:16/little-signed-integer
    >> = D,
    {X, Y, Z, W};
vec4_values({int, 32}, D) ->
    <<
        X:32/little-signed-integer,
        Y:32/little-signed-integer,
        Z:32/little-signed-integer,
        W:32/little-signed-integer
    >> = D,
    {X, Y, Z, W};
vec4_values({int, 64}, D) ->
    <<
        X:64/little-signed-integer,
        Y:64/little-signed-integer,
        Z:64/little-signed-integer,
        W:64/little-signed-integer
    >> = D,
    {X, Y, Z, W};
vec4_values({uint, 8}, D) ->
    <<
        X:8/little-unsigned-integer,
        Y:8/little-unsigned-integer,
        Z:8/little-unsigned-integer,
        W:8/little-unsigned-integer
    >> = D,
    {X, Y, Z, W};
vec4_values({uint, 16}, D) ->
    <<
        X:16/little-unsigned-integer,
        Y:16/little-unsigned-integer,
        Z:16/little-unsigned-integer,
        W:16/little-unsigned-integer
    >> = D,
    {X, Y, Z, W};
vec4_values({uint, 32}, D) ->
    <<
        X:32/little-unsigned-integer,
        Y:32/little-unsigned-integer,
        Z:32/little-unsigned-integer,
        W:32/little-unsigned-integer
    >> = D,
    {X, Y, Z, W};
vec4_values({uint, 64}, D) ->
    <<
        X:64/little-unsigned-integer,
        Y:64/little-unsigned-integer,
        Z:64/little-unsigned-integer,
        W:64/little-unsigned-integer
    >> = D,
    {X, Y, Z, W};
vec4_values(float, D) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    {X, Y, Z, W};
vec4_values(double, D) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    {X, Y, Z, W}.

-doc("""
Returns the zero-filled raw binary representation of a 2x2 matrix with element type `T`.
""").
-spec mat2(glm:type()) -> binary().
mat2(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat2(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2x2 matrix with every element set to `V`.
""").
-spec mat2(glm:type(), float()) -> binary().
mat2(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat2(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2x2 matrix from explicit column-major element values.
""").
-spec mat2(glm:type(), float(), float(), float(), float()) -> binary().
mat2(float, M11, M21, M12, M22) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float
    >>;
mat2(double, M11, M21, M12, M22) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 2x2 matrix payload in column-major order.
""").
-spec mat2_element(glm:type(), binary(), pos_integer()) -> float().
mat2_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M12;
        4 -> M22
    end;
mat2_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M12;
        4 -> M22
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 2x2 matrix payload.
""").
-spec mat2_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat2_element(T, D, C, R) ->
    mat2_element(T, D, (C - 1) * 2 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 2x2 matrix payload.
""").
-spec mat2_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat2_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float
    >> = D,
    case I of
        1 -> mat2(float, V, M21, M12, M22);
        2 -> mat2(float, M11, V, M12, M22);
        3 -> mat2(float, M11, M21, V, M22);
        4 -> mat2(float, M11, M21, M12, V)
    end;
mat2_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float
    >> = D,
    case I of
        1 -> mat2(double, V, M21, M12, M22);
        2 -> mat2(double, M11, V, M12, M22);
        3 -> mat2(double, M11, M21, V, M22);
        4 -> mat2(double, M11, M21, M12, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 2x2 matrix payload.
""").
-spec mat2_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat2_set_element(T, D, C, R, V) ->
    mat2_set_element(T, D, (C - 1) * 2 + R, V).

-doc("""
Decodes a raw 2x2 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat2_values(glm:type(), binary()) -> {float(), float(), float(), float()}.
mat2_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float
    >> = D,
    {M11, M21, M12, M22};
mat2_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float
    >> = D,
    {M11, M21, M12, M22}.

-doc("""
Returns the zero-filled raw binary representation of a 3x3 matrix with element type `T`.
""").
-spec mat3(glm:type()) -> binary().
mat3(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat3(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3x3 matrix with every element set to `V`.
""").
-spec mat3(glm:type(), float()) -> binary().
mat3(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat3(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3x3 matrix from explicit column-major element values.
""").
-spec mat3(glm:type(), float(), float(), float(), float(), float(), float(), float(), float(), float()) -> binary().
mat3(float, M11, M21, M31, M12, M22, M32, M13, M23, M33) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float
    >>;
mat3(double, M11, M21, M31, M12, M22, M32, M13, M23, M33) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 3x3 matrix payload in column-major order.
""").
-spec mat3_element(glm:type(), binary(), pos_integer()) -> float().
mat3_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M12;
        5 -> M22;
        6 -> M32;
        7 -> M13;
        8 -> M23;
        9 -> M33
    end;
mat3_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M12;
        5 -> M22;
        6 -> M32;
        7 -> M13;
        8 -> M23;
        9 -> M33
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 3x3 matrix payload.
""").
-spec mat3_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat3_element(T, D, C, R) ->
    mat3_element(T, D, (C - 1) * 3 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 3x3 matrix payload.
""").
-spec mat3_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat3_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float
    >> = D,
    case I of
        1 -> mat3(float, V, M21, M31, M12, M22, M32, M13, M23, M33);
        2 -> mat3(float, M11, V, M31, M12, M22, M32, M13, M23, M33);
        3 -> mat3(float, M11, M21, V, M12, M22, M32, M13, M23, M33);
        4 -> mat3(float, M11, M21, M31, V, M22, M32, M13, M23, M33);
        5 -> mat3(float, M11, M21, M31, M12, V, M32, M13, M23, M33);
        6 -> mat3(float, M11, M21, M31, M12, M22, V, M13, M23, M33);
        7 -> mat3(float, M11, M21, M31, M12, M22, M32, V, M23, M33);
        8 -> mat3(float, M11, M21, M31, M12, M22, M32, M13, V, M33);
        9 -> mat3(float, M11, M21, M31, M12, M22, M32, M13, M23, V)
    end;
mat3_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float
    >> = D,
    case I of
        1 -> mat3(double, V, M21, M31, M12, M22, M32, M13, M23, M33);
        2 -> mat3(double, M11, V, M31, M12, M22, M32, M13, M23, M33);
        3 -> mat3(double, M11, M21, V, M12, M22, M32, M13, M23, M33);
        4 -> mat3(double, M11, M21, M31, V, M22, M32, M13, M23, M33);
        5 -> mat3(double, M11, M21, M31, M12, V, M32, M13, M23, M33);
        6 -> mat3(double, M11, M21, M31, M12, M22, V, M13, M23, M33);
        7 -> mat3(double, M11, M21, M31, M12, M22, M32, V, M23, M33);
        8 -> mat3(double, M11, M21, M31, M12, M22, M32, M13, V, M33);
        9 -> mat3(double, M11, M21, M31, M12, M22, M32, M13, M23, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 3x3 matrix payload.
""").
-spec mat3_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat3_set_element(T, D, C, R, V) ->
    mat3_set_element(T, D, (C - 1) * 3 + R, V).

-doc("""
Decodes a raw 3x3 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat3_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float(), float(), float(), float()}.
mat3_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float
    >> = D,
    {M11, M21, M31, M12, M22, M32, M13, M23, M33};
mat3_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float
    >> = D,
    {M11, M21, M31, M12, M22, M32, M13, M23, M33}.

-doc("""
Returns the zero-filled raw binary representation of a 4x4 matrix with element type `T`.
""").
-spec mat4(glm:type()) -> binary().
mat4(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat4(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4x4 matrix with every element set to `V`.
""").
-spec mat4(glm:type(), float()) -> binary().
mat4(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat4(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4x4 matrix from explicit column-major element values.
""").
-spec mat4(glm:type(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()) -> binary().
mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float,
        M44:32/little-float
    >>;
mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float,
        M44:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 4x4 matrix payload in column-major order.
""").
-spec mat4_element(glm:type(), binary(), pos_integer()) -> float().
mat4_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float,
        M44:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M41;
        5 -> M12;
        6 -> M22;
        7 -> M32;
        8 -> M42;
        9 -> M13;
        10 -> M23;
        11 -> M33;
        12 -> M43;
        13 -> M14;
        14 -> M24;
        15 -> M34;
        16 -> M44
    end;
mat4_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float,
        M44:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M41;
        5 -> M12;
        6 -> M22;
        7 -> M32;
        8 -> M42;
        9 -> M13;
        10 -> M23;
        11 -> M33;
        12 -> M43;
        13 -> M14;
        14 -> M24;
        15 -> M34;
        16 -> M44
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 4x4 matrix payload.
""").
-spec mat4_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat4_element(T, D, C, R) ->
    mat4_element(T, D, (C - 1) * 4 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 4x4 matrix payload.
""").
-spec mat4_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat4_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float,
        M44:32/little-float
    >> = D,
    case I of
        1 -> mat4(float, V, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        2 -> mat4(float, M11, V, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        3 -> mat4(float, M11, M21, V, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        4 -> mat4(float, M11, M21, M31, V, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        5 -> mat4(float, M11, M21, M31, M41, V, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        6 -> mat4(float, M11, M21, M31, M41, M12, V, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        7 -> mat4(float, M11, M21, M31, M41, M12, M22, V, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        8 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, V, M13, M23, M33, M43, M14, M24, M34, M44);
        9 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, V, M23, M33, M43, M14, M24, M34, M44);
        10 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, V, M33, M43, M14, M24, M34, M44);
        11 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, V, M43, M14, M24, M34, M44);
        12 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, V, M14, M24, M34, M44);
        13 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, V, M24, M34, M44);
        14 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, V, M34, M44);
        15 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, V, M44);
        16 -> mat4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, V)
    end;
mat4_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float,
        M44:64/little-float
    >> = D,
    case I of
        1 -> mat4(double, V, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        2 -> mat4(double, M11, V, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        3 -> mat4(double, M11, M21, V, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        4 -> mat4(double, M11, M21, M31, V, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        5 -> mat4(double, M11, M21, M31, M41, V, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        6 -> mat4(double, M11, M21, M31, M41, M12, V, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        7 -> mat4(double, M11, M21, M31, M41, M12, M22, V, M42, M13, M23, M33, M43, M14, M24, M34, M44);
        8 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, V, M13, M23, M33, M43, M14, M24, M34, M44);
        9 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, V, M23, M33, M43, M14, M24, M34, M44);
        10 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, V, M33, M43, M14, M24, M34, M44);
        11 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, V, M43, M14, M24, M34, M44);
        12 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, V, M14, M24, M34, M44);
        13 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, V, M24, M34, M44);
        14 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, V, M34, M44);
        15 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, V, M44);
        16 -> mat4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 4x4 matrix payload.
""").
-spec mat4_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat4_set_element(T, D, C, R, V) ->
    mat4_set_element(T, D, (C - 1) * 4 + R, V).

-doc("""
Decodes a raw 4x4 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat4_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()}.
mat4_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float,
        M44:32/little-float
    >> = D,
    {M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44};
mat4_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float,
        M44:64/little-float
    >> = D,
    {M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44}.

-doc("""
Returns the zero-filled raw binary representation of a 2x3 matrix with element type `T`.
""").
-spec mat2x3(glm:type()) -> binary().
mat2x3(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat2x3(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2x3 matrix with every element set to `V`.
""").
-spec mat2x3(glm:type(), float()) -> binary().
mat2x3(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat2x3(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2x3 matrix from explicit column-major element values.
""").
-spec mat2x3(glm:type(), float(), float(), float(), float(), float(), float()) -> binary().
mat2x3(float, M11, M21, M31, M12, M22, M32) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float
    >>;
mat2x3(double, M11, M21, M31, M12, M22, M32) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 2x3 matrix payload in column-major order.
""").
-spec mat2x3_element(glm:type(), binary(), pos_integer()) -> float().
mat2x3_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M12;
        5 -> M22;
        6 -> M32
    end;
mat2x3_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M12;
        5 -> M22;
        6 -> M32
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 2x3 matrix payload.
""").
-spec mat2x3_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat2x3_element(T, D, C, R) ->
    mat2x3_element(T, D, (C - 1) * 3 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 2x3 matrix payload.
""").
-spec mat2x3_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat2x3_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float
    >> = D,
    case I of
        1 -> mat2x3(float, V, M21, M31, M12, M22, M32);
        2 -> mat2x3(float, M11, V, M31, M12, M22, M32);
        3 -> mat2x3(float, M11, M21, V, M12, M22, M32);
        4 -> mat2x3(float, M11, M21, M31, V, M22, M32);
        5 -> mat2x3(float, M11, M21, M31, M12, V, M32);
        6 -> mat2x3(float, M11, M21, M31, M12, M22, V)
    end;
mat2x3_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float
    >> = D,
    case I of
        1 -> mat2x3(double, V, M21, M31, M12, M22, M32);
        2 -> mat2x3(double, M11, V, M31, M12, M22, M32);
        3 -> mat2x3(double, M11, M21, V, M12, M22, M32);
        4 -> mat2x3(double, M11, M21, M31, V, M22, M32);
        5 -> mat2x3(double, M11, M21, M31, M12, V, M32);
        6 -> mat2x3(double, M11, M21, M31, M12, M22, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 2x3 matrix payload.
""").
-spec mat2x3_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat2x3_set_element(T, D, C, R, V) ->
    mat2x3_set_element(T, D, (C - 1) * 3 + R, V).

-doc("""
Decodes a raw 2x3 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat2x3_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float()}.
mat2x3_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float
    >> = D,
    {M11, M21, M31, M12, M22, M32};
mat2x3_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float
    >> = D,
    {M11, M21, M31, M12, M22, M32}.

-doc("""
Returns the zero-filled raw binary representation of a 2x4 matrix with element type `T`.
""").
-spec mat2x4(glm:type()) -> binary().
mat2x4(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat2x4(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2x4 matrix with every element set to `V`.
""").
-spec mat2x4(glm:type(), float()) -> binary().
mat2x4(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat2x4(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 2x4 matrix from explicit column-major element values.
""").
-spec mat2x4(glm:type(), float(), float(), float(), float(), float(), float(), float(), float()) -> binary().
mat2x4(float, M11, M21, M31, M41, M12, M22, M32, M42) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float
    >>;
mat2x4(double, M11, M21, M31, M41, M12, M22, M32, M42) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 2x4 matrix payload in column-major order.
""").
-spec mat2x4_element(glm:type(), binary(), pos_integer()) -> float().
mat2x4_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M41;
        5 -> M12;
        6 -> M22;
        7 -> M32;
        8 -> M42
    end;
mat2x4_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M41;
        5 -> M12;
        6 -> M22;
        7 -> M32;
        8 -> M42
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 2x4 matrix payload.
""").
-spec mat2x4_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat2x4_element(T, D, C, R) ->
    mat2x4_element(T, D, (C - 1) * 4 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 2x4 matrix payload.
""").
-spec mat2x4_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat2x4_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float
    >> = D,
    case I of
        1 -> mat2x4(float, V, M21, M31, M41, M12, M22, M32, M42);
        2 -> mat2x4(float, M11, V, M31, M41, M12, M22, M32, M42);
        3 -> mat2x4(float, M11, M21, V, M41, M12, M22, M32, M42);
        4 -> mat2x4(float, M11, M21, M31, V, M12, M22, M32, M42);
        5 -> mat2x4(float, M11, M21, M31, M41, V, M22, M32, M42);
        6 -> mat2x4(float, M11, M21, M31, M41, M12, V, M32, M42);
        7 -> mat2x4(float, M11, M21, M31, M41, M12, M22, V, M42);
        8 -> mat2x4(float, M11, M21, M31, M41, M12, M22, M32, V)
    end;
mat2x4_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float
    >> = D,
    case I of
        1 -> mat2x4(double, V, M21, M31, M41, M12, M22, M32, M42);
        2 -> mat2x4(double, M11, V, M31, M41, M12, M22, M32, M42);
        3 -> mat2x4(double, M11, M21, V, M41, M12, M22, M32, M42);
        4 -> mat2x4(double, M11, M21, M31, V, M12, M22, M32, M42);
        5 -> mat2x4(double, M11, M21, M31, M41, V, M22, M32, M42);
        6 -> mat2x4(double, M11, M21, M31, M41, M12, V, M32, M42);
        7 -> mat2x4(double, M11, M21, M31, M41, M12, M22, V, M42);
        8 -> mat2x4(double, M11, M21, M31, M41, M12, M22, M32, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 2x4 matrix payload.
""").
-spec mat2x4_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat2x4_set_element(T, D, C, R, V) ->
    mat2x4_set_element(T, D, (C - 1) * 4 + R, V).

-doc("""
Decodes a raw 2x4 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat2x4_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float(), float(), float()}.
mat2x4_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float
    >> = D,
    {M11, M21, M31, M41, M12, M22, M32, M42};
mat2x4_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float
    >> = D,
    {M11, M21, M31, M41, M12, M22, M32, M42}.

-doc("""
Returns the zero-filled raw binary representation of a 3x2 matrix with element type `T`.
""").
-spec mat3x2(glm:type()) -> binary().
mat3x2(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat3x2(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3x2 matrix with every element set to `V`.
""").
-spec mat3x2(glm:type(), float()) -> binary().
mat3x2(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat3x2(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3x2 matrix from explicit column-major element values.
""").
-spec mat3x2(glm:type(), float(), float(), float(), float(), float(), float()) -> binary().
mat3x2(float, M11, M21, M12, M22, M13, M23) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float
    >>;
mat3x2(double, M11, M21, M12, M22, M13, M23) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 3x2 matrix payload in column-major order.
""").
-spec mat3x2_element(glm:type(), binary(), pos_integer()) -> float().
mat3x2_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M12;
        4 -> M22;
        5 -> M13;
        6 -> M23
    end;
mat3x2_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M12;
        4 -> M22;
        5 -> M13;
        6 -> M23
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 3x2 matrix payload.
""").
-spec mat3x2_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat3x2_element(T, D, C, R) ->
    mat3x2_element(T, D, (C - 1) * 2 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 3x2 matrix payload.
""").
-spec mat3x2_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat3x2_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float
    >> = D,
    case I of
        1 -> mat3x2(float, V, M21, M12, M22, M13, M23);
        2 -> mat3x2(float, M11, V, M12, M22, M13, M23);
        3 -> mat3x2(float, M11, M21, V, M22, M13, M23);
        4 -> mat3x2(float, M11, M21, M12, V, M13, M23);
        5 -> mat3x2(float, M11, M21, M12, M22, V, M23);
        6 -> mat3x2(float, M11, M21, M12, M22, M13, V)
    end;
mat3x2_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float
    >> = D,
    case I of
        1 -> mat3x2(double, V, M21, M12, M22, M13, M23);
        2 -> mat3x2(double, M11, V, M12, M22, M13, M23);
        3 -> mat3x2(double, M11, M21, V, M22, M13, M23);
        4 -> mat3x2(double, M11, M21, M12, V, M13, M23);
        5 -> mat3x2(double, M11, M21, M12, M22, V, M23);
        6 -> mat3x2(double, M11, M21, M12, M22, M13, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 3x2 matrix payload.
""").
-spec mat3x2_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat3x2_set_element(T, D, C, R, V) ->
    mat3x2_set_element(T, D, (C - 1) * 2 + R, V).

-doc("""
Decodes a raw 3x2 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat3x2_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float()}.
mat3x2_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float
    >> = D,
    {M11, M21, M12, M22, M13, M23};
mat3x2_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float
    >> = D,
    {M11, M21, M12, M22, M13, M23}.

-doc("""
Returns the zero-filled raw binary representation of a 3x4 matrix with element type `T`.
""").
-spec mat3x4(glm:type()) -> binary().
mat3x4(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat3x4(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3x4 matrix with every element set to `V`.
""").
-spec mat3x4(glm:type(), float()) -> binary().
mat3x4(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat3x4(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 3x4 matrix from explicit column-major element values.
""").
-spec mat3x4(glm:type(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()) -> binary().
mat3x4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float
    >>;
mat3x4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 3x4 matrix payload in column-major order.
""").
-spec mat3x4_element(glm:type(), binary(), pos_integer()) -> float().
mat3x4_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M41;
        5 -> M12;
        6 -> M22;
        7 -> M32;
        8 -> M42;
        9 -> M13;
        10 -> M23;
        11 -> M33;
        12 -> M43
    end;
mat3x4_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M41;
        5 -> M12;
        6 -> M22;
        7 -> M32;
        8 -> M42;
        9 -> M13;
        10 -> M23;
        11 -> M33;
        12 -> M43
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 3x4 matrix payload.
""").
-spec mat3x4_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat3x4_element(T, D, C, R) ->
    mat3x4_element(T, D, (C - 1) * 4 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 3x4 matrix payload.
""").
-spec mat3x4_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat3x4_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float
    >> = D,
    case I of
        1 -> mat3x4(float, V, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43);
        2 -> mat3x4(float, M11, V, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43);
        3 -> mat3x4(float, M11, M21, V, M41, M12, M22, M32, M42, M13, M23, M33, M43);
        4 -> mat3x4(float, M11, M21, M31, V, M12, M22, M32, M42, M13, M23, M33, M43);
        5 -> mat3x4(float, M11, M21, M31, M41, V, M22, M32, M42, M13, M23, M33, M43);
        6 -> mat3x4(float, M11, M21, M31, M41, M12, V, M32, M42, M13, M23, M33, M43);
        7 -> mat3x4(float, M11, M21, M31, M41, M12, M22, V, M42, M13, M23, M33, M43);
        8 -> mat3x4(float, M11, M21, M31, M41, M12, M22, M32, V, M13, M23, M33, M43);
        9 -> mat3x4(float, M11, M21, M31, M41, M12, M22, M32, M42, V, M23, M33, M43);
        10 -> mat3x4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, V, M33, M43);
        11 -> mat3x4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, V, M43);
        12 -> mat3x4(float, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, V)
    end;
mat3x4_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float
    >> = D,
    case I of
        1 -> mat3x4(double, V, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43);
        2 -> mat3x4(double, M11, V, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43);
        3 -> mat3x4(double, M11, M21, V, M41, M12, M22, M32, M42, M13, M23, M33, M43);
        4 -> mat3x4(double, M11, M21, M31, V, M12, M22, M32, M42, M13, M23, M33, M43);
        5 -> mat3x4(double, M11, M21, M31, M41, V, M22, M32, M42, M13, M23, M33, M43);
        6 -> mat3x4(double, M11, M21, M31, M41, M12, V, M32, M42, M13, M23, M33, M43);
        7 -> mat3x4(double, M11, M21, M31, M41, M12, M22, V, M42, M13, M23, M33, M43);
        8 -> mat3x4(double, M11, M21, M31, M41, M12, M22, M32, V, M13, M23, M33, M43);
        9 -> mat3x4(double, M11, M21, M31, M41, M12, M22, M32, M42, V, M23, M33, M43);
        10 -> mat3x4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, V, M33, M43);
        11 -> mat3x4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, V, M43);
        12 -> mat3x4(double, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 3x4 matrix payload.
""").
-spec mat3x4_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat3x4_set_element(T, D, C, R, V) ->
    mat3x4_set_element(T, D, (C - 1) * 4 + R, V).

-doc("""
Decodes a raw 3x4 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat3x4_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()}.
mat3x4_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M41:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M42:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M43:32/little-float
    >> = D,
    {M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43};
mat3x4_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M41:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M42:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M43:64/little-float
    >> = D,
    {M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43}.

-doc("""
Returns the zero-filled raw binary representation of a quaternion with element type `T`.
""").
-spec quat(glm:type()) -> binary().
quat(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        1.0:32/little-float
    >>;
quat(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        1.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a quaternion with scalar part `W` and vector part `X`, `Y`, and `Z`.
""").
-spec quat(glm:type(), float(), float(), float(), float()) -> binary().
quat(float, W, X, Y, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >>;
quat(double, W, X, Y, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >>.

-doc("""
Decodes the scalar `w` component from a raw quaternion payload.
""").
-spec quat_w(glm:type(), binary()) -> float().
quat_w(float, D) ->
    <<
        _:32/little-float,
        _:32/little-float,
        _:32/little-float,
        W:32/little-float
    >> = D,
    W;
quat_w(double, D) ->
    <<
        _:64/little-float,
        _:64/little-float,
        _:64/little-float,
        W:64/little-float
    >> = D,
    W.

-doc("""
Returns `D` with the scalar `w` component of a raw quaternion payload replaced by `W`.
""").
-spec quat_set_w(glm:type(), binary(), float()) -> binary().
quat_set_w(float, D, W) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        _:32/little-float
    >> = D,
    quat(float, W, X, Y, Z);
quat_set_w(double, D, W) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        _:64/little-float
    >> = D,
    quat(double, W, X, Y, Z).

-doc("""
Decodes the `x` component from a raw quaternion payload.
""").
-spec quat_x(glm:type(), binary()) -> float().
quat_x(float, D) ->
    <<
        X:32/little-float,
        _:32/little-float,
        _:32/little-float,
        _:32/little-float
    >> = D,
    X;
quat_x(double, D) ->
    <<
        X:64/little-float,
        _:64/little-float,
        _:64/little-float,
        _:64/little-float
    >> = D,
    X.

-doc("""
Returns `D` with the `x` component of a raw quaternion payload replaced by `X`.
""").
-spec quat_set_x(glm:type(), binary(), float()) -> binary().
quat_set_x(float, D, X) ->
    <<
        _:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    quat(float, W, X, Y, Z);
quat_set_x(double, D, X) ->
    <<
        _:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    quat(double, W, X, Y, Z).

-doc("""
Decodes the `y` component from a raw quaternion payload.
""").
-spec quat_y(glm:type(), binary()) -> float().
quat_y(float, D) ->
    <<
        _:32/little-float,
        Y:32/little-float,
        _:32/little-float,
        _:32/little-float
    >> = D,
    Y;
quat_y(double, D) ->
    <<
        _:64/little-float,
        Y:64/little-float,
        _:64/little-float,
        _:64/little-float
    >> = D,
    Y.

-doc("""
Returns `D` with the `y` component of a raw quaternion payload replaced by `Y`.
""").
-spec quat_set_y(glm:type(), binary(), float()) -> binary().
quat_set_y(float, D, Y) ->
    <<
        X:32/little-float,
        _:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    quat(float, W, X, Y, Z);
quat_set_y(double, D, Y) ->
    <<
        X:64/little-float,
        _:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    quat(double, W, X, Y, Z).

-doc("""
Decodes the `z` component from a raw quaternion payload.
""").
-spec quat_z(glm:type(), binary()) -> float().
quat_z(float, D) ->
    <<
        _:32/little-float,
        _:32/little-float,
        Z:32/little-float,
        _:32/little-float
    >> = D,
    Z;
quat_z(double, D) ->
    <<
        _:64/little-float,
        _:64/little-float,
        Z:64/little-float,
        _:64/little-float
    >> = D,
    Z.

-doc("""
Returns `D` with the `z` component of a raw quaternion payload replaced by `Z`.
""").
-spec quat_set_z(glm:type(), binary(), float()) -> binary().
quat_set_z(float, D, Z) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        _:32/little-float,
        W:32/little-float
    >> = D,
    quat(float, W, X, Y, Z);
quat_set_z(double, D, Z) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        _:64/little-float,
        W:64/little-float
    >> = D,
    quat(double, W, X, Y, Z).

-doc("""
Decodes a raw quaternion payload into a BEAM tuple in `{W, X, Y, Z}` order.
""").
-spec quat_values(glm:type(), binary()) -> {float(), float(), float(), float()}.
quat_values(float, D) ->
    <<
        X:32/little-float,
        Y:32/little-float,
        Z:32/little-float,
        W:32/little-float
    >> = D,
    {W, X, Y, Z};
quat_values(double, D) ->
    <<
        X:64/little-float,
        Y:64/little-float,
        Z:64/little-float,
        W:64/little-float
    >> = D,
    {W, X, Y, Z}.

-doc("""
Returns the zero-filled raw binary representation of a 4x2 matrix with element type `T`.
""").
-spec mat4x2(glm:type()) -> binary().
mat4x2(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat4x2(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4x2 matrix with every element set to `V`.
""").
-spec mat4x2(glm:type(), float()) -> binary().
mat4x2(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat4x2(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4x2 matrix from explicit column-major element values.
""").
-spec mat4x2(glm:type(), float(), float(), float(), float(), float(), float(), float(), float()) -> binary().
mat4x2(float, M11, M21, M12, M22, M13, M23, M14, M24) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M14:32/little-float,
        M24:32/little-float
    >>;
mat4x2(double, M11, M21, M12, M22, M13, M23, M14, M24) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M14:64/little-float,
        M24:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 4x2 matrix payload in column-major order.
""").
-spec mat4x2_element(glm:type(), binary(), pos_integer()) -> float().
mat4x2_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M14:32/little-float,
        M24:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M12;
        4 -> M22;
        5 -> M13;
        6 -> M23;
        7 -> M14;
        8 -> M24
    end;
mat4x2_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M14:64/little-float,
        M24:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M12;
        4 -> M22;
        5 -> M13;
        6 -> M23;
        7 -> M14;
        8 -> M24
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 4x2 matrix payload.
""").
-spec mat4x2_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat4x2_element(T, D, C, R) ->
    mat4x2_element(T, D, (C - 1) * 2 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 4x2 matrix payload.
""").
-spec mat4x2_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat4x2_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M14:32/little-float,
        M24:32/little-float
    >> = D,
    case I of
        1 -> mat4x2(float, V, M21, M12, M22, M13, M23, M14, M24);
        2 -> mat4x2(float, M11, V, M12, M22, M13, M23, M14, M24);
        3 -> mat4x2(float, M11, M21, V, M22, M13, M23, M14, M24);
        4 -> mat4x2(float, M11, M21, M12, V, M13, M23, M14, M24);
        5 -> mat4x2(float, M11, M21, M12, M22, V, M23, M14, M24);
        6 -> mat4x2(float, M11, M21, M12, M22, M13, V, M14, M24);
        7 -> mat4x2(float, M11, M21, M12, M22, M13, M23, V, M24);
        8 -> mat4x2(float, M11, M21, M12, M22, M13, M23, M14, V)
    end;
mat4x2_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M14:64/little-float,
        M24:64/little-float
    >> = D,
    case I of
        1 -> mat4x2(double, V, M21, M12, M22, M13, M23, M14, M24);
        2 -> mat4x2(double, M11, V, M12, M22, M13, M23, M14, M24);
        3 -> mat4x2(double, M11, M21, V, M22, M13, M23, M14, M24);
        4 -> mat4x2(double, M11, M21, M12, V, M13, M23, M14, M24);
        5 -> mat4x2(double, M11, M21, M12, M22, V, M23, M14, M24);
        6 -> mat4x2(double, M11, M21, M12, M22, M13, V, M14, M24);
        7 -> mat4x2(double, M11, M21, M12, M22, M13, M23, V, M24);
        8 -> mat4x2(double, M11, M21, M12, M22, M13, M23, M14, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 4x2 matrix payload.
""").
-spec mat4x2_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat4x2_set_element(T, D, C, R, V) ->
    mat4x2_set_element(T, D, (C - 1) * 2 + R, V).

-doc("""
Decodes a raw 4x2 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat4x2_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float(), float(), float()}.
mat4x2_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M14:32/little-float,
        M24:32/little-float
    >> = D,
    {M11, M21, M12, M22, M13, M23, M14, M24};
mat4x2_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M14:64/little-float,
        M24:64/little-float
    >> = D,
    {M11, M21, M12, M22, M13, M23, M14, M24}.

-doc("""
Returns the zero-filled raw binary representation of a 4x3 matrix with element type `T`.
""").
-spec mat4x3(glm:type()) -> binary().
mat4x3(float) ->
    <<
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float,
        0.0:32/little-float
    >>;
mat4x3(double) ->
    <<
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float,
        0.0:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4x3 matrix with every element set to `V`.
""").
-spec mat4x3(glm:type(), float()) -> binary().
mat4x3(float, V) ->
    <<
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float,
        V:32/little-float
    >>;
mat4x3(double, V) ->
    <<
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float,
        V:64/little-float
    >>.

-doc("""
Returns the raw binary representation of a 4x3 matrix from explicit column-major element values.
""").
-spec mat4x3(glm:type(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()) -> binary().
mat4x3(float, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float
    >>;
mat4x3(double, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float
    >>.

-doc("""
Decodes the element at 1-based flat index `I` from a raw 4x3 matrix payload in column-major order.
""").
-spec mat4x3_element(glm:type(), binary(), pos_integer()) -> float().
mat4x3_element(float, D, I) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M12;
        5 -> M22;
        6 -> M32;
        7 -> M13;
        8 -> M23;
        9 -> M33;
        10 -> M14;
        11 -> M24;
        12 -> M34
    end;
mat4x3_element(double, D, I) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float
    >> = D,
    case I of
        1 -> M11;
        2 -> M21;
        3 -> M31;
        4 -> M12;
        5 -> M22;
        6 -> M32;
        7 -> M13;
        8 -> M23;
        9 -> M33;
        10 -> M14;
        11 -> M24;
        12 -> M34
    end.

-doc("""
Decodes the element at column `C` and row `R` from a raw 4x3 matrix payload.
""").
-spec mat4x3_element(glm:type(), binary(), pos_integer(), pos_integer()) -> float().
mat4x3_element(T, D, C, R) ->
    mat4x3_element(T, D, (C - 1) * 3 + R).

-doc("""
Returns `D` with the element at 1-based flat index `I` replaced by `V` in a raw 4x3 matrix payload.
""").
-spec mat4x3_set_element(glm:type(), binary(), pos_integer(), float()) -> binary().
mat4x3_set_element(float, D, I, V) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float
    >> = D,
    case I of
        1 -> mat4x3(float, V, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34);
        2 -> mat4x3(float, M11, V, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34);
        3 -> mat4x3(float, M11, M21, V, M12, M22, M32, M13, M23, M33, M14, M24, M34);
        4 -> mat4x3(float, M11, M21, M31, V, M22, M32, M13, M23, M33, M14, M24, M34);
        5 -> mat4x3(float, M11, M21, M31, M12, V, M32, M13, M23, M33, M14, M24, M34);
        6 -> mat4x3(float, M11, M21, M31, M12, M22, V, M13, M23, M33, M14, M24, M34);
        7 -> mat4x3(float, M11, M21, M31, M12, M22, M32, V, M23, M33, M14, M24, M34);
        8 -> mat4x3(float, M11, M21, M31, M12, M22, M32, M13, V, M33, M14, M24, M34);
        9 -> mat4x3(float, M11, M21, M31, M12, M22, M32, M13, M23, V, M14, M24, M34);
        10 -> mat4x3(float, M11, M21, M31, M12, M22, M32, M13, M23, M33, V, M24, M34);
        11 -> mat4x3(float, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, V, M34);
        12 -> mat4x3(float, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, V)
    end;
mat4x3_set_element(double, D, I, V) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float
    >> = D,
    case I of
        1 -> mat4x3(double, V, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34);
        2 -> mat4x3(double, M11, V, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34);
        3 -> mat4x3(double, M11, M21, V, M12, M22, M32, M13, M23, M33, M14, M24, M34);
        4 -> mat4x3(double, M11, M21, M31, V, M22, M32, M13, M23, M33, M14, M24, M34);
        5 -> mat4x3(double, M11, M21, M31, M12, V, M32, M13, M23, M33, M14, M24, M34);
        6 -> mat4x3(double, M11, M21, M31, M12, M22, V, M13, M23, M33, M14, M24, M34);
        7 -> mat4x3(double, M11, M21, M31, M12, M22, M32, V, M23, M33, M14, M24, M34);
        8 -> mat4x3(double, M11, M21, M31, M12, M22, M32, M13, V, M33, M14, M24, M34);
        9 -> mat4x3(double, M11, M21, M31, M12, M22, M32, M13, M23, V, M14, M24, M34);
        10 -> mat4x3(double, M11, M21, M31, M12, M22, M32, M13, M23, M33, V, M24, M34);
        11 -> mat4x3(double, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, V, M34);
        12 -> mat4x3(double, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, V)
    end.

-doc("""
Returns `D` with the element at column `C` and row `R` replaced by `V` in a raw 4x3 matrix payload.
""").
-spec mat4x3_set_element(glm:type(), binary(), pos_integer(), pos_integer(), float()) -> binary().
mat4x3_set_element(T, D, C, R, V) ->
    mat4x3_set_element(T, D, (C - 1) * 3 + R, V).

-doc("""
Decodes a raw 4x3 matrix payload into a BEAM tuple in column-major order.
""").
-spec mat4x3_values(glm:type(), binary()) -> {float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()}.
mat4x3_values(float, D) ->
    <<
        M11:32/little-float,
        M21:32/little-float,
        M31:32/little-float,
        M12:32/little-float,
        M22:32/little-float,
        M32:32/little-float,
        M13:32/little-float,
        M23:32/little-float,
        M33:32/little-float,
        M14:32/little-float,
        M24:32/little-float,
        M34:32/little-float
    >> = D,
    {M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34};
mat4x3_values(double, D) ->
    <<
        M11:64/little-float,
        M21:64/little-float,
        M31:64/little-float,
        M12:64/little-float,
        M22:64/little-float,
        M32:64/little-float,
        M13:64/little-float,
        M23:64/little-float,
        M33:64/little-float,
        M14:64/little-float,
        M24:64/little-float,
        M34:64/little-float
    >> = D,
    {M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34}.

-doc("""
Applies `bit_count` to a raw scalar or vector integer payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec bit_count(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
bit_count(T, L, X) ->
    bit_count_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `bit_count/3` using encoded type metadata.
""").
-spec bit_count_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
bit_count_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `bitfield_extract` to raw scalar or vector integer payloads.

`Value`, `Offset`, and `Bits` are all raw payloads encoded with the same shape.
""").
-spec bitfield_extract(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Value :: binary(),
    Offset :: binary(),
    Bits :: binary()
) -> binary().
bitfield_extract(T, L, Value, Offset, Bits) ->
    bitfield_extract_raw(?GLM_TYPE(T), L, Value, Offset, Bits).

-doc("""
NIF entry point for `bitfield_extract/5` using encoded type metadata.
""").
-spec bitfield_extract_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Value :: binary(),
    Offset :: binary(),
    Bits :: binary()
) -> binary().
bitfield_extract_raw(_T, _L, _Value, _Offset, _Bits) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `bitfield_insert` to raw scalar or vector integer payloads.

`Base`, `Insert`, `Offset`, and `Bits` are all raw payloads encoded with the
same shape.
""").
-spec bitfield_insert(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Base :: binary(),
    Insert :: binary(),
    Offset :: binary(),
    Bits :: binary()
) -> binary().
bitfield_insert(T, L, Base, Insert, Offset, Bits) ->
    bitfield_insert_raw(?GLM_TYPE(T), L, Base, Insert, Offset, Bits).

-doc("""
NIF entry point for `bitfield_insert/6` using encoded type metadata.
""").
-spec bitfield_insert_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Base :: binary(),
    Insert :: binary(),
    Offset :: binary(),
    Bits :: binary()
) -> binary().
bitfield_insert_raw(_T, _L, _Base, _Insert, _Offset, _Bits) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `bitfield_reverse` to a raw scalar or vector integer payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec bitfield_reverse(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
bitfield_reverse(T, L, X) ->
    bitfield_reverse_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `bitfield_reverse/3` using encoded type metadata.
""").
-spec bitfield_reverse_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
bitfield_reverse_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `find_lsb` to a raw scalar or vector integer payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec find_lsb(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
find_lsb(T, L, X) ->
    find_lsb_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `find_lsb/3` using encoded type metadata.
""").
-spec find_lsb_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
find_lsb_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `find_msb` to a raw scalar or vector integer payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec find_msb(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
find_msb(T, L, X) ->
    find_msb_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `find_msb/3` using encoded type metadata.
""").
-spec find_msb_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
find_msb_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `imul_extended` to raw scalar or vector signed 32-bit payloads.

The return value contains the raw most-significant and least-significant result
payloads.
""").
-spec imul_extended(
    T :: {int, 32},
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
imul_extended(T, L, X, Y) ->
    imul_extended_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `imul_extended/4` using encoded type metadata.
""").
-spec imul_extended_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
imul_extended_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `uadd_carry` to raw scalar or vector unsigned 32-bit payloads.

The return value contains the raw sum payload and the raw carry payload.
""").
-spec uadd_carry(
    T :: {uint, 32},
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
uadd_carry(T, L, X, Y) ->
    uadd_carry_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `uadd_carry/4` using encoded type metadata.
""").
-spec uadd_carry_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
uadd_carry_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `umul_extended` to raw scalar or vector unsigned 32-bit payloads.

The return value contains the raw most-significant and least-significant result
payloads.
""").
-spec umul_extended(
    T :: {uint, 32},
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
umul_extended(T, L, X, Y) ->
    umul_extended_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `umul_extended/4` using encoded type metadata.
""").
-spec umul_extended_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
umul_extended_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `usub_borrow` to raw scalar or vector unsigned 32-bit payloads.

The return value contains the raw difference payload and the raw borrow payload.
""").
-spec usub_borrow(
    T :: {uint, 32},
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
usub_borrow(T, L, X, Y) ->
    usub_borrow_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `usub_borrow/4` using encoded type metadata.
""").
-spec usub_borrow_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary(),
    Y :: binary()
) -> {binary(), binary()}.
usub_borrow_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `is_multiple` to raw scalar or vector integer payloads.

`Pattern` selects whether the call uses scalar-scalar, vector-scalar, or
vector-vector broadcasting.
""").
-spec is_multiple(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar} | {vector, scalar} | {vector, vector},
    X :: binary(), Y :: binary()
) -> binary().
is_multiple(T, L, Pattern, X, Y) ->
    is_multiple_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar} -> 0;
            {vector, scalar} -> 1;
            {vector, vector} -> 2
        end,
        X, Y
    ).

-doc("""
NIF entry point for `is_multiple/5` using encoded type and overload metadata.
""").
-spec is_multiple_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), Y :: binary()
) -> binary().
is_multiple_raw(_T, _L, _Pattern, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `is_power_of_two` to a raw scalar or vector integer payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec is_power_of_two(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
is_power_of_two(T, L, X) ->
    is_power_of_two_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `is_power_of_two/3` using encoded type metadata.
""").
-spec is_power_of_two_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
is_power_of_two_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `next_multiple` to raw scalar or vector integer payloads.

`Pattern` selects whether the call uses scalar-scalar, vector-scalar, or
vector-vector broadcasting.
""").
-spec next_multiple(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar} | {vector, scalar} | {vector, vector},
    X :: binary(), Y :: binary()
) -> binary().
next_multiple(T, L, Pattern, X, Y) ->
    next_multiple_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar} -> 0;
            {vector, scalar} -> 1;
            {vector, vector} -> 2
        end,
        X, Y
    ).

-doc("""
NIF entry point for `next_multiple/5` using encoded type and overload metadata.
""").
-spec next_multiple_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), Y :: binary()
) -> binary().
next_multiple_raw(_T, _L, _Pattern, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `next_power_of_two` to a raw scalar or vector integer payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec next_power_of_two(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
next_power_of_two(T, L, X) ->
    next_power_of_two_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `next_power_of_two/3` using encoded type metadata.
""").
-spec next_power_of_two_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
next_power_of_two_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `prev_multiple` to raw scalar or vector integer payloads.

`Pattern` selects whether the call uses scalar-scalar, vector-scalar, or
vector-vector broadcasting.
""").
-spec prev_multiple(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar} | {vector, scalar} | {vector, vector},
    X :: binary(), Y :: binary()
) -> binary().
prev_multiple(T, L, Pattern, X, Y) ->
    prev_multiple_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar} -> 0;
            {vector, scalar} -> 1;
            {vector, vector} -> 2
        end,
        X, Y
    ).

-doc("""
NIF entry point for `prev_multiple/5` using encoded type and overload metadata.
""").
-spec prev_multiple_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), Y :: binary()
) -> binary().
prev_multiple_raw(_T, _L, _Pattern, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `prev_power_of_two` to a raw scalar or vector integer payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec prev_power_of_two(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
prev_power_of_two(T, L, X) ->
    prev_power_of_two_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `prev_power_of_two/3` using encoded type metadata.
""").
-spec prev_power_of_two_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
prev_power_of_two_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `affine_inverse` to a raw matrix payload.

`Shape` identifies whether `X` holds a raw 3x3 or 4x4 matrix payload.
""").
-spec affine_inverse(
    T :: float | double,
    Shape :: {3, 3} | {4, 4},
    X :: binary()
) -> binary().
affine_inverse(T, Shape, X) ->
    affine_inverse_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X).

-doc("""
NIF entry point for `affine_inverse/3` using encoded type and shape metadata.
""").
-spec affine_inverse_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary()
) -> binary().
affine_inverse_raw(_T, _Shape, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `acos` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec acos(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
acos(T, L, X) ->
    acos_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `acos/3` using encoded type metadata.
""").
-spec acos_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
acos_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `acosh` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec acosh(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
acosh(T, L, X) ->
    acosh_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `acosh/3` using encoded type metadata.
""").
-spec acosh_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
acosh_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `asin` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec asin(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
asin(T, L, X) ->
    asin_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `asin/3` using encoded type metadata.
""").
-spec asin_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
asin_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `asinh` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec asinh(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
asinh(T, L, X) ->
    asinh_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `asinh/3` using encoded type metadata.
""").
-spec asinh_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
asinh_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `atan` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec atan(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
atan(T, L, X) ->
    atan_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `atan/3` using encoded type metadata.
""").
-spec atan_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
atan_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `atanh` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec atanh(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
atanh(T, L, X) ->
    atanh_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `atanh/3` using encoded type metadata.
""").
-spec atanh_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
atanh_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `abs` to a raw scalar or vector payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec abs(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
abs(T, L, X) ->
    abs_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `abs/3` using encoded type metadata.
""").
-spec abs_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
abs_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `ceil` to a raw scalar or vector payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec ceil(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
ceil(T, L, X) ->
    ceil_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `ceil/3` using encoded type metadata.
""").
-spec ceil_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
ceil_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Extracts a raw matrix column payload from `X`.

`Shape` identifies the matrix dimensions and `Index` is 1-based.
""").
-spec column(
    T :: float | double,
    Shape :: {glm:length(), glm:length()},
    Index :: pos_integer(),
    X :: binary()
) -> binary().
column(T, Shape, Index, X) ->
    column_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), Index, X).

-doc("""
NIF entry point for `column/4` using encoded type and shape metadata.
""").
-spec column_raw(
    T :: integer(),
    Shape :: integer(),
    Index :: pos_integer(),
    X :: binary()
) -> binary().
column_raw(_T, _Shape, _Index, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `cos` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec cos(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
cos(T, L, X) ->
    cos_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `cos/3` using encoded type metadata.
""").
-spec cos_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
cos_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `cosh` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec cosh(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
cosh(T, L, X) ->
    cosh_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `cosh/3` using encoded type metadata.
""").
-spec cosh_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
cosh_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `cross` to raw 3-component vector payloads.

`X` and `Y` must be encoded with the same floating-point element type.
""").
-spec cross(
    T :: float | double,
    L :: 3,
    X :: binary(),
    Y :: binary()
) -> binary().
cross(T, L, X, Y) ->
    cross_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `cross/4` using encoded type metadata.
""").
-spec cross_raw(
    T :: integer(),
    L :: 3,
    X :: binary(),
    Y :: binary()
) -> binary().
cross_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `determinant` to a raw square matrix payload.

`Shape` identifies whether `X` holds a raw 2x2, 3x3, or 4x4 matrix payload.
""").
-spec determinant(
    T :: float | double,
    Shape :: {2, 2} | {3, 3} | {4, 4},
    X :: binary()
) -> binary().
determinant(T, Shape, X) ->
    determinant_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X).

-doc("""
NIF entry point for `determinant/3` using encoded type and shape metadata.
""").
-spec determinant_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary()
) -> binary().
determinant_raw(_T, _Shape, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `degrees` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec degrees(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
degrees(T, L, X) ->
    degrees_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `degrees/3` using encoded type metadata.
""").
-spec degrees_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
degrees_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `distance` to raw floating-point vector payloads.

`L` identifies the vector length shared by `X` and `Y`.
""").
-spec distance(
    T :: float | double,
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
distance(T, L, X, Y) ->
    distance_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `distance/4` using encoded type metadata.
""").
-spec distance_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
distance_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `dot` to raw floating-point vector payloads.

`L` identifies the vector length shared by `X` and `Y`.
""").
-spec dot(
    T :: float | double,
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
dot(T, L, X, Y) ->
    dot_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `dot/4` using encoded type metadata.
""").
-spec dot_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
dot_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `face_forward` to raw floating-point vector payloads.

`N`, `I`, and `NRef` must all use the same encoded vector shape.
""").
-spec face_forward(
    T :: float | double,
    L :: glm:length(),
    N :: binary(),
    I :: binary(),
    NRef :: binary()
) -> binary().
face_forward(T, L, N, I, NRef) ->
    face_forward_raw(?GLM_TYPE(T), L, N, I, NRef).

-doc("""
NIF entry point for `face_forward/5` using encoded type metadata.
""").
-spec face_forward_raw(
    T :: integer(),
    L :: glm:length(),
    N :: binary(),
    I :: binary(),
    NRef :: binary()
) -> binary().
face_forward_raw(_T, _L, _N, _I, _NRef) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Clamps a raw scalar or vector payload between raw minimum and maximum payloads.

`Pattern` selects which overload shape is being used: scalar/scalar/scalar,
vector/scalar/scalar, or vector/vector/vector.
""").
-spec clamp(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar, scalar} | {vector, scalar, scalar} | {vector, vector, vector},
    X :: binary(), MinVal :: binary(), MaxVal :: binary()
) -> binary().
clamp(T, L, Pattern, X, MinVal, MaxVal) ->
    clamp_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar, scalar} -> 0;
            {vector, scalar, scalar} -> 1;
            {vector, vector, vector} -> 2
        end,
        X, MinVal, MaxVal
    ).

-doc("""
NIF entry point for `clamp/6` using encoded type and overload metadata.
""").
-spec clamp_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), MinVal :: binary(), MaxVal :: binary()
) -> binary().
clamp_raw(_T, _L, _Pattern, _X, _MinVal, _MaxVal) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Reinterprets a raw floating-point payload as a raw signed 32-bit integer payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec float_bits_to_int(
    T :: float,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
float_bits_to_int(T, L, X) ->
    float_bits_to_int_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `float_bits_to_int/3` using encoded type metadata.
""").
-spec float_bits_to_int_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
float_bits_to_int_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Reinterprets a raw floating-point payload as a raw unsigned 32-bit integer payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec float_bits_to_uint(
    T :: float,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
float_bits_to_uint(T, L, X) ->
    float_bits_to_uint_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `float_bits_to_uint/3` using encoded type metadata.
""").
-spec float_bits_to_uint_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
float_bits_to_uint_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `exp` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec exp(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
exp(T, L, X) ->
    exp_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `exp/3` using encoded type metadata.
""").
-spec exp_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
exp_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `exp2` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec exp2(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
exp2(T, L, X) ->
    exp2_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `exp2/3` using encoded type metadata.
""").
-spec exp2_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
exp2_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `fma` to raw floating-point scalar or vector payloads.

`A`, `B`, and `C` must all use the same encoded shape.
""").
-spec fma(
    T :: float | double,
    L :: undefined | glm:length(),
    A :: binary(), B :: binary(), C :: binary()
) -> binary().
fma(T, L, A, B, C) ->
    fma_raw(?GLM_TYPE(T), L, A, B, C).

-doc("""
NIF entry point for `fma/5` using encoded type metadata.
""").
-spec fma_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    A :: binary(), B :: binary(), C :: binary()
) -> binary().
fma_raw(_T, _L, _A, _B, _C) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `fract` to a raw scalar or vector payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec fract(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
fract(T, L, X) ->
    fract_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `fract/3` using encoded type metadata.
""").
-spec fract_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
fract_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `frexp` to a raw floating-point scalar or vector payload.

The return value contains the raw significand payload and the raw exponent
payload.
""").
-spec frexp(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> {binary(), binary()}.
frexp(T, L, X) ->
    frexp_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `frexp/3` using encoded type metadata.
""").
-spec frexp_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> {binary(), binary()}.
frexp_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `floor` to a raw scalar or vector payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec floor(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
floor(T, L, X) ->
    floor_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `floor/3` using encoded type metadata.
""").
-spec floor_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
floor_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `inverse_sqrt` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec inverse_sqrt(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
inverse_sqrt(T, L, X) ->
    inverse_sqrt_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `inverse_sqrt/3` using encoded type metadata.
""").
-spec inverse_sqrt_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
inverse_sqrt_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `inverse` to a raw square matrix payload.

`Shape` identifies whether `X` holds a raw 2x2, 3x3, or 4x4 matrix payload.
""").
-spec inverse(
    T :: float | double,
    Shape :: {2, 2} | {3, 3} | {4, 4},
    X :: binary()
) -> binary().
inverse(T, Shape, X) ->
    inverse_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X).

-doc("""
NIF entry point for `inverse/3` using encoded type and shape metadata.
""").
-spec inverse_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary()
) -> binary().
inverse_raw(_T, _Shape, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `inverse_transpose` to a raw square matrix payload.

`Shape` identifies whether `X` holds a raw 2x2, 3x3, or 4x4 matrix payload.
""").
-spec inverse_transpose(
    T :: float | double,
    Shape :: {2, 2} | {3, 3} | {4, 4},
    X :: binary()
) -> binary().
inverse_transpose(T, Shape, X) ->
    inverse_transpose_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X).

-doc("""
NIF entry point for `inverse_transpose/3` using encoded type and shape metadata.
""").
-spec inverse_transpose_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary()
) -> binary().
inverse_transpose_raw(_T, _Shape, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Reinterprets a raw signed 32-bit integer payload as a raw floating-point payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec int_bits_to_float(
    T :: {int, 32},
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
int_bits_to_float(T, L, X) ->
    int_bits_to_float_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `int_bits_to_float/3` using encoded type metadata.
""").
-spec int_bits_to_float_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
int_bits_to_float_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `is_inf` to a raw floating-point vector payload.

The result is a raw boolean vector payload with the same length.
""").
-spec is_inf(
    T :: float | double,
    L :: glm:length(),
    X :: binary()
) -> binary().
is_inf(T, L, X) ->
    is_inf_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `is_inf/3` using encoded type metadata.
""").
-spec is_inf_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary()
) -> binary().
is_inf_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `is_nan` to a raw floating-point vector payload.

The result is a raw boolean vector payload with the same length.
""").
-spec is_nan(
    T :: float | double,
    L :: glm:length(),
    X :: binary()
) -> binary().
is_nan(T, L, X) ->
    is_nan_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `is_nan/3` using encoded type metadata.
""").
-spec is_nan_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary()
) -> binary().
is_nan_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `ldexp` to raw floating-point scalar or vector payloads.

`Exp` is a raw exponent payload encoded with the same shape as `X`.
""").
-spec ldexp(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary(), Exp :: binary()
) -> binary().
ldexp(T, L, X, Exp) ->
    ldexp_raw(?GLM_TYPE(T), L, X, Exp).

-doc("""
NIF entry point for `ldexp/4` using encoded type metadata.
""").
-spec ldexp_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary(), Exp :: binary()
) -> binary().
ldexp_raw(_T, _L, _X, _Exp) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `length` to a raw floating-point vector payload.

The return value is a raw scalar payload of the same floating-point type.
""").
-spec length(
    T :: float | double,
    L :: glm:length(),
    X :: binary()
) -> binary().
length(T, L, X) ->
    length_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `length/3` using encoded type metadata.
""").
-spec length_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary()
) -> binary().
length_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `matrix_comp_mult` to raw matrix payloads of the same shape.

`Shape` identifies the matrix dimensions shared by `X` and `Y`.
""").
-spec matrix_comp_mult(
    T :: float | double,
    Shape :: {glm:length(), glm:length()},
    X :: binary(),
    Y :: binary()
) -> binary().
matrix_comp_mult(T, Shape, X, Y) ->
    matrix_comp_mult_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X, Y).

-doc("""
NIF entry point for `matrix_comp_mult/4` using encoded type and shape metadata.
""").
-spec matrix_comp_mult_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary(),
    Y :: binary()
) -> binary().
matrix_comp_mult_raw(_T, _Shape, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `log` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec log(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
log(T, L, X) ->
    log_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `log/3` using encoded type metadata.
""").
-spec log_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
log_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `log2` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec log2(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
log2(T, L, X) ->
    log2_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `log2/3` using encoded type metadata.
""").
-spec log2_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
log2_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `max` to raw scalar or vector payloads.

`Pattern` selects whether the call uses scalar-scalar, vector-scalar, or
vector-vector broadcasting.
""").
-spec max(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar} | {vector, scalar} | {vector, vector},
    X :: binary(), Y :: binary()
) -> binary().
max(T, L, Pattern, X, Y) ->
    max_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar} -> 0;
            {vector, scalar} -> 1;
            {vector, vector} -> 2
        end,
        X, Y
    ).

-doc("""
NIF entry point for `max/5` using encoded type and overload metadata.
""").
-spec max_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), Y :: binary()
) -> binary().
max_raw(_T, _L, _Pattern, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `min` to raw scalar or vector payloads.

`Pattern` selects whether the call uses scalar-scalar, vector-scalar, or
vector-vector broadcasting.
""").
-spec min(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar} | {vector, scalar} | {vector, vector},
    X :: binary(), Y :: binary()
) -> binary().
min(T, L, Pattern, X, Y) ->
    min_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar} -> 0;
            {vector, scalar} -> 1;
            {vector, vector} -> 2
        end,
        X, Y
    ).

-doc("""
NIF entry point for `min/5` using encoded type and overload metadata.
""").
-spec min_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), Y :: binary()
) -> binary().
min_raw(_T, _L, _Pattern, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `mix` to raw scalar or vector payloads.

`Pattern` selects scalar-scalar-scalar, vector-vector-scalar, or
vector-vector-vector interpolation.
""").
-spec mix(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar, scalar} | {vector, vector, scalar} | {vector, vector, vector},
    X :: binary(), Y :: binary(), A :: binary()
) -> binary().
mix(T, L, Pattern, X, Y, A) ->
    mix_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar, scalar} -> 0;
            {vector, vector, scalar} -> 1;
            {vector, vector, vector} -> 2
        end,
        X, Y, A
    ).

-doc("""
NIF entry point for `mix/6` using encoded type and overload metadata.
""").
-spec mix_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), Y :: binary(), A :: binary()
) -> binary().
mix_raw(_T, _L, _Pattern, _X, _Y, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `mod` to raw scalar or vector payloads.

`Pattern` selects whether the call uses scalar-scalar, vector-scalar, or
vector-vector broadcasting.
""").
-spec mod(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar} | {vector, scalar} | {vector, vector},
    X :: binary(), Y :: binary()
) -> binary().
mod(T, L, Pattern, X, Y) ->
    mod_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar} -> 0;
            {vector, scalar} -> 1;
            {vector, vector} -> 2
        end,
        X, Y
    ).

-doc("""
NIF entry point for `mod/5` using encoded type and overload metadata.
""").
-spec mod_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    X :: binary(), Y :: binary()
) -> binary().
mod_raw(_T, _L, _Pattern, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `modf` to a raw scalar or vector payload.

The return value contains the raw integer-part payload and the raw fractional
part payload.
""").
-spec modf(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> {binary(), binary()}.
modf(T, L, X) ->
    modf_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `modf/3` using encoded type metadata.
""").
-spec modf_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> {binary(), binary()}.
modf_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `normalize` to a raw floating-point vector payload.

`L` identifies the vector length encoded in `X`.
""").
-spec normalize(
    T :: float | double,
    L :: glm:length(),
    X :: binary()
) -> binary().
normalize(T, L, X) ->
    normalize_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `normalize/3` using encoded type metadata.
""").
-spec normalize_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary()
) -> binary().
normalize_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Builds a raw matrix payload from the outer product of two raw vector payloads.

`Shape` identifies the shape of the result matrix.
""").
-spec outer_product(
    T :: float | double,
    Shape :: {glm:length(), glm:length()},
    X :: binary(),
    Y :: binary()
) -> binary().
outer_product(T, Shape, X, Y) ->
    outer_product_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X, Y).

-doc("""
NIF entry point for `outer_product/4` using encoded type and shape metadata.
""").
-spec outer_product_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary(),
    Y :: binary()
) -> binary().
outer_product_raw(_T, _Shape, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pow` to raw floating-point scalar or vector payloads.

`X` and `Y` must use the same encoded shape.
""").
-spec pow(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary(), Y :: binary()
) -> binary().
pow(T, L, X, Y) ->
    pow_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `pow/4` using encoded type metadata.
""").
-spec pow_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary(), Y :: binary()
) -> binary().
pow_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `radians` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec radians(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
radians(T, L, X) ->
    radians_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `radians/3` using encoded type metadata.
""").
-spec radians_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
radians_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `reflect` to raw floating-point vector payloads.

`I` and `N` must use the same encoded vector shape.
""").
-spec reflect(
    T :: float | double,
    L :: glm:length(),
    I :: binary(),
    N :: binary()
) -> binary().
reflect(T, L, I, N) ->
    reflect_raw(?GLM_TYPE(T), L, I, N).

-doc("""
NIF entry point for `reflect/4` using encoded type metadata.
""").
-spec reflect_raw(
    T :: integer(),
    L :: glm:length(),
    I :: binary(),
    N :: binary()
) -> binary().
reflect_raw(_T, _L, _I, _N) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `refract` to raw floating-point vector payloads.

`Eta` is a raw scalar payload using the same floating-point element type.
""").
-spec refract(
    T :: float | double,
    L :: glm:length(),
    I :: binary(),
    N :: binary(),
    Eta :: binary()
) -> binary().
refract(T, L, I, N, Eta) ->
    refract_raw(?GLM_TYPE(T), L, I, N, Eta).

-doc("""
NIF entry point for `refract/5` using encoded type metadata.
""").
-spec refract_raw(
    T :: integer(),
    L :: glm:length(),
    I :: binary(),
    N :: binary(),
    Eta :: binary()
) -> binary().
refract_raw(_T, _L, _I, _N, _Eta) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Extracts a raw matrix row payload from `X`.

`Shape` identifies the matrix dimensions and `Index` is 1-based.
""").
-spec row(
    T :: float | double,
    Shape :: {glm:length(), glm:length()},
    Index :: pos_integer(),
    X :: binary()
) -> binary().
row(T, Shape, Index, X) ->
    row_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), Index, X).

-doc("""
NIF entry point for `row/4` using encoded type and shape metadata.
""").
-spec row_raw(
    T :: integer(),
    Shape :: integer(),
    Index :: pos_integer(),
    X :: binary()
) -> binary().
row_raw(_T, _Shape, _Index, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `round` to a raw scalar or vector payload.

`T` uses the safe-layer GLM type tag and `L` identifies the encoded shape.
""").
-spec round(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary()
) -> binary().
round(T, L, X) ->
    round_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `round/3` using encoded type metadata.
""").
-spec round_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary()
) -> binary().
round_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `round_even` to a raw scalar or vector payload.

`T` uses the safe-layer GLM type tag and `L` identifies the encoded shape.
""").
-spec round_even(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary()
) -> binary().
round_even(T, L, X) ->
    round_even_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `round_even/3` using encoded type metadata.
""").
-spec round_even_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary()
) -> binary().
round_even_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `sin` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec sin(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sin(T, L, X) ->
    sin_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `sin/3` using encoded type metadata.
""").
-spec sin_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sin_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `sinh` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec sinh(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sinh(T, L, X) ->
    sinh_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `sinh/3` using encoded type metadata.
""").
-spec sinh_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sinh_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `sign` to a raw scalar or vector payload.

`T` uses the safe-layer GLM type tag and `L` is `undefined` for scalars or the
vector length for vectors.
""").
-spec sign(
    T :: glm:type(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sign(T, L, X) ->
    sign_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `sign/3` using encoded type metadata.
""").
-spec sign_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sign_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `smoothstep` to raw scalar or vector payloads.

`Pattern` selects scalar-scalar-scalar, scalar-scalar-vector, or
vector-vector-vector interpolation.
""").
-spec smoothstep(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar, scalar} | {scalar, scalar, vector} | {vector, vector, vector},
    Edge0 :: binary(), Edge1 :: binary(), X :: binary()
) -> binary().
smoothstep(T, L, Pattern, Edge0, Edge1, X) ->
    smoothstep_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar, scalar} -> 0;
            {scalar, scalar, vector} -> 1;
            {vector, vector, vector} -> 2
        end,
        Edge0, Edge1, X
    ).

-doc("""
NIF entry point for `smoothstep/6` using encoded type and overload metadata.
""").
-spec smoothstep_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    Edge0 :: binary(), Edge1 :: binary(), X :: binary()
) -> binary().
smoothstep_raw(_T, _L, _Pattern, _Edge0, _Edge1, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `step` to raw scalar or vector payloads.

`Pattern` selects scalar-scalar, scalar-vector, or vector-vector comparison.
""").
-spec step(
    T :: glm:type(),
    L :: undefined | glm:length(),
    Pattern :: {scalar, scalar} | {scalar, vector} | {vector, vector},
    Edge :: binary(), X :: binary()
) -> binary().
step(T, L, Pattern, Edge, X) ->
    step_raw(
        ?GLM_TYPE(T),
        L,
        case Pattern of
            {scalar, scalar} -> 0;
            {scalar, vector} -> 1;
            {vector, vector} -> 2
        end,
        Edge, X
    ).

-doc("""
NIF entry point for `step/5` using encoded type and overload metadata.
""").
-spec step_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    Pattern :: integer(),
    Edge :: binary(), X :: binary()
) -> binary().
step_raw(_T, _L, _Pattern, _Edge, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `sqrt` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec sqrt(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sqrt(T, L, X) ->
    sqrt_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `sqrt/3` using encoded type metadata.
""").
-spec sqrt_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
sqrt_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `tan` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec tan(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
tan(T, L, X) ->
    tan_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `tan/3` using encoded type metadata.
""").
-spec tan_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
tan_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `tanh` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec tanh(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
tanh(T, L, X) ->
    tanh_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `tanh/3` using encoded type metadata.
""").
-spec tanh_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
tanh_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `transpose` to a raw matrix payload.

`Shape` identifies the matrix dimensions encoded in `X`.
""").
-spec transpose(
    T :: float | double,
    Shape :: {glm:length(), glm:length()},
    X :: binary()
) -> binary().
transpose(T, Shape, X) ->
    transpose_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X).

-doc("""
NIF entry point for `transpose/3` using encoded type and shape metadata.
""").
-spec transpose_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary()
) -> binary().
transpose_raw(_T, _Shape, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_double_2x32` to a raw double payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_double_2x32(
    T :: double,
    X :: binary()
) -> binary().
pack_double_2x32(T, X) ->
    pack_double_2x32_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_double_2x32/2` using encoded type metadata.
""").
-spec pack_double_2x32_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_double_2x32_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_half_1x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_half_1x16(
    T :: float,
    X :: binary()
) -> binary().
pack_half_1x16(T, X) ->
    pack_half_1x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_half_1x16/2` using encoded type metadata.
""").
-spec pack_half_1x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_half_1x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_half_2x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_half_2x16(
    T :: float,
    X :: binary()
) -> binary().
pack_half_2x16(T, X) ->
    pack_half_2x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_half_2x16/2` using encoded type metadata.
""").
-spec pack_half_2x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_half_2x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_half_4x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_half_4x16(
    T :: float,
    X :: binary()
) -> binary().
pack_half_4x16(T, X) ->
    pack_half_4x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_half_4x16/2` using encoded type metadata.
""").
-spec pack_half_4x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_half_4x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_snorm_1x8` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_snorm_1x8(
    T :: float,
    X :: binary()
) -> binary().
pack_snorm_1x8(T, X) ->
    pack_snorm_1x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_snorm_1x8/2` using encoded type metadata.
""").
-spec pack_snorm_1x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_snorm_1x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_snorm_2x8` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_snorm_2x8(
    T :: float,
    X :: binary()
) -> binary().
pack_snorm_2x8(T, X) ->
    pack_snorm_2x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_snorm_2x8/2` using encoded type metadata.
""").
-spec pack_snorm_2x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_snorm_2x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_snorm_1x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_snorm_1x16(
    T :: float,
    X :: binary()
) -> binary().
pack_snorm_1x16(T, X) ->
    pack_snorm_1x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_snorm_1x16/2` using encoded type metadata.
""").
-spec pack_snorm_1x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_snorm_1x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_snorm_2x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_snorm_2x16(
    T :: float,
    X :: binary()
) -> binary().
pack_snorm_2x16(T, X) ->
    pack_snorm_2x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_snorm_2x16/2` using encoded type metadata.
""").
-spec pack_snorm_2x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_snorm_2x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_snorm_4x8` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_snorm_4x8(
    T :: float,
    X :: binary()
) -> binary().
pack_snorm_4x8(T, X) ->
    pack_snorm_4x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_snorm_4x8/2` using encoded type metadata.
""").
-spec pack_snorm_4x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_snorm_4x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_snorm_4x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_snorm_4x16(
    T :: float,
    X :: binary()
) -> binary().
pack_snorm_4x16(T, X) ->
    pack_snorm_4x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_snorm_4x16/2` using encoded type metadata.
""").
-spec pack_snorm_4x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_snorm_4x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_unorm_1x8` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_unorm_1x8(
    T :: float,
    X :: binary()
) -> binary().
pack_unorm_1x8(T, X) ->
    pack_unorm_1x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_unorm_1x8/2` using encoded type metadata.
""").
-spec pack_unorm_1x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_unorm_1x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_unorm_2x8` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_unorm_2x8(
    T :: float,
    X :: binary()
) -> binary().
pack_unorm_2x8(T, X) ->
    pack_unorm_2x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_unorm_2x8/2` using encoded type metadata.
""").
-spec pack_unorm_2x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_unorm_2x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_unorm_1x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_unorm_1x16(
    T :: float,
    X :: binary()
) -> binary().
pack_unorm_1x16(T, X) ->
    pack_unorm_1x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_unorm_1x16/2` using encoded type metadata.
""").
-spec pack_unorm_1x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_unorm_1x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_unorm_2x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_unorm_2x16(
    T :: float,
    X :: binary()
) -> binary().
pack_unorm_2x16(T, X) ->
    pack_unorm_2x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_unorm_2x16/2` using encoded type metadata.
""").
-spec pack_unorm_2x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_unorm_2x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_unorm_4x8` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_unorm_4x8(
    T :: float,
    X :: binary()
) -> binary().
pack_unorm_4x8(T, X) ->
    pack_unorm_4x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_unorm_4x8/2` using encoded type metadata.
""").
-spec pack_unorm_4x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_unorm_4x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `pack_unorm_4x16` to a raw float payload.

The result is the packed raw integer payload produced by GLM.
""").
-spec pack_unorm_4x16(
    T :: float,
    X :: binary()
) -> binary().
pack_unorm_4x16(T, X) ->
    pack_unorm_4x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pack_unorm_4x16/2` using encoded type metadata.
""").
-spec pack_unorm_4x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pack_unorm_4x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_double_2x32` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_double_2x32(
    T :: double,
    X :: binary()
) -> binary().
unpack_double_2x32(T, X) ->
    unpack_double_2x32_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_double_2x32/2` using encoded type metadata.
""").
-spec unpack_double_2x32_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_double_2x32_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_half_1x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_half_1x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_half_1x16(T, X) ->
    unpack_half_1x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_half_1x16/2` using encoded type metadata.
""").
-spec unpack_half_1x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_half_1x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_half_2x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_half_2x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_half_2x16(T, X) ->
    unpack_half_2x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_half_2x16/2` using encoded type metadata.
""").
-spec unpack_half_2x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_half_2x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_half_4x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_half_4x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_half_4x16(T, X) ->
    unpack_half_4x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_half_4x16/2` using encoded type metadata.
""").
-spec unpack_half_4x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_half_4x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_snorm_1x8` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_snorm_1x8(
    T :: float,
    X :: binary()
) -> binary().
unpack_snorm_1x8(T, X) ->
    unpack_snorm_1x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_snorm_1x8/2` using encoded type metadata.
""").
-spec unpack_snorm_1x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_snorm_1x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_snorm_2x8` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_snorm_2x8(
    T :: float,
    X :: binary()
) -> binary().
unpack_snorm_2x8(T, X) ->
    unpack_snorm_2x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_snorm_2x8/2` using encoded type metadata.
""").
-spec unpack_snorm_2x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_snorm_2x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_snorm_1x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_snorm_1x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_snorm_1x16(T, X) ->
    unpack_snorm_1x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_snorm_1x16/2` using encoded type metadata.
""").
-spec unpack_snorm_1x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_snorm_1x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_snorm_2x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_snorm_2x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_snorm_2x16(T, X) ->
    unpack_snorm_2x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_snorm_2x16/2` using encoded type metadata.
""").
-spec unpack_snorm_2x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_snorm_2x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_snorm_4x8` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_snorm_4x8(
    T :: float,
    X :: binary()
) -> binary().
unpack_snorm_4x8(T, X) ->
    unpack_snorm_4x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_snorm_4x8/2` using encoded type metadata.
""").
-spec unpack_snorm_4x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_snorm_4x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_snorm_4x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_snorm_4x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_snorm_4x16(T, X) ->
    unpack_snorm_4x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_snorm_4x16/2` using encoded type metadata.
""").
-spec unpack_snorm_4x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_snorm_4x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_unorm_1x8` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_unorm_1x8(
    T :: float,
    X :: binary()
) -> binary().
unpack_unorm_1x8(T, X) ->
    unpack_unorm_1x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_unorm_1x8/2` using encoded type metadata.
""").
-spec unpack_unorm_1x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_unorm_1x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_unorm_2x8` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_unorm_2x8(
    T :: float,
    X :: binary()
) -> binary().
unpack_unorm_2x8(T, X) ->
    unpack_unorm_2x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_unorm_2x8/2` using encoded type metadata.
""").
-spec unpack_unorm_2x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_unorm_2x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_unorm_1x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_unorm_1x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_unorm_1x16(T, X) ->
    unpack_unorm_1x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_unorm_1x16/2` using encoded type metadata.
""").
-spec unpack_unorm_1x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_unorm_1x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_unorm_2x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_unorm_2x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_unorm_2x16(T, X) ->
    unpack_unorm_2x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_unorm_2x16/2` using encoded type metadata.
""").
-spec unpack_unorm_2x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_unorm_2x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_unorm_4x8` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_unorm_4x8(
    T :: float,
    X :: binary()
) -> binary().
unpack_unorm_4x8(T, X) ->
    unpack_unorm_4x8_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_unorm_4x8/2` using encoded type metadata.
""").
-spec unpack_unorm_4x8_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_unorm_4x8_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `unpack_unorm_4x16` to a packed raw payload.

The result is the raw floating-point payload produced by GLM.
""").
-spec unpack_unorm_4x16(
    T :: float,
    X :: binary()
) -> binary().
unpack_unorm_4x16(T, X) ->
    unpack_unorm_4x16_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `unpack_unorm_4x16/2` using encoded type metadata.
""").
-spec unpack_unorm_4x16_raw(
    T :: integer(),
    X :: binary()
) -> binary().
unpack_unorm_4x16_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `frustum` to raw scalar payloads and returns a raw projection-matrix payload.
""").
-spec frustum(
    T :: float | double,
    Left :: binary(),
    Right :: binary(),
    Bottom :: binary(),
    Top :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
frustum(T, Left, Right, Bottom, Top, Near, Far) ->
    frustum_raw(?GLM_TYPE(T), Left, Right, Bottom, Top, Near, Far).

-doc("""
NIF entry point for `frustum/7` using encoded type metadata.
""").
-spec frustum_raw(
    T :: integer(),
    Left :: binary(),
    Right :: binary(),
    Bottom :: binary(),
    Top :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
frustum_raw(_T, _Left, _Right, _Bottom, _Top, _Near, _Far) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `infinite_perspective` to raw scalar payloads and returns a raw projection-matrix payload.
""").
-spec infinite_perspective(
    T :: float | double,
    Fovy :: binary(),
    Aspect :: binary(),
    Near :: binary()
) -> binary().
infinite_perspective(T, Fovy, Aspect, Near) ->
    infinite_perspective_raw(?GLM_TYPE(T), Fovy, Aspect, Near).

-doc("""
NIF entry point for `infinite_perspective/4` using encoded type metadata.
""").
-spec infinite_perspective_raw(
    T :: integer(),
    Fovy :: binary(),
    Aspect :: binary(),
    Near :: binary()
) -> binary().
infinite_perspective_raw(_T, _Fovy, _Aspect, _Near) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `look_at` to raw vector payloads and returns a raw view-matrix payload.

`Eye`, `Center`, and `Up` must be encoded as compatible 3-component vectors.
""").
-spec look_at(
    T :: float | double,
    Eye :: binary(),
    Center :: binary(),
    Up :: binary()
) -> binary().
look_at(T, Eye, Center, Up) ->
    look_at_raw(?GLM_TYPE(T), Eye, Center, Up).

-doc("""
NIF entry point for `look_at/4` using encoded type metadata.
""").
-spec look_at_raw(
    T :: integer(),
    Eye :: binary(),
    Center :: binary(),
    Up :: binary()
) -> binary().
look_at_raw(_T, _Eye, _Center, _Up) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `ortho` to raw scalar payloads and returns a raw projection-matrix payload.
""").
-spec ortho(
    T :: float | double,
    Left :: binary(),
    Right :: binary(),
    Bottom :: binary(),
    Top :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
ortho(T, Left, Right, Bottom, Top, Near, Far) ->
    ortho_raw(?GLM_TYPE(T), Left, Right, Bottom, Top, Near, Far).

-doc("""
NIF entry point for `ortho/7` using encoded type metadata.
""").
-spec ortho_raw(
    T :: integer(),
    Left :: binary(),
    Right :: binary(),
    Bottom :: binary(),
    Top :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
ortho_raw(_T, _Left, _Right, _Bottom, _Top, _Near, _Far) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `perspective` to raw scalar payloads and returns a raw projection-matrix payload.
""").
-spec perspective(
    T :: float | double,
    Fovy :: binary(),
    Aspect :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
perspective(T, Fovy, Aspect, Near, Far) ->
    perspective_raw(?GLM_TYPE(T), Fovy, Aspect, Near, Far).

-doc("""
NIF entry point for `perspective/5` using encoded type metadata.
""").
-spec perspective_raw(
    T :: integer(),
    Fovy :: binary(),
    Aspect :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
perspective_raw(_T, _Fovy, _Aspect, _Near, _Far) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `perspective_fov` to raw scalar payloads and returns a raw projection-matrix payload.
""").
-spec perspective_fov(
    T :: float | double,
    Fov :: binary(),
    Width :: binary(),
    Height :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
perspective_fov(T, Fov, Width, Height, Near, Far) ->
    perspective_fov_raw(?GLM_TYPE(T), Fov, Width, Height, Near, Far).

-doc("""
NIF entry point for `perspective_fov/6` using encoded type metadata.
""").
-spec perspective_fov_raw(
    T :: integer(),
    Fov :: binary(),
    Width :: binary(),
    Height :: binary(),
    Near :: binary(),
    Far :: binary()
) -> binary().
perspective_fov_raw(_T, _Fov, _Width, _Height, _Near, _Far) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `project` to raw vector, matrix, and viewport payloads.

The result is a raw projected-window-coordinate payload.
""").
-spec project(
    T :: float | double,
    Object :: binary(),
    Model :: binary(),
    Projection :: binary(),
    Viewport :: binary()
) -> binary().
project(T, Object, Model, Projection, Viewport) ->
    project_raw(?GLM_TYPE(T), Object, Model, Projection, Viewport).

-doc("""
NIF entry point for `project/5` using encoded type metadata.
""").
-spec project_raw(
    T :: integer(),
    Object :: binary(),
    Model :: binary(),
    Projection :: binary(),
    Viewport :: binary()
) -> binary().
project_raw(_T, _Object, _Model, _Projection, _Viewport) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `scale` to a raw transformation matrix payload using a raw scale-vector payload.
""").
-spec scale(
    T :: float | double,
    Matrix :: binary(),
    Vector :: binary()
) -> binary().
scale(T, Matrix, Vector) ->
    scale_raw(?GLM_TYPE(T), Matrix, Vector).

-doc("""
NIF entry point for `scale/3` using encoded type metadata.
""").
-spec scale_raw(
    T :: integer(),
    Matrix :: binary(),
    Vector :: binary()
) -> binary().
scale_raw(_T, _Matrix, _Vector) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `translate` to a raw transformation matrix payload using a raw translation-vector payload.
""").
-spec translate(
    T :: float | double,
    Matrix :: binary(),
    Vector :: binary()
) -> binary().
translate(T, Matrix, Vector) ->
    translate_raw(?GLM_TYPE(T), Matrix, Vector).

-doc("""
NIF entry point for `translate/3` using encoded type metadata.
""").
-spec translate_raw(
    T :: integer(),
    Matrix :: binary(),
    Vector :: binary()
) -> binary().
translate_raw(_T, _Matrix, _Vector) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `un_project` to raw vector, matrix, and viewport payloads.

The result is a raw object-space coordinate payload.
""").
-spec un_project(
    T :: float | double,
    Window :: binary(),
    Model :: binary(),
    Projection :: binary(),
    Viewport :: binary()
) -> binary().
un_project(T, Window, Model, Projection, Viewport) ->
    un_project_raw(?GLM_TYPE(T), Window, Model, Projection, Viewport).

-doc("""
NIF entry point for `un_project/5` using encoded type metadata.
""").
-spec un_project_raw(
    T :: integer(),
    Window :: binary(),
    Model :: binary(),
    Projection :: binary(),
    Viewport :: binary()
) -> binary().
un_project_raw(_T, _Window, _Model, _Projection, _Viewport) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `all` to a raw boolean vector payload.

The result is a raw boolean scalar payload.
""").
-spec all(
    L :: glm:length(),
    X :: binary()
) -> binary().
all(L, X) ->
    all_raw(L, X).

-doc("""
NIF entry point for `all/2` using encoded length metadata.
""").
-spec all_raw(
    L :: glm:length(),
    X :: binary()
) -> binary().
all_raw(_L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `any` to a raw boolean vector payload.

The result is a raw boolean scalar payload.
""").
-spec any(
    L :: glm:length(),
    X :: binary()
) -> binary().
any(L, X) ->
    any_raw(L, X).

-doc("""
NIF entry point for `any/2` using encoded length metadata.
""").
-spec any_raw(
    L :: glm:length(),
    X :: binary()
) -> binary().
any_raw(_L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `equal` to raw scalar or vector payloads of the same shape.

The result is a raw boolean vector payload.
""").
-spec equal(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
equal(T, L, X, Y) ->
    equal_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `equal/4` using encoded type metadata.
""").
-spec equal_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
equal_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `greater_than` to raw scalar or vector payloads of the same shape.

The result is a raw boolean vector payload.
""").
-spec greater_than(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
greater_than(T, L, X, Y) ->
    greater_than_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `greater_than/4` using encoded type metadata.
""").
-spec greater_than_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
greater_than_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `greater_than_equal` to raw scalar or vector payloads of the same shape.

The result is a raw boolean vector payload.
""").
-spec greater_than_equal(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
greater_than_equal(T, L, X, Y) ->
    greater_than_equal_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `greater_than_equal/4` using encoded type metadata.
""").
-spec greater_than_equal_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
greater_than_equal_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `less_than` to raw scalar or vector payloads of the same shape.

The result is a raw boolean vector payload.
""").
-spec less_than(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
less_than(T, L, X, Y) ->
    less_than_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `less_than/4` using encoded type metadata.
""").
-spec less_than_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
less_than_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `less_than_equal` to raw scalar or vector payloads of the same shape.

The result is a raw boolean vector payload.
""").
-spec less_than_equal(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
less_than_equal(T, L, X, Y) ->
    less_than_equal_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `less_than_equal/4` using encoded type metadata.
""").
-spec less_than_equal_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
less_than_equal_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies logical `not` to a raw boolean vector payload.

The result is a raw boolean vector payload of the same length.
""").
-spec 'not'(
    L :: glm:length(),
    X :: binary()
) -> binary().
'not'(L, X) ->
    not_raw(L, X).

-doc("""
NIF entry point for `not/2` using encoded length metadata.
""").
-spec not_raw(
    L :: glm:length(),
    X :: binary()
) -> binary().
not_raw(_L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `not_equal` to raw scalar or vector payloads of the same shape.

The result is a raw boolean vector payload.
""").
-spec not_equal(
    T :: glm:type(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
not_equal(T, L, X, Y) ->
    not_equal_raw(?GLM_TYPE(T), L, X, Y).

-doc("""
NIF entry point for `not_equal/4` using encoded type metadata.
""").
-spec not_equal_raw(
    T :: integer(),
    L :: glm:length(),
    X :: binary(),
    Y :: binary()
) -> binary().
not_equal_raw(_T, _L, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `angle` to a raw quaternion payload.

The result is a raw scalar angle payload.
""").
-spec angle(
    T :: float | double,
    X :: binary()
) -> binary().
angle(T, X) ->
    angle_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `angle/2` using encoded type metadata.
""").
-spec angle_raw(
    T :: integer(),
    X :: binary()
) -> binary().
angle_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Builds a raw quaternion payload from a raw angle payload and a raw axis-vector payload.
""").
-spec angle_axis(
    T :: float | double,
    Angle :: binary(),
    Axis :: binary()
) -> binary().
angle_axis(T, Angle, Axis) ->
    angle_axis_raw(?GLM_TYPE(T), Angle, Axis).

-doc("""
NIF entry point for `angle_axis/3` using encoded type metadata.
""").
-spec angle_axis_raw(
    T :: integer(),
    Angle :: binary(),
    Axis :: binary()
) -> binary().
angle_axis_raw(_T, _Angle, _Axis) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Extracts the rotation axis from a raw quaternion payload.

The result is a raw 3-component vector payload.
""").
-spec axis(
    T :: float | double,
    X :: binary()
) -> binary().
axis(T, X) ->
    axis_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `axis/2` using encoded type metadata.
""").
-spec axis_raw(
    T :: integer(),
    X :: binary()
) -> binary().
axis_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `conjugate` to a raw quaternion payload.
""").
-spec conjugate(
    T :: float | double,
    X :: binary()
) -> binary().
conjugate(T, X) ->
    conjugate_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `conjugate/2` using encoded type metadata.
""").
-spec conjugate_raw(
    T :: integer(),
    X :: binary()
) -> binary().
conjugate_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies quaternion `dot` to raw quaternion payloads.

The result is a raw scalar payload.
""").
-spec dot(
    T :: float | double,
    X :: binary(),
    Y :: binary()
) -> binary().
dot(T, X, Y) ->
    dot_raw(?GLM_TYPE(T), X, Y).

-doc("""
NIF entry point for quaternion `dot/3` using encoded type metadata.
""").
-spec dot_raw(
    T :: integer(),
    X :: binary(),
    Y :: binary()
) -> binary().
dot_raw(_T, _X, _Y) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Converts a raw quaternion payload into a raw Euler-angle vector payload.
""").
-spec euler_angles(
    T :: float | double,
    X :: binary()
) -> binary().
euler_angles(T, X) ->
    euler_angles_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `euler_angles/2` using encoded type metadata.
""").
-spec euler_angles_raw(
    T :: integer(),
    X :: binary()
) -> binary().
euler_angles_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies quaternion `inverse` to a raw quaternion payload.
""").
-spec inverse(
    T :: float | double,
    X :: binary()
) -> binary().
inverse(T, X) ->
    inverse_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for quaternion `inverse/2` using encoded type metadata.
""").
-spec inverse_raw(
    T :: integer(),
    X :: binary()
) -> binary().
inverse_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies quaternion `length` to a raw quaternion payload.

The result is a raw scalar payload.
""").
-spec length(
    T :: float | double,
    X :: binary()
) -> binary().
length(T, X) ->
    length_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for quaternion `length/2` using encoded type metadata.
""").
-spec length_raw(
    T :: integer(),
    X :: binary()
) -> binary().
length_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Converts a raw quaternion payload into a raw 3x3 matrix payload.
""").
-spec mat3_cast(
    T :: float | double,
    X :: binary()
) -> binary().
mat3_cast(T, X) ->
    mat3_cast_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `mat3_cast/2` using encoded type metadata.
""").
-spec mat3_cast_raw(
    T :: integer(),
    X :: binary()
) -> binary().
mat3_cast_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Converts a raw quaternion payload into a raw 4x4 matrix payload.
""").
-spec mat4_cast(
    T :: float | double,
    X :: binary()
) -> binary().
mat4_cast(T, X) ->
    mat4_cast_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `mat4_cast/2` using encoded type metadata.
""").
-spec mat4_cast_raw(
    T :: integer(),
    X :: binary()
) -> binary().
mat4_cast_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies quaternion `mix` to raw quaternion payloads using a raw interpolation payload.
""").
-spec mix(
    T :: float | double,
    X :: binary(),
    Y :: binary(),
    A :: binary()
) -> binary().
mix(T, X, Y, A) ->
    mix_raw(?GLM_TYPE(T), X, Y, A).

-doc("""
NIF entry point for quaternion `mix/4` using encoded type metadata.
""").
-spec mix_raw(
    T :: integer(),
    X :: binary(),
    Y :: binary(),
    A :: binary()
) -> binary().
mix_raw(_T, _X, _Y, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies quaternion `normalize` to a raw quaternion payload.
""").
-spec normalize(
    T :: float | double,
    X :: binary()
) -> binary().
normalize(T, X) ->
    normalize_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for quaternion `normalize/2` using encoded type metadata.
""").
-spec normalize_raw(
    T :: integer(),
    X :: binary()
) -> binary().
normalize_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Extracts the pitch angle from a raw quaternion payload.

The result is a raw scalar payload.
""").
-spec pitch(
    T :: float | double,
    X :: binary()
) -> binary().
pitch(T, X) ->
    pitch_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `pitch/2` using encoded type metadata.
""").
-spec pitch_raw(
    T :: integer(),
    X :: binary()
) -> binary().
pitch_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Converts a raw 3x3 or 4x4 matrix payload into a raw quaternion payload.

`Shape` identifies the matrix dimensions encoded in `X`.
""").
-spec quat_cast(
    T :: float | double,
    Shape :: {3, 3} | {4, 4},
    X :: binary()
) -> binary().
quat_cast(T, Shape, X) ->
    quat_cast_raw(?GLM_TYPE(T), glm_matrix_shape(Shape), X).

-doc("""
NIF entry point for `quat_cast/3` using encoded type and shape metadata.
""").
-spec quat_cast_raw(
    T :: integer(),
    Shape :: integer(),
    X :: binary()
) -> binary().
quat_cast_raw(_T, _Shape, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Extracts the roll angle from a raw quaternion payload.

The result is a raw scalar payload.
""").
-spec roll(
    T :: float | double,
    X :: binary()
) -> binary().
roll(T, X) ->
    roll_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `roll/2` using encoded type metadata.
""").
-spec roll_raw(
    T :: integer(),
    X :: binary()
) -> binary().
roll_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Rotates a raw vector payload by a raw quaternion payload.
""").
-spec rotate(
    T :: float | double,
    Q :: binary(),
    X :: binary()
) -> binary().
rotate(T, Q, X) ->
    rotate_raw(?GLM_TYPE(T), Q, X).

-doc("""
Applies matrix `rotate` to a raw transformation matrix using raw angle and axis payloads.
""").
-spec rotate(
    T :: float | double,
    Matrix :: binary(),
    Angle :: binary(),
    Axis :: binary()
) -> binary().
rotate(T, Matrix, Angle, Axis) ->
    rotate_raw(?GLM_TYPE(T), Matrix, Angle, Axis).

-doc("""
NIF entry point for quaternion `rotate/3` using encoded type metadata.
""").
-spec rotate_raw(
    T :: integer(),
    Q :: binary(),
    X :: binary()
) -> binary().
rotate_raw(_T, _Q, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
NIF entry point for matrix `rotate/4` using encoded type metadata.
""").
-spec rotate_raw(
    T :: integer(),
    Matrix :: binary(),
    Angle :: binary(),
    Axis :: binary()
) -> binary().
rotate_raw(_T, _Matrix, _Angle, _Axis) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `slerp` to raw quaternion payloads using a raw interpolation payload.
""").
-spec slerp(
    T :: float | double,
    X :: binary(),
    Y :: binary(),
    A :: binary()
) -> binary().
slerp(T, X, Y, A) ->
    slerp_raw(?GLM_TYPE(T), X, Y, A).

-doc("""
NIF entry point for `slerp/4` using encoded type metadata.
""").
-spec slerp_raw(
    T :: integer(),
    X :: binary(),
    Y :: binary(),
    A :: binary()
) -> binary().
slerp_raw(_T, _X, _Y, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Extracts the yaw angle from a raw quaternion payload.

The result is a raw scalar payload.
""").
-spec yaw(
    T :: float | double,
    X :: binary()
) -> binary().
yaw(T, X) ->
    yaw_raw(?GLM_TYPE(T), X).

-doc("""
NIF entry point for `yaw/2` using encoded type metadata.
""").
-spec yaw_raw(
    T :: integer(),
    X :: binary()
) -> binary().
yaw_raw(_T, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `trunc` to a raw floating-point scalar or vector payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec trunc(
    T :: float | double,
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
trunc(T, L, X) ->
    trunc_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `trunc/3` using encoded type metadata.
""").
-spec trunc_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
trunc_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Reinterprets a raw unsigned 32-bit integer payload as a raw floating-point payload.

`L` is `undefined` for scalars or the vector length for vectors.
""").
-spec uint_bits_to_float(
    T :: {uint, 32},
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
uint_bits_to_float(T, L, X) ->
    uint_bits_to_float_raw(?GLM_TYPE(T), L, X).

-doc("""
NIF entry point for `uint_bits_to_float/3` using encoded type metadata.
""").
-spec uint_bits_to_float_raw(
    T :: integer(),
    L :: undefined | glm:length(),
    X :: binary()
) -> binary().
uint_bits_to_float_raw(_T, _L, _X) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `back_ease_in` to a raw scalar payload.
""").
-spec back_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
back_ease_in(T, A) ->
    back_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `back_ease_in/2` using encoded type metadata.
""").
-spec back_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
back_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `back_ease_in` to a raw scalar payload using a raw overshoot payload `O`.
""").
-spec back_ease_in(
    T :: glm:type(),
    A :: binary(),
    O :: binary()
) -> binary().
back_ease_in(T, A, O) ->
    back_ease_in_raw(?GLM_TYPE(T), A, O).

-doc("""
NIF entry point for `back_ease_in/3` using encoded type metadata.
""").
-spec back_ease_in_raw(
    T :: integer(),
    A :: binary(),
    O :: binary()
) -> binary().
back_ease_in_raw(_T, _A, _O) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `back_ease_in_out` to a raw scalar payload.
""").
-spec back_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
back_ease_in_out(T, A) ->
    back_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `back_ease_in_out/2` using encoded type metadata.
""").
-spec back_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
back_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `back_ease_in_out` to a raw scalar payload using a raw overshoot payload `O`.
""").
-spec back_ease_in_out(
    T :: glm:type(),
    A :: binary(),
    O :: binary()
) -> binary().
back_ease_in_out(T, A, O) ->
    back_ease_in_out_raw(?GLM_TYPE(T), A, O).

-doc("""
NIF entry point for `back_ease_in_out/3` using encoded type metadata.
""").
-spec back_ease_in_out_raw(
    T :: integer(),
    A :: binary(),
    O :: binary()
) -> binary().
back_ease_in_out_raw(_T, _A, _O) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `back_ease_out` to a raw scalar payload.
""").
-spec back_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
back_ease_out(T, A) ->
    back_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `back_ease_out/2` using encoded type metadata.
""").
-spec back_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
back_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `back_ease_out` to a raw scalar payload using a raw overshoot payload `O`.
""").
-spec back_ease_out(
    T :: glm:type(),
    A :: binary(),
    O :: binary()
) -> binary().
back_ease_out(T, A, O) ->
    back_ease_out_raw(?GLM_TYPE(T), A, O).

-doc("""
NIF entry point for `back_ease_out/3` using encoded type metadata.
""").
-spec back_ease_out_raw(
    T :: integer(),
    A :: binary(),
    O :: binary()
) -> binary().
back_ease_out_raw(_T, _A, _O) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `bounce_ease_in` to a raw scalar payload.
""").
-spec bounce_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
bounce_ease_in(T, A) ->
    bounce_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `bounce_ease_in/2` using encoded type metadata.
""").
-spec bounce_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
bounce_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `bounce_ease_in_out` to a raw scalar payload.
""").
-spec bounce_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
bounce_ease_in_out(T, A) ->
    bounce_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `bounce_ease_in_out/2` using encoded type metadata.
""").
-spec bounce_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
bounce_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `bounce_ease_out` to a raw scalar payload.
""").
-spec bounce_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
bounce_ease_out(T, A) ->
    bounce_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `bounce_ease_out/2` using encoded type metadata.
""").
-spec bounce_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
bounce_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `circular_ease_in` to a raw scalar payload.
""").
-spec circular_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
circular_ease_in(T, A) ->
    circular_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `circular_ease_in/2` using encoded type metadata.
""").
-spec circular_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
circular_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `circular_ease_in_out` to a raw scalar payload.
""").
-spec circular_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
circular_ease_in_out(T, A) ->
    circular_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `circular_ease_in_out/2` using encoded type metadata.
""").
-spec circular_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
circular_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `circular_ease_out` to a raw scalar payload.
""").
-spec circular_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
circular_ease_out(T, A) ->
    circular_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `circular_ease_out/2` using encoded type metadata.
""").
-spec circular_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
circular_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `cubic_ease_in` to a raw scalar payload.
""").
-spec cubic_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
cubic_ease_in(T, A) ->
    cubic_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `cubic_ease_in/2` using encoded type metadata.
""").
-spec cubic_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
cubic_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `cubic_ease_in_out` to a raw scalar payload.
""").
-spec cubic_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
cubic_ease_in_out(T, A) ->
    cubic_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `cubic_ease_in_out/2` using encoded type metadata.
""").
-spec cubic_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
cubic_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `cubic_ease_out` to a raw scalar payload.
""").
-spec cubic_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
cubic_ease_out(T, A) ->
    cubic_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `cubic_ease_out/2` using encoded type metadata.
""").
-spec cubic_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
cubic_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `elastic_ease_in` to a raw scalar payload.
""").
-spec elastic_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
elastic_ease_in(T, A) ->
    elastic_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `elastic_ease_in/2` using encoded type metadata.
""").
-spec elastic_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
elastic_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `elastic_ease_in_out` to a raw scalar payload.
""").
-spec elastic_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
elastic_ease_in_out(T, A) ->
    elastic_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `elastic_ease_in_out/2` using encoded type metadata.
""").
-spec elastic_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
elastic_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `elastic_ease_out` to a raw scalar payload.
""").
-spec elastic_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
elastic_ease_out(T, A) ->
    elastic_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `elastic_ease_out/2` using encoded type metadata.
""").
-spec elastic_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
elastic_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `exponential_ease_in` to a raw scalar payload.
""").
-spec exponential_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
exponential_ease_in(T, A) ->
    exponential_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `exponential_ease_in/2` using encoded type metadata.
""").
-spec exponential_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
exponential_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `exponential_ease_in_out` to a raw scalar payload.
""").
-spec exponential_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
exponential_ease_in_out(T, A) ->
    exponential_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `exponential_ease_in_out/2` using encoded type metadata.
""").
-spec exponential_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
exponential_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `exponential_ease_out` to a raw scalar payload.
""").
-spec exponential_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
exponential_ease_out(T, A) ->
    exponential_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `exponential_ease_out/2` using encoded type metadata.
""").
-spec exponential_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
exponential_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `linear_interpolation` to a raw scalar payload.
""").
-spec linear_interpolation(
    T :: glm:type(),
    A :: binary()
) -> binary().
linear_interpolation(T, A) ->
    linear_interpolation_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `linear_interpolation/2` using encoded type metadata.
""").
-spec linear_interpolation_raw(
    T :: integer(),
    A :: binary()
) -> binary().
linear_interpolation_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quadratic_ease_in` to a raw scalar payload.
""").
-spec quadratic_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
quadratic_ease_in(T, A) ->
    quadratic_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quadratic_ease_in/2` using encoded type metadata.
""").
-spec quadratic_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quadratic_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quadratic_ease_in_out` to a raw scalar payload.
""").
-spec quadratic_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
quadratic_ease_in_out(T, A) ->
    quadratic_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quadratic_ease_in_out/2` using encoded type metadata.
""").
-spec quadratic_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quadratic_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quadratic_ease_out` to a raw scalar payload.
""").
-spec quadratic_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
quadratic_ease_out(T, A) ->
    quadratic_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quadratic_ease_out/2` using encoded type metadata.
""").
-spec quadratic_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quadratic_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quartic_ease_in` to a raw scalar payload.
""").
-spec quartic_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
quartic_ease_in(T, A) ->
    quartic_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quartic_ease_in/2` using encoded type metadata.
""").
-spec quartic_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quartic_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quartic_ease_in_out` to a raw scalar payload.
""").
-spec quartic_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
quartic_ease_in_out(T, A) ->
    quartic_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quartic_ease_in_out/2` using encoded type metadata.
""").
-spec quartic_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quartic_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quartic_ease_out` to a raw scalar payload.
""").
-spec quartic_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
quartic_ease_out(T, A) ->
    quartic_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quartic_ease_out/2` using encoded type metadata.
""").
-spec quartic_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quartic_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quintic_ease_in` to a raw scalar payload.
""").
-spec quintic_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
quintic_ease_in(T, A) ->
    quintic_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quintic_ease_in/2` using encoded type metadata.
""").
-spec quintic_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quintic_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quintic_ease_in_out` to a raw scalar payload.
""").
-spec quintic_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
quintic_ease_in_out(T, A) ->
    quintic_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quintic_ease_in_out/2` using encoded type metadata.
""").
-spec quintic_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quintic_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `quintic_ease_out` to a raw scalar payload.
""").
-spec quintic_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
quintic_ease_out(T, A) ->
    quintic_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `quintic_ease_out/2` using encoded type metadata.
""").
-spec quintic_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
quintic_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `sine_ease_in` to a raw scalar payload.
""").
-spec sine_ease_in(
    T :: glm:type(),
    A :: binary()
) -> binary().
sine_ease_in(T, A) ->
    sine_ease_in_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `sine_ease_in/2` using encoded type metadata.
""").
-spec sine_ease_in_raw(
    T :: integer(),
    A :: binary()
) -> binary().
sine_ease_in_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `sine_ease_in_out` to a raw scalar payload.
""").
-spec sine_ease_in_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
sine_ease_in_out(T, A) ->
    sine_ease_in_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `sine_ease_in_out/2` using encoded type metadata.
""").
-spec sine_ease_in_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
sine_ease_in_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).

-doc("""
Applies `sine_ease_out` to a raw scalar payload.
""").
-spec sine_ease_out(
    T :: glm:type(),
    A :: binary()
) -> binary().
sine_ease_out(T, A) ->
    sine_ease_out_raw(?GLM_TYPE(T), A).

-doc("""
NIF entry point for `sine_ease_out/2` using encoded type metadata.
""").
-spec sine_ease_out_raw(
    T :: integer(),
    A :: binary()
) -> binary().
sine_ease_out_raw(_T, _A) ->
    erlang:nif_error(beam_glm_not_loaded).
