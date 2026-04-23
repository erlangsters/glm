%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm).
-moduledoc """
OpenGL Mathematics (GLM) for the BEAM.

It binds GLM for the Erlang and Elixir programming languages. Just like GLM
itself, it is not limited to OpenGL and can be used as a general-purpose math
library.

It is the entry point for the wrapped values used throughout the binding. GLM
operations work most naturally on compact native memory layouts, so the binding
carries scalars, vectors, matrices, and quaternions around as tagged Erlang
tuples with a binary payload. 

```erlang
{scalar, float, Bin} = glm:float(42.0).
```

The tags keep the BEAM-facing shape visible,
while the binary keeps the value in a form that the native layer can pass
around efficiently. You normally do not inspect that binary directly.

The usual workflow is:

- construct wrapped values with functions such as `double/1`, `vec3/4`,
  `mat4/1`, or `quat/5`
- pass those wrapped values to the focused `glm_*` modules
- read results back with accessors such as `double_value/1`, `vec3_values/1`,
  `mat4_element/3`, or `quat_values/1`

Typical first steps look like:

```erlang
Factor = glm:double(0.5).
0.5 = glm:double_value(Factor).

Position = glm:vec3(double, 1.0, 2.0, 3.0).
{1.0, 2.0, 3.0} = glm:vec3_values(Position).

UnitFactor = glm_common:clamp(Factor, glm:double(0.0), glm:double(1.0)).
0.5 = glm:double_value(UnitFactor).
```

These examples show the normal rhythm of the binding: build wrapped values in
`glm`, pass them to an operational module, and unwrap only when you need an
ordinary Erlang value.

The safe public surface is split into focused modules:

| Module | Focus |
| --- | --- |
| `glm_common` | Common scalar and vector helpers such as `clamp/3`, rounding, and bit reinterpretation helpers |
| `glm_angle` | Angle conversion, trigonometric, inverse-trigonometric, and hyperbolic functions |
| `glm_exponential` | Power, exponential, logarithmic, and root functions |
| `glm_vector` | Vector geometry such as `dot/2`, `cross/2`, `normalize/1`, and `reflect/2` |
| `glm_matrix` | Matrix operations such as `transpose/1`, `determinant/1`, and `inverse/1` |
| `glm_transform` | Projection, view, and matrix transform helpers |
| `glm_quat` | Quaternion construction, conversion, interpolation, and rotation helpers |
| `glm_integer` | Integer bit operations, integer predicates, and extended arithmetic helpers |
| `glm_relational` | Component-wise comparison and bool-vector reduction helpers |
| `glm_packing` | Packing and unpacking helpers for compact scalar representations |
| `glm_easing` | Easing functions |

These modules are the recommended way to use the binding. They preserve the
wrapped shapes, check that arguments match the documented types and sizes, and
perform extra validation when a public contract requires it.

The repository also provides `glm_raw`, which sits beneath this safe surface as
the thin binary-oriented layer used by the wrapper modules. It can bypass some
of the checks done by the safe API and may be useful for specialized
performance-sensitive code, but most callers should avoid it. Start with
`glm` plus the focused `glm_*` modules, and only reach for `glm_raw` when you
fully understand the trade-off and are prepared to manage the unchecked layer
yourself.
""".

-compile({nowarn_redefined_builtin_type, [bool/0, float/0]}).

-export_type([
    type/0
]).
-export_type([
    bool/0,
    int8/0,
    int16/0,
    int32/0,
    int64/0,
    uint8/0,
    uint16/0,
    uint32/0,
    uint64/0,
    float/0,
    double/0
]).
-export_type([
    scalar/1
]).
-export_type([
    length/0,
    vec/2, vec/1,
    vec2/1, vec3/1, vec4/1
]).
-export_type([
    columns/0,
    rows/0,
    mat/3, mat/2, mat/1,
    mat2/1, mat3/1, mat4/1,
    mat2x2/1, mat2x3/1, mat2x4/1,
    mat3x2/1, mat3x3/1, mat3x4/1,
    mat4x2/1, mat4x3/1, mat4x4/1
]).
-export_type([
    quat/1
]).

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
    scalar/1, scalar/2,
    scalar_value/1
]).
-export([
    vec2/1, vec2/2, vec2/3,
    vec2_x/1, vec2_set_x/2,
    vec2_y/1, vec2_set_y/2,
    vec2_values/1
]).
-export([
    vec3/1, vec3/2, vec3/4,
    vec3_x/1, vec3_set_x/2,
    vec3_y/1, vec3_set_y/2,
    vec3_z/1, vec3_set_z/2,
    vec3_values/1
]).
-export([
    vec4/1, vec4/2, vec4/5,
    vec4_x/1, vec4_set_x/2,
    vec4_y/1, vec4_set_y/2,
    vec4_z/1, vec4_set_z/2,
    vec4_w/1, vec4_set_w/2,
    vec4_values/1
]).
-export([
    mat2/1, mat2/2, mat2/5,
    mat2_element/2, mat2_element/3,
    mat2_set_element/3, mat2_set_element/4,
    mat2_values/1
]).
-export([
    mat3/1, mat3/2, mat3/10,
    mat3_element/2, mat3_element/3,
    mat3_set_element/3, mat3_set_element/4,
    mat3_values/1
]).
-export([
    mat4/1, mat4/2, mat4/17,
    mat4_element/2, mat4_element/3,
    mat4_set_element/3, mat4_set_element/4,
    mat4_values/1
]).
-export([
    mat2x3/1, mat2x3/2, mat2x3/7,
    mat2x3_element/2, mat2x3_element/3,
    mat2x3_set_element/3, mat2x3_set_element/4,
    mat2x3_values/1
]).
-export([
    mat2x4/1, mat2x4/2, mat2x4/9,
    mat2x4_element/2, mat2x4_element/3,
    mat2x4_set_element/3, mat2x4_set_element/4,
    mat2x4_values/1
]).
-export([
    mat3x2/1, mat3x2/2, mat3x2/7,
    mat3x2_element/2, mat3x2_element/3,
    mat3x2_set_element/3, mat3x2_set_element/4,
    mat3x2_values/1
]).
-export([
    mat3x4/1, mat3x4/2, mat3x4/13,
    mat3x4_element/2, mat3x4_element/3,
    mat3x4_set_element/3, mat3x4_set_element/4,
    mat3x4_values/1
]).
-export([
    mat4x2/1, mat4x2/2, mat4x2/9,
    mat4x2_element/2, mat4x2_element/3,
    mat4x2_set_element/3, mat4x2_set_element/4,
    mat4x2_values/1
]).
-export([
    mat4x3/1, mat4x3/2, mat4x3/13,
    mat4x3_element/2, mat4x3_element/3,
    mat4x3_set_element/3, mat4x3_set_element/4,
    mat4x3_values/1
]).
-export([
    quat/1, quat/5,
    quat_w/1, quat_set_w/2,
    quat_x/1, quat_set_x/2,
    quat_y/1, quat_set_y/2,
    quat_z/1, quat_set_z/2,
    quat_values/1
]).

-include("glm.hrl").

-define(EXCEPTION_IF_INVALID_VALUE(V, T),
    begin
        case T of
            float when is_float(V) -> ok;
            double when is_float(V) -> ok;
            {int, 8} when is_integer(V), V >= ?INT8_MIN, V =< ?INT8_MAX -> ok;
            {int, 16} when is_integer(V), V >= ?INT16_MIN, V =< ?INT16_MAX -> ok;
            {int, 32} when is_integer(V), V >= ?INT32_MIN, V =< ?INT32_MAX -> ok;
            {int, 64} when is_integer(V), V >= ?INT64_MIN, V =< ?INT64_MAX -> ok;
            {uint, 8} when is_integer(V), V >= ?UINT8_MIN, V =< ?UINT8_MAX -> ok;
            {uint, 16} when is_integer(V), V >= ?UINT16_MIN, V =< ?UINT16_MAX -> ok;
            {uint, 32} when is_integer(V), V >= ?UINT32_MIN, V =< ?UINT32_MAX -> ok;
            {uint, 64} when is_integer(V), V >= ?UINT64_MIN, V =< ?UINT64_MAX -> ok;
            bool when is_boolean(V) -> ok;
            _ ->
                case T of
                    float -> erlang:error({invalid_value, {expected_float, V}});
                    double -> erlang:error({invalid_value, {expected_double, V}});
                    {int, 8} -> erlang:error({invalid_value, {expected_int8, V}});
                    {int, 16} -> erlang:error({invalid_value, {expected_int16, V}});
                    {int, 32} -> erlang:error({invalid_value, {expected_int32, V}});
                    {int, 64} -> erlang:error({invalid_value, {expected_int64, V}});
                    {uint, 8} -> erlang:error({invalid_value, {expected_uint8, V}});
                    {uint, 16} -> erlang:error({invalid_value, {expected_uint16, V}});
                    {uint, 32} -> erlang:error({invalid_value, {expected_uint32, V}});
                    {uint, 64} -> erlang:error({invalid_value, {expected_uint64, V}});
                    bool -> erlang:error({invalid_value, {expected_boolean, V}})
                end
        end
    end
).

-doc("""
Supported scalar element type tags.
""").
-type type() ::
    bool |
    {int, 8 | 16 | 32 | 64} |
    {uint, 8 | 16 | 32 | 64} |
    float |
    double
.

-doc("""
Wrapped boolean scalar.

```erlang
{scalar, bool, Bin} = glm:bool(true).
```
""").
-type bool() :: scalar(bool).

-doc("""
Wrapped signed 8-bit scalar.

```erlang
{scalar, {int, 8}, Bin} = glm:int8(-1).
```
""").
-type int8() :: scalar({int, 8}).

-doc("""
Wrapped signed 16-bit scalar.

```erlang
{scalar, {int, 16}, Bin} = glm:int16(-1).
```
""").
-type int16() :: scalar({int, 16}).

-doc("""
Wrapped signed 32-bit scalar.

```erlang
{scalar, {int, 32}, Bin} = glm:int32(-1).
```
""").
-type int32() :: scalar({int, 32}).

-doc("""
Wrapped signed 64-bit scalar.

```erlang
{scalar, {int, 64}, Bin} = glm:int64(-1).
```
""").
-type int64() :: scalar({int, 64}).

-doc("""
Wrapped unsigned 8-bit scalar.

```erlang
{scalar, {uint, 8}, Bin} = glm:uint8(1).
```
""").
-type uint8() :: scalar({uint, 8}).

-doc("""
Wrapped unsigned 16-bit scalar.

```erlang
{scalar, {uint, 16}, Bin} = glm:uint16(1).
```
""").
-type uint16() :: scalar({uint, 16}).

-doc("""
Wrapped unsigned 32-bit scalar.

```erlang
{scalar, {uint, 32}, Bin} = glm:uint32(1).
```
""").
-type uint32() :: scalar({uint, 32}).

-doc("""
Wrapped unsigned 64-bit scalar.

```erlang
{scalar, {uint, 64}, Bin} = glm:uint64(1).
```
""").
-type uint64() :: scalar({uint, 64}).

-doc("""
Wrapped single-precision floating-point scalar.

```erlang
{scalar, float, Bin} = glm:float(42.0).
```
""").
-type float() :: scalar(float).

-doc("""
Wrapped double-precision floating-point scalar.

```erlang
{scalar, double, Bin} = glm:double(42.0).
```
""").
-type double() :: scalar(double).

-doc("""
Wrapped scalar value tagged with its GLM element type.

```erlang
{scalar, float, Bin} = glm:scalar(float, 42.0).
```
""").
-type scalar(T) :: {scalar, T, binary()}.

-doc("""
Supported vector lengths.
""").
-type length() :: 2 | 3 | 4.

-doc("""
Wrapped vector with an explicit length and element type.

```erlang
{vec, 3, float, Bin} = glm:vec3(float, 1.0, 2.0, 3.0).
```
""").
-type vec(Length, Type) :: {vec, Length, Type, binary()}.

-doc("""
Wrapped vector with any supported length.

```erlang
{vec, 4, float, Bin} = glm:vec4(float, 1.0).
```
""").
-type vec(Type) :: vec(length(), Type).

-doc("""
Wrapped 2-component vector.

```erlang
{vec, 2, float, Bin} = glm:vec2(float, 1.0, 2.0).
```
""").
-type vec2(Type) :: vec(2, Type).

-doc("""
Wrapped 3-component vector.

```erlang
{vec, 3, float, Bin} = glm:vec3(float, 1.0, 2.0, 3.0).
```
""").
-type vec3(Type) :: vec(3, Type).

-doc("""
Wrapped 4-component vector.

```erlang
{vec, 4, float, Bin} = glm:vec4(float, 1.0, 2.0, 3.0, 4.0).
```
""").
-type vec4(Type) :: vec(4, Type).

-doc("""
Supported matrix column counts.
""").
-type columns() :: 2 | 3 | 4.

-doc("""
Supported matrix row counts.
""").
-type rows() :: 2 | 3 | 4.

-doc("""
Wrapped column-major matrix with an explicit shape and element type.

```erlang
{mat, 4, 4, double, Bin} = glm:mat4(double).
```
""").
-type mat(Columns, Rows, Type) :: {mat, Columns, Rows, Type, binary()}.

-doc("""
Wrapped square matrix with the given dimension and element type.
""").
-type mat(Length, Type) :: mat(Length, Length, Type).

-doc("""
Wrapped matrix with any supported shape.
""").
-type mat(Type) :: mat(columns(), rows(), Type).

-doc("""
Wrapped 2x2 matrix.

```erlang
{mat, 2, 2, double, Bin} = glm:mat2(double, 1.0).
```
""").
-type mat2(Type) :: mat(2, 2, Type).

-doc("""
Wrapped 3x3 matrix.

```erlang
{mat, 3, 3, double, Bin} = glm:mat3(double, 1.0).
```
""").
-type mat3(Type) :: mat(3, 3, Type).

-doc("""
Wrapped 4x4 matrix.

```erlang
{mat, 4, 4, double, Bin} = glm:mat4(double).
```
""").
-type mat4(Type) :: mat(4, 4, Type).

-doc("""
Wrapped 2x2 matrix.
""").
-type mat2x2(Type) :: mat2(Type).

-doc("""
Wrapped 2x3 matrix.
""").
-type mat2x3(Type) :: mat(2, 3, Type).

-doc("""
Wrapped 2x4 matrix.
""").
-type mat2x4(Type) :: mat(2, 4, Type).

-doc("""
Wrapped 3x2 matrix.
""").
-type mat3x2(Type) :: mat(3, 2, Type).

-doc("""
Wrapped 3x3 matrix.
""").
-type mat3x3(Type) :: mat3(Type).

-doc("""
Wrapped 3x4 matrix.
""").
-type mat3x4(Type) :: mat(3, 4, Type).

-doc("""
Wrapped 4x2 matrix.
""").
-type mat4x2(Type) :: mat(4, 2, Type).

-doc("""
Wrapped 4x3 matrix.
""").
-type mat4x3(Type) :: mat(4, 3, Type).

-doc("""
Wrapped 4x4 matrix.
""").
-type mat4x4(Type) :: mat4(Type).

-doc("""
Wrapped quaternion with a floating-point element type.

```erlang
{quat, double, Bin} = glm:quat(double).
```
""").
-type quat(Type) :: {quat, Type, binary()}.

-doc("""
Returns the wrapped boolean scalar initialized to `false`.
""").
-spec bool() -> scalar(bool).
bool() ->
    {scalar, bool, glm_raw:bool()}.

-doc("""
Wraps a BEAM boolean as a GLM boolean scalar.

```erlang
True = glm:bool(true).
False = glm:bool(false).
```

Raises `{invalid_value, {expected_boolean, V}}` when `V` is not a boolean.
""").
-spec bool(boolean()) -> scalar(bool).
bool(V) ->
    case V of
        _ when erlang:is_boolean(V) ->
            {scalar, bool, glm_raw:bool(V)};
        _ ->
            erlang:error({invalid_value, {expected_boolean, V}})
    end.

-doc("""
Extracts the BEAM boolean value from a wrapped GLM boolean scalar.

```erlang
true = glm:bool_value(glm:bool(true)).
```
""").
-spec bool_value(scalar(bool)) -> boolean().
bool_value({scalar, bool, D}) when byte_size(D) =:= ?BOOL_BYTE_SIZE ->
    glm_raw:bool_value(D).

-doc("""
Returns the wrapped signed 8-bit scalar initialized to `0`.
""").
-spec int8() -> scalar({int, 8}).
int8() ->
    {scalar, {int, 8}, glm_raw:int8()}.

-doc("""
Wraps a BEAM integer as a signed 8-bit GLM scalar.

```erlang
Int8 = glm:int8(-42).
```

Raises `{invalid_value, {expected_int8, V}}` when `V` falls outside the valid
8-bit signed range.
""").
-spec int8(integer()) -> scalar({int, 8}).
int8(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT8_MIN, V =< ?INT8_MAX ->
            {scalar, {int, 8}, glm_raw:int8(V)};
        _ ->
            erlang:error({invalid_value, {expected_int8, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped signed 8-bit scalar.

```erlang
-42 = glm:int8_value(glm:int8(-42)).
```
""").
-spec int8_value(scalar({int, 8})) -> integer().
int8_value({scalar, {int, 8}, D}) when byte_size(D) =:= ?INT8_BYTE_SIZE ->
    glm_raw:int8_value(D).

-doc("""
Returns the wrapped signed 16-bit scalar initialized to `0`.

```erlang
Zero = glm:int16().
```
""").
-spec int16() -> scalar({int, 16}).
int16() ->
    {scalar, {int, 16}, glm_raw:int16()}.

-doc("""
Wraps a BEAM integer as a signed 16-bit GLM scalar.

```erlang
Int16 = glm:int16(-1024).
```

Raises `{invalid_value, {expected_int16, V}}` when `V` falls outside the valid
16-bit signed range.
""").
-spec int16(integer()) -> scalar({int, 16}).
int16(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT16_MIN, V =< ?INT16_MAX ->
            {scalar, {int, 16}, glm_raw:int16(V)};
        _ ->
            erlang:error({invalid_value, {expected_int16, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped signed 16-bit scalar.

```erlang
-1024 = glm:int16_value(glm:int16(-1024)).
```
""").
-spec int16_value(scalar({int, 16})) -> integer().
int16_value({scalar, {int, 16}, D}) when byte_size(D) =:= ?INT16_BYTE_SIZE ->
    glm_raw:int16_value(D).

-doc("""
Returns the wrapped signed 32-bit scalar initialized to `0`.

```erlang
Zero = glm:int32().
```
""").
-spec int32() -> scalar({int, 32}).
int32() ->
    {scalar, {int, 32}, glm_raw:int32()}.

-doc("""
Wraps a BEAM integer as a signed 32-bit GLM scalar.

```erlang
Int32 = glm:int32(1 bsl 20).
```

Raises `{invalid_value, {expected_int32, V}}` when `V` falls outside the valid
32-bit signed range.
""").
-spec int32(integer()) -> scalar({int, 32}).
int32(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT32_MIN, V =< ?INT32_MAX ->
            {scalar, {int, 32}, glm_raw:int32(V)};
        _ ->
            erlang:error({invalid_value, {expected_int32, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped signed 32-bit scalar.

```erlang
1048576 = glm:int32_value(glm:int32(1 bsl 20)).
```
""").
-spec int32_value(scalar({int, 32})) -> integer().
int32_value({scalar, {int, 32}, D}) when byte_size(D) =:= ?INT32_BYTE_SIZE ->
    glm_raw:int32_value(D).

-doc("""
Returns the wrapped signed 64-bit scalar initialized to `0`.

```erlang
Zero = glm:int64().
```
""").
-spec int64() -> scalar({int, 64}).
int64() ->
    {scalar, {int, 64}, glm_raw:int64()}.

-doc("""
Wraps a BEAM integer as a signed 64-bit GLM scalar.

```erlang
Int64 = glm:int64(1 bsl 40).
```

Raises `{invalid_value, {expected_int64, V}}` when `V` falls outside the valid
64-bit signed range.
""").
-spec int64(integer()) -> scalar({int, 64}).
int64(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT64_MIN, V =< ?INT64_MAX ->
            {scalar, {int, 64}, glm_raw:int64(V)};
        _ ->
            erlang:error({invalid_value, {expected_int64, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped signed 64-bit scalar.

```erlang
1099511627776 = glm:int64_value(glm:int64(1 bsl 40)).
```
""").
-spec int64_value(scalar({int, 64})) -> integer().
int64_value({scalar, {int, 64}, D}) when byte_size(D) =:= ?INT64_BYTE_SIZE ->
    glm_raw:int64_value(D).

-doc("""
Returns the wrapped unsigned 8-bit scalar initialized to `0`.

```erlang
Zero = glm:uint8().
```
""").
-spec uint8() -> scalar({uint, 8}).
uint8() ->
    {scalar, {uint, 8}, glm_raw:uint8()}.

-doc("""
Wraps a non-negative BEAM integer as an unsigned 8-bit GLM scalar.

```erlang
Uint8 = glm:uint8(255).
```

Raises `{invalid_value, {expected_uint8, V}}` when `V` falls outside the valid
8-bit unsigned range.
""").
-spec uint8(non_neg_integer()) -> scalar({uint, 8}).
uint8(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT8_MAX ->
            {scalar, {uint, 8}, glm_raw:uint8(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint8, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped unsigned 8-bit scalar.

```erlang
255 = glm:uint8_value(glm:uint8(255)).
```
""").
-spec uint8_value(scalar({uint, 8})) -> non_neg_integer().
uint8_value({scalar, {uint, 8}, D}) when byte_size(D) =:= ?UINT8_BYTE_SIZE ->
    glm_raw:uint8_value(D).

-doc("""
Returns the wrapped unsigned 16-bit scalar initialized to `0`.

```erlang
Zero = glm:uint16().
```
""").
-spec uint16() -> scalar({uint, 16}).
uint16() ->
    {scalar, {uint, 16}, glm_raw:uint16()}.

-doc("""
Wraps a non-negative BEAM integer as an unsigned 16-bit GLM scalar.

```erlang
Uint16 = glm:uint16(4096).
```

Raises `{invalid_value, {expected_uint16, V}}` when `V` falls outside the
valid 16-bit unsigned range.
""").
-spec uint16(non_neg_integer()) -> scalar({uint, 16}).
uint16(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT16_MAX ->
            {scalar, {uint, 16}, glm_raw:uint16(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint16, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped unsigned 16-bit scalar.

```erlang
4096 = glm:uint16_value(glm:uint16(4096)).
```
""").
-spec uint16_value(scalar({uint, 16})) -> non_neg_integer().
uint16_value({scalar, {uint, 16}, D}) when byte_size(D) =:= ?UINT16_BYTE_SIZE ->
    glm_raw:uint16_value(D).

-doc("""
Returns the wrapped unsigned 32-bit scalar initialized to `0`.

```erlang
Zero = glm:uint32().
```
""").
-spec uint32() -> scalar({uint, 32}).
uint32() ->
    {scalar, {uint, 32}, glm_raw:uint32()}.

-doc("""
Wraps a non-negative BEAM integer as an unsigned 32-bit GLM scalar.

```erlang
Uint32 = glm:uint32(1 bsl 24).
```

Raises `{invalid_value, {expected_uint32, V}}` when `V` falls outside the
valid 32-bit unsigned range.
""").
-spec uint32(non_neg_integer()) -> scalar({uint, 32}).
uint32(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT32_MAX ->
            {scalar, {uint, 32}, glm_raw:uint32(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint32, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped unsigned 32-bit scalar.

```erlang
16777216 = glm:uint32_value(glm:uint32(1 bsl 24)).
```
""").
-spec uint32_value(scalar({uint, 32})) -> non_neg_integer().
uint32_value({scalar, {uint, 32}, D}) when byte_size(D) =:= ?UINT32_BYTE_SIZE ->
    glm_raw:uint32_value(D).

-doc("""
Returns the wrapped unsigned 64-bit scalar initialized to `0`.

```erlang
Zero = glm:uint64().
```
""").
-spec uint64() -> scalar({uint, 64}).
uint64() ->
    {scalar, {uint, 64}, glm_raw:uint64()}.

-doc("""
Wraps a non-negative BEAM integer as an unsigned 64-bit GLM scalar.

```erlang
Uint64 = glm:uint64(1 bsl 48).
```

Raises `{invalid_value, {expected_uint64, V}}` when `V` falls outside the
valid 64-bit unsigned range.
""").
-spec uint64(non_neg_integer()) -> scalar({uint, 64}).
uint64(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT64_MAX ->
            {scalar, {uint, 64}, glm_raw:uint64(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint64, V}})
    end.

-doc("""
Extracts the BEAM integer value from a wrapped unsigned 64-bit scalar.

```erlang
281474976710656 = glm:uint64_value(glm:uint64(1 bsl 48)).
```
""").
-spec uint64_value(scalar({uint, 64})) -> non_neg_integer().
uint64_value({scalar, {uint, 64}, D}) when byte_size(D) =:= ?UINT64_BYTE_SIZE ->
    glm_raw:uint64_value(D).

-doc("""
Returns the wrapped single-precision scalar initialized to `0.0`.

```erlang
Zero = glm:float().
```
""").
-spec float() -> scalar(float).
float() ->
    {scalar, float, glm_raw:float()}.

-doc("""
Wraps a BEAM float as a single-precision GLM scalar.

```erlang
Scalar = glm:float(3.5).
```

Raises `{invalid_value, {expected_float, V}}` when `V` is not a float.
""").
-spec float(float()) -> scalar(float).
float(V) ->
    case V of
        _ when erlang:is_float(V) ->
            {scalar, float, glm_raw:float(V)};
        _ ->
            erlang:error({invalid_value, {expected_float, V}})
    end.

-doc("""
Extracts the BEAM float value from a wrapped single-precision scalar.

```erlang
3.5 = glm:float_value(glm:float(3.5)).
```
""").
-spec float_value(scalar(float)) -> float().
float_value({scalar, float, Data}) when byte_size(Data) =:= ?FLOAT_BYTE_SIZE ->
    glm_raw:float_value(Data).

-doc("""
Returns the wrapped double-precision scalar initialized to `0.0`.

```erlang
Zero = glm:double().
```
""").
-spec double() -> scalar(double).
double() ->
    {scalar, double, glm_raw:double()}.

-doc("""
Wraps a BEAM float as a double-precision GLM scalar.

```erlang
Scalar = glm:double(3.5).
```

Raises `{invalid_value, {expected_double, V}}` when `V` is not a float.
""").
-spec double(float()) -> scalar(double).
double(V) ->
    case V of
        _ when erlang:is_float(V) ->
            {scalar, double, glm_raw:double(V)};
        _ ->
            erlang:error({invalid_value, {expected_double, V}})
    end.

-doc("""
Extracts the BEAM float value from a wrapped double-precision scalar.

```erlang
3.5 = glm:double_value(glm:double(3.5)).
```
""").
-spec double_value(scalar(double)) -> float().
double_value({scalar, double, Data}) when byte_size(Data) =:= ?DOUBLE_BYTE_SIZE ->
    glm_raw:double_value(Data).

-doc("""
Returns the zero-initialized wrapped scalar for the given GLM type tag.

```erlang
{scalar, float, _} = glm:scalar(float).
{scalar, {int, 32}, _} = glm:scalar({int, 32}).
```
""").
-spec scalar(T) -> scalar(T) when T :: type().
scalar(bool) ->
    {scalar, bool, glm_raw:bool()};
scalar({int, 8}) ->
    {scalar, {int, 8}, glm_raw:int8()};
scalar({int, 16}) ->
    {scalar, {int, 16}, glm_raw:int16()};
scalar({int, 32}) ->
    {scalar, {int, 32}, glm_raw:int32()};
scalar({int, 64}) ->
    {scalar, {int, 64}, glm_raw:int64()};
scalar({uint, 8}) ->
    {scalar, {uint, 8}, glm_raw:uint8()};
scalar({uint, 16}) ->
    {scalar, {uint, 16}, glm_raw:uint16()};
scalar({uint, 32}) ->
    {scalar, {uint, 32}, glm_raw:uint32()};
scalar({uint, 64}) ->
    {scalar, {uint, 64}, glm_raw:uint64()};
scalar(float) ->
    {scalar, float, glm_raw:float()};
scalar(double) ->
    {scalar, double, glm_raw:double()}.

-doc("""
Wraps a BEAM value using an explicit GLM type tag.

```erlang
{scalar, {uint, 16}, _} = glm:scalar({uint, 16}, 7).
{scalar, float, _} = glm:scalar(float, 1.5).
```

Raises the same `{invalid_value, ...}` exceptions as the specialized scalar
constructors when `V` does not match `T`.
""").
-spec scalar(T, term()) -> scalar(T) when T :: type().
scalar(T, V) ->
    % XXX: Rework impl.
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    case T of
        bool -> {scalar, bool, glm_raw:bool(V)};
        {int, 8} -> {scalar, {int, 8}, glm_raw:int8(V)};
        {int, 16} -> {scalar, {int, 16}, glm_raw:int16(V)};
        {int, 32} -> {scalar, {int, 32}, glm_raw:int32(V)};
        {int, 64} -> {scalar, {int, 64}, glm_raw:int64(V)};
        {uint, 8} -> {scalar, {uint, 8}, glm_raw:uint8(V)};
        {uint, 16} -> {scalar, {uint, 16}, glm_raw:uint16(V)};
        {uint, 32} -> {scalar, {uint, 32}, glm_raw:uint32(V)};
        {uint, 64} -> {scalar, {uint, 64}, glm_raw:uint64(V)};
        float -> {scalar, float, glm_raw:float(V)};
        double -> {scalar, double, glm_raw:double(V)}
    end.

-doc("""
Extracts the BEAM value stored in any wrapped scalar.

```erlang
7 = glm:scalar_value(glm:scalar({uint, 16}, 7)).
1.5 = glm:scalar_value(glm:scalar(float, 1.5)).
```
""").
-spec scalar_value(scalar(T)) -> term() when T :: type().
scalar_value({scalar, T, D}) ->
    case T of
        bool when byte_size(D) =:= ?BOOL_BYTE_SIZE -> glm_raw:bool_value(D);
        {int, 8} when byte_size(D) =:= ?INT8_BYTE_SIZE -> glm_raw:int8_value(D);
        {int, 16} when byte_size(D) =:= ?INT16_BYTE_SIZE -> glm_raw:int16_value(D);
        {int, 32} when byte_size(D) =:= ?INT32_BYTE_SIZE -> glm_raw:int32_value(D);
        {int, 64} when byte_size(D) =:= ?INT64_BYTE_SIZE -> glm_raw:int64_value(D);
        {uint, 8} when byte_size(D) =:= ?UINT8_BYTE_SIZE -> glm_raw:uint8_value(D);
        {uint, 16} when byte_size(D) =:= ?UINT16_BYTE_SIZE -> glm_raw:uint16_value(D);
        {uint, 32} when byte_size(D) =:= ?UINT32_BYTE_SIZE -> glm_raw:uint32_value(D);
        {uint, 64} when byte_size(D) =:= ?UINT64_BYTE_SIZE -> glm_raw:uint64_value(D);
        float when byte_size(D) =:= ?FLOAT_BYTE_SIZE -> glm_raw:float_value(D);
        double when byte_size(D) =:= ?DOUBLE_BYTE_SIZE -> glm_raw:double_value(D)
    end.

-doc("""
Returns the wrapped 2-component vector initialized to zeroes.

```erlang
Zero = glm:vec2(float).
```
""").
-spec vec2(T) -> vec2(T) when T :: type().
vec2(T) ->
    {vec, 2, T, glm_raw:vec2(T)}.

-doc("""
Returns a wrapped 2-component vector with both components initialized to `V`.

```erlang
Unit = glm:vec2(float, 1.0).
```
""").
-spec vec2(T, term()) -> vec2(T) when T :: type().
vec2(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {vec, 2, T, glm_raw:vec2(T, V)}.

-doc("""
Builds a wrapped 2-component vector from explicit component values.

```erlang
Position = glm:vec2(float, 10.0, 20.0).
```
""").
-spec vec2(T, term(), term()) -> vec2(T) when T :: type().
vec2(T, X, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 2, T, glm_raw:vec2(T, X, Y)}.

-doc("""
Returns the `x` component of a wrapped 2-component vector.

```erlang
10.0 = glm:vec2_x(glm:vec2(float, 10.0, 20.0)).
```
""").
-spec vec2_x(vec2(T)) -> term() when T :: type().
vec2_x({vec, 2, T, D}) ->
    glm_raw:vec2_x(T, D).

-doc("""
Returns a copy of a wrapped 2-component vector with its `x` component replaced.

```erlang
Moved = glm:vec2_set_x(glm:vec2(float, 10.0, 20.0), 15.0).
```
""").
-spec vec2_set_x(vec2(T), term()) -> vec2(T) when T :: type().
vec2_set_x({vec, 2, T, D}, X) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    {vec, 2, T, glm_raw:vec2_set_x(T, D, X)}.

-doc("""
Returns the `y` component of a wrapped 2-component vector.

```erlang
20.0 = glm:vec2_y(glm:vec2(float, 10.0, 20.0)).
```
""").
-spec vec2_y(vec2(T)) -> term() when T :: type().
vec2_y({vec, 2, T, D}) ->
    glm_raw:vec2_y(T, D).

-doc("""
Returns a copy of a wrapped 2-component vector with its `y` component replaced.

```erlang
Moved = glm:vec2_set_y(glm:vec2(float, 10.0, 20.0), 25.0).
```
""").
-spec vec2_set_y(vec2(T), term()) -> vec2(T) when T :: type().
vec2_set_y({vec, 2, T, D}, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 2, T, glm_raw:vec2_set_y(T, D, Y)}.

-doc("""
Returns the components of a wrapped 2-component vector as a BEAM tuple.

```erlang
{10.0, 20.0} = glm:vec2_values(glm:vec2(float, 10.0, 20.0)).
```
""").
-spec vec2_values(vec2(T)) -> {term(), term()} when T :: type().
vec2_values({vec, 2, T, D}) ->
    glm_raw:vec2_values(T, D).

-doc("""
Returns the wrapped 3-component vector initialized to zeroes.

```erlang
Zero = glm:vec3(double).
```
""").
-spec vec3(T) -> vec3(T) when T :: type().
vec3(T) ->
    {vec, 3, T, glm_raw:vec3(T)}.

-doc("""
Returns a wrapped 3-component vector with every component initialized to `V`.

```erlang
Ones = glm:vec3(double, 1.0).
```
""").
-spec vec3(T, term()) -> vec3(T) when T :: type().
vec3(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {vec, 3, T, glm_raw:vec3(T, V)}.

-doc("""
Builds a wrapped 3-component vector from explicit component values.

```erlang
Axis = glm:vec3(double, 0.0, 1.0, 0.0).
```
""").
-spec vec3(T, term(), term(), term()) -> vec3(T) when T :: type().
vec3(T, X, Y, Z) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {vec, 3, T, glm_raw:vec3(T, X, Y, Z)}.

-doc("""
Returns the `x` component of a wrapped 3-component vector.

```erlang
1.0 = glm:vec3_x(glm:vec3(double, 1.0, 2.0, 3.0)).
```
""").
-spec vec3_x(vec3(T)) -> term() when T :: type().
vec3_x({vec, 3, T, D}) ->
    glm_raw:vec3_x(T, D).

-doc("""
Returns a copy of a wrapped 3-component vector with its `x` component replaced.

```erlang
Updated = glm:vec3_set_x(glm:vec3(double, 1.0, 2.0, 3.0), 4.0).
```
""").
-spec vec3_set_x(vec3(T), term()) -> vec3(T) when T :: type().
vec3_set_x({vec, 3, T, D}, X) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    {vec, 3, T, glm_raw:vec3_set_x(T, D, X)}.

-doc("""
Returns the `y` component of a wrapped 3-component vector.

```erlang
2.0 = glm:vec3_y(glm:vec3(double, 1.0, 2.0, 3.0)).
```
""").
-spec vec3_y(vec3(T)) -> term() when T :: type().
vec3_y({vec, 3, T, D}) ->
    glm_raw:vec3_y(T, D).

-doc("""
Returns a copy of a wrapped 3-component vector with its `y` component replaced.

```erlang
Updated = glm:vec3_set_y(glm:vec3(double, 1.0, 2.0, 3.0), 5.0).
```
""").
-spec vec3_set_y(vec3(T), term()) -> vec3(T) when T :: type().
vec3_set_y({vec, 3, T, D}, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 3, T, glm_raw:vec3_set_y(T, D, Y)}.

-doc("""
Returns the `z` component of a wrapped 3-component vector.

```erlang
3.0 = glm:vec3_z(glm:vec3(double, 1.0, 2.0, 3.0)).
```
""").
-spec vec3_z(vec3(T)) -> term() when T :: type().
vec3_z({vec, 3, T, D}) ->
    glm_raw:vec3_z(T, D).

-doc("""
Returns a copy of a wrapped 3-component vector with its `z` component replaced.

```erlang
Updated = glm:vec3_set_z(glm:vec3(double, 1.0, 2.0, 3.0), 6.0).
```
""").
-spec vec3_set_z(vec3(T), term()) -> vec3(T) when T :: type().
vec3_set_z({vec, 3, T, D}, Z) ->
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {vec, 3, T, glm_raw:vec3_set_z(T, D, Z)}.

-doc("""
Returns the components of a wrapped 3-component vector as a BEAM tuple.

```erlang
{1.0, 2.0, 3.0} = glm:vec3_values(glm:vec3(double, 1.0, 2.0, 3.0)).
```
""").
-spec vec3_values(vec3(T)) -> {term(), term(), term()} when T :: type().
vec3_values({vec, 3, T, D}) ->
    glm_raw:vec3_values(T, D).

-doc("""
Returns the wrapped 4-component vector initialized to zeroes.

```erlang
Zero = glm:vec4(float).
```
""").
-spec vec4(T) -> vec4(T) when T :: type().
vec4(T) ->
    {vec, 4, T, glm_raw:vec4(T)}.

-doc("""
Returns a wrapped 4-component vector with every component initialized to `V`.

```erlang
Color = glm:vec4(float, 1.0).
```
""").
-spec vec4(T, term()) -> vec4(T) when T :: type().
vec4(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {vec, 4, T, glm_raw:vec4(T, V)}.

-doc("""
Builds a wrapped 4-component vector from explicit component values.

```erlang
Color = glm:vec4(float, 1.0, 0.5, 0.0, 1.0).
```
""").
-spec vec4(T, term(), term(), term(), term()) -> vec4(T) when T :: type().
vec4(T, X, Y, Z, W) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    ?EXCEPTION_IF_INVALID_VALUE(W, T),
    {vec, 4, T, glm_raw:vec4(T, X, Y, Z, W)}.

-doc("""
Returns the `x` component of a wrapped 4-component vector.

```erlang
1.0 = glm:vec4_x(glm:vec4(float, 1.0, 2.0, 3.0, 4.0)).
```
""").
-spec vec4_x(vec4(T)) -> term() when T :: type().
vec4_x({vec, 4, T, D}) ->
    glm_raw:vec4_x(T, D).

-doc("""
Returns a copy of a wrapped 4-component vector with its `x` component replaced.

```erlang
Updated = glm:vec4_set_x(glm:vec4(float, 1.0, 2.0, 3.0, 4.0), 5.0).
```
""").
-spec vec4_set_x(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_x({vec, 4, T, D}, X) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    {vec, 4, T, glm_raw:vec4_set_x(T, D, X)}.

-doc("""
Returns the `y` component of a wrapped 4-component vector.

```erlang
2.0 = glm:vec4_y(glm:vec4(float, 1.0, 2.0, 3.0, 4.0)).
```
""").
-spec vec4_y(vec4(T)) -> term() when T :: type().
vec4_y({vec, 4, T, D}) ->
    glm_raw:vec4_y(T, D).

-doc("""
Returns a copy of a wrapped 4-component vector with its `y` component replaced.

```erlang
Updated = glm:vec4_set_y(glm:vec4(float, 1.0, 2.0, 3.0, 4.0), 6.0).
```
""").
-spec vec4_set_y(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_y({vec, 4, T, D}, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 4, T, glm_raw:vec4_set_y(T, D, Y)}.

-doc("""
Returns the `z` component of a wrapped 4-component vector.

```erlang
3.0 = glm:vec4_z(glm:vec4(float, 1.0, 2.0, 3.0, 4.0)).
```
""").
-spec vec4_z(vec4(T)) -> term() when T :: type().
vec4_z({vec, 4, T, D}) ->
    glm_raw:vec4_z(T, D).

-doc("""
Returns a copy of a wrapped 4-component vector with its `z` component replaced.

```erlang
Updated = glm:vec4_set_z(glm:vec4(float, 1.0, 2.0, 3.0, 4.0), 7.0).
```
""").
-spec vec4_set_z(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_z({vec, 4, T, D}, Z) ->
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {vec, 4, T, glm_raw:vec4_set_z(T, D, Z)}.

-doc("""
Returns the `w` component of a wrapped 4-component vector.

```erlang
4.0 = glm:vec4_w(glm:vec4(float, 1.0, 2.0, 3.0, 4.0)).
```
""").
-spec vec4_w(vec4(T)) -> term() when T :: type().
vec4_w({vec, 4, T, D}) ->
    glm_raw:vec4_w(T, D).

-doc("""
Returns a copy of a wrapped 4-component vector with its `w` component replaced.

```erlang
Updated = glm:vec4_set_w(glm:vec4(float, 1.0, 2.0, 3.0, 4.0), 8.0).
```
""").
-spec vec4_set_w(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_w({vec, 4, T, D}, W) ->
    ?EXCEPTION_IF_INVALID_VALUE(W, T),
    {vec, 4, T, glm_raw:vec4_set_w(T, D, W)}.

-doc("""
Returns the components of a wrapped 4-component vector as a BEAM tuple.

```erlang
{1.0, 2.0, 3.0, 4.0} = glm:vec4_values(glm:vec4(float, 1.0, 2.0, 3.0, 4.0)).
```
""").
-spec vec4_values(vec4(T)) -> {term(), term(), term(), term()} when T :: type().
vec4_values({vec, 4, T, D}) ->
    glm_raw:vec4_values(T, D).

-doc("""
Returns the wrapped 2x2 matrix initialized to zeroes.

```erlang
Zero = glm:mat2(double).
```
""").
-spec mat2(T) -> mat2(T) when T :: float | double.
mat2(T) ->
    {mat, 2, 2, T, glm_raw:mat2(T)}.

-doc("""
Returns a wrapped 2x2 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat2(double, 1.0).
```
""").
-spec mat2(T, float()) -> mat2(T) when T :: float | double.
mat2(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 2, T, glm_raw:mat2(T, V)}.

-doc("""
Builds a wrapped 2x2 matrix from explicit column-major values.

```erlang
Matrix = glm:mat2(double, 1.0, 2.0, 3.0, 4.0).
```
""").
-spec mat2(T, float(), float(), float(), float()) -> mat2(T) when T :: float | double.
mat2(T, M11, M21, M12, M22) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    {mat, 2, 2, T, glm_raw:mat2(T, M11, M21, M12, M22)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
2.0 = glm:mat2_element(glm:mat2(double, 1.0, 2.0, 3.0, 4.0), 2).
```
""").
-spec mat2_element(mat2(T), 1..4) -> float() when T :: float | double.
mat2_element({mat, 2, 2, T, D}, I) when I >= 1, I =< 4 ->
    glm_raw:mat2_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
3.0 = glm:mat2_element(glm:mat2(double, 1.0, 2.0, 3.0, 4.0), 2, 1).
```
""").
-spec mat2_element(mat2(T), 1..2, 1..2) -> float() when T :: float | double.
mat2_element({mat, 2, 2, T, D}, C, R) when C >= 1, C =< 2, R >= 1, R =< 2 ->
    glm_raw:mat2_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 2x2 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat2_set_element(glm:mat2(double), 3, 4.0).
```
""").
-spec mat2_set_element(mat2(T), 1..4, float()) -> mat2(T) when T :: float | double.
mat2_set_element({mat, 2, 2, T, D}, I, V) when I >= 1, I =< 4 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 2, T, glm_raw:mat2_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 2x2 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat2_set_element(glm:mat2(double), 2, 1, 4.0).
```
""").
-spec mat2_set_element(mat2(T), 1..2, 1..2, float()) -> mat2(T) when T :: float | double.
mat2_set_element({mat, 2, 2, T, D}, C, R, V) when C >= 1, C =< 2, R >= 1, R =< 2 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 2, T, glm_raw:mat2_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
{1.0, 2.0, 3.0, 4.0} = glm:mat2_values(glm:mat2(double, 1.0, 2.0, 3.0, 4.0)).
```
""").
-spec mat2_values(mat2(T)) -> {float(), float(), float(), float()} when T :: float | double.
mat2_values({mat, 2, 2, T, D}) ->
    glm_raw:mat2_values(T, D).

-doc("""
Returns the wrapped 3x3 matrix initialized to zeroes.

```erlang
Zero = glm:mat3(double).
```
""").
-spec mat3(T) -> mat3(T) when T :: float | double.
mat3(T) ->
    {mat, 3, 3, T, glm_raw:mat3(T)}.

-doc("""
Returns a wrapped 3x3 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat3(double, 1.0).
```
""").
-spec mat3(T, float()) -> mat3(T) when T :: float | double.
mat3(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 3, T, glm_raw:mat3(T, V)}.

-doc("""
Builds a wrapped 3x3 matrix from explicit column-major values.

```erlang
Basis = glm:mat3(double,
    1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0).
```
""").
-spec mat3(T, float(), float(), float(), float(), float(), float(), float(), float(), float()) -> mat3(T) when T :: float | double.
mat3(T, M11, M21, M31, M12, M22, M32, M13, M23, M33) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M31, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M32, T),
    ?EXCEPTION_IF_INVALID_VALUE(M13, T),
    ?EXCEPTION_IF_INVALID_VALUE(M23, T),
    ?EXCEPTION_IF_INVALID_VALUE(M33, T),
    {mat, 3, 3, T, glm_raw:mat3(T, M11, M21, M31, M12, M22, M32, M13, M23, M33)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
5.0 = glm:mat3_element(glm:mat3(double,
    1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0), 5).
```
""").
-spec mat3_element(mat3(T), 1..9) -> float() when T :: float | double.
mat3_element({mat, 3, 3, T, D}, I) when I >= 1, I =< 9 ->
    glm_raw:mat3_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
8.0 = glm:mat3_element(glm:mat3(double,
    1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0), 3, 2).
```
""").
-spec mat3_element(mat3(T), 1..3, 1..3) -> float() when T :: float | double.
mat3_element({mat, 3, 3, T, D}, C, R) when C >= 1, C =< 3, R >= 1, R =< 3 ->
    glm_raw:mat3_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 3x3 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat3_set_element(glm:mat3(double), 5, 2.0).
```
""").
-spec mat3_set_element(mat3(T), 1..9, float()) -> mat3(T) when T :: float | double.
mat3_set_element({mat, 3, 3, T, D}, I, V) when I >= 1, I =< 9 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 3, T, glm_raw:mat3_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 3x3 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat3_set_element(glm:mat3(double), 2, 3, 6.0).
```
""").
-spec mat3_set_element(mat3(T), 1..3, 1..3, float()) -> mat3(T) when T :: float | double.
mat3_set_element({mat, 3, 3, T, D}, C, R, V) when C >= 1, C =< 3, R >= 1, R =< 3 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 3, T, glm_raw:mat3_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0} =
    glm:mat3_values(glm:mat3(double,
        1.0, 2.0, 3.0,
        4.0, 5.0, 6.0,
        7.0, 8.0, 9.0)).
```
""").
-spec mat3_values(mat3(T)) -> {float(), float(), float(), float(), float(), float(), float(), float(), float()} when T :: float | double.
mat3_values({mat, 3, 3, T, D}) ->
    glm_raw:mat3_values(T, D).

-doc("""
Returns the wrapped 4x4 matrix initialized to zeroes.

```erlang
Zero = glm:mat4(double).
```
""").
-spec mat4(T) -> mat4(T) when T :: float | double.
mat4(T) ->
    {mat, 4, 4, T, glm_raw:mat4(T)}.

-doc("""
Returns a wrapped 4x4 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat4(double, 1.0).
```
""").
-spec mat4(T, float()) -> mat4(T) when T :: float | double.
mat4(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 4, T, glm_raw:mat4(T, V)}.

-doc("""
Builds a wrapped 4x4 matrix from explicit column-major values.

```erlang
Transform = glm:mat4(double,
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0).
```
""").
-spec mat4(T, float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()) -> mat4(T) when T :: float | double.
mat4(T, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M31, T),
    ?EXCEPTION_IF_INVALID_VALUE(M41, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M32, T),
    ?EXCEPTION_IF_INVALID_VALUE(M42, T),
    ?EXCEPTION_IF_INVALID_VALUE(M13, T),
    ?EXCEPTION_IF_INVALID_VALUE(M23, T),
    ?EXCEPTION_IF_INVALID_VALUE(M33, T),
    ?EXCEPTION_IF_INVALID_VALUE(M43, T),
    ?EXCEPTION_IF_INVALID_VALUE(M14, T),
    ?EXCEPTION_IF_INVALID_VALUE(M24, T),
    ?EXCEPTION_IF_INVALID_VALUE(M34, T),
    ?EXCEPTION_IF_INVALID_VALUE(M44, T),
    {mat, 4, 4, T, glm_raw:mat4(T, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43, M14, M24, M34, M44)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
1.0 = glm:mat4_element(glm:mat4(double,
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0), 1).
```
""").
-spec mat4_element(mat4(T), 1..16) -> float() when T :: float | double.
mat4_element({mat, 4, 4, T, D}, I) when I >= 1, I =< 16 ->
    glm_raw:mat4_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
1.0 = glm:mat4_element(glm:mat4(double,
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0), 4, 4).
```
""").
-spec mat4_element(mat4(T), 1..4, 1..4) -> float() when T :: float | double.
mat4_element({mat, 4, 4, T, D}, C, R) when C >= 1, C =< 4, R >= 1, R =< 4 ->
    glm_raw:mat4_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 4x4 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat4_set_element(glm:mat4(double), 16, 1.0).
```
""").
-spec mat4_set_element(mat4(T), 1..16, float()) -> mat4(T) when T :: float | double.
mat4_set_element({mat, 4, 4, T, D}, I, V) when I >= 1, I =< 16 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 4, T, glm_raw:mat4_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 4x4 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat4_set_element(glm:mat4(double), 4, 4, 1.0).
```
""").
-spec mat4_set_element(mat4(T), 1..4, 1..4, float()) -> mat4(T) when T :: float | double.
mat4_set_element({mat, 4, 4, T, D}, C, R, V) when C >= 1, C =< 4, R >= 1, R =< 4 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 4, T, glm_raw:mat4_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
Values = glm:mat4_values(glm:mat4(double,
    1.0, 0.0, 0.0, 0.0,
    0.0, 1.0, 0.0, 0.0,
    0.0, 0.0, 1.0, 0.0,
    0.0, 0.0, 0.0, 1.0)).
```
""").
-spec mat4_values(mat4(T)) -> {float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()} when T :: float | double.
mat4_values({mat, 4, 4, T, D}) ->
    glm_raw:mat4_values(T, D).

-doc("""
Returns the wrapped 2x3 matrix initialized to zeroes.

```erlang
Zero = glm:mat2x3(double).
```
""").
-spec mat2x3(T) -> mat2x3(T) when T :: float | double.
mat2x3(T) ->
    {mat, 2, 3, T, glm_raw:mat2x3(T)}.

-doc("""
Returns a wrapped 2x3 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat2x3(double, 1.0).
```
""").
-spec mat2x3(T, float()) -> mat2x3(T) when T :: float | double.
mat2x3(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 3, T, glm_raw:mat2x3(T, V)}.

-doc("""
Builds a wrapped 2x3 matrix from explicit column-major values.

```erlang
Matrix = glm:mat2x3(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0).
```
""").
-spec mat2x3(T, float(), float(), float(), float(), float(), float()) -> mat2x3(T) when T :: float | double.
mat2x3(T, M11, M21, M12, M22, M13, M23) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M13, T),
    ?EXCEPTION_IF_INVALID_VALUE(M23, T),
    {mat, 2, 3, T, glm_raw:mat2x3(T, M11, M21, M12, M22, M13, M23)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
4.0 = glm:mat2x3_element(glm:mat2x3(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 4).
```
""").
-spec mat2x3_element(mat2x3(T), 1..6) -> float() when T :: float | double.
mat2x3_element({mat, 2, 3, T, D}, I) when I >= 1, I =< 6 ->
    glm_raw:mat2x3_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
6.0 = glm:mat2x3_element(glm:mat2x3(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 2, 3).
```
""").
-spec mat2x3_element(mat2x3(T), 1..2, 1..3) -> float() when T :: float | double.
mat2x3_element({mat, 2, 3, T, D}, C, R) when C >= 1, C =< 2, R >= 1, R =< 3 ->
    glm_raw:mat2x3_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 2x3 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat2x3_set_element(glm:mat2x3(double), 6, 9.0).
```
""").
-spec mat2x3_set_element(mat2x3(T), 1..6, float()) -> mat2x3(T) when T :: float | double.
mat2x3_set_element({mat, 2, 3, T, D}, I, V) when I >= 1, I =< 6 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 3, T, glm_raw:mat2x3_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 2x3 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat2x3_set_element(glm:mat2x3(double), 2, 3, 9.0).
```
""").
-spec mat2x3_set_element(mat2x3(T), 1..2, 1..3, float()) -> mat2x3(T) when T :: float | double.
mat2x3_set_element({mat, 2, 3, T, D}, C, R, V) when C >= 1, C =< 2, R >= 1, R =< 3 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 3, T, glm_raw:mat2x3_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
{1.0, 2.0, 3.0, 4.0, 5.0, 6.0} =
    glm:mat2x3_values(glm:mat2x3(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)).
```
""").
-spec mat2x3_values(mat2x3(T)) -> {float(), float(), float(), float(), float(), float()} when T :: float | double.
mat2x3_values({mat, 2, 3, T, D}) ->
    glm_raw:mat2x3_values(T, D).

-doc("""
Returns the wrapped 2x4 matrix initialized to zeroes.

```erlang
Zero = glm:mat2x4(double).
```
""").
-spec mat2x4(T) -> mat2x4(T) when T :: float | double.
mat2x4(T) ->
    {mat, 2, 4, T, glm_raw:mat2x4(T)}.

-doc("""
Returns a wrapped 2x4 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat2x4(double, 1.0).
```
""").
-spec mat2x4(T, float()) -> mat2x4(T) when T :: float | double.
mat2x4(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 4, T, glm_raw:mat2x4(T, V)}.

-doc("""
Builds a wrapped 2x4 matrix from explicit column-major values.

```erlang
Matrix = glm:mat2x4(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0).
```
""").
-spec mat2x4(T, float(), float(), float(), float(), float(), float(), float(), float()) -> mat2x4(T) when T :: float | double.
mat2x4(T, M11, M21, M12, M22, M13, M23, M14, M24) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M13, T),
    ?EXCEPTION_IF_INVALID_VALUE(M23, T),
    ?EXCEPTION_IF_INVALID_VALUE(M14, T),
    ?EXCEPTION_IF_INVALID_VALUE(M24, T),
    {mat, 2, 4, T, glm_raw:mat2x4(T, M11, M21, M12, M22, M13, M23, M14, M24)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
8.0 = glm:mat2x4_element(glm:mat2x4(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), 8).
```
""").
-spec mat2x4_element(mat2x4(T), 1..8) -> float() when T :: float | double.
mat2x4_element({mat, 2, 4, T, D}, I) when I >= 1, I =< 8 ->
    glm_raw:mat2x4_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
8.0 = glm:mat2x4_element(glm:mat2x4(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), 2, 4).
```
""").
-spec mat2x4_element(mat2x4(T), 1..2, 1..4) -> float() when T :: float | double.
mat2x4_element({mat, 2, 4, T, D}, C, R) when C >= 1, C =< 2, R >= 1, R =< 4 ->
    glm_raw:mat2x4_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 2x4 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat2x4_set_element(glm:mat2x4(double), 8, 9.0).
```
""").
-spec mat2x4_set_element(mat2x4(T), 1..8, float()) -> mat2x4(T) when T :: float | double.
mat2x4_set_element({mat, 2, 4, T, D}, I, V) when I >= 1, I =< 8 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 4, T, glm_raw:mat2x4_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 2x4 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat2x4_set_element(glm:mat2x4(double), 2, 4, 9.0).
```
""").
-spec mat2x4_set_element(mat2x4(T), 1..2, 1..4, float()) -> mat2x4(T) when T :: float | double.
mat2x4_set_element({mat, 2, 4, T, D}, C, R, V) when C >= 1, C =< 2, R >= 1, R =< 4 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 2, 4, T, glm_raw:mat2x4_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0} =
    glm:mat2x4_values(glm:mat2x4(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)).
```
""").
-spec mat2x4_values(mat2x4(T)) -> {float(), float(), float(), float(), float(), float(), float(), float()} when T :: float | double.
mat2x4_values({mat, 2, 4, T, D}) ->
    glm_raw:mat2x4_values(T, D).

-doc("""
Returns the wrapped 3x2 matrix initialized to zeroes.

```erlang
Zero = glm:mat3x2(double).
```
""").
-spec mat3x2(T) -> mat3x2(T) when T :: float | double.
mat3x2(T) ->
    {mat, 3, 2, T, glm_raw:mat3x2(T)}.

-doc("""
Returns a wrapped 3x2 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat3x2(double, 1.0).
```
""").
-spec mat3x2(T, float()) -> mat3x2(T) when T :: float | double.
mat3x2(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 2, T, glm_raw:mat3x2(T, V)}.

-doc("""
Builds a wrapped 3x2 matrix from explicit column-major values.

```erlang
Matrix = glm:mat3x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0).
```
""").
-spec mat3x2(T, float(), float(), float(), float(), float(), float()) -> mat3x2(T) when T :: float | double.
mat3x2(T, M11, M21, M31, M12, M22, M32) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M31, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M32, T),
    {mat, 3, 2, T, glm_raw:mat3x2(T, M11, M21, M31, M12, M22, M32)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
6.0 = glm:mat3x2_element(glm:mat3x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 6).
```
""").
-spec mat3x2_element(mat3x2(T), 1..6) -> float() when T :: float | double.
mat3x2_element({mat, 3, 2, T, D}, I) when I >= 1, I =< 6 ->
    glm_raw:mat3x2_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
6.0 = glm:mat3x2_element(glm:mat3x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0), 3, 2).
```
""").
-spec mat3x2_element(mat3x2(T), 1..3, 1..2) -> float() when T :: float | double.
mat3x2_element({mat, 3, 2, T, D}, C, R) when C >= 1, C =< 3, R >= 1, R =< 2 ->
    glm_raw:mat3x2_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 3x2 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat3x2_set_element(glm:mat3x2(double), 6, 9.0).
```
""").
-spec mat3x2_set_element(mat3x2(T), 1..6, float()) -> mat3x2(T) when T :: float | double.
mat3x2_set_element({mat, 3, 2, T, D}, I, V) when I >= 1, I =< 6 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 2, T, glm_raw:mat3x2_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 3x2 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat3x2_set_element(glm:mat3x2(double), 3, 2, 9.0).
```
""").
-spec mat3x2_set_element(mat3x2(T), 1..3, 1..2, float()) -> mat3x2(T) when T :: float | double.
mat3x2_set_element({mat, 3, 2, T, D}, C, R, V) when C >= 1, C =< 3, R >= 1, R =< 2 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 2, T, glm_raw:mat3x2_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
{1.0, 2.0, 3.0, 4.0, 5.0, 6.0} =
    glm:mat3x2_values(glm:mat3x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0)).
```
""").
-spec mat3x2_values(mat3x2(T)) -> {float(), float(), float(), float(), float(), float()} when T :: float | double.
mat3x2_values({mat, 3, 2, T, D}) ->
    glm_raw:mat3x2_values(T, D).

-doc("""
Returns the wrapped 3x4 matrix initialized to zeroes.

```erlang
Zero = glm:mat3x4(double).
```
""").
-spec mat3x4(T) -> mat3x4(T) when T :: float | double.
mat3x4(T) ->
    {mat, 3, 4, T, glm_raw:mat3x4(T)}.

-doc("""
Returns a wrapped 3x4 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat3x4(double, 1.0).
```
""").
-spec mat3x4(T, float()) -> mat3x4(T) when T :: float | double.
mat3x4(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 4, T, glm_raw:mat3x4(T, V)}.

-doc("""
Builds a wrapped 3x4 matrix from explicit column-major values.

```erlang
Matrix = glm:mat3x4(double,
    1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0,
    10.0, 11.0, 12.0).
```
""").
-spec mat3x4(T, float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()) -> mat3x4(T) when T :: float | double.
mat3x4(T, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M31, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M32, T),
    ?EXCEPTION_IF_INVALID_VALUE(M13, T),
    ?EXCEPTION_IF_INVALID_VALUE(M23, T),
    ?EXCEPTION_IF_INVALID_VALUE(M33, T),
    ?EXCEPTION_IF_INVALID_VALUE(M14, T),
    ?EXCEPTION_IF_INVALID_VALUE(M24, T),
    ?EXCEPTION_IF_INVALID_VALUE(M34, T),
    {mat, 3, 4, T, glm_raw:mat3x4(T, M11, M21, M31, M12, M22, M32, M13, M23, M33, M14, M24, M34)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
12.0 = glm:mat3x4_element(glm:mat3x4(double,
    1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0,
    10.0, 11.0, 12.0), 12).
```
""").
-spec mat3x4_element(mat3x4(T), 1..12) -> float() when T :: float | double.
mat3x4_element({mat, 3, 4, T, D}, I) when I >= 1, I =< 12 ->
    glm_raw:mat3x4_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
12.0 = glm:mat3x4_element(glm:mat3x4(double,
    1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0,
    10.0, 11.0, 12.0), 3, 4).
```
""").
-spec mat3x4_element(mat3x4(T), 1..3, 1..4) -> float() when T :: float | double.
mat3x4_element({mat, 3, 4, T, D}, C, R) when C >= 1, C =< 3, R >= 1, R =< 4 ->
    glm_raw:mat3x4_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 3x4 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat3x4_set_element(glm:mat3x4(double), 12, 9.0).
```
""").
-spec mat3x4_set_element(mat3x4(T), 1..12, float()) -> mat3x4(T) when T :: float | double.
mat3x4_set_element({mat, 3, 4, T, D}, I, V) when I >= 1, I =< 12 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 4, T, glm_raw:mat3x4_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 3x4 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat3x4_set_element(glm:mat3x4(double), 3, 4, 9.0).
```
""").
-spec mat3x4_set_element(mat3x4(T), 1..3, 1..4, float()) -> mat3x4(T) when T :: float | double.
mat3x4_set_element({mat, 3, 4, T, D}, C, R, V) when C >= 1, C =< 3, R >= 1, R =< 4 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 3, 4, T, glm_raw:mat3x4_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
Values = glm:mat3x4_values(glm:mat3x4(double,
    1.0, 2.0, 3.0,
    4.0, 5.0, 6.0,
    7.0, 8.0, 9.0,
    10.0, 11.0, 12.0)).
```
""").
-spec mat3x4_values(mat3x4(T)) -> {float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()} when T :: float | double.
mat3x4_values({mat, 3, 4, T, D}) ->
    glm_raw:mat3x4_values(T, D).

-doc("""
Returns the wrapped 4x2 matrix initialized to zeroes.

```erlang
Zero = glm:mat4x2(double).
```
""").
-spec mat4x2(T) -> mat4x2(T) when T :: float | double.
mat4x2(T) ->
    {mat, 4, 2, T, glm_raw:mat4x2(T)}.

-doc("""
Returns a wrapped 4x2 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat4x2(double, 1.0).
```
""").
-spec mat4x2(T, float()) -> mat4x2(T) when T :: float | double.
mat4x2(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 2, T, glm_raw:mat4x2(T, V)}.

-doc("""
Builds a wrapped 4x2 matrix from explicit column-major values.

```erlang
Matrix = glm:mat4x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0).
```
""").
-spec mat4x2(T, float(), float(), float(), float(), float(), float(), float(), float()) -> mat4x2(T) when T :: float | double.
mat4x2(T, M11, M21, M31, M41, M12, M22, M32, M42) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M31, T),
    ?EXCEPTION_IF_INVALID_VALUE(M41, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M32, T),
    ?EXCEPTION_IF_INVALID_VALUE(M42, T),
    {mat, 4, 2, T, glm_raw:mat4x2(T, M11, M21, M31, M41, M12, M22, M32, M42)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
8.0 = glm:mat4x2_element(glm:mat4x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), 8).
```
""").
-spec mat4x2_element(mat4x2(T), 1..8) -> float() when T :: float | double.
mat4x2_element({mat, 4, 2, T, D}, I) when I >= 1, I =< 8 ->
    glm_raw:mat4x2_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
8.0 = glm:mat4x2_element(glm:mat4x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0), 4, 2).
```
""").
-spec mat4x2_element(mat4x2(T), 1..4, 1..2) -> float() when T :: float | double.
mat4x2_element({mat, 4, 2, T, D}, C, R) when C >= 1, C =< 4, R >= 1, R =< 2 ->
    glm_raw:mat4x2_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 4x2 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat4x2_set_element(glm:mat4x2(double), 8, 9.0).
```
""").
-spec mat4x2_set_element(mat4x2(T), 1..8, float()) -> mat4x2(T) when T :: float | double.
mat4x2_set_element({mat, 4, 2, T, D}, I, V) when I >= 1, I =< 8 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 2, T, glm_raw:mat4x2_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 4x2 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat4x2_set_element(glm:mat4x2(double), 4, 2, 9.0).
```
""").
-spec mat4x2_set_element(mat4x2(T), 1..4, 1..2, float()) -> mat4x2(T) when T :: float | double.
mat4x2_set_element({mat, 4, 2, T, D}, C, R, V) when C >= 1, C =< 4, R >= 1, R =< 2 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 2, T, glm_raw:mat4x2_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
{1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0} =
    glm:mat4x2_values(glm:mat4x2(double, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0)).
```
""").
-spec mat4x2_values(mat4x2(T)) -> {float(), float(), float(), float(), float(), float(), float(), float()} when T :: float | double.
mat4x2_values({mat, 4, 2, T, D}) ->
    glm_raw:mat4x2_values(T, D).

-doc("""
Returns the wrapped 4x3 matrix initialized to zeroes.

```erlang
Zero = glm:mat4x3(double).
```
""").
-spec mat4x3(T) -> mat4x3(T) when T :: float | double.
mat4x3(T) ->
    {mat, 4, 3, T, glm_raw:mat4x3(T)}.

-doc("""
Returns a wrapped 4x3 matrix with every element initialized to `V`.

```erlang
Filled = glm:mat4x3(double, 1.0).
```
""").
-spec mat4x3(T, float()) -> mat4x3(T) when T :: float | double.
mat4x3(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 3, T, glm_raw:mat4x3(T, V)}.

-doc("""
Builds a wrapped 4x3 matrix from explicit column-major values.

```erlang
Matrix = glm:mat4x3(double,
    1.0, 2.0, 3.0, 4.0,
    5.0, 6.0, 7.0, 8.0,
    9.0, 10.0, 11.0, 12.0).
```
""").
-spec mat4x3(T, float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()) -> mat4x3(T) when T :: float | double.
mat4x3(T, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43) ->
    ?EXCEPTION_IF_INVALID_VALUE(M11, T),
    ?EXCEPTION_IF_INVALID_VALUE(M21, T),
    ?EXCEPTION_IF_INVALID_VALUE(M31, T),
    ?EXCEPTION_IF_INVALID_VALUE(M41, T),
    ?EXCEPTION_IF_INVALID_VALUE(M12, T),
    ?EXCEPTION_IF_INVALID_VALUE(M22, T),
    ?EXCEPTION_IF_INVALID_VALUE(M32, T),
    ?EXCEPTION_IF_INVALID_VALUE(M42, T),
    ?EXCEPTION_IF_INVALID_VALUE(M13, T),
    ?EXCEPTION_IF_INVALID_VALUE(M23, T),
    ?EXCEPTION_IF_INVALID_VALUE(M33, T),
    ?EXCEPTION_IF_INVALID_VALUE(M43, T),
    {mat, 4, 3, T, glm_raw:mat4x3(T, M11, M21, M31, M41, M12, M22, M32, M42, M13, M23, M33, M43)}.

-doc("""
Returns the element at the 1-based linear index `I` in column-major order.

```erlang
12.0 = glm:mat4x3_element(glm:mat4x3(double,
    1.0, 2.0, 3.0, 4.0,
    5.0, 6.0, 7.0, 8.0,
    9.0, 10.0, 11.0, 12.0), 12).
```
""").
-spec mat4x3_element(mat4x3(T), 1..12) -> float() when T :: float | double.
mat4x3_element({mat, 4, 3, T, D}, I) when I >= 1, I =< 12 ->
    glm_raw:mat4x3_element(T, D, I).

-doc("""
Returns the element at column `C` and row `R`.

```erlang
12.0 = glm:mat4x3_element(glm:mat4x3(double,
    1.0, 2.0, 3.0, 4.0,
    5.0, 6.0, 7.0, 8.0,
    9.0, 10.0, 11.0, 12.0), 4, 3).
```
""").
-spec mat4x3_element(mat4x3(T), 1..4, 1..3) -> float() when T :: float | double.
mat4x3_element({mat, 4, 3, T, D}, C, R) when C >= 1, C =< 4, R >= 1, R =< 3 ->
    glm_raw:mat4x3_element(T, D, C, R).

-doc("""
Returns a copy of a wrapped 4x3 matrix with the element at linear index `I`
replaced by `V`.

```erlang
Updated = glm:mat4x3_set_element(glm:mat4x3(double), 12, 9.0).
```
""").
-spec mat4x3_set_element(mat4x3(T), 1..12, float()) -> mat4x3(T) when T :: float | double.
mat4x3_set_element({mat, 4, 3, T, D}, I, V) when I >= 1, I =< 12 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 3, T, glm_raw:mat4x3_set_element(T, D, I, V)}.

-doc("""
Returns a copy of a wrapped 4x3 matrix with the element at column `C` and row
`R` replaced by `V`.

```erlang
Updated = glm:mat4x3_set_element(glm:mat4x3(double), 4, 3, 9.0).
```
""").
-spec mat4x3_set_element(mat4x3(T), 1..4, 1..3, float()) -> mat4x3(T) when T :: float | double.
mat4x3_set_element({mat, 4, 3, T, D}, C, R, V) when C >= 1, C =< 4, R >= 1, R =< 3 ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {mat, 4, 3, T, glm_raw:mat4x3_set_element(T, D, C, R, V)}.

-doc("""
Returns the matrix elements as a BEAM tuple in column-major order.

```erlang
Values = glm:mat4x3_values(glm:mat4x3(double,
    1.0, 2.0, 3.0, 4.0,
    5.0, 6.0, 7.0, 8.0,
    9.0, 10.0, 11.0, 12.0)).
```
""").
-spec mat4x3_values(mat4x3(T)) -> {float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float(), float()} when T :: float | double.
mat4x3_values({mat, 4, 3, T, D}) ->
    glm_raw:mat4x3_values(T, D).

-doc("""
Returns the wrapped quaternion initialized to the identity rotation.

```erlang
Identity = glm:quat(double).
```
""").
-spec quat(T) -> quat(T) when T :: float | double.
quat(T) when T =:= float; T =:= double ->
    {quat, T, glm_raw:quat(T)}.

-doc("""
Builds a wrapped quaternion from explicit `w`, `x`, `y`, and `z` components.

```erlang
QuarterTurn = glm:quat(double, 0.7071067811865476, 0.0, 0.0, 0.7071067811865475).
```
""").
-spec quat(T, float(), float(), float(), float()) -> quat(T) when T :: float | double.
quat(T, W, X, Y, Z) when T =:= float; T =:= double ->
    ?EXCEPTION_IF_INVALID_VALUE(W, T),
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {quat, T, glm_raw:quat(T, W, X, Y, Z)}.

-doc("""
Returns the `w` component of a wrapped quaternion.

```erlang
1.0 = glm:quat_w(glm:quat(double)).
```
""").
-spec quat_w(quat(T)) -> float() when T :: float | double.
quat_w({quat, T, D}) when T =:= float; T =:= double ->
    glm_raw:quat_w(T, D).

-doc("""
Returns a copy of a wrapped quaternion with its `w` component replaced.

```erlang
Updated = glm:quat_set_w(glm:quat(double), 0.5).
```
""").
-spec quat_set_w(quat(T), float()) -> quat(T) when T :: float | double.
quat_set_w({quat, T, D}, W) when T =:= float; T =:= double ->
    ?EXCEPTION_IF_INVALID_VALUE(W, T),
    {quat, T, glm_raw:quat_set_w(T, D, W)}.

-doc("""
Returns the `x` component of a wrapped quaternion.

```erlang
0.0 = glm:quat_x(glm:quat(double)).
```
""").
-spec quat_x(quat(T)) -> float() when T :: float | double.
quat_x({quat, T, D}) when T =:= float; T =:= double ->
    glm_raw:quat_x(T, D).

-doc("""
Returns a copy of a wrapped quaternion with its `x` component replaced.

```erlang
Updated = glm:quat_set_x(glm:quat(double), 0.25).
```
""").
-spec quat_set_x(quat(T), float()) -> quat(T) when T :: float | double.
quat_set_x({quat, T, D}, X) when T =:= float; T =:= double ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    {quat, T, glm_raw:quat_set_x(T, D, X)}.

-doc("""
Returns the `y` component of a wrapped quaternion.

```erlang
0.0 = glm:quat_y(glm:quat(double)).
```
""").
-spec quat_y(quat(T)) -> float() when T :: float | double.
quat_y({quat, T, D}) when T =:= float; T =:= double ->
    glm_raw:quat_y(T, D).

-doc("""
Returns a copy of a wrapped quaternion with its `y` component replaced.

```erlang
Updated = glm:quat_set_y(glm:quat(double), 0.25).
```
""").
-spec quat_set_y(quat(T), float()) -> quat(T) when T :: float | double.
quat_set_y({quat, T, D}, Y) when T =:= float; T =:= double ->
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {quat, T, glm_raw:quat_set_y(T, D, Y)}.

-doc("""
Returns the `z` component of a wrapped quaternion.

```erlang
0.0 = glm:quat_z(glm:quat(double)).
```
""").
-spec quat_z(quat(T)) -> float() when T :: float | double.
quat_z({quat, T, D}) when T =:= float; T =:= double ->
    glm_raw:quat_z(T, D).

-doc("""
Returns a copy of a wrapped quaternion with its `z` component replaced.

```erlang
Updated = glm:quat_set_z(glm:quat(double), 0.25).
```
""").
-spec quat_set_z(quat(T), float()) -> quat(T) when T :: float | double.
quat_set_z({quat, T, D}, Z) when T =:= float; T =:= double ->
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {quat, T, glm_raw:quat_set_z(T, D, Z)}.

-doc("""
Returns the components of a wrapped quaternion as a `{W, X, Y, Z}` tuple.

```erlang
{1.0, 0.0, 0.0, 0.0} = glm:quat_values(glm:quat(double)).
```
""").
-spec quat_values(quat(T)) -> {float(), float(), float(), float()} when T :: float | double.
quat_values({quat, T, D}) when T =:= float; T =:= double ->
    glm_raw:quat_values(T, D).
