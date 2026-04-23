%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_packing).
-moduledoc """
OpenGL Mathematics (GLM) packing functions for the BEAM.

It exposes the compact core GLSL packing surface that maps cleanly to fixed
BEAM shapes. It includes half-float packing, signed and unsigned normalized
packing, and the `pack_double_2x32/1` plus `unpack_double_2x32/1` round-trip
between `uvec2` and `double`.

Supported surface:

- `pack_double_2x32/1` and `unpack_double_2x32/1` round-trip between `uvec2`
  and `double`
- `pack_half_1x16/1`, `pack_half_2x16/1`, and `pack_half_4x16/1` pack
	`float`, `vec2(float)`, and `vec4(float)` into compact half-float lanes
- `pack_snorm_1x8/1`, `pack_snorm_2x8/1`, `pack_snorm_1x16/1`,
	`pack_snorm_2x16/1`, `pack_snorm_4x8/1`, and `pack_snorm_4x16/1` pack
	normalized `float` values into compact signed integer lanes
- `pack_unorm_1x8/1`, `pack_unorm_2x8/1`, `pack_unorm_1x16/1`,
	`pack_unorm_2x16/1`, `pack_unorm_4x8/1`, and `pack_unorm_4x16/1` pack
	normalized `float` values into compact unsigned integer lanes
- the matching `unpack_*` functions recover wrapped `float`, `vec2(float)`,
	and `vec4(float)` values from those packed scalar formats

Typical calls look like:

```erlang
Packed = glm_packing:pack_unorm_4x8(glm:vec4(float, 1.0, 0.0, 0.0, 1.0)).
16#FF0000FF = glm:uint32_value(Packed).

Roundtrip = glm_packing:unpack_half_2x16(glm:uint32(16#40003C00)).
{1.0, 2.0} = glm:vec2_values(Roundtrip).
```

These examples show both directions of the wrapped packing workflow.
""".

-export([
	pack_double_2x32/1,
	pack_half_1x16/1,
	pack_half_2x16/1,
	pack_half_4x16/1,
	pack_snorm_1x8/1,
	pack_snorm_2x8/1,
	pack_snorm_1x16/1,
	pack_snorm_2x16/1,
	pack_snorm_4x8/1,
	pack_snorm_4x16/1,
	pack_unorm_1x8/1,
	pack_unorm_2x8/1,
	pack_unorm_1x16/1,
	pack_unorm_2x16/1,
	pack_unorm_4x8/1,
	pack_unorm_4x16/1,
	unpack_double_2x32/1,
	unpack_half_1x16/1,
	unpack_half_2x16/1,
	unpack_half_4x16/1,
	unpack_snorm_1x8/1,
	unpack_snorm_2x8/1,
	unpack_snorm_1x16/1,
	unpack_snorm_2x16/1,
	unpack_snorm_4x8/1,
	unpack_snorm_4x16/1,
	unpack_unorm_1x8/1,
	unpack_unorm_2x8/1,
	unpack_unorm_1x16/1,
	unpack_unorm_2x16/1,
	unpack_unorm_4x8/1,
	unpack_unorm_4x16/1
]).

-doc("""
Pack a `uvec2` into a `double`.

```erlang
Packed = glm_packing:pack_double_2x32(glm:vec2({uint, 32}, 16#01234567, 16#89ABCDEF)).
_ = glm:double_value(Packed).
```
""").
-spec pack_double_2x32(glm:vec2({uint, 32})) -> glm:double().
pack_double_2x32({vec, 2, {uint, 32}, D}) ->
	{scalar, double, glm_raw:pack_double_2x32(double, D)}.

-doc("""
Pack a `float` into a `uint16` half-float lane.

```erlang
Packed = glm_packing:pack_half_1x16(glm:float(1.0)).
16#3C00 = glm:uint16_value(Packed).
```
""").
-spec pack_half_1x16(glm:float()) -> glm:uint16().
pack_half_1x16({scalar, float, D}) ->
	{scalar, {uint, 16}, glm_raw:pack_half_1x16(float, D)}.

-doc("""
Pack two `float` values into a single `uint32` using 16-bit half floats.

```erlang
Packed = glm_packing:pack_half_2x16(glm:vec2(float, 1.0, 2.0)).
16#40003C00 = glm:uint32_value(Packed).
```
""").
-spec pack_half_2x16(glm:vec2(float)) -> glm:uint32().
pack_half_2x16({vec, 2, float, D}) ->
	{scalar, {uint, 32}, glm_raw:pack_half_2x16(float, D)}.

-doc("""
Pack four `float` values into a single `uint64` using 16-bit half floats.

```erlang
Packed = glm_packing:pack_half_4x16(glm:vec4(float, 1.0, 2.0, 0.0, -2.0)).
16#C000000040003C00 = glm:uint64_value(Packed).
```
""").
-spec pack_half_4x16(glm:vec4(float)) -> glm:uint64().
pack_half_4x16({vec, 4, float, D}) ->
	{scalar, {uint, 64}, glm_raw:pack_half_4x16(float, D)}.

-doc("""
Pack a normalized `float` into a single signed 8-bit lane.

```erlang
Packed = glm_packing:pack_snorm_1x8(glm:float(1.0)).
16#7F = glm:uint8_value(Packed).
```
""").
-spec pack_snorm_1x8(glm:float()) -> glm:uint8().
pack_snorm_1x8({scalar, float, D}) ->
	{scalar, {uint, 8}, glm_raw:pack_snorm_1x8(float, D)}.

-doc("""
Pack two normalized `float` values into a single `uint16` using signed 8-bit lanes.

```erlang
Packed = glm_packing:pack_snorm_2x8(glm:vec2(float, 1.0, 0.0)).
16#007F = glm:uint16_value(Packed).
```
""").
-spec pack_snorm_2x8(glm:vec2(float)) -> glm:uint16().
pack_snorm_2x8({vec, 2, float, D}) ->
	{scalar, {uint, 16}, glm_raw:pack_snorm_2x8(float, D)}.

-doc("""
Pack a normalized `float` into a single signed 16-bit lane.

```erlang
Packed = glm_packing:pack_snorm_1x16(glm:float(1.0)).
16#7FFF = glm:uint16_value(Packed).
```
""").
-spec pack_snorm_1x16(glm:float()) -> glm:uint16().
pack_snorm_1x16({scalar, float, D}) ->
	{scalar, {uint, 16}, glm_raw:pack_snorm_1x16(float, D)}.

-doc("""
Pack two normalized `float` values into a single `uint32` using signed 16-bit lanes.

```erlang
Packed = glm_packing:pack_snorm_2x16(glm:vec2(float, 1.0, 0.0)).
16#00007FFF = glm:uint32_value(Packed).
```
""").
-spec pack_snorm_2x16(glm:vec2(float)) -> glm:uint32().
pack_snorm_2x16({vec, 2, float, D}) ->
	{scalar, {uint, 32}, glm_raw:pack_snorm_2x16(float, D)}.

-doc("""
Pack four normalized `float` values into a single `uint32` using signed 8-bit lanes.

```erlang
Packed = glm_packing:pack_snorm_4x8(glm:vec4(float, 1.0, 0.0, -0.5, -1.0)).
16#81C0007F = glm:uint32_value(Packed).
```
""").
-spec pack_snorm_4x8(glm:vec4(float)) -> glm:uint32().
pack_snorm_4x8({vec, 4, float, D}) ->
	{scalar, {uint, 32}, glm_raw:pack_snorm_4x8(float, D)}.

-doc("""
Pack four normalized `float` values into a single `uint64` using signed 16-bit lanes.

```erlang
Packed = glm_packing:pack_snorm_4x16(glm:vec4(float, 1.0, 0.0, -0.5, -1.0)).
16#8001C00000007FFF = glm:uint64_value(Packed).
```
""").
-spec pack_snorm_4x16(glm:vec4(float)) -> glm:uint64().
pack_snorm_4x16({vec, 4, float, D}) ->
	{scalar, {uint, 64}, glm_raw:pack_snorm_4x16(float, D)}.

-doc("""
Pack a normalized `float` into a single unsigned 8-bit lane.

```erlang
Packed = glm_packing:pack_unorm_1x8(glm:float(1.0)).
16#FF = glm:uint8_value(Packed).
```
""").
-spec pack_unorm_1x8(glm:float()) -> glm:uint8().
pack_unorm_1x8({scalar, float, D}) ->
	{scalar, {uint, 8}, glm_raw:pack_unorm_1x8(float, D)}.

-doc("""
Pack two normalized `float` values into a single `uint16` using unsigned 8-bit lanes.

```erlang
Packed = glm_packing:pack_unorm_2x8(glm:vec2(float, 1.0, 0.5)).
16#80FF = glm:uint16_value(Packed).
```
""").
-spec pack_unorm_2x8(glm:vec2(float)) -> glm:uint16().
pack_unorm_2x8({vec, 2, float, D}) ->
	{scalar, {uint, 16}, glm_raw:pack_unorm_2x8(float, D)}.

-doc("""
Pack a normalized `float` into a single unsigned 16-bit lane.

```erlang
Packed = glm_packing:pack_unorm_1x16(glm:float(1.0)).
16#FFFF = glm:uint16_value(Packed).
```
""").
-spec pack_unorm_1x16(glm:float()) -> glm:uint16().
pack_unorm_1x16({scalar, float, D}) ->
	{scalar, {uint, 16}, glm_raw:pack_unorm_1x16(float, D)}.

-doc("""
Pack two normalized `float` values into a single `uint32` using unsigned 16-bit lanes.

```erlang
Packed = glm_packing:pack_unorm_2x16(glm:vec2(float, 1.0, 0.0)).
16#0000FFFF = glm:uint32_value(Packed).
```
""").
-spec pack_unorm_2x16(glm:vec2(float)) -> glm:uint32().
pack_unorm_2x16({vec, 2, float, D}) ->
	{scalar, {uint, 32}, glm_raw:pack_unorm_2x16(float, D)}.

-doc("""
Pack four normalized `float` values into a single `uint32` using unsigned 8-bit lanes.

```erlang
Packed = glm_packing:pack_unorm_4x8(glm:vec4(float, 1.0, 0.5, 0.0, 1.0)).
16#FF0080FF = glm:uint32_value(Packed).
```
""").
-spec pack_unorm_4x8(glm:vec4(float)) -> glm:uint32().
pack_unorm_4x8({vec, 4, float, D}) ->
	{scalar, {uint, 32}, glm_raw:pack_unorm_4x8(float, D)}.

-doc("""
Pack four normalized `float` values into a single `uint64` using unsigned 16-bit lanes.

```erlang
Packed = glm_packing:pack_unorm_4x16(glm:vec4(float, 1.0, 0.5, 0.0, 1.0)).
16#FFFF00008000FFFF = glm:uint64_value(Packed).
```
""").
-spec pack_unorm_4x16(glm:vec4(float)) -> glm:uint64().
pack_unorm_4x16({vec, 4, float, D}) ->
	{scalar, {uint, 64}, glm_raw:pack_unorm_4x16(float, D)}.

-doc("""
Unpack a `double` into a `uvec2`.

```erlang
Roundtrip = glm_packing:unpack_double_2x32(
	glm_packing:pack_double_2x32(glm:vec2({uint, 32}, 1, 2))
).
{1, 2} = glm:vec2_values(Roundtrip).
```
""").
-spec unpack_double_2x32(glm:double()) -> glm:vec2({uint, 32}).
unpack_double_2x32({scalar, double, D}) ->
	{vec, 2, {uint, 32}, glm_raw:unpack_double_2x32(double, D)}.

-doc("""
Unpack a half float from a packed `uint16`.

```erlang
Value = glm_packing:unpack_half_1x16(glm:uint16(16#3C00)).
1.0 = glm:float_value(Value).
```
""").
-spec unpack_half_1x16(glm:uint16()) -> glm:float().
unpack_half_1x16({scalar, {uint, 16}, D}) ->
	{scalar, float, glm_raw:unpack_half_1x16(float, D)}.

-doc("""
Unpack two half floats from a packed `uint32`.

```erlang
Value = glm_packing:unpack_half_2x16(glm:uint32(16#40003C00)).
{1.0, 2.0} = glm:vec2_values(Value).
```
""").
-spec unpack_half_2x16(glm:uint32()) -> glm:vec2(float).
unpack_half_2x16({scalar, {uint, 32}, D}) ->
	{vec, 2, float, glm_raw:unpack_half_2x16(float, D)}.

-doc("""
Unpack four half floats from a packed `uint64`.

```erlang
Value = glm_packing:unpack_half_4x16(glm:uint64(16#C000000040003C00)).
{1.0, 2.0, 0.0, -2.0} = glm:vec4_values(Value).
```
""").
-spec unpack_half_4x16(glm:uint64()) -> glm:vec4(float).
unpack_half_4x16({scalar, {uint, 64}, D}) ->
	{vec, 4, float, glm_raw:unpack_half_4x16(float, D)}.

-doc("""
Unpack a signed normalized 8-bit lane from a packed `uint8`.

```erlang
Value = glm_packing:unpack_snorm_1x8(glm:uint8(16#C0)).
_ = glm:float_value(Value).
```
""").
-spec unpack_snorm_1x8(glm:uint8()) -> glm:float().
unpack_snorm_1x8({scalar, {uint, 8}, D}) ->
	{scalar, float, glm_raw:unpack_snorm_1x8(float, D)}.

-doc("""
Unpack two signed normalized 8-bit lanes from a packed `uint16`.

```erlang
Value = glm_packing:unpack_snorm_2x8(glm:uint16(16#C07F)).
_ = glm:vec2_values(Value).
```
""").
-spec unpack_snorm_2x8(glm:uint16()) -> glm:vec2(float).
unpack_snorm_2x8({scalar, {uint, 16}, D}) ->
	{vec, 2, float, glm_raw:unpack_snorm_2x8(float, D)}.

-doc("""
Unpack a signed normalized 16-bit lane from a packed `uint16`.

```erlang
Value = glm_packing:unpack_snorm_1x16(glm:uint16(16#C000)).
_ = glm:float_value(Value).
```
""").
-spec unpack_snorm_1x16(glm:uint16()) -> glm:float().
unpack_snorm_1x16({scalar, {uint, 16}, D}) ->
	{scalar, float, glm_raw:unpack_snorm_1x16(float, D)}.

-doc("""
Unpack two signed normalized 16-bit lanes from a packed `uint32`.

```erlang
Value = glm_packing:unpack_snorm_2x16(glm:uint32(16#00007FFF)).
{1.0, 0.0} = glm:vec2_values(Value).
```
""").
-spec unpack_snorm_2x16(glm:uint32()) -> glm:vec2(float).
unpack_snorm_2x16({scalar, {uint, 32}, D}) ->
	{vec, 2, float, glm_raw:unpack_snorm_2x16(float, D)}.

-doc("""
Unpack four signed normalized 8-bit lanes from a packed `uint32`.

```erlang
Value = glm_packing:unpack_snorm_4x8(glm:uint32(16#81C0007F)).
_ = glm:vec4_values(Value).
```
""").
-spec unpack_snorm_4x8(glm:uint32()) -> glm:vec4(float).
unpack_snorm_4x8({scalar, {uint, 32}, D}) ->
	{vec, 4, float, glm_raw:unpack_snorm_4x8(float, D)}.

-doc("""
Unpack four signed normalized 16-bit lanes from a packed `uint64`.

```erlang
Value = glm_packing:unpack_snorm_4x16(glm:uint64(16#8001C00000007FFF)).
_ = glm:vec4_values(Value).
```
""").
-spec unpack_snorm_4x16(glm:uint64()) -> glm:vec4(float).
unpack_snorm_4x16({scalar, {uint, 64}, D}) ->
	{vec, 4, float, glm_raw:unpack_snorm_4x16(float, D)}.

-doc("""
Unpack an unsigned normalized 8-bit lane from a packed `uint8`.

```erlang
Value = glm_packing:unpack_unorm_1x8(glm:uint8(16#80)).
_ = glm:float_value(Value).
```
""").
-spec unpack_unorm_1x8(glm:uint8()) -> glm:float().
unpack_unorm_1x8({scalar, {uint, 8}, D}) ->
	{scalar, float, glm_raw:unpack_unorm_1x8(float, D)}.

-doc("""
Unpack two unsigned normalized 8-bit lanes from a packed `uint16`.

```erlang
Value = glm_packing:unpack_unorm_2x8(glm:uint16(16#80FF)).
_ = glm:vec2_values(Value).
```
""").
-spec unpack_unorm_2x8(glm:uint16()) -> glm:vec2(float).
unpack_unorm_2x8({scalar, {uint, 16}, D}) ->
	{vec, 2, float, glm_raw:unpack_unorm_2x8(float, D)}.

-doc("""
Unpack an unsigned normalized 16-bit lane from a packed `uint16`.

```erlang
Value = glm_packing:unpack_unorm_1x16(glm:uint16(16#8000)).
_ = glm:float_value(Value).
```
""").
-spec unpack_unorm_1x16(glm:uint16()) -> glm:float().
unpack_unorm_1x16({scalar, {uint, 16}, D}) ->
	{scalar, float, glm_raw:unpack_unorm_1x16(float, D)}.

-doc("""
Unpack two unsigned normalized 16-bit lanes from a packed `uint32`.

```erlang
Value = glm_packing:unpack_unorm_2x16(glm:uint32(16#0000FFFF)).
{1.0, 0.0} = glm:vec2_values(Value).
```
""").
-spec unpack_unorm_2x16(glm:uint32()) -> glm:vec2(float).
unpack_unorm_2x16({scalar, {uint, 32}, D}) ->
	{vec, 2, float, glm_raw:unpack_unorm_2x16(float, D)}.

-doc("""
Unpack four unsigned normalized 8-bit lanes from a packed `uint32`.

```erlang
Value = glm_packing:unpack_unorm_4x8(glm:uint32(16#FF0080FF)).
_ = glm:vec4_values(Value).
```
""").
-spec unpack_unorm_4x8(glm:uint32()) -> glm:vec4(float).
unpack_unorm_4x8({scalar, {uint, 32}, D}) ->
	{vec, 4, float, glm_raw:unpack_unorm_4x8(float, D)}.

-doc("""
Unpack four unsigned normalized 16-bit lanes from a packed `uint64`.

```erlang
Value = glm_packing:unpack_unorm_4x16(glm:uint64(16#FFFF00008000FFFF)).
_ = glm:vec4_values(Value).
```
""").
-spec unpack_unorm_4x16(glm:uint64()) -> glm:vec4(float).
unpack_unorm_4x16({scalar, {uint, 64}, D}) ->
	{vec, 4, float, glm_raw:unpack_unorm_4x16(float, D)}.
