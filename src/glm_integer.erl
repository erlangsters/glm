%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_integer).
-moduledoc """
OpenGL Mathematics (GLM) integer functions for the BEAM.

It focuses on the common GLSL and GLM integer helpers that map cleanly to the
existing scalar and vector wrappers. It includes bit counting and bitfield
helpers, power-of-two and multiple predicates, next and previous multiple
helpers, and the carry, borrow, and extended-multiplication operations.

Supported operands:

- `bit_count/1`, `bitfield_extract/3`, `bitfield_insert/4`, `bitfield_reverse/1`, `find_lsb/1`, and `find_msb/1` accept integer scalars and `vec2` to `vec4` integer vectors
- `is_power_of_two/1`, `next_power_of_two/1`, and `prev_power_of_two/1` accept integer scalars and `vec2` to `vec4` integer vectors
- `is_multiple/2`, `next_multiple/2`, and `prev_multiple/2` accept integer scalars, `vecN` with scalar multiples, and `vecN` with same-length integer vector multiples
- `uadd_carry/2`, `usub_borrow/2`, and `umul_extended/2` accept `uint32` scalars and `vec2` to `vec4` `uvec32` values
- `imul_extended/2` accepts `int32` scalars and `vec2` to `vec4` `ivec32` values

Deferred for now:

- `find_nsb` and the remaining lower-priority extension helpers

Typical calls look like:

```erlang
3 = glm:int32_value(glm_integer:bit_count(glm:uint32(7))).
1 = glm:int32_value(glm_integer:find_lsb(glm:uint32(6))).

true = glm:bool_value(glm_integer:is_power_of_two(glm:uint32(8))).
16 = glm:uint32_value(glm_integer:next_power_of_two(glm:uint32(15))).

Mask = glm_integer:is_multiple(
	glm:vec3({int, 32}, 6, 7, 9),
	glm:int32(3)
).
{true, false, true} = glm:vec3_values(Mask).

{Result, Carry} = glm_integer:uadd_carry(glm:uint32(16#ffffffff), glm:uint32(1)).
0 = glm:uint32_value(Result).
1 = glm:uint32_value(Carry).
```

These examples cover the main wrapped integer shapes exposed by the module.
""".

-export([
	bit_count/1,
	bitfield_extract/3,
	bitfield_insert/4,
	bitfield_reverse/1,
	find_lsb/1,
	find_msb/1,
	imul_extended/2,
	is_multiple/2,
	is_power_of_two/1,
	next_multiple/2,
	next_power_of_two/1,
	uadd_carry/2,
	umul_extended/2,
	prev_multiple/2,
	prev_power_of_two/1,
	usub_borrow/2
]).

-doc("""
Return the number of bits set to `1`.

```erlang
Count = glm_integer:bit_count(glm:uint32(7)).
3 = glm:int32_value(Count).
```
""").
-spec bit_count(glm:scalar(T)) -> glm:int32() when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
		  (glm:vec(L, T)) -> glm:vec(L, {int, 32}) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
bit_count({scalar, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, {int, 32}, glm_raw:bit_count(T, undefined, D)};
bit_count({vec, L, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, {int, 32}, glm_raw:bit_count(T, L, D)}.

-doc("""
Extract the requested bit range from an integer value.

```erlang
Value = glm_integer:bitfield_extract(
	glm:uint32(16#F0000000),
	glm:int32(28),
	glm:int32(4)
).
16#0000000F = glm:uint32_value(Value).
```
""").
-spec bitfield_extract(glm:scalar(T), glm:int32(), glm:int32()) -> glm:scalar(T) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
		  (glm:vec(L, T), glm:int32(), glm:int32()) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
bitfield_extract({scalar, T, D}, {scalar, {int, 32}, Offset}, {scalar, {int, 32}, Bits}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, T, glm_raw:bitfield_extract(T, undefined, D, Offset, Bits)};
bitfield_extract({vec, L, T, D}, {scalar, {int, 32}, Offset}, {scalar, {int, 32}, Bits}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:bitfield_extract(T, L, D, Offset, Bits)}.

-doc("""
Insert the requested least-significant bits from one integer value into another.

```erlang
Value = glm_integer:bitfield_insert(
	glm:uint32(16#FF000000),
	glm:uint32(16#000000FF),
	glm:int32(8),
	glm:int32(8)
).
16#FF00FF00 = glm:uint32_value(Value).
```
""").
-spec bitfield_insert(glm:scalar(T), glm:scalar(T), glm:int32(), glm:int32()) -> glm:scalar(T) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
		  (glm:vec(L, T), glm:vec(L, T), glm:int32(), glm:int32()) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
bitfield_insert({scalar, T, Base}, {scalar, T, Insert}, {scalar, {int, 32}, Offset}, {scalar, {int, 32}, Bits}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, T, glm_raw:bitfield_insert(T, undefined, Base, Insert, Offset, Bits)};
bitfield_insert({vec, L, T, Base}, {vec, L, T, Insert}, {scalar, {int, 32}, Offset}, {scalar, {int, 32}, Bits}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:bitfield_insert(T, L, Base, Insert, Offset, Bits)}.

-doc("""
Reverse the bits in an integer value.

```erlang
Value = glm_integer:bitfield_reverse(glm:uint32(16#00000001)).
16#80000000 = glm:uint32_value(Value).
```
""").
-spec bitfield_reverse(glm:scalar(T)) -> glm:scalar(T) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
		  (glm:vec(L, T)) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
bitfield_reverse({scalar, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, T, glm_raw:bitfield_reverse(T, undefined, D)};
bitfield_reverse({vec, L, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:bitfield_reverse(T, L, D)}.

-doc("""
Return the index of the least significant set bit, or `-1` for zero.

```erlang
Index = glm_integer:find_lsb(glm:uint32(6)).
1 = glm:int32_value(Index).
```
""").
-spec find_lsb(glm:scalar(T)) -> glm:int32() when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
		  (glm:vec(L, T)) -> glm:vec(L, {int, 32}) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
find_lsb({scalar, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, {int, 32}, glm_raw:find_lsb(T, undefined, D)};
find_lsb({vec, L, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, {int, 32}, glm_raw:find_lsb(T, L, D)}.

-doc("""
Return the index of the most significant bit as defined by GLSL and GLM.

```erlang
Index = glm_integer:find_msb(glm:uint32(4)).
2 = glm:int32_value(Index).
```
""").
-spec find_msb(glm:scalar(T)) -> glm:int32() when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
		  (glm:vec(L, T)) -> glm:vec(L, {int, 32}) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
find_msb({scalar, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, {int, 32}, glm_raw:find_msb(T, undefined, D)};
find_msb({vec, L, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, {int, 32}, glm_raw:find_msb(T, L, D)}.

-doc("""
Multiply two signed 32-bit integers and return `{Msb, Lsb}`.

```erlang
{Msb, Lsb} = glm_integer:imul_extended(glm:int32(2), glm:int32(3)).
0 = glm:int32_value(Msb).
6 = glm:int32_value(Lsb).
```
""").
-spec imul_extended(glm:int32(), glm:int32()) -> {glm:int32(), glm:int32()};
		  (glm:vec(L, {int, 32}), glm:vec(L, {int, 32})) -> {glm:vec(L, {int, 32}), glm:vec(L, {int, 32})} when L :: glm:length().
imul_extended({scalar, {int, 32}, D1}, {scalar, {int, 32}, D2}) ->
	{Msb, Lsb} = glm_raw:imul_extended({int, 32}, undefined, D1, D2),
	{{scalar, {int, 32}, Msb}, {scalar, {int, 32}, Lsb}};
imul_extended({vec, L, {int, 32}, D1}, {vec, L, {int, 32}, D2}) ->
	{Msb, Lsb} = glm_raw:imul_extended({int, 32}, L, D1, D2),
	{{vec, L, {int, 32}, Msb}, {vec, L, {int, 32}, Lsb}}.

-doc("""
Return `true` when the integer value is a multiple of the provided divisor.

```erlang
Mask = glm_integer:is_multiple(
	glm:vec3({int, 32}, 6, 7, 9),
	glm:int32(3)
).
{true, false, true} = glm:vec3_values(Mask).
```
""").
-spec is_multiple(glm:scalar(T), glm:scalar(T)) -> glm:scalar(bool) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
				 (glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, bool) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
				 (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, bool) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
is_multiple({scalar, T, D1}, {scalar, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, bool, glm_raw:is_multiple(T, undefined, {scalar, scalar}, D1, D2)};
is_multiple({vec, L, T, D1}, {scalar, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, bool, glm_raw:is_multiple(T, L, {vector, scalar}, D1, D2)};
is_multiple({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, bool, glm_raw:is_multiple(T, L, {vector, vector}, D1, D2)}.

-doc("""
Return `true` when the integer value is a power of two.

```erlang
Flag = glm_integer:is_power_of_two(glm:uint32(8)).
true = glm:bool_value(Flag).
```
""").
-spec is_power_of_two(glm:scalar(T)) -> glm:scalar(bool) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
					 (glm:vec(L, T)) -> glm:vec(L, bool) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
is_power_of_two({scalar, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, bool, glm_raw:is_power_of_two(T, undefined, D)};
is_power_of_two({vec, L, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, bool, glm_raw:is_power_of_two(T, L, D)}.

-doc("""
Round up to the next multiple of the provided divisor.

```erlang
Value = glm_integer:next_multiple(glm:uint32(8), glm:uint32(3)).
9 = glm:uint32_value(Value).
```
""").
-spec next_multiple(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
				   (glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
				   (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
next_multiple({scalar, T, D1}, {scalar, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, T, glm_raw:next_multiple(T, undefined, {scalar, scalar}, D1, D2)};
next_multiple({vec, L, T, D1}, {scalar, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:next_multiple(T, L, {vector, scalar}, D1, D2)};
next_multiple({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:next_multiple(T, L, {vector, vector}, D1, D2)}.

-doc("""
Round up to the next power of two.

```erlang
Value = glm_integer:next_power_of_two(glm:uint32(15)).
16 = glm:uint32_value(Value).
```
""").
-spec next_power_of_two(glm:scalar(T)) -> glm:scalar(T) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
					   (glm:vec(L, T)) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
next_power_of_two({scalar, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, T, glm_raw:next_power_of_two(T, undefined, D)};
next_power_of_two({vec, L, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:next_power_of_two(T, L, D)}.

-doc("""
Add two unsigned 32-bit integers and return `{Result, Carry}`.

```erlang
{Result, Carry} = glm_integer:uadd_carry(glm:uint32(16#FFFFFFFF), glm:uint32(1)).
0 = glm:uint32_value(Result).
1 = glm:uint32_value(Carry).
```
""").
-spec uadd_carry(glm:uint32(), glm:uint32()) -> {glm:uint32(), glm:uint32()};
		  (glm:vec(L, {uint, 32}), glm:vec(L, {uint, 32})) -> {glm:vec(L, {uint, 32}), glm:vec(L, {uint, 32})} when L :: glm:length().
uadd_carry({scalar, {uint, 32}, D1}, {scalar, {uint, 32}, D2}) ->
	{Result, Carry} = glm_raw:uadd_carry({uint, 32}, undefined, D1, D2),
	{{scalar, {uint, 32}, Result}, {scalar, {uint, 32}, Carry}};
uadd_carry({vec, L, {uint, 32}, D1}, {vec, L, {uint, 32}, D2}) ->
	{Result, Carry} = glm_raw:uadd_carry({uint, 32}, L, D1, D2),
	{{vec, L, {uint, 32}, Result}, {vec, L, {uint, 32}, Carry}}.

-doc("""
Multiply two unsigned 32-bit integers and return `{Msb, Lsb}`.

```erlang
{Msb, Lsb} = glm_integer:umul_extended(glm:uint32(2), glm:uint32(3)).
0 = glm:uint32_value(Msb).
6 = glm:uint32_value(Lsb).
```
""").
-spec umul_extended(glm:uint32(), glm:uint32()) -> {glm:uint32(), glm:uint32()};
		  (glm:vec(L, {uint, 32}), glm:vec(L, {uint, 32})) -> {glm:vec(L, {uint, 32}), glm:vec(L, {uint, 32})} when L :: glm:length().
umul_extended({scalar, {uint, 32}, D1}, {scalar, {uint, 32}, D2}) ->
	{Msb, Lsb} = glm_raw:umul_extended({uint, 32}, undefined, D1, D2),
	{{scalar, {uint, 32}, Msb}, {scalar, {uint, 32}, Lsb}};
umul_extended({vec, L, {uint, 32}, D1}, {vec, L, {uint, 32}, D2}) ->
	{Msb, Lsb} = glm_raw:umul_extended({uint, 32}, L, D1, D2),
	{{vec, L, {uint, 32}, Msb}, {vec, L, {uint, 32}, Lsb}}.

-doc("""
Round down to the previous multiple of the provided divisor.

```erlang
Value = glm_integer:prev_multiple(glm:uint32(8), glm:uint32(3)).
6 = glm:uint32_value(Value).
```
""").
-spec prev_multiple(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
				   (glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
				   (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
prev_multiple({scalar, T, D1}, {scalar, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, T, glm_raw:prev_multiple(T, undefined, {scalar, scalar}, D1, D2)};
prev_multiple({vec, L, T, D1}, {scalar, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:prev_multiple(T, L, {vector, scalar}, D1, D2)};
prev_multiple({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:prev_multiple(T, L, {vector, vector}, D1, D2)}.

-doc("""
Round down to the previous power of two.

```erlang
Value = glm_integer:prev_power_of_two(glm:uint32(15)).
8 = glm:uint32_value(Value).
```
""").
-spec prev_power_of_two(glm:scalar(T)) -> glm:scalar(T) when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64};
					   (glm:vec(L, T)) -> glm:vec(L, T) when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64}.
prev_power_of_two({scalar, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{scalar, T, glm_raw:prev_power_of_two(T, undefined, D)};
prev_power_of_two({vec, L, T, D}) when T =/= bool, T =/= float, T =/= double ->
	{vec, L, T, glm_raw:prev_power_of_two(T, L, D)}.

-doc("""
Subtract two unsigned 32-bit integers and return `{Result, Borrow}`.

```erlang
{Result, Borrow} = glm_integer:usub_borrow(glm:uint32(16), glm:uint32(17)).
1 = glm:uint32_value(Result).
1 = glm:uint32_value(Borrow).
```
""").
-spec usub_borrow(glm:uint32(), glm:uint32()) -> {glm:uint32(), glm:uint32()};
		  (glm:vec(L, {uint, 32}), glm:vec(L, {uint, 32})) -> {glm:vec(L, {uint, 32}), glm:vec(L, {uint, 32})} when L :: glm:length().
usub_borrow({scalar, {uint, 32}, D1}, {scalar, {uint, 32}, D2}) ->
	{Result, Borrow} = glm_raw:usub_borrow({uint, 32}, undefined, D1, D2),
	{{scalar, {uint, 32}, Result}, {scalar, {uint, 32}, Borrow}};
usub_borrow({vec, L, {uint, 32}, D1}, {vec, L, {uint, 32}, D2}) ->
	{Result, Borrow} = glm_raw:usub_borrow({uint, 32}, L, D1, D2),
	{{vec, L, {uint, 32}, Result}, {vec, L, {uint, 32}, Borrow}}.
