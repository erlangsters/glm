%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_common).
-compile({no_auto_import, [max/2, min/2]}).
-moduledoc """
OpenGL Mathematics (GLM) common functions for the BEAM.

It exposes GLM's common scalar and vector helpers while preserving the wrapped
BEAM shapes defined in `glm`. It includes arithmetic helpers such as `abs/1`,
`min/2`, `max/2`, `clamp/3`, `mix/3`, `step/2`, and `smoothstep/3`, rounding
helpers such as `ceil/1`, `floor/1`, `round/1`, `round_even/1`, and `trunc/1`,
bit reinterpretation helpers, and tuple-returning operations such as `frexp/1`
and `modf/1`.

Functions that naturally produce more than one value return wrapped tuples
instead of raw payloads.
""".

-export([
	abs/1,
	ceil/1,
	clamp/3,
	float_bits_to_int/1,
	float_bits_to_uint/1,
	fma/3,
	fract/1,
	frexp/1,
	floor/1,
	int_bits_to_float/1,
	is_inf/1,
	is_nan/1,
	ldexp/2,
	max/2,
	min/2,
	mix/3,
	mod/2,
	modf/1,
	round/1,
	round_even/1,
	sign/1,
	smoothstep/3,
	step/2,
	trunc/1,
	uint_bits_to_float/1
]).

-doc("""
Returns the absolute value of a wrapped scalar or of each component in a
wrapped vector.

It accepts signed integer and floating-point inputs and preserves the input
shape.

```erlang
Absolute = glm_common:abs(glm:vec3(double, -1.0, 2.0, -3.0)).
```
""").
-spec abs(glm:scalar(T)) -> glm:scalar(T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | float | double;
	      (glm:vec(L, T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | float | double,
	     L :: glm:length().
abs({scalar, T, D}) when T =:= {int, 8}; T =:= {int, 16}; T =:= {int, 32}; T =:= {int, 64}; T =:= float; T =:= double ->
	{scalar, T, glm_raw:abs(T, undefined, D)};
abs({vec, L, T, D}) when T =:= {int, 8}; T =:= {int, 16}; T =:= {int, 32}; T =:= {int, 64}; T =:= float; T =:= double ->
	{vec, L, T, glm_raw:abs(T, L, D)}.

-doc("""
Rounds each floating-point value upward while preserving the wrapped input
shape.

```erlang
Ceiled = glm_common:ceil(glm:vec2(double, 1.2, -1.2)).
```
""").
-spec ceil(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
ceil({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:ceil(T, undefined, D)};
ceil({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:ceil(T, L, D)}.

-doc("""
Clamps each value in `X` between `MinVal` and `MaxVal`.

Accepted operand shapes are scalar/scalar/scalar, vector/scalar/scalar, and
vector/vector/vector. All operands must use the same wrapped element type.

```erlang
Clamped = glm_common:clamp(
	glm:vec3(double, -1.0, 0.5, 3.0),
	glm:double(0.0),
	glm:double(1.0)
).
```
""").
-spec clamp(glm:scalar(T), glm:scalar(T), glm:scalar(T)) -> glm:scalar(T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double;
	      (glm:vec(L, T), glm:scalar(T), glm:scalar(T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double,
	     L :: glm:length();
	      (glm:vec(L, T), glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double,
	     L :: glm:length().
clamp({scalar, T, D1}, {scalar, T, D2}, {scalar, T, D3}) ->
	R = glm_raw:clamp(
		T,
		undefined,
		{scalar, scalar, scalar},
		D1, D2, D3
	),
	{scalar, T, R};
clamp({vec, L, T, D1}, {scalar, T, D2}, {scalar, T, D3}) ->
	R = glm_raw:clamp(
		T,
		L,
		{vector, scalar, scalar},
		D1, D2, D3
	),
	{vec, L, T, R};
clamp({vec, L, T, D1}, {vec, L, T, D2}, {vec, L, T, D3}) ->
	R = glm_raw:clamp(
		T,
		L,
		{vector, vector, vector},
		D1, D2, D3
	),
	{vec, L, T, R}.

-doc("""
Reinterprets the bit pattern of a float scalar or vector as signed 32-bit
integers.

This is a bit-cast operation. It does not perform a numeric conversion.

```erlang
IntBits = glm_common:float_bits_to_int(glm:float(1.0)).
```
""").
-spec float_bits_to_int(glm:float()) -> glm:int32();
		  (glm:vec(L, float)) -> glm:vec(L, {int, 32}) when L :: glm:length().
float_bits_to_int({scalar, float, D}) ->
	{scalar, {int, 32}, glm_raw:float_bits_to_int(float, undefined, D)};
float_bits_to_int({vec, L, float, D}) ->
	{vec, L, {int, 32}, glm_raw:float_bits_to_int(float, L, D)}.

-doc("""
Reinterprets the bit pattern of a float scalar or vector as unsigned 32-bit
integers.

This is a bit-cast operation. It does not perform a numeric conversion.

```erlang
UintBits = glm_common:float_bits_to_uint(glm:float(1.0)).
```
""").
-spec float_bits_to_uint(glm:float()) -> glm:uint32();
		  (glm:vec(L, float)) -> glm:vec(L, {uint, 32}) when L :: glm:length().
float_bits_to_uint({scalar, float, D}) ->
	{scalar, {uint, 32}, glm_raw:float_bits_to_uint(float, undefined, D)};
float_bits_to_uint({vec, L, float, D}) ->
	{vec, L, {uint, 32}, glm_raw:float_bits_to_uint(float, L, D)}.

-doc("""
Computes `X * Y + Z` for wrapped floating-point scalars or vectors.

The result keeps the same wrapped shape and element type as the inputs.

```erlang
Combined = glm_common:fma(glm:double(2.0), glm:double(3.0), glm:double(4.0)).
```
""").
-spec fma(glm:scalar(T), glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T), glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
fma({scalar, T, D1}, {scalar, T, D2}, {scalar, T, D3}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:fma(T, undefined, D1, D2, D3)};
fma({vec, L, T, D1}, {vec, L, T, D2}, {vec, L, T, D3}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:fma(T, L, D1, D2, D3)}.

-doc("""
Rounds each floating-point value downward while preserving the wrapped input
shape.

```erlang
Floored = glm_common:floor(glm:vec2(double, 1.8, -1.2)).
```
""").
-spec floor(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		   (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
floor({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:floor(T, undefined, D)};
floor({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:floor(T, L, D)}.

-doc("""
Returns the fractional part of each wrapped floating-point value.

```erlang
Fractional = glm_common:fract(glm:vec2(double, 1.25, -1.75)).
```
""").
-spec fract(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
fract({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:fract(T, undefined, D)};
fract({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:fract(T, L, D)}.

-doc("""
Decomposes wrapped floating-point values into significand and exponent parts.

The result is a tuple of wrapped values. The significand keeps the input shape,
and the exponent is returned as `glm:int32()` or `glm:vec(L, {int, 32})`.

```erlang
{Significand, Exponent} = glm_common:frexp(glm:double(12.0)).
```
""").
-spec frexp(glm:scalar(T)) -> {glm:scalar(T), glm:int32()} when T :: float | double;
		  (glm:vec(L, T)) -> {glm:vec(L, T), glm:vec(L, {int, 32})} when T :: float | double, L :: glm:length().
frexp({scalar, T, D}) when T =:= float; T =:= double ->
	{Significand, Exponent} = glm_raw:frexp(T, undefined, D),
	{{scalar, T, Significand}, {scalar, {int, 32}, Exponent}};
frexp({vec, L, T, D}) when T =:= float; T =:= double ->
	{Significand, Exponent} = glm_raw:frexp(T, L, D),
	{{vec, L, T, Significand}, {vec, L, {int, 32}, Exponent}}.

-doc("""
Reinterprets signed 32-bit integer bits as floating-point values.

This is a bit-cast operation. It does not perform a numeric conversion.

```erlang
FloatValue = glm_common:int_bits_to_float(glm:int32(16#3F800000)).
```
""").
-spec int_bits_to_float(glm:int32()) -> glm:float();
		  (glm:vec(L, {int, 32})) -> glm:vec(L, float) when L :: glm:length().
int_bits_to_float({scalar, {int, 32}, D}) ->
	{scalar, float, glm_raw:int_bits_to_float({int, 32}, undefined, D)};
int_bits_to_float({vec, L, {int, 32}, D}) ->
	{vec, L, float, glm_raw:int_bits_to_float({int, 32}, L, D)}.

-doc("""
Returns a wrapped bool vector indicating which components are infinite.

```erlang
Flags = glm_common:is_inf(glm:vec2(double, 1.0, 2.0)).
```
""").
-spec is_inf(glm:vec(L, T)) -> glm:vec(L, bool) when T :: float | double, L :: glm:length().
is_inf({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, bool, glm_raw:is_inf(T, L, D)}.

-doc("""
Returns a wrapped bool vector indicating which components are NaN.

```erlang
Flags = glm_common:is_nan(glm:vec2(double, 1.0, 2.0)).
```
""").
-spec is_nan(glm:vec(L, T)) -> glm:vec(L, bool) when T :: float | double, L :: glm:length().
is_nan({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, bool, glm_raw:is_nan(T, L, D)}.

-doc("""
Builds floating-point values from a significand and a base-2 exponent.

```erlang
Restored = glm_common:ldexp(glm:double(0.75), glm:int32(4)).
```
""").
-spec ldexp(glm:scalar(T), glm:int32()) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T), glm:vec(L, {int, 32})) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
ldexp({scalar, T, D1}, {scalar, {int, 32}, D2}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:ldexp(T, undefined, D1, D2)};
ldexp({vec, L, T, D1}, {vec, L, {int, 32}, D2}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:ldexp(T, L, D1, D2)}.

-doc("""
Returns the lesser value of each numeric operand pair.

Accepted operand shapes are scalar/scalar, vector/scalar, and vector/vector.

```erlang
Lower = glm_common:min(glm:vec3(double, 1.0, 4.0, 2.0), glm:double(2.5)).
```
""").
-spec min(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double;
	      (glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double,
	     L :: glm:length();
	      (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double,
	     L :: glm:length().
min({scalar, T, D1}, {scalar, T, D2}) when T =/= bool ->
	{scalar, T, glm_raw:min(T, undefined, {scalar, scalar}, D1, D2)};
min({vec, L, T, D1}, {scalar, T, D2}) when T =/= bool ->
	{vec, L, T, glm_raw:min(T, L, {vector, scalar}, D1, D2)};
min({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool ->
	{vec, L, T, glm_raw:min(T, L, {vector, vector}, D1, D2)}.

-doc("""
Returns the greater value of each numeric operand pair.

Accepted operand shapes are scalar/scalar, vector/scalar, and vector/vector.

```erlang
Upper = glm_common:max(glm:vec3(double, 1.0, 4.0, 2.0), glm:double(2.5)).
```
""").
-spec max(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double;
	      (glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double,
	     L :: glm:length();
	      (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} |
	          {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} |
	          float | double,
	     L :: glm:length().
max({scalar, T, D1}, {scalar, T, D2}) when T =/= bool ->
	{scalar, T, glm_raw:max(T, undefined, {scalar, scalar}, D1, D2)};
max({vec, L, T, D1}, {scalar, T, D2}) when T =/= bool ->
	{vec, L, T, glm_raw:max(T, L, {vector, scalar}, D1, D2)};
max({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool ->
	{vec, L, T, glm_raw:max(T, L, {vector, vector}, D1, D2)}.

-doc("""
Linearly interpolates between `X` and `Y` using the interpolation factor `A`.

`A` can be a wrapped scalar or a wrapped vector depending on the accepted shape.

```erlang
Midpoint = glm_common:mix(glm:double(0.0), glm:double(10.0), glm:double(0.5)).
```
""").
-spec mix(glm:scalar(T), glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T), glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length();
		  (glm:vec(L, T), glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
mix({scalar, T, D1}, {scalar, T, D2}, {scalar, T, D3}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:mix(T, undefined, {scalar, scalar, scalar}, D1, D2, D3)};
mix({vec, L, T, D1}, {vec, L, T, D2}, {scalar, T, D3}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:mix(T, L, {vector, vector, scalar}, D1, D2, D3)};
mix({vec, L, T, D1}, {vec, L, T, D2}, {vec, L, T, D3}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:mix(T, L, {vector, vector, vector}, D1, D2, D3)}.

-doc("""
Returns the floating-point remainder of `X / Y`.

Accepted operand shapes are scalar/scalar, vector/scalar, and vector/vector.

```erlang
Remainder = glm_common:mod(glm:vec2(double, 5.5, 7.0), glm:double(2.0)).
```
""").
-spec mod(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length();
		  (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
mod({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:mod(T, undefined, {scalar, scalar}, D1, D2)};
mod({vec, L, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:mod(T, L, {vector, scalar}, D1, D2)};
mod({vec, L, T, D1}, {vec, L, T, D2}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:mod(T, L, {vector, vector}, D1, D2)}.

-doc("""
Splits each floating-point value into fractional and integral parts.

The result is a tuple of wrapped values with the same shape and element type as
the input.

```erlang
{Fractional, Integral} = glm_common:modf(glm:vec2(double, 1.25, -1.75)).
```
""").
-spec modf(glm:scalar(T)) -> {glm:scalar(T), glm:scalar(T)} when T :: float | double;
		  (glm:vec(L, T)) -> {glm:vec(L, T), glm:vec(L, T)} when T :: float | double, L :: glm:length().
modf({scalar, T, D}) when T =:= float; T =:= double ->
	{Fractional, Integral} = glm_raw:modf(T, undefined, D),
	{{scalar, T, Fractional}, {scalar, T, Integral}};
modf({vec, L, T, D}) when T =:= float; T =:= double ->
	{Fractional, Integral} = glm_raw:modf(T, L, D),
	{{vec, L, T, Fractional}, {vec, L, T, Integral}}.

-doc("""
Rounds each floating-point vector component to the nearest integral value.

```erlang
Rounded = glm_common:round(glm:vec3(double, 1.2, 1.5, 1.8)).
```
""").
-spec round(glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
round({vec, L, T, D}) when T =:= float; T =:= double ->
	R = glm_raw:round(T, L, D),
	{vec, L, T, R}.

-doc("""
Rounds each floating-point vector component to the nearest even integral value.

```erlang
Rounded = glm_common:round_even(glm:vec2(double, 0.5, 1.5)).
```
""").
-spec round_even(glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
round_even({vec, L, T, D}) when T =:= float; T =:= double ->
	R = glm_raw:round_even(T, L, D),
	{vec, L, T, R}.

-doc("""
Returns the sign of each signed integer or floating-point value.

It preserves the wrapped input shape.

```erlang
Signs = glm_common:sign(glm:vec3(double, -2.0, 0.0, 5.0)).
```
""").
-spec sign(glm:scalar(T)) -> glm:scalar(T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | float | double;
	      (glm:vec(L, T)) -> glm:vec(L, T)
	when T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | float | double,
	     L :: glm:length().
sign({scalar, T, D}) when T =:= {int, 8}; T =:= {int, 16}; T =:= {int, 32}; T =:= {int, 64}; T =:= float; T =:= double ->
	{scalar, T, glm_raw:sign(T, undefined, D)};
sign({vec, L, T, D}) when T =:= {int, 8}; T =:= {int, 16}; T =:= {int, 32}; T =:= {int, 64}; T =:= float; T =:= double ->
	{vec, L, T, glm_raw:sign(T, L, D)}.

-doc("""
Performs smooth Hermite interpolation between `0` and `1` for values in the
range delimited by `Edge0` and `Edge1`.

```erlang
Blend = glm_common:smoothstep(glm:double(0.0), glm:double(1.0), glm:double(0.25)).
```
""").
-spec smoothstep(glm:scalar(T), glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:scalar(T), glm:scalar(T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length();
		  (glm:vec(L, T), glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
smoothstep({scalar, T, D1}, {scalar, T, D2}, {scalar, T, D3}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:smoothstep(T, undefined, {scalar, scalar, scalar}, D1, D2, D3)};
smoothstep({scalar, T, D1}, {scalar, T, D2}, {vec, L, T, D3}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:smoothstep(T, L, {scalar, scalar, vector}, D1, D2, D3)};
smoothstep({vec, L, T, D1}, {vec, L, T, D2}, {vec, L, T, D3}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:smoothstep(T, L, {vector, vector, vector}, D1, D2, D3)}.

-doc("""
Evaluates the step function for each value in `X` against `Edge`.

```erlang
Mask = glm_common:step(glm:double(0.5), glm:vec3(double, 0.25, 0.5, 0.75)).
```
""").
-spec step(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:scalar(T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length();
		  (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
step({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:step(T, undefined, {scalar, scalar}, D1, D2)};
step({scalar, T, D1}, {vec, L, T, D2}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:step(T, L, {scalar, vector}, D1, D2)};
step({vec, L, T, D1}, {vec, L, T, D2}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:step(T, L, {vector, vector}, D1, D2)}.

-doc("""
Rounds each floating-point value toward zero while preserving the wrapped input
shape.

```erlang
Whole = glm_common:trunc(glm:vec2(double, 1.9, -1.9)).
```
""").
-spec trunc(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
trunc({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:trunc(T, undefined, D)};
trunc({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:trunc(T, L, D)}.

-doc("""
Reinterprets unsigned 32-bit integer bits as floating-point values.

This is a bit-cast operation. It does not perform a numeric conversion.

```erlang
FloatValue = glm_common:uint_bits_to_float(glm:uint32(16#3F800000)).
```
""").
-spec uint_bits_to_float(glm:uint32()) -> glm:float();
		  (glm:vec(L, {uint, 32})) -> glm:vec(L, float) when L :: glm:length().
uint_bits_to_float({scalar, {uint, 32}, D}) ->
	{scalar, float, glm_raw:uint_bits_to_float({uint, 32}, undefined, D)};
uint_bits_to_float({vec, L, {uint, 32}, D}) ->
	{vec, L, float, glm_raw:uint_bits_to_float({uint, 32}, L, D)}.
