%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_exponential).
-moduledoc """
OpenGL Mathematics (GLM) exponential functions for the BEAM.

It groups the exponential family on wrapped floating-point scalars and
vectors. It includes `exp/1`, `exp2/1`, `log/1`, `log2/1`, `sqrt/1`,
`inverse_sqrt/1`, and `pow/2`.

Supported operands:

- `float` and `double` scalars
- `vec2`, `vec3`, and `vec4` with `float` or `double` components

`pow/2` follows the currently supported BEAM-facing shapes and accepts either
two scalars of the same type or two vectors of the same length and type.

Typical calls look like:

```erlang
Eight = glm_exponential:exp2(glm:float(3.0)).
8.0 = glm:float_value(Eight).

Roots = glm_exponential:sqrt(glm:vec2(double, 1.0, 9.0)).
1.0 = glm:vec2_x(Roots).
3.0 = glm:vec2_y(Roots).

Powers = glm_exponential:pow(
	glm:vec2(float, 2.0, 9.0),
	glm:vec2(float, 3.0, 0.5)
).
8.0 = glm:vec2_x(Powers).
3.0 = glm:vec2_y(Powers).
```

These examples show the wrapped scalar and vector shapes used across the whole
module.
""".

-export([
	exp/1,
	exp2/1,
	inverse_sqrt/1,
	log/1,
	log2/1,
	pow/2,
	sqrt/1
]).

-doc("""
Compute $e^x$ component-wise.

```erlang
Value = glm_exponential:exp(glm:double(1.0)).
2.718281828459045 = glm:double_value(Value).
```
""").
-spec exp(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
exp({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:exp(T, undefined, D)};
exp({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:exp(T, L, D)}.

-doc("""
Compute $2^x$ component-wise.

```erlang
Value = glm_exponential:exp2(glm:float(3.0)).
8.0 = glm:float_value(Value).
```
""").
-spec exp2(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
exp2({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:exp2(T, undefined, D)};
exp2({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:exp2(T, L, D)}.

-doc("""
Compute $1 / \sqrt{x}$ component-wise.

```erlang
Value = glm_exponential:inverse_sqrt(glm:double(0.25)).
2.0 = glm:double_value(Value).
```
""").
-spec inverse_sqrt(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
inverse_sqrt({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:inverse_sqrt(T, undefined, D)};
inverse_sqrt({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:inverse_sqrt(T, L, D)}.

-doc("""
Compute the natural logarithm component-wise.

```erlang
Value = glm_exponential:log(glm:double(2.718281828459045)).
1.0 = glm:double_value(Value).
```
""").
-spec log(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
log({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:log(T, undefined, D)};
log({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:log(T, L, D)}.

-doc("""
Compute the base-2 logarithm component-wise.

```erlang
Value = glm_exponential:log2(glm:float(8.0)).
3.0 = glm:float_value(Value).
```
""").
-spec log2(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
log2({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:log2(T, undefined, D)};
log2({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:log2(T, L, D)}.

-doc("""
Raise the left operand to the power of the right operand.

Currently supported shapes:

- scalar with scalar
- vector with vector of the same length and type

```erlang
Value = glm_exponential:pow(glm:double(9.0), glm:double(0.5)).
3.0 = glm:double_value(Value).
```
""").
-spec pow(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
pow({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:pow(T, undefined, D1, D2)};
pow({vec, L, T, D1}, {vec, L, T, D2}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:pow(T, L, D1, D2)}.

-doc("""
Compute the square root component-wise.

```erlang
Value = glm_exponential:sqrt(glm:vec3(double, 1.0, 4.0, 9.0)).
1.0 = glm:vec3_x(Value).
2.0 = glm:vec3_y(Value).
3.0 = glm:vec3_z(Value).
```
""").
-spec sqrt(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
sqrt({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:sqrt(T, undefined, D)};
sqrt({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:sqrt(T, L, D)}.
