%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_angle).
-moduledoc """
OpenGL Mathematics (GLM) angle and trigonometric functions for the BEAM.

It groups angle conversion together with trigonometric,
inverse-trigonometric, and hyperbolic functions on wrapped floating-point
scalars and vectors. It includes `radians/1`, `degrees/1`, `sin/1`, `cos/1`,
`tan/1`, `asin/1`, `acos/1`, `atan/1`, `sinh/1`, `cosh/1`, `tanh/1`,
`asinh/1`, `acosh/1`, and `atanh/1`.

Supported operands:

- `float` and `double` scalars
- `vec2`, `vec3`, and `vec4` with `float` or `double` components

Typical calls look like:

```erlang
QuarterTurn = glm_angle:radians(glm:double(90.0)).
HalfPi = glm:double_value(QuarterTurn).

UnitY = glm_angle:sin(glm:vec2(double, 0.0, HalfPi)).
0.0 = glm:vec2_x(UnitY).
1.0 = glm:vec2_y(UnitY).

BackToDegrees = glm_angle:degrees(glm:vec2(double, HalfPi, math:pi())).
90.0 = glm:vec2_x(BackToDegrees).
180.0 = glm:vec2_y(BackToDegrees).
```

These examples show the same wrapped unary workflow on both scalars and
vectors.
""".

-export([
	acos/1,
	acosh/1,
	asin/1,
	asinh/1,
	atan/1,
	atanh/1,
	cos/1,
	cosh/1,
	degrees/1,
	radians/1,
	sin/1,
	sinh/1,
	tan/1,
	tanh/1
]).

-doc("""
Compute the arc cosine component-wise.

```erlang
Angle = glm_angle:acos(glm:double(1.0)).
0.0 = glm:double_value(Angle).
```
""").
-spec acos(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
acos({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:acos(T, undefined, D)};
acos({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:acos(T, L, D)}.

-doc("""
Compute the inverse hyperbolic cosine component-wise.

```erlang
Value = glm_angle:acosh(glm:double(1.0)).
0.0 = glm:double_value(Value).
```
""").
-spec acosh(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		   (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
acosh({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:acosh(T, undefined, D)};
acosh({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:acosh(T, L, D)}.

-doc("""
Compute the arc sine component-wise.

```erlang
Angle = glm_angle:asin(glm:double(0.0)).
0.0 = glm:double_value(Angle).
```
""").
-spec asin(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
asin({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:asin(T, undefined, D)};
asin({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:asin(T, L, D)}.

-doc("""
Compute the inverse hyperbolic sine component-wise.

```erlang
Value = glm_angle:asinh(glm:double(0.0)).
0.0 = glm:double_value(Value).
```
""").
-spec asinh(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		   (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
asinh({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:asinh(T, undefined, D)};
asinh({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:asinh(T, L, D)}.

-doc("""
Compute the arc tangent component-wise.

```erlang
Angle = glm_angle:atan(glm:double(1.0)).
_ = glm:double_value(Angle).
```
""").
-spec atan(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
atan({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:atan(T, undefined, D)};
atan({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:atan(T, L, D)}.

-doc("""
Compute the inverse hyperbolic tangent component-wise.

```erlang
Value = glm_angle:atanh(glm:double(0.0)).
0.0 = glm:double_value(Value).
```
""").
-spec atanh(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		   (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
atanh({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:atanh(T, undefined, D)};
atanh({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:atanh(T, L, D)}.

-doc("""
Compute the cosine component-wise.

```erlang
Value = glm_angle:cos(glm:double(0.0)).
1.0 = glm:double_value(Value).
```
""").
-spec cos(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		 (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
cos({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:cos(T, undefined, D)};
cos({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:cos(T, L, D)}.

-doc("""
Compute the hyperbolic cosine component-wise.

```erlang
Value = glm_angle:cosh(glm:double(0.0)).
1.0 = glm:double_value(Value).
```
""").
-spec cosh(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
cosh({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:cosh(T, undefined, D)};
cosh({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:cosh(T, L, D)}.

-doc("""
Convert radians to degrees component-wise.

```erlang
Degrees = glm_angle:degrees(glm:double(math:pi())).
180.0 = glm:double_value(Degrees).
```
""").
-spec degrees(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		   (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
degrees({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:degrees(T, undefined, D)};
degrees({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:degrees(T, L, D)}.

-doc("""
Convert degrees to radians component-wise.

```erlang
Radians = glm_angle:radians(glm:double(180.0)).
_ = glm:double_value(Radians).
```
""").
-spec radians(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		   (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
radians({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:radians(T, undefined, D)};
radians({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:radians(T, L, D)}.

-doc("""
Compute the sine component-wise.

```erlang
Value = glm_angle:sin(glm:double(math:pi() / 2.0)).
1.0 = glm:double_value(Value).
```
""").
-spec sin(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		 (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
sin({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:sin(T, undefined, D)};
sin({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:sin(T, L, D)}.

-doc("""
Compute the hyperbolic sine component-wise.

```erlang
Value = glm_angle:sinh(glm:double(0.0)).
0.0 = glm:double_value(Value).
```
""").
-spec sinh(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
sinh({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:sinh(T, undefined, D)};
sinh({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:sinh(T, L, D)}.

-doc("""
Compute the tangent component-wise.

```erlang
Value = glm_angle:tan(glm:double(math:pi() / 4.0)).
1.0 = glm:double_value(Value).
```
""").
-spec tan(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		 (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
tan({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:tan(T, undefined, D)};
tan({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:tan(T, L, D)}.

-doc("""
Compute the hyperbolic tangent component-wise.

```erlang
Value = glm_angle:tanh(glm:double(0.0)).
0.0 = glm:double_value(Value).
```
""").
-spec tanh(glm:scalar(T)) -> glm:scalar(T) when T :: float | double;
		  (glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
tanh({scalar, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:tanh(T, undefined, D)};
tanh({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:tanh(T, L, D)}.
