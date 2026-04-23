%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_vector).
-moduledoc """
OpenGL Mathematics (GLM) vector operations for the BEAM.

It groups the first vector-geometric operations on wrapped vector values. It
includes `dot/2`, `cross/2`, `length/1`, `distance/2`, `normalize/1`,
`reflect/2`, `refract/3`, and `face_forward/3`.

Supported operands:

- `vec2`, `vec3`, and `vec4` with `float` or `double` components
- `cross/2` is currently limited to `vec3`
- `refract/3` takes a vector incident direction, a vector normal, and a
  scalar refraction index ratio of the same floating type

Typical calls look like:

```erlang
Projection = glm_vector:dot(
	glm:vec3(double, 1.0, 2.0, 3.0),
	glm:vec3(double, 4.0, 5.0, 6.0)
).
32.0 = glm:double_value(Projection).

Unit = glm_vector:normalize(glm:vec2(double, 3.0, 4.0)).
0.6 = glm:vec2_x(Unit).
0.8 = glm:vec2_y(Unit).
```

These examples show both scalar-returning and vector-returning operations in
the wrapped vector API.
""".

-export([
	cross/2,
	distance/2,
	dot/2,
	face_forward/3,
	length/1,
	normalize/1,
	reflect/2,
	refract/3
]).

-doc("""
Compute the cross product of two 3D vectors.

```erlang
Axis = glm_vector:cross(
	glm:vec3(double, 1.0, 0.0, 0.0),
	glm:vec3(double, 0.0, 1.0, 0.0)
).
0.0 = glm:vec3_x(Axis).
0.0 = glm:vec3_y(Axis).
1.0 = glm:vec3_z(Axis).
```
""").
-spec cross(glm:vec(3, T), glm:vec(3, T)) -> glm:vec(3, T) when T :: float | double.
cross({vec, 3, T, D1}, {vec, 3, T, D2}) when T =:= float; T =:= double ->
	{vec, 3, T, glm_raw:cross(T, 3, D1, D2)}.

-doc("""
Compute the distance between two vectors.

```erlang
Distance = glm_vector:distance(
	glm:vec2(double, 0.0, 0.0),
	glm:vec2(double, 3.0, 4.0)
).
5.0 = glm:double_value(Distance).
```
""").
-spec distance(glm:vec(L, T), glm:vec(L, T)) -> glm:scalar(T) when T :: float | double, L :: glm:length().
distance({vec, L, T, D1}, {vec, L, T, D2}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:distance(T, L, D1, D2)}.

-doc("""
Compute the dot product of two vectors.

```erlang
Projection = glm_vector:dot(
	glm:vec3(double, 1.0, 2.0, 3.0),
	glm:vec3(double, 4.0, 5.0, 6.0)
).
32.0 = glm:double_value(Projection).
```
""").
-spec dot(glm:vec(L, T), glm:vec(L, T)) -> glm:scalar(T) when T :: float | double, L :: glm:length().
dot({vec, L, T, D1}, {vec, L, T, D2}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:dot(T, L, D1, D2)}.

-doc("""
Orient a normal so it faces away from the reference direction.

```erlang
Normal = glm_vector:face_forward(
	glm:vec2(double, 0.0, 1.0),
	glm:vec2(double, 0.0, 1.0),
	glm:vec2(double, 0.0, 1.0)
).
0.0 = glm:vec2_x(Normal).
-1.0 = glm:vec2_y(Normal).
```
""").
-spec face_forward(glm:vec(L, T), glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
face_forward({vec, L, T, N}, {vec, L, T, I}, {vec, L, T, NRef}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:face_forward(T, L, N, I, NRef)}.

-doc("""
Compute the Euclidean length of a vector.

```erlang
Magnitude = glm_vector:length(glm:vec2(double, 3.0, 4.0)).
5.0 = glm:double_value(Magnitude).
```
""").
-spec length(glm:vec(L, T)) -> glm:scalar(T) when T :: float | double, L :: glm:length().
length({vec, L, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:length(T, L, D)}.

-doc("""
Normalize a vector.

```erlang
Unit = glm_vector:normalize(glm:vec2(double, 3.0, 4.0)).
0.6 = glm:vec2_x(Unit).
0.8 = glm:vec2_y(Unit).
```
""").
-spec normalize(glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
normalize({vec, L, T, D}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:normalize(T, L, D)}.

-doc("""
Reflect an incident vector around a normal.

```erlang
Reflected = glm_vector:reflect(
	glm:vec2(double, 1.0, -1.0),
	glm:vec2(double, 0.0, 1.0)
).
1.0 = glm:vec2_x(Reflected).
1.0 = glm:vec2_y(Reflected).
```
""").
-spec reflect(glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
reflect({vec, L, T, I}, {vec, L, T, N}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:reflect(T, L, I, N)}.

-doc("""
Refract an incident vector through a surface with the given ratio of indices.

```erlang
Refracted = glm_vector:refract(
	glm:vec2(double, 0.0, -1.0),
	glm:vec2(double, 0.0, 1.0),
	glm:double(0.5)
).
0.0 = glm:vec2_x(Refracted).
-1.0 = glm:vec2_y(Refracted).
```
""").
-spec refract(glm:vec(L, T), glm:vec(L, T), glm:scalar(T)) -> glm:vec(L, T) when T :: float | double, L :: glm:length().
refract({vec, L, T, I}, {vec, L, T, N}, {scalar, T, Eta}) when T =:= float; T =:= double ->
	{vec, L, T, glm_raw:refract(T, L, I, N, Eta)}.
