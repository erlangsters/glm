%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_quat).
-moduledoc """
OpenGL Mathematics (GLM) quaternion operations for the BEAM.

It groups quaternion math, conversions, and vector-rotation helpers on wrapped
quaternion values. It includes construction and decomposition helpers such as
`angle_axis/2`, `angle/1`, and `axis/1`, normalization and interpolation
helpers such as `normalize/1`, `mix/3`, and `slerp/3`, matrix casts, Euler
angle extraction, and `rotate/2` for vectors.

Supported operands:

- `quat(float)` and `quat(double)` values
- `angle_axis/2` uses a scalar angle and a `vec3` axis of the same type
- `rotate/2` supports rotating `vec3` and `vec4` values by a quaternion
- `quat_cast/1` currently accepts `mat3` and `mat4`

Typical calls look like:

```erlang
QuarterTurn = glm_quat:angle_axis(
	glm:double(math:pi() / 2.0),
	glm:vec3(double, 0.0, 0.0, 1.0)
).

Rotated = glm_quat:rotate(QuarterTurn, glm:vec3(double, 1.0, 0.0, 0.0)).
0.0 = glm:vec3_x(Rotated).
1.0 = glm:vec3_y(Rotated).

Matrix = glm_quat:mat3_cast(QuarterTurn).
0.0 = glm:mat3_element(Matrix, 1, 1).
-1.0 = glm:mat3_element(Matrix, 2, 1).
```

These examples show quaternion construction, vector rotation, and matrix
conversion in the wrapped API.
""".

-export([
	angle/1,
	angle_axis/2,
	axis/1,
	conjugate/1,
	dot/2,
	euler_angles/1,
	inverse/1,
	length/1,
	mat3_cast/1,
	mat4_cast/1,
	mix/3,
	normalize/1,
	pitch/1,
	quat_cast/1,
	roll/1,
	rotate/2,
	slerp/3,
	yaw/1
]).

-doc("""
Return the rotation angle represented by a quaternion.

```erlang
QuarterTurn = glm_quat:angle_axis(
	glm:double(math:pi() / 2.0),
	glm:vec3(double, 0.0, 0.0, 1.0)
).
Angle = glm_quat:angle(QuarterTurn).
math:pi() / 2.0 = glm:double_value(Angle).
```
""").
-spec angle(glm:quat(T)) -> glm:scalar(T) when T :: float | double.
angle({quat, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:angle(T, D)}.

-doc("""
Build a quaternion from an angle and rotation axis.

```erlang
QuarterTurn = glm_quat:angle_axis(
	glm:double(math:pi() / 2.0),
	glm:vec3(double, 0.0, 0.0, 1.0)
).
_ = glm:quat_values(QuarterTurn).
```
""").
-spec angle_axis(glm:scalar(T), glm:vec(3, T)) -> glm:quat(T) when T :: float | double.
angle_axis({scalar, T, Angle}, {vec, 3, T, Axis}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:angle_axis(T, Angle, Axis)}.

-doc("""
Return the rotation axis represented by a quaternion.

```erlang
Axis = glm_quat:axis(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 0.0, 0.0, 1.0)
	)
).
0.0 = glm:vec3_x(Axis).
0.0 = glm:vec3_y(Axis).
1.0 = glm:vec3_z(Axis).
```
""").
-spec axis(glm:quat(T)) -> glm:vec(3, T) when T :: float | double.
axis({quat, T, D}) when T =:= float; T =:= double ->
	{vec, 3, T, glm_raw:axis(T, D)}.

-doc("""
Return the quaternion conjugate.

```erlang
Conjugate = glm_quat:conjugate(glm:quat(double, 1.0, 2.0, 3.0, 4.0)).
{1.0, -2.0, -3.0, -4.0} = glm:quat_values(Conjugate).
```
""").
-spec conjugate(glm:quat(T)) -> glm:quat(T) when T :: float | double.
conjugate({quat, T, D}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:conjugate(T, D)}.

-doc("""
Compute the quaternion dot product.

```erlang
Dot = glm_quat:dot(
	glm:quat(double, 1.0, 2.0, 3.0, 4.0),
	glm:quat(double, 5.0, 6.0, 7.0, 8.0)
).
70.0 = glm:double_value(Dot).
```
""").
-spec dot(glm:quat(T), glm:quat(T)) -> glm:scalar(T) when T :: float | double.
dot({quat, T, D1}, {quat, T, D2}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:dot(T, D1, D2)}.

-doc("""
Convert a quaternion to Euler angles.

```erlang
Angles = glm_quat:euler_angles(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 0.0, 0.0, 1.0)
	)
).
math:pi() / 2.0 = glm:vec3_z(Angles).
```
""").
-spec euler_angles(glm:quat(T)) -> glm:vec(3, T) when T :: float | double.
euler_angles({quat, T, D}) when T =:= float; T =:= double ->
	{vec, 3, T, glm_raw:euler_angles(T, D)}.

-doc("""
Return the quaternion inverse.

```erlang
Inverse = glm_quat:inverse(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 0.0, 0.0, 1.0)
	)
).
_ = glm:quat_values(Inverse).
```
""").
-spec inverse(glm:quat(T)) -> glm:quat(T) when T :: float | double.
inverse({quat, T, D}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:inverse(T, D)}.

-doc("""
Return the quaternion length.

```erlang
Length = glm_quat:length(glm:quat(double, 1.0, 2.0, 2.0, 1.0)).
math:sqrt(10.0) = glm:double_value(Length).
```
""").
-spec length(glm:quat(T)) -> glm:scalar(T) when T :: float | double.
length({quat, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:length(T, D)}.

-doc("""
Convert a quaternion to a 3x3 rotation matrix.

```erlang
Matrix = glm_quat:mat3_cast(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 0.0, 0.0, 1.0)
	)
).
0.0 = glm:mat3_element(Matrix, 1, 1).
-1.0 = glm:mat3_element(Matrix, 2, 1).
```
""").
-spec mat3_cast(glm:quat(T)) -> glm:mat3(T) when T :: float | double.
mat3_cast({quat, T, D}) when T =:= float; T =:= double ->
	{mat, 3, 3, T, glm_raw:mat3_cast(T, D)}.

-doc("""
Convert a quaternion to a 4x4 rotation matrix.

```erlang
Matrix = glm_quat:mat4_cast(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 0.0, 0.0, 1.0)
	)
).
0.0 = glm:mat4_element(Matrix, 1, 1).
-1.0 = glm:mat4_element(Matrix, 2, 1).
```
""").
-spec mat4_cast(glm:quat(T)) -> glm:mat4(T) when T :: float | double.
mat4_cast({quat, T, D}) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:mat4_cast(T, D)}.

-doc("""
Interpolate between two quaternions.

```erlang
Mixed = glm_quat:mix(
	glm:quat(double),
	glm_quat:angle_axis(glm:double(math:pi()), glm:vec3(double, 0.0, 0.0, 1.0)),
	glm:double(0.5)
).
math:pi() / 2.0 = glm:double_value(glm_quat:angle(Mixed)).
```
""").
-spec mix(glm:quat(T), glm:quat(T), glm:scalar(T)) -> glm:quat(T) when T :: float | double.
mix({quat, T, D1}, {quat, T, D2}, {scalar, T, A}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:mix(T, D1, D2, A)}.

-doc("""
Normalize a quaternion.

```erlang
Normalized = glm_quat:normalize(glm:quat(double, 1.0, 2.0, 2.0, 1.0)).
_ = glm:quat_values(Normalized).
```
""").
-spec normalize(glm:quat(T)) -> glm:quat(T) when T :: float | double.
normalize({quat, T, D}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:normalize(T, D)}.

-doc("""
Return the quaternion pitch angle.

```erlang
Pitch = glm_quat:pitch(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 1.0, 0.0, 0.0)
	)
).
math:pi() / 2.0 = glm:double_value(Pitch).
```
""").
-spec pitch(glm:quat(T)) -> glm:scalar(T) when T :: float | double.
pitch({quat, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:pitch(T, D)}.

-doc("""
Convert a rotation matrix to a quaternion.

```erlang
Quat = glm_quat:quat_cast(glm:mat3(
	double,
	0.0, 1.0, 0.0,
	-1.0, 0.0, 0.0,
	0.0, 0.0, 1.0
)).
Rotated = glm_quat:rotate(Quat, glm:vec3(double, 1.0, 0.0, 0.0)).
0.0 = glm:vec3_x(Rotated).
1.0 = glm:vec3_y(Rotated).
```
""").
-spec quat_cast(glm:mat3(T)) -> glm:quat(T) when T :: float | double;
		  (glm:mat4(T)) -> glm:quat(T) when T :: float | double.
quat_cast({mat, 3, 3, T, D}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:quat_cast(T, {3, 3}, D)};
quat_cast({mat, 4, 4, T, D}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:quat_cast(T, {4, 4}, D)}.

-doc("""
Return the quaternion roll angle.

```erlang
Roll = glm_quat:roll(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 0.0, 0.0, 1.0)
	)
).
math:pi() / 2.0 = glm:double_value(Roll).
```
""").
-spec roll(glm:quat(T)) -> glm:scalar(T) when T :: float | double.
roll({quat, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:roll(T, D)}.

-doc("""
Rotate a vector by a quaternion.

```erlang
QuarterTurn = glm_quat:angle_axis(
	glm:double(math:pi() / 2.0),
	glm:vec3(double, 0.0, 0.0, 1.0)
).
Rotated = glm_quat:rotate(QuarterTurn, glm:vec3(double, 1.0, 0.0, 0.0)).
0.0 = glm:vec3_x(Rotated).
1.0 = glm:vec3_y(Rotated).
```
""").
-spec rotate(glm:quat(T), glm:vec(3, T)) -> glm:vec(3, T) when T :: float | double;
		  (glm:quat(T), glm:vec(4, T)) -> glm:vec(4, T) when T :: float | double.
rotate({quat, T, Q}, {vec, 3, T, V}) when T =:= float; T =:= double ->
	{vec, 3, T, glm_raw:rotate(T, Q, V)};
rotate({quat, T, Q}, {vec, 4, T, V}) when T =:= float; T =:= double ->
	{vec, 4, T, glm_raw:rotate(T, Q, V)}.

-doc("""
Perform spherical linear interpolation between two quaternions.

```erlang
Interpolated = glm_quat:slerp(
	glm:quat(double),
	glm_quat:angle_axis(glm:double(math:pi()), glm:vec3(double, 0.0, 0.0, 1.0)),
	glm:double(0.5)
).
Rotated = glm_quat:rotate(Interpolated, glm:vec3(double, 1.0, 0.0, 0.0)).
0.0 = glm:vec3_x(Rotated).
1.0 = glm:vec3_y(Rotated).
```
""").
-spec slerp(glm:quat(T), glm:quat(T), glm:scalar(T)) -> glm:quat(T) when T :: float | double.
slerp({quat, T, D1}, {quat, T, D2}, {scalar, T, A}) when T =:= float; T =:= double ->
	{quat, T, glm_raw:slerp(T, D1, D2, A)}.

-doc("""
Return the quaternion yaw angle.

```erlang
Yaw = glm_quat:yaw(
	glm_quat:angle_axis(
		glm:double(math:pi() / 2.0),
		glm:vec3(double, 0.0, 1.0, 0.0)
	)
).
math:pi() / 2.0 = glm:double_value(Yaw).
```
""").
-spec yaw(glm:quat(T)) -> glm:scalar(T) when T :: float | double.
yaw({quat, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:yaw(T, D)}.
