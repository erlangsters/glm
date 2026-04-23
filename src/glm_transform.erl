%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_transform).
-moduledoc """
OpenGL Mathematics (GLM) transform functions for the BEAM.

It focuses on the core projection and view matrix builders together with the
matrix-space transform helpers that are the most broadly useful in graphics
code. It includes `frustum/6`, `ortho/6`, `perspective/4`,
`infinite_perspective/3`, `perspective_fov/5`, `look_at/3`, `project/4`,
`un_project/4`, `translate/2`, `scale/2`, and `rotate/3`.

Supported operands:

- projection helpers return `mat4(float)` or `mat4(double)`
- `look_at/3` uses three `vec3` values of the same floating type
- `project/4` and `un_project/4` use a `vec4` viewport of the same floating type
- `translate/2`, `scale/2`, and `rotate/3` transform an existing `mat4`

Typical calls look like:

```erlang
Projection = glm_transform:perspective(
	glm:double(math:pi() / 2.0),
	glm:double(16.0 / 9.0),
	glm:double(0.1),
	glm:double(100.0)
).

View = glm_transform:look_at(
	glm:vec3(double, 0.0, 0.0, 5.0),
	glm:vec3(double, 0.0, 0.0, 0.0),
	glm:vec3(double, 0.0, 1.0, 0.0)
).

Model = glm_transform:translate(
	glm:mat4(double),
	glm:vec3(double, 1.0, 2.0, 3.0)
).

1.0 = glm:mat4_element(Model, 4, 1).
2.0 = glm:mat4_element(Model, 4, 2).
3.0 = glm:mat4_element(Model, 4, 3).
```

These examples show the wrapped matrix shapes used for projection, view, and
model-space transforms.
""".

-export([
	frustum/6,
	infinite_perspective/3,
	look_at/3,
	ortho/6,
	perspective/4,
	perspective_fov/5,
	project/4,
	rotate/3,
	scale/2,
	translate/2,
	un_project/4
]).

-doc("""
Build a perspective frustum projection matrix.

```erlang
Projection = glm_transform:frustum(
	glm:double(-1.0),
	glm:double(1.0),
	glm:double(-1.0),
	glm:double(1.0),
	glm:double(1.0),
	glm:double(10.0)
).
1.0 = glm:mat4_element(Projection, 1, 1).
```
""").
-spec frustum(
	glm:scalar(T), glm:scalar(T), glm:scalar(T),
	glm:scalar(T), glm:scalar(T), glm:scalar(T)
) -> glm:mat4(T) when T :: float | double.
frustum(
	{scalar, T, Left},
	{scalar, T, Right},
	{scalar, T, Bottom},
	{scalar, T, Top},
	{scalar, T, Near},
	{scalar, T, Far}
) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:frustum(T, Left, Right, Bottom, Top, Near, Far)}.

-doc("""
Build an infinite-far-plane perspective projection matrix.

```erlang
Projection = glm_transform:infinite_perspective(
	glm:double(math:pi() / 2.0),
	glm:double(1.0),
	glm:double(1.0)
).
-1.0 = glm:mat4_element(Projection, 3, 3).
```
""").
-spec infinite_perspective(glm:scalar(T), glm:scalar(T), glm:scalar(T)) -> glm:mat4(T) when T :: float | double.
infinite_perspective({scalar, T, Fovy}, {scalar, T, Aspect}, {scalar, T, Near}) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:infinite_perspective(T, Fovy, Aspect, Near)}.

-doc("""
Build a right-handed view matrix from eye, center, and up vectors.

```erlang
View = glm_transform:look_at(
	glm:vec3(double, 0.0, 0.0, 1.0),
	glm:vec3(double, 0.0, 0.0, 0.0),
	glm:vec3(double, 0.0, 1.0, 0.0)
).
-1.0 = glm:mat4_element(View, 4, 3).
```
""").
-spec look_at(glm:vec(3, T), glm:vec(3, T), glm:vec(3, T)) -> glm:mat4(T) when T :: float | double.
look_at({vec, 3, T, Eye}, {vec, 3, T, Center}, {vec, 3, T, Up}) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:look_at(T, Eye, Center, Up)}.

-doc("""
Build an orthographic projection matrix.

```erlang
Projection = glm_transform:ortho(
	glm:double(-1.0),
	glm:double(1.0),
	glm:double(-2.0),
	glm:double(2.0),
	glm:double(1.0),
	glm:double(11.0)
).
0.5 = glm:mat4_element(Projection, 2, 2).
```
""").
-spec ortho(
	glm:scalar(T), glm:scalar(T), glm:scalar(T),
	glm:scalar(T), glm:scalar(T), glm:scalar(T)
) -> glm:mat4(T) when T :: float | double.
ortho(
	{scalar, T, Left},
	{scalar, T, Right},
	{scalar, T, Bottom},
	{scalar, T, Top},
	{scalar, T, Near},
	{scalar, T, Far}
) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:ortho(T, Left, Right, Bottom, Top, Near, Far)}.

-doc("""
Build a symmetric perspective projection matrix.

```erlang
Projection = glm_transform:perspective(
	glm:double(math:pi() / 2.0),
	glm:double(1.0),
	glm:double(1.0),
	glm:double(10.0)
).
1.0 = glm:mat4_element(Projection, 1, 1).
```
""").
-spec perspective(glm:scalar(T), glm:scalar(T), glm:scalar(T), glm:scalar(T)) -> glm:mat4(T) when T :: float | double.
perspective(
	{scalar, T, Fovy},
	{scalar, T, Aspect},
	{scalar, T, Near},
	{scalar, T, Far}
) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:perspective(T, Fovy, Aspect, Near, Far)}.

-doc("""
Build a perspective projection matrix from explicit field of view and viewport size.

```erlang
Projection = glm_transform:perspective_fov(
	glm:double(math:pi() / 2.0),
	glm:double(2.0),
	glm:double(2.0),
	glm:double(1.0),
	glm:double(10.0)
).
1.0 = glm:mat4_element(Projection, 1, 1).
```
""").
-spec perspective_fov(
	glm:scalar(T), glm:scalar(T), glm:scalar(T), glm:scalar(T), glm:scalar(T)
) -> glm:mat4(T) when T :: float | double.
perspective_fov(
	{scalar, T, Fov},
	{scalar, T, Width},
	{scalar, T, Height},
	{scalar, T, Near},
	{scalar, T, Far}
) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:perspective_fov(T, Fov, Width, Height, Near, Far)}.

-doc("""
Map object coordinates into window coordinates using model, projection, and viewport.

```erlang
Window = glm_transform:project(
	glm:vec3(double, 0.25, -0.5, 0.0),
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	glm:vec4(double, 10.0, 20.0, 800.0, 600.0)
).
510.0 = glm:vec3_x(Window).
170.0 = glm:vec3_y(Window).
```
""").
-spec project(glm:vec(3, T), glm:mat4(T), glm:mat4(T), glm:vec(4, T)) -> glm:vec(3, T) when T :: float | double.
project(
	{vec, 3, T, Object},
	{mat, 4, 4, T, Model},
	{mat, 4, 4, T, Projection},
	{vec, 4, T, Viewport}
) when T =:= float; T =:= double ->
	{vec, 3, T, glm_raw:project(T, Object, Model, Projection, Viewport)}.

-doc("""
Rotate a matrix around an axis by the given angle.

```erlang
Matrix = glm_transform:rotate(
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	glm:double(math:pi() / 2.0),
	glm:vec3(double, 0.0, 0.0, 1.0)
).
1.0 = glm:mat4_element(Matrix, 1, 2).
```
""").
-spec rotate(glm:mat4(T), glm:scalar(T), glm:vec(3, T)) -> glm:mat4(T) when T :: float | double.
rotate({mat, 4, 4, T, Matrix}, {scalar, T, Angle}, {vec, 3, T, Axis}) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:rotate(T, Matrix, Angle, Axis)}.

-doc("""
Scale a matrix by the provided vector.

```erlang
Matrix = glm_transform:scale(
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	glm:vec3(double, 2.0, 3.0, 4.0)
).
2.0 = glm:mat4_element(Matrix, 1, 1).
3.0 = glm:mat4_element(Matrix, 2, 2).
4.0 = glm:mat4_element(Matrix, 3, 3).
```
""").
-spec scale(glm:mat4(T), glm:vec(3, T)) -> glm:mat4(T) when T :: float | double.
scale({mat, 4, 4, T, Matrix}, {vec, 3, T, Vector}) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:scale(T, Matrix, Vector)}.

-doc("""
Translate a matrix by the provided vector.

```erlang
Matrix = glm_transform:translate(
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	glm:vec3(double, 1.0, 2.0, 3.0)
).
1.0 = glm:mat4_element(Matrix, 4, 1).
2.0 = glm:mat4_element(Matrix, 4, 2).
3.0 = glm:mat4_element(Matrix, 4, 3).
```
""").
-spec translate(glm:mat4(T), glm:vec(3, T)) -> glm:mat4(T) when T :: float | double.
translate({mat, 4, 4, T, Matrix}, {vec, 3, T, Vector}) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:translate(T, Matrix, Vector)}.

-doc("""
Map window coordinates back into object coordinates using model, projection, and viewport.

```erlang
Viewport = glm:vec4(double, 10.0, 20.0, 800.0, 600.0),
Window = glm_transform:project(
	glm:vec3(double, 0.25, -0.5, 0.0),
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	Viewport
),
Object = glm_transform:un_project(
	Window,
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	glm:mat4(double,
		1.0, 0.0, 0.0, 0.0,
		0.0, 1.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		0.0, 0.0, 0.0, 1.0
	),
	Viewport
).
0.25 = glm:vec3_x(Object).
-0.5 = glm:vec3_y(Object).
```
""").
-spec un_project(glm:vec(3, T), glm:mat4(T), glm:mat4(T), glm:vec(4, T)) -> glm:vec(3, T) when T :: float | double.
un_project(
	{vec, 3, T, Window},
	{mat, 4, 4, T, Model},
	{mat, 4, 4, T, Projection},
	{vec, 4, T, Viewport}
) when T =:= float; T =:= double ->
	{vec, 3, T, glm_raw:un_project(T, Window, Model, Projection, Viewport)}.
