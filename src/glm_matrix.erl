%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_matrix).
-moduledoc """
OpenGL Mathematics (GLM) matrix operations for the BEAM.

It groups the first matrix operations on wrapped matrix and vector values. It
includes structural helpers such as `transpose/1`, `row/2`, and `column/2`,
matrix products such as `matrix_comp_mult/2` and `outer_product/2`, and the
square-matrix helpers `determinant/1`, `inverse/1`, `inverse_transpose/1`, and
`affine_inverse/1`.

Supported operands:

- `mat2`, `mat3`, `mat4`, `mat2x3`, `mat2x4`, `mat3x2`, `mat3x4`, `mat4x2`,
  and `mat4x3` with `float` or `double` components where the operation makes
  sense
- `determinant/1`, `inverse/1`, and `inverse_transpose/1` are square-matrix
  only
- `affine_inverse/1` is currently limited to `mat3` and `mat4`

Typical calls look like:

```erlang
Transposed = glm_matrix:transpose(glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0)).
1.0 = glm:mat3x2_element(Transposed, 1, 1).
4.0 = glm:mat3x2_element(Transposed, 2, 2).

Det = glm_matrix:determinant(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)).
-2.0 = glm:double_value(Det).
```

These examples show both the shape-changing and scalar-producing operations in
the current matrix surface.
""".

-export([
	affine_inverse/1,
	column/2,
	determinant/1,
	inverse/1,
	inverse_transpose/1,
	matrix_comp_mult/2,
	outer_product/2,
	row/2,
	transpose/1
]).

-doc("""
Compute the affine inverse of a 3x3 or 4x4 matrix.

```erlang
Inverse = glm_matrix:affine_inverse(
	glm:mat4(double,
		2.0, 0.0, 0.0, 0.0,
		0.0, 3.0, 0.0, 0.0,
		0.0, 0.0, 1.0, 0.0,
		4.0, 5.0, 6.0, 1.0
	)
).
0.5 = glm:mat4_element(Inverse, 1, 1).
```
""").
-spec affine_inverse(glm:mat(3, 3, T)) -> glm:mat(3, 3, T) when T :: float | double;
		      (glm:mat(4, 4, T)) -> glm:mat(4, 4, T) when T :: float | double.
affine_inverse({mat, 3, 3, T, D}) when T =:= float; T =:= double ->
	{mat, 3, 3, T, glm_raw:affine_inverse(T, {3, 3}, D)};
affine_inverse({mat, 4, 4, T, D}) when T =:= float; T =:= double ->
	{mat, 4, 4, T, glm_raw:affine_inverse(T, {4, 4}, D)}.

-doc("""
Extract a matrix column as a vector.

```erlang
Column = glm_matrix:column(
	glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0),
	2
).
2.0 = glm:vec3_x(Column).
4.0 = glm:vec3_y(Column).
6.0 = glm:vec3_z(Column).
```
""").
-spec column(glm:mat(C, R, T), pos_integer()) -> glm:vec(R, T) when T :: float | double, C :: glm:length(), R :: glm:length().
column({mat, C, R, T, D}, I) when (T =:= float orelse T =:= double) andalso I >= 1 andalso I =< C ->
	{vec, R, T, glm_raw:column(T, {C, R}, I, D)}.

-doc("""
Compute the determinant of a square matrix.

```erlang
Det = glm_matrix:determinant(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)).
-2.0 = glm:double_value(Det).
```
""").
-spec determinant(glm:mat(N, N, T)) -> glm:scalar(T) when T :: float | double, N :: glm:length().
determinant({mat, N, N, T, D}) when T =:= float; T =:= double ->
	{scalar, T, glm_raw:determinant(T, {N, N}, D)}.

-doc("""
Compute the inverse of a square matrix.

```erlang
Inverse = glm_matrix:inverse(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)).
-2.0 = glm:mat2_element(Inverse, 1).
```
""").
-spec inverse(glm:mat(N, N, T)) -> glm:mat(N, N, T) when T :: float | double, N :: glm:length().
inverse({mat, N, N, T, D}) when T =:= float; T =:= double ->
	{mat, N, N, T, glm_raw:inverse(T, {N, N}, D)}.

-doc("""
Compute the inverse transpose of a square matrix.

```erlang
InverseTranspose = glm_matrix:inverse_transpose(glm:mat2(double, 1.0, 3.0, 2.0, 4.0)).
-2.0 = glm:mat2_element(InverseTranspose, 1).
```
""").
-spec inverse_transpose(glm:mat(N, N, T)) -> glm:mat(N, N, T) when T :: float | double, N :: glm:length().
inverse_transpose({mat, N, N, T, D}) when T =:= float; T =:= double ->
	{mat, N, N, T, glm_raw:inverse_transpose(T, {N, N}, D)}.

-doc("""
Multiply two matrices component-wise.

```erlang
Product = glm_matrix:matrix_comp_mult(
	glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0),
	glm:mat2x3(double, 6.0, 4.0, 2.0, 5.0, 3.0, 1.0)
).
6.0 = glm:mat2x3_element(Product, 1).
```
""").
-spec matrix_comp_mult(glm:mat(C, R, T), glm:mat(C, R, T)) -> glm:mat(C, R, T) when T :: float | double, C :: glm:length(), R :: glm:length().
matrix_comp_mult({mat, C, R, T, D1}, {mat, C, R, T, D2}) when T =:= float; T =:= double ->
	{mat, C, R, T, glm_raw:matrix_comp_mult(T, {C, R}, D1, D2)}.

-doc("""
Build a matrix from the outer product of two vectors.

```erlang
Matrix = glm_matrix:outer_product(
	glm:vec3(double, 1.0, 2.0, 3.0),
	glm:vec2(double, 4.0, 5.0)
).
4.0 = glm:mat2x3_element(Matrix, 1, 1).
15.0 = glm:mat2x3_element(Matrix, 2, 3).
```
""").
-spec outer_product(glm:vec(R, T), glm:vec(C, T)) -> glm:mat(C, R, T) when T :: float | double, C :: glm:length(), R :: glm:length().
outer_product({vec, R, T, D1}, {vec, C, T, D2}) when T =:= float; T =:= double ->
	{mat, C, R, T, glm_raw:outer_product(T, {C, R}, D1, D2)}.

-doc("""
Extract a matrix row as a vector.

```erlang
Row = glm_matrix:row(
	glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0),
	3
).
5.0 = glm:vec2_x(Row).
6.0 = glm:vec2_y(Row).
```
""").
-spec row(glm:mat(C, R, T), pos_integer()) -> glm:vec(C, T) when T :: float | double, C :: glm:length(), R :: glm:length().
row({mat, C, R, T, D}, I) when (T =:= float orelse T =:= double) andalso I >= 1 andalso I =< R ->
	{vec, C, T, glm_raw:row(T, {C, R}, I, D)}.

-doc("""
Transpose a matrix.

```erlang
Transposed = glm_matrix:transpose(glm:mat2x3(double, 1.0, 3.0, 5.0, 2.0, 4.0, 6.0)).
1.0 = glm:mat3x2_element(Transposed, 1, 1).
4.0 = glm:mat3x2_element(Transposed, 2, 2).
```
""").
-spec transpose(glm:mat(C, R, T)) -> glm:mat(R, C, T) when T :: float | double, C :: glm:length(), R :: glm:length().
transpose({mat, C, R, T, D}) when T =:= float; T =:= double ->
	{mat, R, C, T, glm_raw:transpose(T, {C, R}, D)}.
