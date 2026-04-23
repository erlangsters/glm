%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_relational).
-moduledoc """
OpenGL Mathematics (GLM) relational functions for the BEAM.

It focuses on component-wise vector comparisons and reductions over bool
vectors. It includes `equal/2`, `not_equal/2`, the ordering predicates,
`all/1`, `any/1`, and `not/1`.

Supported operands:

- `all/1`, `any/1`, and `not/1` operate on `vec2(bool)` to `vec4(bool)`
- `equal/2` and `not_equal/2` operate on vectors of the same length and type
- ordering predicates operate on non-bool vectors of the same length and type

Deferred for now:

- scalar epsilon and ULP comparisons
- matrix relational helpers
- quaternion relational helpers

Typical calls look like:

```erlang
Mask = glm_relational:greater_than(
	glm:vec3(double, 2.0, 1.0, 3.0),
	glm:vec3(double, 1.0, 1.0, 4.0)
).
{true, false, false} = glm:vec3_values(Mask).

true = glm:bool_value(glm_relational:any(Mask)).
false = glm:bool_value(glm_relational:all(Mask)).
```

These examples show the usual flow from component-wise comparison to bool-vector
reduction.
""".

-export([
	all/1,
	any/1,
	equal/2,
	greater_than/2,
	greater_than_equal/2,
	less_than/2,
	less_than_equal/2,
	'not'/1,
	not_equal/2
]).

-doc("""
Return `true` when all components of a bool vector are `true`.

```erlang
Flag = glm_relational:all(glm:vec3(bool, true, true, true)).
true = glm:bool_value(Flag).
```
""").
-spec all(glm:vec(L, bool)) -> glm:scalar(bool) when L :: glm:length().
all({vec, L, bool, D}) ->
	{scalar, bool, glm_raw:all(L, D)}.

-doc("""
Return `true` when any component of a bool vector is `true`.

```erlang
Flag = glm_relational:any(glm:vec4(bool, false, true, false, false)).
true = glm:bool_value(Flag).
```
""").
-spec any(glm:vec(L, bool)) -> glm:scalar(bool) when L :: glm:length().
any({vec, L, bool, D}) ->
	{scalar, bool, glm_raw:any(L, D)}.

-doc("""
Perform a component-wise equality comparison.

```erlang
Mask = glm_relational:equal(
	glm:vec3(double, 1.0, 2.0, 3.0),
	glm:vec3(double, 1.0, 0.0, 3.0)
).
{true, false, true} = glm:vec3_values(Mask).
```
""").
-spec equal(glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, bool) when L :: glm:length(), T :: glm:type().
equal({vec, L, T, D1}, {vec, L, T, D2}) ->
	{vec, L, bool, glm_raw:equal(T, L, D1, D2)}.

-doc("""
Perform a component-wise greater-than comparison.

```erlang
Mask = glm_relational:greater_than(
	glm:vec3({int, 32}, 3, 1, 2),
	glm:vec3({int, 32}, 2, 1, 5)
).
{true, false, false} = glm:vec3_values(Mask).
```
""").
-spec greater_than(glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, bool)
	when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} | float | double.
greater_than({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool ->
	{vec, L, bool, glm_raw:greater_than(T, L, D1, D2)}.

-doc("""
Perform a component-wise greater-than-or-equal comparison.

```erlang
Mask = glm_relational:greater_than_equal(
	glm:vec2(double, 2.0, 1.0),
	glm:vec2(double, 1.0, 1.0)
).
{true, true} = glm:vec2_values(Mask).
```
""").
-spec greater_than_equal(glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, bool)
	when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} | float | double.
greater_than_equal({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool ->
	{vec, L, bool, glm_raw:greater_than_equal(T, L, D1, D2)}.

-doc("""
Perform a component-wise less-than comparison.

```erlang
Mask = glm_relational:less_than(
	glm:vec3({uint, 32}, 1, 5, 2),
	glm:vec3({uint, 32}, 2, 5, 1)
).
{true, false, false} = glm:vec3_values(Mask).
```
""").
-spec less_than(glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, bool)
	when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} | float | double.
less_than({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool ->
	{vec, L, bool, glm_raw:less_than(T, L, D1, D2)}.

-doc("""
Perform a component-wise less-than-or-equal comparison.

```erlang
Mask = glm_relational:less_than_equal(
	glm:vec4(double, 1.0, 2.0, 3.0, 4.0),
	glm:vec4(double, 1.0, 1.0, 3.0, 5.0)
).
{true, false, true, true} = glm:vec4_values(Mask).
```
""").
-spec less_than_equal(glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, bool)
	when L :: glm:length(), T :: {int, 8} | {int, 16} | {int, 32} | {int, 64} | {uint, 8} | {uint, 16} | {uint, 32} | {uint, 64} | float | double.
less_than_equal({vec, L, T, D1}, {vec, L, T, D2}) when T =/= bool ->
	{vec, L, bool, glm_raw:less_than_equal(T, L, D1, D2)}.

-doc("""
Perform a component-wise logical negation on a bool vector.

```erlang
Mask = glm_relational:'not'(glm:vec3(bool, true, false, true)).
{false, true, false} = glm:vec3_values(Mask).
```
""").
-spec 'not'(glm:vec(L, bool)) -> glm:vec(L, bool) when L :: glm:length().
'not'({vec, L, bool, D}) ->
	{vec, L, bool, glm_raw:'not'(L, D)}.

-doc("""
Perform a component-wise inequality comparison.

```erlang
Mask = glm_relational:not_equal(
	glm:vec3({int, 16}, 1, 2, 3),
	glm:vec3({int, 16}, 0, 2, 4)
).
{true, false, true} = glm:vec3_values(Mask).
```
""").
-spec not_equal(glm:vec(L, T), glm:vec(L, T)) -> glm:vec(L, bool) when L :: glm:length(), T :: glm:type().
not_equal({vec, L, T, D1}, {vec, L, T, D2}) ->
	{vec, L, bool, glm_raw:not_equal(T, L, D1, D2)}.
