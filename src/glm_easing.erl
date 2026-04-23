%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_easing).
-moduledoc """
OpenGL Mathematics (GLM) easing functions for the BEAM.

It exposes GLM's easing curves on wrapped floating-point scalars. It includes:

- `back_ease_in/1`, `back_ease_in/2`, `back_ease_in_out/1`, `back_ease_in_out/2`, `back_ease_out/1`, and `back_ease_out/2`
- `bounce_ease_in/1`, `bounce_ease_in_out/1`, and `bounce_ease_out/1`
- `circular_ease_in/1`, `circular_ease_in_out/1`, and `circular_ease_out/1`
- `cubic_ease_in/1`, `cubic_ease_in_out/1`, and `cubic_ease_out/1`
- `elastic_ease_in/1`, `elastic_ease_in_out/1`, and `elastic_ease_out/1`
- `exponential_ease_in/1`, `exponential_ease_in_out/1`, and `exponential_ease_out/1`
- `linear_interpolation/1`
- `quadratic_ease_in/1`, `quadratic_ease_in_out/1`, and `quadratic_ease_out/1`
- `quartic_ease_in/1`, `quartic_ease_in_out/1`, and `quartic_ease_out/1`
- `quintic_ease_in/1`, `quintic_ease_in_out/1`, and `quintic_ease_out/1`
- `sine_ease_in/1`, `sine_ease_in_out/1`, and `sine_ease_out/1`

All public functions accept a wrapped `float` or `double` scalar whose BEAM
value lies in the unit interval `[0.0, 1.0]`, and the `back_*` variants also
accept a wrapped overshoot parameter.

Typical calls look like:

```erlang
EaseIn = glm_easing:cubic_ease_in(glm:double(0.25)).
0.015625 = glm:double_value(EaseIn).

BackOut = glm_easing:back_ease_out(glm:double(0.25), glm:double(1.0)).
_ = glm:double_value(BackOut).
```

These examples show the common wrapped workflow: build the interpolation
factor, evaluate the easing curve, and unwrap the result when needed.
""".

-export([
    back_ease_in/1, back_ease_in/2,
    back_ease_in_out/1, back_ease_in_out/2,
    back_ease_out/1, back_ease_out/2
]).
-export([
    bounce_ease_in/1,
    bounce_ease_in_out/1,
    bounce_ease_out/1
]).
-export([
    circular_ease_in/1,
    circular_ease_in_out/1,
    circular_ease_out/1
]).
-export([
    cubic_ease_in/1,
    cubic_ease_in_out/1,
    cubic_ease_out/1
]).
-export([
    elastic_ease_in/1,
    elastic_ease_in_out/1,
    elastic_ease_out/1
]).
-export([
    exponential_ease_in/1,
    exponential_ease_in_out/1,
    exponential_ease_out/1
]).
-export([
    linear_interpolation/1
]).
-export([
    quadratic_ease_in/1,
    quadratic_ease_in_out/1,
    quadratic_ease_out/1
]).
-export([
    quartic_ease_in/1,
    quartic_ease_in_out/1,
    quartic_ease_out/1
]).
-export([
    quintic_ease_in/1,
    quintic_ease_in_out/1,
    quintic_ease_out/1
]).
-export([
    sine_ease_in/1,
    sine_ease_in_out/1,
    sine_ease_out/1
]).

-doc("""
Evaluates the back ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:back_ease_in(glm:double(0.25)).
```
""").
-spec
    back_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:back_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the back ease-in curve using an explicit overshoot amount.

```erlang
Ease = glm_easing:back_ease_in(glm:double(0.25), glm:double(1.0)).
```
""").
-spec
    back_ease_in(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
    binary_ease(fun glm_raw:back_ease_in/3, {scalar, T, D1}, {scalar, T, D2}).

-doc("""
Evaluates the back ease-in-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:back_ease_in_out(glm:double(0.5)).
```
""").
-spec
    back_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:back_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the back ease-in-out curve using an explicit overshoot amount.

```erlang
Ease = glm_easing:back_ease_in_out(glm:double(0.5), glm:double(1.0)).
```
""").
-spec
    back_ease_in_out(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in_out({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
    binary_ease(fun glm_raw:back_ease_in_out/3, {scalar, T, D1}, {scalar, T, D2}).

-doc("""
Evaluates the back ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:back_ease_out(glm:double(0.75)).
```
""").
-spec
    back_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:back_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the back ease-out curve using an explicit overshoot amount.

```erlang
Ease = glm_easing:back_ease_out(glm:double(0.75), glm:double(1.0)).
```
""").
-spec
    back_ease_out(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_out({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
    binary_ease(fun glm_raw:back_ease_out/3, {scalar, T, D1}, {scalar, T, D2}).

-doc("""
Evaluates the bounce ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:bounce_ease_in(glm:double(0.25)).
```
""").
-spec
    bounce_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
bounce_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:bounce_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the bounce ease-in-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:bounce_ease_in_out(glm:double(0.5)).
```
""").
-spec
    bounce_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
bounce_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:bounce_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the bounce ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:bounce_ease_out(glm:double(0.75)).
```
""").
-spec
    bounce_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
bounce_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:bounce_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the circular ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:circular_ease_in(glm:double(0.25)).
```
""").
-spec
    circular_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
circular_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:circular_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the circular ease-in-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:circular_ease_in_out(glm:double(0.5)).
```
""").
-spec
    circular_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
circular_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:circular_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the circular ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:circular_ease_out(glm:double(0.75)).
```
""").
-spec
    circular_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
circular_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:circular_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the cubic ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:cubic_ease_in(glm:double(0.25)).
```
""").
-spec
    cubic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
cubic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:cubic_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the cubic ease-in-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:cubic_ease_in_out(glm:double(0.5)).
```
""").
-spec
    cubic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
cubic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:cubic_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the cubic ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:cubic_ease_out(glm:double(0.75)).
```
""").
-spec
    cubic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
cubic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:cubic_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the elastic ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:elastic_ease_in(glm:double(0.25)).
```
""").
-spec
    elastic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
elastic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:elastic_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the elastic ease-in-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:elastic_ease_in_out(glm:double(0.5)).
```
""").
-spec
    elastic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
elastic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:elastic_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the elastic ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:elastic_ease_out(glm:double(0.75)).
```
""").
-spec
    elastic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
elastic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:elastic_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the exponential ease-in curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:exponential_ease_in(glm:double(0.25)).
```
""").
-spec
    exponential_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
exponential_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:exponential_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the exponential ease-in-out curve for a wrapped interpolation factor
in the unit interval.

```erlang
Ease = glm_easing:exponential_ease_in_out(glm:double(0.5)).
```
""").
-spec
    exponential_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
exponential_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:exponential_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the exponential ease-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:exponential_ease_out(glm:double(0.75)).
```
""").
-spec
    exponential_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
exponential_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:exponential_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the identity easing curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:linear_interpolation(glm:double(0.25)).
0.25 = glm:double_value(Ease).
```
""").
-spec
    linear_interpolation(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
linear_interpolation({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:linear_interpolation/2, {scalar, T, D}).

-doc("""
Evaluates the quadratic ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:quadratic_ease_in(glm:double(0.25)).
```
""").
-spec
    quadratic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quadratic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quadratic_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the quadratic ease-in-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:quadratic_ease_in_out(glm:double(0.5)).
```
""").
-spec
    quadratic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quadratic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quadratic_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the quadratic ease-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:quadratic_ease_out(glm:double(0.75)).
```
""").
-spec
    quadratic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quadratic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quadratic_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the quartic ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:quartic_ease_in(glm:double(0.25)).
```
""").
-spec
    quartic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quartic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quartic_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the quartic ease-in-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:quartic_ease_in_out(glm:double(0.5)).
```
""").
-spec
    quartic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quartic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quartic_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the quartic ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:quartic_ease_out(glm:double(0.75)).
```
""").
-spec
    quartic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quartic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quartic_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the quintic ease-in curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:quintic_ease_in(glm:double(0.25)).
```
""").
-spec
    quintic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quintic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quintic_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the quintic ease-in-out curve for a wrapped interpolation factor in
the unit interval.

```erlang
Ease = glm_easing:quintic_ease_in_out(glm:double(0.5)).
```
""").
-spec
    quintic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quintic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quintic_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the quintic ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:quintic_ease_out(glm:double(0.75)).
```
""").
-spec
    quintic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quintic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:quintic_ease_out/2, {scalar, T, D}).

-doc("""
Evaluates the sine ease-in curve for a wrapped interpolation factor in the unit
interval.

```erlang
Ease = glm_easing:sine_ease_in(glm:double(0.25)).
```
""").
-spec
    sine_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
sine_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:sine_ease_in/2, {scalar, T, D}).

-doc("""
Evaluates the sine ease-in-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:sine_ease_in_out(glm:double(0.5)).
```
""").
-spec
    sine_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
sine_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:sine_ease_in_out/2, {scalar, T, D}).

-doc("""
Evaluates the sine ease-out curve for a wrapped interpolation factor in the
unit interval.

```erlang
Ease = glm_easing:sine_ease_out(glm:double(0.75)).
```
""").
-spec
    sine_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
sine_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    unary_ease(fun glm_raw:sine_ease_out/2, {scalar, T, D}).

unary_ease(RawFun, {scalar, T, D} = A) when T =:= float; T =:= double ->
    ensure_unit_interval(A),
    {scalar, T, RawFun(T, D)}.

binary_ease(RawFun, {scalar, T, D1} = A, {scalar, T, D2}) when T =:= float; T =:= double ->
    ensure_unit_interval(A),
    {scalar, T, RawFun(T, D1, D2)}.

ensure_unit_interval(Scalar) ->
    Value = glm:scalar_value(Scalar),
    case Value >= 0.0 andalso Value =< 1.0 of
        true ->
            ok;
        false ->
            erlang:error({invalid_value, {expected_unit_interval, Value}})
    end.
