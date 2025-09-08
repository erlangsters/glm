%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_easing).
-moduledoc """
OpenGL Mathematics (GLM) for the BEAM.
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
To be written.
""").
-spec
    back_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:back_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    back_ease_in(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:back_ease_in(T, D1, D2)}.

-doc("""
To be written.
""").
-spec
    back_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:back_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    back_ease_in_out(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_in_out({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:back_ease_in_out(T, D1, D2)}.

-doc("""
To be written.
""").
-spec
    back_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:back_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    back_ease_out(glm:scalar(T), glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
back_ease_out({scalar, T, D1}, {scalar, T, D2}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:back_ease_out(T, D1, D2)}.

-doc("""
To be written.
""").
-spec
    bounce_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
bounce_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:bounce_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    bounce_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
bounce_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:bounce_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    bounce_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
bounce_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:bounce_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    circular_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
circular_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:circular_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    circular_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
circular_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:circular_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    circular_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
circular_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:circular_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    cubic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
cubic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:cubic_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    cubic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
cubic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:cubic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    cubic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
cubic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:cubic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    elastic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
elastic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:elastic_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    elastic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
elastic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:elastic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    elastic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
elastic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:elastic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    exponential_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
exponential_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:exponential_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    exponential_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
exponential_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:exponential_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    exponential_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
exponential_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:exponential_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    linear_interpolation(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
linear_interpolation({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:linear_interpolation(T, D)}.

-doc("""
To be written.
""").
-spec
    quadratic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quadratic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quadratic_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    quadratic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quadratic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quadratic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    quadratic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quadratic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quadratic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    quartic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quartic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quartic_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    quartic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quartic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quartic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    quartic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quartic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quartic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    quintic_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quintic_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quintic_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    quintic_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quintic_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quintic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    quintic_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
quintic_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:quintic_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    sine_ease_in(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
sine_ease_in({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:sine_ease_in(T, D)}.

-doc("""
To be written.
""").
-spec
    sine_ease_in_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
sine_ease_in_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:sine_ease_in_out(T, D)}.

-doc("""
To be written.
""").
-spec
    sine_ease_out(glm:scalar(T)) -> glm:scalar(T) when T :: float | double
.
sine_ease_out({scalar, T, D}) when T =:= float; T =:= double ->
    {scalar, T, glm_raw:sine_ease_in_out(T, D)}.
