%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm).
-moduledoc """
OpenGL Mathematics (GLM) for the BEAM.
""".

-compile({nowarn_redefined_builtin_type, [bool/0, float/0]}).

-export_type([
    type/0
]).
-export_type([
    scalar/1
]).
-export_type([
    bool/0,
    int8/0,
    int16/0,
    int32/0,
    int64/0,
    uint8/0,
    uint16/0,
    uint32/0,
    uint64/0,
    float/0,
    double/0
]).
-export_type([
    length/0,
    vec/2, vec/1,
    vec2/1, vec3/1, vec4/1
]).

-export([
    bool/0, bool/1,
    bool_value/1
]).
-export([
    int8/0, int8/1,
    int8_value/1,
    int16/0, int16/1,
    int16_value/1,
    int32/0, int32/1,
    int32_value/1,
    int64/0, int64/1,
    int64_value/1
]).
-export([
    uint8/0, uint8/1,
    uint8_value/1,
    uint16/0, uint16/1,
    uint16_value/1,
    uint32/0, uint32/1,
    uint32_value/1,
    uint64/0, uint64/1,
    uint64_value/1
]).
-export([
    float/0, float/1,
    float_value/1,
    double/0, double/1,
    double_value/1
]).
-export([
    scalar/1, scalar/2,
    scalar_value/1
]).
-export([
    vec2/1, vec2/2, vec2/3,
    vec2_x/1, vec2_set_x/2,
    vec2_y/1, vec2_set_y/2
]).
-export([
    vec3/1, vec3/2, vec3/4,
    vec3_x/1, vec3_set_x/2,
    vec3_y/1, vec3_set_y/2,
    vec3_z/1, vec3_set_z/2
]).
-export([
    vec4/1, vec4/2, vec4/5,
    vec4_x/1, vec4_set_x/2,
    vec4_y/1, vec4_set_y/2,
    vec4_z/1, vec4_set_z/2,
    vec4_w/1, vec4_set_w/2
]).
-export([
    clamp/3
]).

-include("glm.hrl").

-define(EXCEPTION_IF_INVALID_VALUE(V, T),
    begin
        case T of
            float when is_float(V) -> ok;
            double when is_float(V) -> ok;
            {int, 8} when is_integer(V), V >= ?INT8_MIN, V =< ?INT8_MAX -> ok;
            {int, 16} when is_integer(V), V >= ?INT16_MIN, V =< ?INT16_MAX -> ok;
            {int, 32} when is_integer(V), V >= ?INT32_MIN, V =< ?INT32_MAX -> ok;
            {int, 64} when is_integer(V), V >= ?INT64_MIN, V =< ?INT64_MAX -> ok;
            {uint, 8} when is_integer(V), V >= ?UINT8_MIN, V =< ?UINT8_MAX -> ok;
            {uint, 16} when is_integer(V), V >= ?UINT16_MIN, V =< ?UINT16_MAX -> ok;
            {uint, 32} when is_integer(V), V >= ?UINT32_MIN, V =< ?UINT32_MAX -> ok;
            {uint, 64} when is_integer(V), V >= ?UINT64_MIN, V =< ?UINT64_MAX -> ok;
            bool when is_boolean(V) -> ok;
            _ ->
                case T of
                    float -> erlang:error({invalid_value, {expected_float, V}});
                    double -> erlang:error({invalid_value, {expected_double, V}});
                    {int, 8} -> erlang:error({invalid_value, {expected_int8, V}});
                    {int, 16} -> erlang:error({invalid_value, {expected_int16, V}});
                    {int, 32} -> erlang:error({invalid_value, {expected_int32, V}});
                    {int, 64} -> erlang:error({invalid_value, {expected_int64, V}});
                    {uint, 8} -> erlang:error({invalid_value, {expected_uint8, V}});
                    {uint, 16} -> erlang:error({invalid_value, {expected_uint16, V}});
                    {uint, 32} -> erlang:error({invalid_value, {expected_uint32, V}});
                    {uint, 64} -> erlang:error({invalid_value, {expected_uint64, V}});
                    bool -> erlang:error({invalid_value, {expected_boolean, V}})
                end
        end
    end
).

-type type() ::
    bool |
    {int, 8 | 16 | 32 | 64} |
    {uint, 8 | 16 | 32 | 64} |
    float |
    double
.

-type scalar(T) :: {scalar, T, binary()}.

-type bool() :: scalar(bool).
-type int8() :: scalar({int, 8}).
-type int16() :: scalar({int, 16}).
-type int32() :: scalar({int, 32}).
-type int64() :: scalar({int, 64}).
-type uint8() :: scalar({uint, 8}).
-type uint16() :: scalar({uint, 16}).
-type uint32() :: scalar({uint, 32}).
-type uint64() :: scalar({uint, 64}).
-type float() :: scalar(float).
-type double() :: scalar(double).

-type length() :: 2 | 3 | 4.

-type vec(Length, Type) :: {vec, Length, Type, binary()}.
-type vec(Type) :: vec(length(), Type).

-type vec2(Type) :: vec(2, Type).
-type vec3(Type) :: vec(3, Type).
-type vec4(Type) :: vec(4, Type).

-doc("""
To be written.
""").
-spec bool() -> scalar(bool).
bool() ->
    {scalar, bool, glm_raw:bool()}.

-doc("""
To be written.
""").
-spec bool(boolean()) -> scalar(bool).
bool(V) ->
    case V of
        _ when erlang:is_boolean(V) ->
            {scalar, bool, glm_raw:bool(V)};
        _ ->
            erlang:error({invalid_value, {expected_boolean, V}})
    end.

-doc("""
To be written.
""").
-spec bool_value(scalar(bool)) -> boolean().
bool_value({scalar, bool, D}) when byte_size(D) =:= ?BOOL_BYTE_SIZE ->
    glm_raw:bool_value(D).

-doc("""
To be written.
""").
-spec int8() -> scalar({int, 8}).
int8() ->
    {scalar, {int, 8}, glm_raw:int8()}.

-doc("""
To be written.
""").
-spec int8(integer()) -> scalar({int, 8}).
int8(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT8_MIN, V =< ?INT8_MAX ->
            {scalar, {int, 8}, glm_raw:int8(V)};
        _ ->
            erlang:error({invalid_value, {expected_int8, V}})
    end.

-doc("""
To be written.
""").
-spec int8_value(scalar({int, 8})) -> integer().
int8_value({scalar, {int, 8}, D}) when byte_size(D) =:= ?INT8_BYTE_SIZE ->
    glm_raw:int8_value(D).

-doc("""
To be written.
""").
-spec int16() -> scalar({int, 16}).
int16() ->
    {scalar, {int, 16}, glm_raw:int16()}.

-doc("""
To be written.
""").
-spec int16(integer()) -> scalar({int, 16}).
int16(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT16_MIN, V =< ?INT16_MAX ->
            {scalar, {int, 16}, glm_raw:int16(V)};
        _ ->
            erlang:error({invalid_value, {expected_int16, V}})
    end.

-doc("""
To be written.
""").
-spec int16_value(scalar({int, 16})) -> integer().
int16_value({scalar, {int, 16}, D}) when byte_size(D) =:= ?INT16_BYTE_SIZE ->
    glm_raw:int16_value(D).

-doc("""
To be written.
""").
-spec int32() -> scalar({int, 32}).
int32() ->
    {scalar, {int, 32}, glm_raw:int32()}.

-doc("""
To be written.
""").
-spec int32(integer()) -> scalar({int, 32}).
int32(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT32_MIN, V =< ?INT32_MAX ->
            {scalar, {int, 32}, glm_raw:int32(V)};
        _ ->
            erlang:error({invalid_value, {expected_int32, V}})
    end.

-doc("""
To be written.
""").
-spec int32_value(scalar({int, 32})) -> integer().
int32_value({scalar, {int, 32}, D}) when byte_size(D) =:= ?INT32_BYTE_SIZE ->
    glm_raw:int32_value(D).

-doc("""
To be written.
""").
-spec int64() -> scalar({int, 64}).
int64() ->
    {scalar, {int, 64}, glm_raw:int64()}.

-doc("""
To be written.
""").
-spec int64(integer()) -> scalar({int, 64}).
int64(V) ->
    case V of
        _ when erlang:is_integer(V), V >= ?INT64_MIN, V =< ?INT64_MAX ->
            {scalar, {int, 64}, glm_raw:int64(V)};
        _ ->
            erlang:error({invalid_value, {expected_int64, V}})
    end.

-doc("""
To be written.
""").
-spec int64_value(scalar({int, 64})) -> integer().
int64_value({scalar, {int, 64}, D}) when byte_size(D) =:= ?INT64_BYTE_SIZE ->
    glm_raw:int64_value(D).

-doc("""
To be written.
""").
-spec uint8() -> scalar({uint, 8}).
uint8() ->
    {scalar, {uint, 8}, glm_raw:uint8()}.

-doc("""
To be written.
""").
-spec uint8(non_neg_integer()) -> scalar({uint, 8}).
uint8(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT8_MAX ->
            {scalar, {uint, 8}, glm_raw:uint8(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint8, V}})
    end.

-doc("""
To be written.
""").
-spec uint8_value(scalar({uint, 8})) -> non_neg_integer().
uint8_value({scalar, {uint, 8}, D}) when byte_size(D) =:= ?UINT8_BYTE_SIZE ->
    glm_raw:uint8_value(D).

-doc("""
To be written.
""").
-spec uint16() -> scalar({uint, 16}).
uint16() ->
    {scalar, {uint, 16}, glm_raw:uint16()}.

-doc("""
To be written.
""").
-spec uint16(non_neg_integer()) -> scalar({uint, 16}).
uint16(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT16_MAX ->
            {scalar, {uint, 16}, glm_raw:uint16(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint16, V}})
    end.

-doc("""
To be written.
""").
-spec uint16_value(scalar({uint, 16})) -> non_neg_integer().
uint16_value({scalar, {uint, 16}, D}) when byte_size(D) =:= ?UINT16_BYTE_SIZE ->
    glm_raw:uint16_value(D).

-doc("""
To be written.
""").
-spec uint32() -> scalar({uint, 32}).
uint32() ->
    {scalar, {uint, 32}, glm_raw:uint32()}.

-doc("""
To be written.
""").
-spec uint32(non_neg_integer()) -> scalar({uint, 32}).
uint32(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT32_MAX ->
            {scalar, {uint, 32}, glm_raw:uint32(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint32, V}})
    end.

-doc("""
To be written.
""").
-spec uint32_value(scalar({uint, 32})) -> non_neg_integer().
uint32_value({scalar, {uint, 32}, D}) when byte_size(D) =:= ?UINT32_BYTE_SIZE ->
    glm_raw:uint32_value(D).

-doc("""
To be written.
""").
-spec uint64() -> scalar({uint, 64}).
uint64() ->
    {scalar, {uint, 64}, glm_raw:uint64()}.

-doc("""
To be written.
""").
-spec uint64(non_neg_integer()) -> scalar({uint, 64}).
uint64(V) ->
    case V of
        _ when is_integer(V), V >= 0, V =< ?UINT64_MAX ->
            {scalar, {uint, 64}, glm_raw:uint64(V)};
        _ ->
            erlang:error({invalid_value, {expected_uint64, V}})
    end.

-doc("""
To be written.
""").
-spec uint64_value(scalar({uint, 64})) -> non_neg_integer().
uint64_value({scalar, {uint, 64}, D}) when byte_size(D) =:= ?UINT64_BYTE_SIZE ->
    glm_raw:uint64_value(D).

-doc("""
To be written.
""").
-spec float() -> scalar(float).
float() ->
    {scalar, float, glm_raw:float()}.

-doc("""
To be written.
""").
-spec float(float()) -> scalar(float).
float(V) ->
    case V of
        _ when erlang:is_float(V) ->
            {scalar, float, glm_raw:float(V)};
        _ ->
            erlang:error({invalid_value, {expected_float, V}})
    end.

-doc("""
To be written.
""").
-spec float_value(scalar(float)) -> float().
float_value({scalar, float, Data}) when byte_size(Data) =:= ?FLOAT_BYTE_SIZE ->
    glm_raw:float_value(Data).

-doc("""
To be written.
""").
-spec double() -> scalar(double).
double() ->
    {scalar, double, glm_raw:double()}.

-doc("""
To be written.
""").
-spec double(float()) -> scalar(double).
double(V) ->
    case V of
        _ when erlang:is_float(V) ->
            {scalar, double, glm_raw:double(V)};
        _ ->
            erlang:error({invalid_value, {expected_double, V}})
    end.

-doc("""
To be written.
""").
-spec double_value(scalar(double)) -> float().
double_value({scalar, double, Data}) when byte_size(Data) =:= ?DOUBLE_BYTE_SIZE ->
    glm_raw:double_value(Data).

-doc("""
To be written.
""").
-spec scalar(T) -> scalar(T) when T :: type().
scalar(bool) ->
    {scalar, bool, glm_raw:bool()};
scalar({int, 8}) ->
    {scalar, {int, 8}, glm_raw:int8()};
scalar({int, 16}) ->
    {scalar, {int, 16}, glm_raw:int16()};
scalar({int, 32}) ->
    {scalar, {int, 32}, glm_raw:int32()};
scalar({int, 64}) ->
    {scalar, {int, 64}, glm_raw:int64()};
scalar({uint, 8}) ->
    {scalar, {uint, 8}, glm_raw:uint8()};
scalar({uint, 16}) ->
    {scalar, {uint, 16}, glm_raw:uint16()};
scalar({uint, 32}) ->
    {scalar, {uint, 32}, glm_raw:uint32()};
scalar({uint, 64}) ->
    {scalar, {uint, 64}, glm_raw:uint64()};
scalar(float) ->
    {scalar, float, glm_raw:float()};
scalar(double) ->
    {scalar, double, glm_raw:double()}.

-doc("""
To be written.
""").
-spec scalar(T, term()) -> scalar(T) when T :: type().
scalar(T, V) ->
    % XXX: Rework impl.
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    case T of
        bool -> {scalar, bool, glm_raw:bool(V)};
        {int, 8} -> {scalar, {int, 8}, glm_raw:int8(V)};
        {int, 16} -> {scalar, {int, 16}, glm_raw:int16(V)};
        {int, 32} -> {scalar, {int, 32}, glm_raw:int32(V)};
        {int, 64} -> {scalar, {int, 64}, glm_raw:int64(V)};
        {uint, 8} -> {scalar, {uint, 8}, glm_raw:uint8(V)};
        {uint, 16} -> {scalar, {uint, 16}, glm_raw:uint16(V)};
        {uint, 32} -> {scalar, {uint, 32}, glm_raw:uint32(V)};
        {uint, 64} -> {scalar, {uint, 64}, glm_raw:uint64(V)};
        float -> {scalar, float, glm_raw:float(V)};
        double -> {scalar, double, glm_raw:double(V)}
    end.

-doc("""
To be written.
""").
-spec scalar_value(scalar(T)) -> term() when T :: type().
scalar_value({scalar, T, D}) ->
    case T of
        bool when byte_size(D) =:= ?BOOL_BYTE_SIZE -> glm_raw:bool_value(D);
        {int, 8} when byte_size(D) =:= ?INT8_BYTE_SIZE -> glm_raw:int8_value(D);
        {int, 16} when byte_size(D) =:= ?INT16_BYTE_SIZE -> glm_raw:int16_value(D);
        {int, 32} when byte_size(D) =:= ?INT32_BYTE_SIZE -> glm_raw:int32_value(D);
        {int, 64} when byte_size(D) =:= ?INT64_BYTE_SIZE -> glm_raw:int64_value(D);
        {uint, 8} when byte_size(D) =:= ?UINT8_BYTE_SIZE -> glm_raw:uint8_value(D);
        {uint, 16} when byte_size(D) =:= ?UINT16_BYTE_SIZE -> glm_raw:uint16_value(D);
        {uint, 32} when byte_size(D) =:= ?UINT32_BYTE_SIZE -> glm_raw:uint32_value(D);
        {uint, 64} when byte_size(D) =:= ?UINT64_BYTE_SIZE -> glm_raw:uint64_value(D);
        float when byte_size(D) =:= ?FLOAT_BYTE_SIZE -> glm_raw:float_value(D);
        double when byte_size(D) =:= ?DOUBLE_BYTE_SIZE -> glm_raw:double_value(D)
    end.

-doc("""
To be written.
""").
-spec vec2(T) -> vec2(T) when T :: type().
vec2(T) ->
    {vec, 2, T, glm_raw:vec2(T)}.

-doc("""
To be written.
""").
-spec vec2(T, term()) -> vec2(T) when T :: type().
vec2(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {vec, 2, T, glm_raw:vec2(T, V)}.

-doc("""
To be written.
""").
-spec vec2(T, term(), term()) -> vec2(T) when T :: type().
vec2(T, X, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 2, T, glm_raw:vec2(T, X, Y)}.

-doc("""
To be written.
""").
-spec vec2_x(vec2(T)) -> term() when T :: type().
vec2_x({vec, 2, T, D}) ->
    glm_raw:vec2_x(T, D).

-doc("""
To be written.
""").
-spec vec2_set_x(vec2(T), term()) -> vec2(T) when T :: type().
vec2_set_x({vec, 2, T, D}, X) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    {vec, 2, T, glm_raw:vec2_set_x(T, D, X)}.

-doc("""
To be written.
""").
-spec vec2_y(vec2(T)) -> term() when T :: type().
vec2_y({vec, 2, T, D}) ->
    glm_raw:vec2_y(T, D).

-doc("""
To be written.
""").
-spec vec2_set_y(vec2(T), term()) -> vec2(T) when T :: type().
vec2_set_y({vec, 2, T, D}, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 2, T, glm_raw:vec2_set_y(T, D, Y)}.

-doc("""
To be written.
""").
-spec vec3(T) -> vec3(T) when T :: type().
vec3(T) ->
    {vec, 3, T, glm_raw:vec3(T)}.

-doc("""
To be written.
""").
-spec vec3(T, term()) -> vec3(T) when T :: type().
vec3(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {vec, 3, T, glm_raw:vec3(T, V)}.

-doc("""
To be written.
""").
-spec vec3(T, term(), term(), term()) -> vec3(T) when T :: type().
vec3(T, X, Y, Z) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {vec, 3, T, glm_raw:vec3(T, X, Y, Z)}.

-doc("""
To be written.
""").
-spec vec3_x(vec3(T)) -> term() when T :: type().
vec3_x({vec, 3, T, D}) ->
    glm_raw:vec3_x(T, D).

-doc("""
To be written.
""").
-spec vec3_set_x(vec3(T), term()) -> vec3(T) when T :: type().
vec3_set_x({vec, 3, T, D}, X) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    {vec, 3, T, glm_raw:vec3_set_x(T, D, X)}.

-doc("""
To be written.
""").
-spec vec3_y(vec3(T)) -> term() when T :: type().
vec3_y({vec, 3, T, D}) ->
    glm_raw:vec3_y(T, D).

-doc("""
To be written.
""").
-spec vec3_set_y(vec3(T), term()) -> vec3(T) when T :: type().
vec3_set_y({vec, 3, T, D}, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 3, T, glm_raw:vec3_set_y(T, D, Y)}.

-doc("""
To be written.
""").
-spec vec3_z(vec3(T)) -> term() when T :: type().
vec3_z({vec, 3, T, D}) ->
    glm_raw:vec3_z(T, D).

-doc("""
To be written.
""").
-spec vec3_set_z(vec3(T), term()) -> vec3(T) when T :: type().
vec3_set_z({vec, 3, T, D}, Z) ->
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {vec, 3, T, glm_raw:vec3_set_z(T, D, Z)}.

-doc("""
To be written.
""").
-spec vec4(T) -> vec4(T) when T :: type().
vec4(T) ->
    {vec, 4, T, glm_raw:vec4(T)}.

-doc("""
To be written.
""").
-spec vec4(T, term()) -> vec4(T) when T :: type().
vec4(T, V) ->
    ?EXCEPTION_IF_INVALID_VALUE(V, T),
    {vec, 4, T, glm_raw:vec4(T, V)}.

-doc("""
To be written.
""").
-spec vec4(T, term(), term(), term(), term()) -> vec4(T) when T :: type().
vec4(T, X, Y, Z, W) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    ?EXCEPTION_IF_INVALID_VALUE(W, T),
    {vec, 4, T, glm_raw:vec4(T, X, Y, Z, W)}.

-doc("""
To be written.
""").
-spec vec4_x(vec4(T)) -> term() when T :: type().
vec4_x({vec, 4, T, D}) ->
    glm_raw:vec4_x(T, D).

-doc("""
To be written.
""").
-spec vec4_set_x(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_x({vec, 4, T, D}, X) ->
    ?EXCEPTION_IF_INVALID_VALUE(X, T),
    {vec, 4, T, glm_raw:vec4_set_x(T, D, X)}.

-doc("""
To be written.
""").
-spec vec4_y(vec4(T)) -> term() when T :: type().
vec4_y({vec, 4, T, D}) ->
    glm_raw:vec4_y(T, D).

-doc("""
To be written.
""").
-spec vec4_set_y(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_y({vec, 4, T, D}, Y) ->
    ?EXCEPTION_IF_INVALID_VALUE(Y, T),
    {vec, 4, T, glm_raw:vec4_set_y(T, D, Y)}.

-doc("""
To be written.
""").
-spec vec4_z(vec4(T)) -> term() when T :: type().
vec4_z({vec, 4, T, D}) ->
    glm_raw:vec4_z(T, D).

-doc("""
To be written.
""").
-spec vec4_set_z(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_z({vec, 4, T, D}, Z) ->
    ?EXCEPTION_IF_INVALID_VALUE(Z, T),
    {vec, 4, T, glm_raw:vec4_set_z(T, D, Z)}.

-doc("""
To be written.
""").
-spec vec4_w(vec4(T)) -> term() when T :: type().
vec4_w({vec, 4, T, D}) ->
    glm_raw:vec4_w(T, D).

-doc("""
To be written.
""").
-spec vec4_set_w(vec4(T), term()) -> vec4(T) when T :: type().
vec4_set_w({vec, 4, T, D}, W) ->
    ?EXCEPTION_IF_INVALID_VALUE(W, T),
    {vec, 4, T, glm_raw:vec4_set_w(T, D, W)}.

-doc("""
To be written.
""").
% -spec % XXXXXX: constraint T to float double and int ????
%     clamp(scalar(T), scalar(T), scalar(T)) -> scalar(T) when T :: float | double | int; 
%     clamp(vec(L, T), scalar(T), scalar(T)) -> vec(L, T) when T :: float | double | int, L :: length();
%     clamp(vec(L, T), vec(L, T), vec(L, T)) -> vec(L, T) when T :: float | double | int, L :: length()
% .
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
