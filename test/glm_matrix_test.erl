%%
%% Copyright (c) 2026, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_matrix_test).
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

mat2_test() ->
    ok = run_matrix_test(mat2, 2, 2).

mat3_test() ->
    ok = run_matrix_test(mat3, 3, 3).

mat4_test() ->
    ok = run_matrix_test(mat4, 4, 4).

mat2x3_test() ->
    ok = run_matrix_test(mat2x3, 2, 3).

mat2x4_test() ->
    ok = run_matrix_test(mat2x4, 2, 4).

mat3x2_test() ->
    ok = run_matrix_test(mat3x2, 3, 2).

mat3x4_test() ->
    ok = run_matrix_test(mat3x4, 3, 4).

mat4x2_test() ->
    ok = run_matrix_test(mat4x2, 4, 2).

mat4x3_test() ->
    ok = run_matrix_test(mat4x3, 4, 3).

run_matrix_test(Shape, Cols, Rows) ->
    ok = run_matrix_test(Shape, Cols, Rows, float),
    ok = run_matrix_test(Shape, Cols, Rows, double).

run_matrix_test(Shape, Cols, Rows, T) ->
    ElementCount = Cols * Rows,
    ZeroRowMajor = lists:duplicate(ElementCount, 0.0),
    SplatValue = 7.5,
    SplatRowMajor = lists:duplicate(ElementCount, SplatValue),
    ReplacementValue = 99.25,
    InvalidValue = true,
    RowMajorValues = [erlang:float(I) || I <- lists:seq(1, ElementCount)],
    ColumnMajorValues = row_major_to_column_major(Cols, Rows, RowMajorValues),
    Matrix = apply(glm, Shape, [T | ColumnMajorValues]),

    ok = assert_matrix(Shape, T, apply(glm, Shape, [T]), ZeroRowMajor),
    ok = assert_matrix(Shape, T, apply(glm, Shape, [T, SplatValue]), SplatRowMajor),
    ok = assert_matrix(Shape, T, Matrix, RowMajorValues),
    ok = assert_flat_elements(Shape, T, Matrix, ColumnMajorValues),
    ok = assert_values(Shape, T, Matrix, ColumnMajorValues),
    ok = assert_flat_setters(Shape, Cols, Rows, T, Matrix, ColumnMajorValues, ReplacementValue),
    ok = assert_coordinate_setters(Shape, Cols, Rows, T, Matrix, ColumnMajorValues, ReplacementValue),
    ok = assert_invalid_constructor_values(Shape, T, ColumnMajorValues, InvalidValue),
    ok = assert_invalid_setter_values(Shape, Cols, Rows, T, Matrix, InvalidValue),
    ok.

assert_matrix(Shape, T, Matrix, RowMajorValues) ->
    apply(test_glm, assert_fun(Shape), [T, Matrix, erlang:list_to_tuple(RowMajorValues)]).

assert_flat_elements(Shape, T, Matrix, ColumnMajorValues) ->
    Fun = element_fun(Shape),
    lists:foreach(
        fun({Index, Expected}) ->
            assert_float_value(T, Expected, apply(glm, Fun, [Matrix, Index]))
        end,
        lists:zip(lists:seq(1, erlang:length(ColumnMajorValues)), ColumnMajorValues)
    ),
    ok.

assert_values(Shape, T, Matrix, ColumnMajorValues) ->
    ActualValues = apply(glm, values_fun(Shape), [Matrix]),
    assert_float_tuple(T, erlang:list_to_tuple(ColumnMajorValues), ActualValues).

assert_flat_setters(Shape, Cols, Rows, T, Matrix, ColumnMajorValues, ReplacementValue) ->
    Fun = set_element_fun(Shape),
    lists:foreach(
        fun(Index) ->
            UpdatedColumnMajorValues = replace_nth(ColumnMajorValues, Index, ReplacementValue),
            UpdatedRowMajorValues = column_major_to_row_major(Cols, Rows, UpdatedColumnMajorValues),
            UpdatedMatrix = apply(glm, Fun, [Matrix, Index, ReplacementValue]),
            ok = assert_matrix(Shape, T, UpdatedMatrix, UpdatedRowMajorValues)
        end,
        lists:seq(1, erlang:length(ColumnMajorValues))
    ),
    ok.

assert_coordinate_setters(Shape, Cols, Rows, T, Matrix, ColumnMajorValues, ReplacementValue) ->
    Fun = set_element_fun(Shape),
    lists:foreach(
        fun({Row, Column}) ->
            Index = column_major_index(Rows, Row, Column),
            UpdatedColumnMajorValues = replace_nth(ColumnMajorValues, Index, ReplacementValue),
            UpdatedRowMajorValues = column_major_to_row_major(Cols, Rows, UpdatedColumnMajorValues),
            UpdatedMatrix = apply(glm, Fun, [Matrix, Column, Row, ReplacementValue]),
            ok = assert_matrix(Shape, T, UpdatedMatrix, UpdatedRowMajorValues)
        end,
        coordinates(Rows, Cols)
    ),
    ok.

assert_invalid_constructor_values(Shape, T, ColumnMajorValues, InvalidValue) ->
    ?assertInvalidValue(T, InvalidValue, apply(glm, Shape, [T, InvalidValue])),
    lists:foreach(
        fun(Index) ->
            InvalidConstructorValues = replace_nth(ColumnMajorValues, Index, InvalidValue),
            ?assertInvalidValue(T, InvalidValue, apply(glm, Shape, [T | InvalidConstructorValues]))
        end,
        lists:seq(1, erlang:length(ColumnMajorValues))
    ),
    ok.

assert_invalid_setter_values(Shape, Cols, Rows, T, Matrix, InvalidValue) ->
    Fun = set_element_fun(Shape),
    ElementCount = Cols * Rows,
    lists:foreach(
        fun(Index) ->
            ?assertInvalidValue(T, InvalidValue, apply(glm, Fun, [Matrix, Index, InvalidValue]))
        end,
        lists:seq(1, ElementCount)
    ),
    lists:foreach(
        fun({Row, Column}) ->
            ?assertInvalidValue(T, InvalidValue, apply(glm, Fun, [Matrix, Column, Row, InvalidValue]))
        end,
        coordinates(Rows, Cols)
    ),
    ok.

assert_float_tuple(T, ExpectedTuple, ActualTuple) ->
    ExpectedValues = erlang:tuple_to_list(ExpectedTuple),
    ActualValues = erlang:tuple_to_list(ActualTuple),
    lists:foreach(
        fun({Expected, Actual}) ->
            assert_float_value(T, Expected, Actual)
        end,
        lists:zip(ExpectedValues, ActualValues)
    ),
    ok.

assert_float_value(float, Expected, Actual) ->
    ?assert(erlang:abs(Actual - Expected) < 1.0e-9),
    ok;
assert_float_value(double, Expected, Actual) ->
    ?assert(erlang:abs(Actual - Expected) < 1.0e-12),
    ok.

assert_fun(Shape) ->
    erlang:list_to_atom("assert_" ++ erlang:atom_to_list(Shape)).

element_fun(Shape) ->
    erlang:list_to_atom(erlang:atom_to_list(Shape) ++ "_element").

set_element_fun(Shape) ->
    erlang:list_to_atom(erlang:atom_to_list(Shape) ++ "_set_element").

values_fun(Shape) ->
    erlang:list_to_atom(erlang:atom_to_list(Shape) ++ "_values").

row_major_to_column_major(Cols, Rows, RowMajorValues) ->
    [
        lists:nth((Row - 1) * Cols + Column, RowMajorValues)
     || Column <- lists:seq(1, Cols), Row <- lists:seq(1, Rows)
    ].

column_major_to_row_major(Cols, Rows, ColumnMajorValues) ->
    [
        lists:nth((Column - 1) * Rows + Row, ColumnMajorValues)
     || Row <- lists:seq(1, Rows), Column <- lists:seq(1, Cols)
    ].

replace_nth(Values, Index, Replacement) ->
    {Prefix, [_ | Suffix]} = lists:split(Index - 1, Values),
    Prefix ++ [Replacement] ++ Suffix.

column_major_index(Rows, Row, Column) ->
    (Column - 1) * Rows + Row.

coordinates(Rows, Cols) ->
    [{Row, Column} || Row <- lists:seq(1, Rows), Column <- lists:seq(1, Cols)].