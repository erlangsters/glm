%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%

-define(assertInvalidValue(Reason, Expr),
    begin
        ?assertException(error, {invalid_value, Reason}, Expr)
    end
).

-define(assertInvalidValue(Type, InvalidValue, Expr),
    begin
        case Type of
            bool ->
                ?assertException(error, {invalid_value, {expected_boolean, InvalidValue}}, Expr);
            {int, 8} ->
                ?assertException(error, {invalid_value, {expected_int8, InvalidValue}}, Expr);
            {int, 16} ->
                ?assertException(error, {invalid_value, {expected_int16, InvalidValue}}, Expr);
            {int, 32} ->
                ?assertException(error, {invalid_value, {expected_int32, InvalidValue}}, Expr);
            {int, 64} ->
                ?assertException(error, {invalid_value, {expected_int64, InvalidValue}}, Expr);
            {uint, 8} ->
                ?assertException(error, {invalid_value, {expected_uint8, InvalidValue}}, Expr);
            {uint, 16} ->
                ?assertException(error, {invalid_value, {expected_uint16, InvalidValue}}, Expr);
            {uint, 32} ->
                ?assertException(error, {invalid_value, {expected_uint32, InvalidValue}}, Expr);
            {uint, 64} ->
                ?assertException(error, {invalid_value, {expected_uint64, InvalidValue}}, Expr);
            float ->
                ?assertException(error, {invalid_value, {expected_float, InvalidValue}}, Expr);
            double ->
                ?assertException(error, {invalid_value, {expected_double, InvalidValue}}, Expr)
        end
    end
).
