%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(test_glm).
-include_lib("eunit/include/eunit.hrl").
-include_lib("glm/include/glm.hrl").

assert_byte_size(T, N, D) ->
    ByteSize = case T of
        bool -> ?BOOL_BYTE_SIZE;
        {int, 8} -> ?INT8_BYTE_SIZE;
        {int, 16} -> ?INT16_BYTE_SIZE;
        {int, 32} -> ?INT32_BYTE_SIZE;
        {int, 64} -> ?INT64_BYTE_SIZE;
        {uint, 8} -> ?UINT8_BYTE_SIZE;
        {uint, 16} -> ?UINT16_BYTE_SIZE;
        {uint, 32} -> ?UINT32_BYTE_SIZE;
        {uint, 64} -> ?UINT64_BYTE_SIZE;
        float -> ?FLOAT_BYTE_SIZE;
        double -> ?DOUBLE_BYTE_SIZE
    end * N,
    ?assertEqual(ByteSize, erlang:byte_size(D)),
    ok.
