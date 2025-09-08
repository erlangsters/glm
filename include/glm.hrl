%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%

-define(BOOL_BYTE_SIZE, 1).
-define(INT8_BYTE_SIZE, 1).
-define(INT16_BYTE_SIZE, 2).
-define(INT32_BYTE_SIZE, 4).
-define(INT64_BYTE_SIZE, 8).
-define(UINT8_BYTE_SIZE, 1).
-define(UINT16_BYTE_SIZE, 2).
-define(UINT32_BYTE_SIZE, 4).
-define(UINT64_BYTE_SIZE, 8).
-define(FLOAT_BYTE_SIZE, 4).
-define(DOUBLE_BYTE_SIZE, 8).

-define(INT8_MIN, -128).
-define(INT8_MAX, 127).
-define(INT16_MIN, -32768).
-define(INT16_MAX, 32767).
-define(INT32_MIN, -2147483648).
-define(INT32_MAX, 2147483647).
-define(INT64_MIN, -9223372036854775808).
-define(INT64_MAX, 9223372036854775807).

-define(UINT8_MIN, 0).
-define(UINT8_MAX, 255).
-define(UINT16_MIN, 0).
-define(UINT16_MAX, 65535).
-define(UINT32_MIN, 0).
-define(UINT32_MAX, 4294967295).
-define(UINT64_MIN, 0).
-define(UINT64_MAX, 18446744073709551615).
