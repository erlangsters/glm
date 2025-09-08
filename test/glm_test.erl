%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(glm_test).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("test_glm.hrl").

clamp_test() ->
    % Test correctness of the output (make sure the correct function is
    % called).
    R1 = glm:clamp(glm:float(0.5), glm:float(0.0), glm:float(1.0)),
    0.5 = glm:float_value(R1),
    R2 = glm:clamp(glm:float(1.1), glm:float(0.0), glm:float(1.0)),
    1.0 = glm:float_value(R2),
    R3 = glm:clamp(glm:vec2(float, -0.1, 1.1), glm:float(0.0), glm:float(1.0)),
    0.0 = glm:vec2_x(R3),
    1.0 = glm:vec2_y(R3),
    R4 = glm:clamp(
        glm:vec2(float, -0.1, 1.1),
        glm:vec2(float, 0.0, 0.0),
        glm:vec2(float, 1.0, 1.0)
    ),
    0.0 = glm:vec2_x(R4),
    1.0 = glm:vec2_y(R4),

    % Test each patterns and type constraints.
    ?assert(proper:quickcheck(prop_clamp())),

    ok.

prop_clamp() ->
    ?FORALL(
        % We generate 3 vectors of the same type and length. To test the scalar
        % versions of the clamp/3 function, we use the value of the first
        % vector component.  We also generate a vector of a different type to
        % test type mismatch errors.
        {T, L, T_Diff, V1, V2, V3, V_Diff} = Input,
        ?LET(
            {T, T_Diff, L},
            ?SUCHTHAT(
                {T1, T2, Length},
                {test_glm:gen_type(), test_glm:gen_type(), test_glm:gen_vec_length()},
                T1 =/= T2
            ),
            ?LET(
                {V1, V2, V3, V_Diff},
                {test_glm:gen_vec(T, L), test_glm:gen_vec(T, L), test_glm:gen_vec(T, L), test_glm:gen_vec(T_Diff, L)},
                {T, L, T_Diff, V1, V2, V3, V_Diff}
            )
        ),
        begin
            % To test the versions of clamp/3 with scalars, we use the first
            % component of the vectors.
            {S1, S2, S3, S_Diff} = case L of
                2 ->
                    {
                        glm:scalar(T, glm:vec2_x(V1)),
                        glm:scalar(T, glm:vec2_x(V2)),
                        glm:scalar(T, glm:vec2_x(V3)),
                        glm:scalar(T_Diff, glm:vec2_x(V_Diff))
                    };
                3 ->
                    {
                        glm:scalar(T, glm:vec3_x(V1)),
                        glm:scalar(T, glm:vec3_x(V2)),
                        glm:scalar(T, glm:vec3_x(V3)),
                        glm:scalar(T_Diff, glm:vec3_x(V_Diff))
                    };
                4 ->
                    {
                        glm:scalar(T, glm:vec4_x(V1)),
                        glm:scalar(T, glm:vec4_x(V2)),
                        glm:scalar(T, glm:vec4_x(V3)),
                        glm:scalar(T_Diff, glm:vec4_x(V_Diff))
                    }
            end,

            % Test scalar/scalar/scalar pattern.
            ok = test_glm:assert_scalar(T, glm:clamp(S1, S2, S3)),
            ?assertException(error, _, glm:clamp(S_Diff, S2, S3)),
            ?assertException(error, _, glm:clamp(S1, S_Diff, S3)),
            ?assertException(error, _, glm:clamp(S1, S2, S_Diff)),

            % Test vector/scalar/scalar pattern.
            ok = test_glm:assert_vec(T, L, glm:clamp(V1, S2, S3)),
            ?assertException(error, _, glm:clamp(V_Diff, S2, S3)),
            ?assertException(error, _, glm:clamp(V1, S_Diff, S3)),
            ?assertException(error, _, glm:clamp(V1, S2, S_Diff)),

            % Test vector/vector/scalar pattern.
            ok = test_glm:assert_vec(T, L, glm:clamp(V1, V2, V3)),
            ?assertException(error, _, glm:clamp(V_Diff, V2, V3)),
            ?assertException(error, _, glm:clamp(V1, V_Diff, V3)),
            ?assertException(error, _, glm:clamp(V1, V2, V_Diff)),

            true
        end
    ).

round_test() ->
    R1 = glm:round(glm:vec2(float, 0.1, 0.9)),
    0.0 = glm:vec2_x(R1),
    1.0 = glm:vec2_y(R1),
    R2 = glm:round(glm:vec2(double, 0.1, 0.9)),
    0.0 = glm:vec2_x(R2),
    1.0 = glm:vec2_y(R2),

    R3 = glm:round(glm:vec3(float, 0.1, 0.9, 0.1)),
    0.0 = glm:vec3_x(R3),
    1.0 = glm:vec3_y(R3),
    0.0 = glm:vec3_z(R3),
    R4 = glm:round(glm:vec3(double, 0.1, 0.9, 0.1)),
    0.0 = glm:vec3_x(R4),
    1.0 = glm:vec3_y(R4),
    0.0 = glm:vec3_z(R4),

    R5 = glm:round(glm:vec4(float, 0.1, 0.9, 0.1, 0.9)),
    0.0 = glm:vec4_x(R5),
    1.0 = glm:vec4_y(R5),
    0.0 = glm:vec4_z(R5),
    1.0 = glm:vec4_w(R5),
    R6 = glm:round(glm:vec4(double, 0.1, 0.9, 0.1, 0.9)),
    0.0 = glm:vec4_x(R6),
    1.0 = glm:vec4_y(R6),
    0.0 = glm:vec4_z(R6),
    1.0 = glm:vec4_w(R6),

    ok.

round_even_test() ->
    R1 = glm:round_even(glm:vec2(float, 3.5, 4.5)),
    4.0 = glm:vec2_x(R1),
    4.0 = glm:vec2_y(R1),
    R2 = glm:round_even(glm:vec2(double, 3.5, 4.5)),
    4.0 = glm:vec2_x(R2),
    4.0 = glm:vec2_y(R2),
    R3 = glm:round_even(glm:vec3(float, 3.5, 4.5, 5.5)),
    4.0 = glm:vec3_x(R3),
    4.0 = glm:vec3_y(R3),
    6.0 = glm:vec3_z(R3),
    R4 = glm:round_even(glm:vec3(double, 3.5, 4.5, 5.5)),
    4.0 = glm:vec3_x(R4),
    4.0 = glm:vec3_y(R4),
    6.0 = glm:vec3_z(R4),
    R5 = glm:round_even(glm:vec4(float, 3.5, 4.5, 5.5, 6.5)),
    4.0 = glm:vec4_x(R5),
    4.0 = glm:vec4_y(R5),
    6.0 = glm:vec4_z(R5),
    6.0 = glm:vec4_w(R5),
    R6 = glm:round_even(glm:vec4(double, 3.5, 4.5, 5.5, 6.5)),
    4.0 = glm:vec4_x(R6),
    4.0 = glm:vec4_y(R6),
    6.0 = glm:vec4_z(R6),
    6.0 = glm:vec4_w(R6),

    ok.
