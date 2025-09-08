#include <stdio.h>
#include <stdint.h>
#include <erl_nif.h>
#include <glm/glm.hpp>
#include "beam_glm.hpp"

enum GlmType {
    GLM_BOOL = 1,
    GLM_INT8 = 2,
    GLM_INT16 = 3,
    GLM_INT32 = 4,
    GLM_INT64 = 5,
    GLM_UINT8 = 6,
    GLM_UINT16 = 7,
    GLM_UINT32 = 8,
    GLM_UINT64 = 9,
    GLM_FLOAT = 10,
    GLM_DOUBLE = 11
};

static int get_glm_type(ErlNifEnv* env, ERL_NIF_TERM arg, int* type)
{
    if (!enif_get_int(env, arg, type)) {
        return 0;
    }

    if (*type < 1 || *type > 11) {
        return 0;
    }

    return 1;
}

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    (void)priv_data;
    (void)arg;

    return 0;
}

static void nif_module_unload(ErlNifEnv* caller_env, void* priv_data)
{
    (void)caller_env;
    (void)priv_data;
}

static ERL_NIF_TERM nif_glm_clamp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin)) {
        return enif_make_badarg(env);
    }
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }
    ErlNifBinary input3_bin;
    if (!enif_inspect_binary(env, argv[5], &input3_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        switch (type) {
            case GLM_BOOL:
                beam_clamp_sss<bool>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT8:
                beam_clamp_sss<int8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT16:
                beam_clamp_sss<int16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT32:
                beam_clamp_sss<int32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT64:
                beam_clamp_sss<int64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT8:
                beam_clamp_sss<uint8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT16:
                beam_clamp_sss<uint16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT32:
                beam_clamp_sss<uint32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT64:
                beam_clamp_sss<uint64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_FLOAT:
                beam_clamp_sss<float>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_clamp_sss<double>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
        }
    } else if (pattern == 1) {
        switch (type) {
            case GLM_BOOL:
                beam_clamp_vss<2, bool>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT8:
                beam_clamp_vss<2, int8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT16:
                beam_clamp_vss<2, int16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT32:
                beam_clamp_vss<2, int32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT64:
                beam_clamp_vss<2, int64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT8:
                beam_clamp_vss<2, uint8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT16:
                beam_clamp_vss<2, uint16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT32:
                beam_clamp_vss<2, uint32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT64:
                beam_clamp_vss<2, uint64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_FLOAT:
                beam_clamp_vss<2, float>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_clamp_vss<2, double>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
        }
    } else if (pattern == 2) {
        switch (type) {
            case GLM_BOOL:
                beam_clamp_vvv<2, bool>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT8:
                beam_clamp_vvv<2, int8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT16:
                beam_clamp_vvv<2, int16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT32:
                beam_clamp_vvv<2, int32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_INT64:
                beam_clamp_vvv<2, int64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT8:
                beam_clamp_vvv<2, uint8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT16:
                beam_clamp_vvv<2, uint16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT32:
                beam_clamp_vvv<2, uint32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_UINT64:
                beam_clamp_vvv<2, uint64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_FLOAT:
                beam_clamp_vvv<2, float>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_clamp_vvv<2, double>(input1_bin, input2_bin, input3_bin, output_bin);
                break;
        }
    } else {
        return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ErlNifFunc nif_functions[] = {
    {"clamp_raw", 6, nif_glm_clamp, 0}
};

ERL_NIF_INIT(
    glm_raw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
