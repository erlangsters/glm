#include <stdio.h>
#include <stdint.h>
#include <erl_nif.h>
#include <glm/glm.hpp>
#include "beam_glm.hpp"

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

static int get_glm_length(ErlNifEnv* env, ERL_NIF_TERM arg, int* length)
{
    if (!enif_get_int(env, arg, length)) {
        return 0;
    }

    if (*length < 2 || *length > 4) {
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
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
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
        } else if (length == 3) {
            switch (type) {
                case GLM_BOOL:
                    beam_clamp_vss<3, bool>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT8:
                    beam_clamp_vss<3, int8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT16:
                    beam_clamp_vss<3, int16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT32:
                    beam_clamp_vss<3, int32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT64:
                    beam_clamp_vss<3, int64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT8:
                    beam_clamp_vss<3, uint8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT16:
                    beam_clamp_vss<3, uint16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT32:
                    beam_clamp_vss<3, uint32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT64:
                    beam_clamp_vss<3, uint64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_FLOAT:
                    beam_clamp_vss<3, float>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_clamp_vss<3, double>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_BOOL:
                    beam_clamp_vss<4, bool>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT8:
                    beam_clamp_vss<4, int8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT16:
                    beam_clamp_vss<4, int16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT32:
                    beam_clamp_vss<4, int32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT64:
                    beam_clamp_vss<4, int64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT8:
                    beam_clamp_vss<4, uint8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT16:
                    beam_clamp_vss<4, uint16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT32:
                    beam_clamp_vss<4, uint32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT64:
                    beam_clamp_vss<4, uint64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_FLOAT:
                    beam_clamp_vss<4, float>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_clamp_vss<4, double>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
            }
        } else {
            return enif_make_badarg(env);
        }
    } else if (pattern == 2) {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
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
        } else if (length == 3) {
            switch (type) {
                case GLM_BOOL:
                    beam_clamp_vvv<3, bool>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT8:
                    beam_clamp_vvv<3, int8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT16:
                    beam_clamp_vvv<3, int16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT32:
                    beam_clamp_vvv<3, int32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT64:
                    beam_clamp_vvv<3, int64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT8:
                    beam_clamp_vvv<3, uint8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT16:
                    beam_clamp_vvv<3, uint16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT32:
                    beam_clamp_vvv<3, uint32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT64:
                    beam_clamp_vvv<3, uint64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_FLOAT:
                    beam_clamp_vvv<3, float>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_clamp_vvv<3, double>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_BOOL:
                    beam_clamp_vvv<4, bool>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT8:
                    beam_clamp_vvv<4, int8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT16:
                    beam_clamp_vvv<4, int16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT32:
                    beam_clamp_vvv<4, int32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_INT64:
                    beam_clamp_vvv<4, int64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT8:
                    beam_clamp_vvv<4, uint8_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT16:
                    beam_clamp_vvv<4, uint16_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT32:
                    beam_clamp_vvv<4, uint32_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_UINT64:
                    beam_clamp_vvv<4, uint64_t>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_FLOAT:
                    beam_clamp_vvv<4, float>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_clamp_vvv<4, double>(input1_bin, input2_bin, input3_bin, output_bin);
                    break;
            }
        } else {
            return enif_make_badarg(env);
        }

    } else {
        return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_round(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!enif_get_int(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (length == 2) {
        switch (type) {
            case GLM_FLOAT:
                beam_round<2, float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_round<2, double>(input_bin, output_bin);
                break;
        }
    } else if (length == 3) {
        switch (type) {
            case GLM_FLOAT:
                beam_round<3, float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_round<3, double>(input_bin, output_bin);
                break;
        }
    } else if (length == 4) {
        switch (type) {
            case GLM_FLOAT:
                beam_round<4, float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_round<4, double>(input_bin, output_bin);
                break;
        }
    } else {
        return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_round_even(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!enif_get_int(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (length == 2) {
        switch (type) {
            case GLM_FLOAT:
                beam_round_even<2, float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_round_even<2, double>(input_bin, output_bin);
                break;
        }
    } else if (length == 3) {
        switch (type) {
            case GLM_FLOAT:
                beam_round_even<3, float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_round_even<3, double>(input_bin, output_bin);
                break;
        }
    } else if (length == 4) {
        switch (type) {
            case GLM_FLOAT:
                beam_round_even<4, float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_round_even<4, double>(input_bin, output_bin);
                break;
        }
    } else {
        return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

template<typename T>
using EaseFunc2 = void(*)(const ErlNifBinary&, ErlNifBinary&);

template<typename T>
using EaseFunc3 = void(*)(const ErlNifBinary&, const ErlNifBinary&, ErlNifBinary&);

inline ERL_NIF_TERM nif_glm_ease_2arg(
    ErlNifEnv* env,
    int argc, const
    ERL_NIF_TERM argv[],
    EaseFunc2<float> float_func,
    EaseFunc2<double> double_func
) {
    int type;
    if (!enif_get_int(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            float_func(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            double_func(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

inline ERL_NIF_TERM nif_glm_ease_3arg(
    ErlNifEnv* env,
    int argc, const
    ERL_NIF_TERM argv[],
    EaseFunc3<float> float_func,
    EaseFunc3<double> double_func
) {
    int type;
    if (!enif_get_int(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    if (!enif_inspect_binary(env, argv[1], &input1_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            float_func(input1_bin, input2_bin, output_bin);
            break;
        case GLM_DOUBLE:
            double_func(input1_bin, input2_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

#define DEFINE_EASE_2ARG(name) \
static ERL_NIF_TERM nif_glm_##name##_2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    return nif_glm_ease_2arg(env, argc, argv, beam_##name<float>, beam_##name<double>); \
}

#define DEFINE_EASE_3ARG(name) \
static ERL_NIF_TERM nif_glm_##name##_3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    return nif_glm_ease_3arg(env, argc, argv, beam_##name<float>, beam_##name<double>); \
}

DEFINE_EASE_2ARG(backEaseIn)
// DEFINE_EASE_3ARG(backEaseIn)
DEFINE_EASE_2ARG(backEaseInOut)
// DEFINE_EASE_3ARG(backEaseInOut)
DEFINE_EASE_2ARG(backEaseOut)
// DEFINE_EASE_3ARG(backEaseOut)
DEFINE_EASE_2ARG(bounceEaseIn)
DEFINE_EASE_2ARG(bounceEaseInOut)
DEFINE_EASE_2ARG(bounceEaseOut)
DEFINE_EASE_2ARG(circularEaseIn)
DEFINE_EASE_2ARG(circularEaseInOut)
DEFINE_EASE_2ARG(circularEaseOut)
DEFINE_EASE_2ARG(cubicEaseIn)
DEFINE_EASE_2ARG(cubicEaseInOut)
DEFINE_EASE_2ARG(cubicEaseOut)
DEFINE_EASE_2ARG(elasticEaseIn)
DEFINE_EASE_2ARG(elasticEaseInOut)
DEFINE_EASE_2ARG(elasticEaseOut)
DEFINE_EASE_2ARG(exponentialEaseIn)
DEFINE_EASE_2ARG(exponentialEaseInOut)
DEFINE_EASE_2ARG(exponentialEaseOut)
DEFINE_EASE_2ARG(linearInterpolation)
DEFINE_EASE_2ARG(quadraticEaseIn)
DEFINE_EASE_2ARG(quadraticEaseInOut)
DEFINE_EASE_2ARG(quadraticEaseOut)
DEFINE_EASE_2ARG(quarticEaseIn)
DEFINE_EASE_2ARG(quarticEaseInOut)
DEFINE_EASE_2ARG(quarticEaseOut)
DEFINE_EASE_2ARG(quinticEaseIn)
DEFINE_EASE_2ARG(quinticEaseInOut)
DEFINE_EASE_2ARG(quinticEaseOut)
DEFINE_EASE_2ARG(sineEaseIn)
DEFINE_EASE_2ARG(sineEaseInOut)
DEFINE_EASE_2ARG(sineEaseOut)

static ErlNifFunc nif_functions[] = {
    {"clamp_raw", 6, nif_glm_clamp, 0},
    {"round_raw", 3, nif_glm_round, 0},
    {"round_even_raw", 3, nif_glm_round_even, 0},
    {"back_ease_in_raw", 2, nif_glm_backEaseIn_2, 0},
    // {"back_ease_in_raw", 3, nif_glm_back_ease_in3, 0},
    {"back_ease_in_out_raw", 2, nif_glm_backEaseInOut_2, 0},
    // {"back_ease_in_out_raw", 3, nif_glm_back_ease_in_out3, 0},
    {"back_ease_out_raw", 2, nif_glm_backEaseOut_2, 0},
    // {"back_ease_out_raw", 3, nif_glm_back_ease_out3, 0},
    {"bounce_ease_in_raw", 2, nif_glm_bounceEaseIn_2, 0},
    {"bounce_ease_in_out_raw", 2, nif_glm_bounceEaseInOut_2, 0},
    {"bounce_ease_out_raw", 2, nif_glm_bounceEaseOut_2, 0},
    {"circular_ease_in_raw", 2, nif_glm_circularEaseIn_2, 0},
    {"circular_ease_in_out_raw", 2, nif_glm_circularEaseInOut_2, 0},
    {"circular_ease_out_raw", 2, nif_glm_circularEaseOut_2, 0},
    {"cubic_ease_in_raw", 2, nif_glm_cubicEaseIn_2, 0},
    {"cubic_ease_in_out_raw", 2, nif_glm_cubicEaseInOut_2, 0},
    {"cubic_ease_out_raw", 2, nif_glm_cubicEaseOut_2, 0},
    {"elastic_ease_in_raw", 2, nif_glm_elasticEaseIn_2, 0},
    {"elastic_ease_in_out_raw", 2, nif_glm_elasticEaseInOut_2, 0},
    {"elastic_ease_out_raw", 2, nif_glm_elasticEaseOut_2, 0},
    {"exponential_ease_in_raw", 2, nif_glm_exponentialEaseIn_2, 0},
    {"exponential_ease_in_out_raw", 2, nif_glm_exponentialEaseInOut_2, 0},
    {"exponential_ease_out_raw", 2, nif_glm_exponentialEaseOut_2, 0},
    {"linear_interpolation_raw", 2, nif_glm_linearInterpolation_2, 0},
    {"quadratic_ease_in_raw", 2, nif_glm_quadraticEaseIn_2, 0},
    {"quadratic_ease_in_out_raw", 2, nif_glm_quadraticEaseInOut_2, 0},
    {"quadratic_ease_out_raw", 2, nif_glm_quadraticEaseOut_2, 0},
    {"quartic_ease_in_raw", 2, nif_glm_quarticEaseIn_2, 0},
    {"quartic_ease_in_out_raw", 2, nif_glm_quarticEaseInOut_2, 0},
    {"quartic_ease_out_raw", 2, nif_glm_quarticEaseOut_2, 0},
    {"quintic_ease_in_raw", 2, nif_glm_quinticEaseIn_2, 0},
    {"quintic_ease_in_out_raw", 2, nif_glm_quinticEaseInOut_2, 0},
    {"quintic_ease_out_raw", 2, nif_glm_quinticEaseOut_2, 0},
    {"sine_ease_in_raw", 2, nif_glm_sineEaseIn_2, 0},
    {"sine_ease_in_out_raw", 2, nif_glm_sineEaseInOut_2, 0},
    {"sine_ease_out_raw", 2, nif_glm_sineEaseOut_2, 0}
};

ERL_NIF_INIT(
    glm_raw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
