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

static int get_glm_matrix_shape(ErlNifEnv* env, ERL_NIF_TERM arg, int* shape)
{
    if (!enif_get_int(env, arg, shape)) {
        return 0;
    }

    if (*shape < 0 || *shape > 8) {
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

static bool is_scalar_length(ErlNifEnv* env, ERL_NIF_TERM arg)
{
    return enif_is_identical(arg, enif_make_atom(env, "undefined"));
}

static size_t glm_float_type_size(int type)
{
    switch (type) {
        case GLM_FLOAT:
            return sizeof(float);
        case GLM_DOUBLE:
            return sizeof(double);
        default:
            return 0;
    }
}

static size_t glm_type_size(int type)
{
    switch (type) {
        case GLM_BOOL:
            return sizeof(uint8_t);
        case GLM_INT8:
            return sizeof(int8_t);
        case GLM_INT16:
            return sizeof(int16_t);
        case GLM_INT32:
            return sizeof(int32_t);
        case GLM_INT64:
            return sizeof(int64_t);
        case GLM_UINT8:
            return sizeof(uint8_t);
        case GLM_UINT16:
            return sizeof(uint16_t);
        case GLM_UINT32:
            return sizeof(uint32_t);
        case GLM_UINT64:
            return sizeof(uint64_t);
        case GLM_FLOAT:
            return sizeof(float);
        case GLM_DOUBLE:
            return sizeof(double);
        default:
            return 0;
    }
}

static int glm_matrix_columns(int shape)
{
    switch (shape) {
        case 0:
        case 3:
        case 4:
            return 2;
        case 1:
        case 5:
        case 6:
            return 3;
        case 2:
        case 7:
        case 8:
            return 4;
        default:
            return 0;
    }
}

static int glm_matrix_rows(int shape)
{
    switch (shape) {
        case 0:
        case 5:
        case 7:
            return 2;
        case 1:
        case 3:
        case 8:
            return 3;
        case 2:
        case 4:
        case 6:
            return 4;
        default:
            return 0;
    }
}

static size_t glm_vector_binary_size(int type, int length)
{
    return glm_type_size(type) * static_cast<size_t>(length);
}

static size_t glm_matrix_binary_size(int type, int shape)
{
    return glm_type_size(type) * static_cast<size_t>(glm_matrix_columns(shape)) * static_cast<size_t>(glm_matrix_rows(shape));
}

static size_t glm_quat_binary_size(int type)
{
    return glm_float_type_size(type) * static_cast<size_t>(4);
}

#define DISPATCH_NUMERIC_SCALAR_BINARY(OP, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_ss<int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_ss<int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_ss<int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_ss<int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_ss<uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_ss<uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_ss<uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_ss<uint64_t>(X, Y, OUT); break; \
        case GLM_FLOAT: beam_##OP##_ss<float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_ss<double>(X, Y, OUT); break; \
    }

#define DISPATCH_NUMERIC_VECTOR_SCALAR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_vs<LENGTH, int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_vs<LENGTH, int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_vs<LENGTH, int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_vs<LENGTH, int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_vs<LENGTH, uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_vs<LENGTH, uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_vs<LENGTH, uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_vs<LENGTH, uint64_t>(X, Y, OUT); break; \
        case GLM_FLOAT: beam_##OP##_vs<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vs<LENGTH, double>(X, Y, OUT); break; \
    }

#define DISPATCH_NUMERIC_VECTOR_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_vv<LENGTH, int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_vv<LENGTH, int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_vv<LENGTH, int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_vv<LENGTH, int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_vv<LENGTH, uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_vv<LENGTH, uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_vv<LENGTH, uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_vv<LENGTH, uint64_t>(X, Y, OUT); break; \
        case GLM_FLOAT: beam_##OP##_vv<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vv<LENGTH, double>(X, Y, OUT); break; \
    }

#define DISPATCH_INTEGER_SCALAR_UNARY(OP, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_s<int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_s<int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_s<int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_s<int64_t>(X, OUT); break; \
        case GLM_UINT8: beam_##OP##_s<uint8_t>(X, OUT); break; \
        case GLM_UINT16: beam_##OP##_s<uint16_t>(X, OUT); break; \
        case GLM_UINT32: beam_##OP##_s<uint32_t>(X, OUT); break; \
        case GLM_UINT64: beam_##OP##_s<uint64_t>(X, OUT); break; \
    }

#define DISPATCH_INTEGER_VECTOR_UNARY(OP, LENGTH, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_v<LENGTH, int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_v<LENGTH, int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_v<LENGTH, int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_v<LENGTH, int64_t>(X, OUT); break; \
        case GLM_UINT8: beam_##OP##_v<LENGTH, uint8_t>(X, OUT); break; \
        case GLM_UINT16: beam_##OP##_v<LENGTH, uint16_t>(X, OUT); break; \
        case GLM_UINT32: beam_##OP##_v<LENGTH, uint32_t>(X, OUT); break; \
        case GLM_UINT64: beam_##OP##_v<LENGTH, uint64_t>(X, OUT); break; \
    }

#define DISPATCH_INTEGER_SCALAR_BINARY(OP, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_ss<int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_ss<int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_ss<int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_ss<int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_ss<uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_ss<uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_ss<uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_ss<uint64_t>(X, Y, OUT); break; \
    }

#define DISPATCH_INTEGER_VECTOR_SCALAR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_vs<LENGTH, int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_vs<LENGTH, int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_vs<LENGTH, int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_vs<LENGTH, int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_vs<LENGTH, uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_vs<LENGTH, uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_vs<LENGTH, uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_vs<LENGTH, uint64_t>(X, Y, OUT); break; \
    }

#define DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_vv<LENGTH, int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_vv<LENGTH, int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_vv<LENGTH, int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_vv<LENGTH, int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_vv<LENGTH, uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_vv<LENGTH, uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_vv<LENGTH, uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_vv<LENGTH, uint64_t>(X, Y, OUT); break; \
    }

#define DISPATCH_INTEGER_SCALAR_TO_INT32_SCALAR_UNARY(OP, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_s<int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_s<int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_s<int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_s<int64_t>(X, OUT); break; \
        case GLM_UINT8: beam_##OP##_s<uint8_t>(X, OUT); break; \
        case GLM_UINT16: beam_##OP##_s<uint16_t>(X, OUT); break; \
        case GLM_UINT32: beam_##OP##_s<uint32_t>(X, OUT); break; \
        case GLM_UINT64: beam_##OP##_s<uint64_t>(X, OUT); break; \
    }

#define DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(OP, LENGTH, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_v<LENGTH, int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_v<LENGTH, int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_v<LENGTH, int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_v<LENGTH, int64_t>(X, OUT); break; \
        case GLM_UINT8: beam_##OP##_v<LENGTH, uint8_t>(X, OUT); break; \
        case GLM_UINT16: beam_##OP##_v<LENGTH, uint16_t>(X, OUT); break; \
        case GLM_UINT32: beam_##OP##_v<LENGTH, uint32_t>(X, OUT); break; \
        case GLM_UINT64: beam_##OP##_v<LENGTH, uint64_t>(X, OUT); break; \
    }

#define DISPATCH_FLOAT_SCALAR_TERNARY(OP, X, Y, A, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_sss<float>(X, Y, A, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_sss<double>(X, Y, A, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_SCALAR_TERNARY(OP, LENGTH, X, Y, A, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_vvs<LENGTH, float>(X, Y, A, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vvs<LENGTH, double>(X, Y, A, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(OP, LENGTH, X, Y, A, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_vvv<LENGTH, float>(X, Y, A, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vvv<LENGTH, double>(X, Y, A, OUT); break; \
    }

#define DISPATCH_SIGNED_OR_FLOAT_SCALAR_UNARY(OP, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_s<int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_s<int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_s<int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_s<int64_t>(X, OUT); break; \
        case GLM_FLOAT: beam_##OP##_s<float>(X, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_s<double>(X, OUT); break; \
    }

#define DISPATCH_SIGNED_OR_FLOAT_VECTOR_UNARY(OP, LENGTH, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_v<LENGTH, int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_v<LENGTH, int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_v<LENGTH, int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_v<LENGTH, int64_t>(X, OUT); break; \
        case GLM_FLOAT: beam_##OP##_v<LENGTH, float>(X, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_v<LENGTH, double>(X, OUT); break; \
    }

#define DISPATCH_FLOAT_SCALAR_BINARY(OP, X, Y, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_ss<float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_ss<double>(X, Y, OUT); break; \
    }

#define DISPATCH_FLOAT_SCALAR_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_sv<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_sv<LENGTH, double>(X, Y, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_vv<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vv<LENGTH, double>(X, Y, OUT); break; \
    }

#define DISPATCH_FLOAT_SCALAR_VECTOR_TERNARY(OP, LENGTH, X, Y, A, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_ssv<LENGTH, float>(X, Y, A, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_ssv<LENGTH, double>(X, Y, A, OUT); break; \
    }

#define DISPATCH_FLOAT_SCALAR_UNARY(OP, X, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_s<float>(X, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_s<double>(X, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_UNARY(OP, LENGTH, X, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_v<LENGTH, float>(X, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_v<LENGTH, double>(X, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_TO_SCALAR_UNARY(OP, LENGTH, X, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_v<LENGTH, float>(X, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_v<LENGTH, double>(X, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_TO_SCALAR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_vv<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vv<LENGTH, double>(X, Y, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_TO_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_vv<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vv<LENGTH, double>(X, Y, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_TO_VECTOR_TERNARY(OP, LENGTH, X, Y, Z, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_vvv<LENGTH, float>(X, Y, Z, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vvv<LENGTH, double>(X, Y, Z, OUT); break; \
    }

#define DISPATCH_FLOAT_VECTOR_VECTOR_SCALAR_TERNARY(OP, LENGTH, X, Y, Z, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_vvs<LENGTH, float>(X, Y, Z, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vvs<LENGTH, double>(X, Y, Z, OUT); break; \
    }

#define DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, COLS, ROWS, X, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_m<COLS, ROWS, float>(X, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_m<COLS, ROWS, double>(X, OUT); break; \
    }

#define DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, SIZE, X, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_m<SIZE, float>(X, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_m<SIZE, double>(X, OUT); break; \
    }

#define DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, COLS, ROWS, X, Y, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_##OP##_m<COLS, ROWS, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_m<COLS, ROWS, double>(X, Y, OUT); break; \
    }

#define DISPATCH_FLOAT_MATRIX_COLUMN_CASE(COLS, ROWS, X, INDEX, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_column_m<COLS, ROWS, float>(X, INDEX, OUT); break; \
        case GLM_DOUBLE: beam_column_m<COLS, ROWS, double>(X, INDEX, OUT); break; \
    }

#define DISPATCH_FLOAT_MATRIX_ROW_CASE(COLS, ROWS, X, INDEX, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_row_m<COLS, ROWS, float>(X, INDEX, OUT); break; \
        case GLM_DOUBLE: beam_row_m<COLS, ROWS, double>(X, INDEX, OUT); break; \
    }

#define DISPATCH_FLOAT_OUTER_PRODUCT_CASE(COLS, ROWS, X, Y, OUT) \
    switch (type) { \
        case GLM_FLOAT: beam_outer_product_vv<COLS, ROWS, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_outer_product_vv<COLS, ROWS, double>(X, Y, OUT); break; \
    }

#define DISPATCH_FLOAT_MATRIX_SHAPE_UNARY(OP, SHAPE, X, OUT) \
    switch (SHAPE) { \
        case 0: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 2, 2, X, OUT); break; \
        case 1: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 3, 3, X, OUT); break; \
        case 2: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 4, 4, X, OUT); break; \
        case 3: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 2, 3, X, OUT); break; \
        case 4: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 2, 4, X, OUT); break; \
        case 5: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 3, 2, X, OUT); break; \
        case 6: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 3, 4, X, OUT); break; \
        case 7: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 4, 2, X, OUT); break; \
        case 8: DISPATCH_FLOAT_MATRIX_UNARY_CASE(OP, 4, 3, X, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_FLOAT_SQUARE_MATRIX_SHAPE_TO_SCALAR(OP, SHAPE, X, OUT) \
    switch (SHAPE) { \
        case 0: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 2, X, OUT); break; \
        case 1: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 3, X, OUT); break; \
        case 2: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 4, X, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_FLOAT_SQUARE_MATRIX_SHAPE_UNARY(OP, SHAPE, X, OUT) \
    switch (SHAPE) { \
        case 0: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 2, X, OUT); break; \
        case 1: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 3, X, OUT); break; \
        case 2: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 4, X, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_FLOAT_AFFINE_MATRIX_SHAPE_UNARY(OP, SHAPE, X, OUT) \
    switch (SHAPE) { \
        case 1: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 3, X, OUT); break; \
        case 2: DISPATCH_FLOAT_SQUARE_MATRIX_UNARY_CASE(OP, 4, X, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_FLOAT_MATRIX_SHAPE_BINARY(OP, SHAPE, X, Y, OUT) \
    switch (SHAPE) { \
        case 0: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 2, 2, X, Y, OUT); break; \
        case 1: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 3, 3, X, Y, OUT); break; \
        case 2: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 4, 4, X, Y, OUT); break; \
        case 3: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 2, 3, X, Y, OUT); break; \
        case 4: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 2, 4, X, Y, OUT); break; \
        case 5: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 3, 2, X, Y, OUT); break; \
        case 6: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 3, 4, X, Y, OUT); break; \
        case 7: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 4, 2, X, Y, OUT); break; \
        case 8: DISPATCH_FLOAT_MATRIX_BINARY_CASE(OP, 4, 3, X, Y, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_FLOAT_MATRIX_SHAPE_COLUMN(SHAPE, X, INDEX, OUT) \
    switch (SHAPE) { \
        case 0: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(2, 2, X, INDEX, OUT); break; \
        case 1: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(3, 3, X, INDEX, OUT); break; \
        case 2: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(4, 4, X, INDEX, OUT); break; \
        case 3: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(2, 3, X, INDEX, OUT); break; \
        case 4: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(2, 4, X, INDEX, OUT); break; \
        case 5: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(3, 2, X, INDEX, OUT); break; \
        case 6: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(3, 4, X, INDEX, OUT); break; \
        case 7: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(4, 2, X, INDEX, OUT); break; \
        case 8: DISPATCH_FLOAT_MATRIX_COLUMN_CASE(4, 3, X, INDEX, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_FLOAT_MATRIX_SHAPE_ROW(SHAPE, X, INDEX, OUT) \
    switch (SHAPE) { \
        case 0: DISPATCH_FLOAT_MATRIX_ROW_CASE(2, 2, X, INDEX, OUT); break; \
        case 1: DISPATCH_FLOAT_MATRIX_ROW_CASE(3, 3, X, INDEX, OUT); break; \
        case 2: DISPATCH_FLOAT_MATRIX_ROW_CASE(4, 4, X, INDEX, OUT); break; \
        case 3: DISPATCH_FLOAT_MATRIX_ROW_CASE(2, 3, X, INDEX, OUT); break; \
        case 4: DISPATCH_FLOAT_MATRIX_ROW_CASE(2, 4, X, INDEX, OUT); break; \
        case 5: DISPATCH_FLOAT_MATRIX_ROW_CASE(3, 2, X, INDEX, OUT); break; \
        case 6: DISPATCH_FLOAT_MATRIX_ROW_CASE(3, 4, X, INDEX, OUT); break; \
        case 7: DISPATCH_FLOAT_MATRIX_ROW_CASE(4, 2, X, INDEX, OUT); break; \
        case 8: DISPATCH_FLOAT_MATRIX_ROW_CASE(4, 3, X, INDEX, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_FLOAT_MATRIX_SHAPE_OUTER_PRODUCT(SHAPE, X, Y, OUT) \
    switch (SHAPE) { \
        case 0: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(2, 2, X, Y, OUT); break; \
        case 1: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(3, 3, X, Y, OUT); break; \
        case 2: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(4, 4, X, Y, OUT); break; \
        case 3: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(2, 3, X, Y, OUT); break; \
        case 4: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(2, 4, X, Y, OUT); break; \
        case 5: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(3, 2, X, Y, OUT); break; \
        case 6: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(3, 4, X, Y, OUT); break; \
        case 7: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(4, 2, X, Y, OUT); break; \
        case 8: DISPATCH_FLOAT_OUTER_PRODUCT_CASE(4, 3, X, Y, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DISPATCH_BOOL_VECTOR_REDUCTION(OP, LENGTH, X, OUT) \
    beam_##OP##_v<LENGTH>(X, OUT)

#define DISPATCH_BOOL_VECTOR_UNARY(OP, LENGTH, X, OUT) \
    beam_##OP##_v<LENGTH>(X, OUT)

#define DISPATCH_INTEGER_SCALAR_TO_BOOL_SCALAR_UNARY(OP, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_s<int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_s<int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_s<int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_s<int64_t>(X, OUT); break; \
        case GLM_UINT8: beam_##OP##_s<uint8_t>(X, OUT); break; \
        case GLM_UINT16: beam_##OP##_s<uint16_t>(X, OUT); break; \
        case GLM_UINT32: beam_##OP##_s<uint32_t>(X, OUT); break; \
        case GLM_UINT64: beam_##OP##_s<uint64_t>(X, OUT); break; \
    }

#define DISPATCH_INTEGER_VECTOR_TO_BOOL_VECTOR_UNARY(OP, LENGTH, X, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_v<LENGTH, int8_t>(X, OUT); break; \
        case GLM_INT16: beam_##OP##_v<LENGTH, int16_t>(X, OUT); break; \
        case GLM_INT32: beam_##OP##_v<LENGTH, int32_t>(X, OUT); break; \
        case GLM_INT64: beam_##OP##_v<LENGTH, int64_t>(X, OUT); break; \
        case GLM_UINT8: beam_##OP##_v<LENGTH, uint8_t>(X, OUT); break; \
        case GLM_UINT16: beam_##OP##_v<LENGTH, uint16_t>(X, OUT); break; \
        case GLM_UINT32: beam_##OP##_v<LENGTH, uint32_t>(X, OUT); break; \
        case GLM_UINT64: beam_##OP##_v<LENGTH, uint64_t>(X, OUT); break; \
    }

#define DISPATCH_INTEGER_SCALAR_TO_BOOL_SCALAR_BINARY(OP, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_ss<int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_ss<int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_ss<int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_ss<int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_ss<uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_ss<uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_ss<uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_ss<uint64_t>(X, Y, OUT); break; \
    }

#define DISPATCH_INTEGER_VECTOR_SCALAR_TO_BOOL_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_vs<LENGTH, int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_vs<LENGTH, int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_vs<LENGTH, int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_vs<LENGTH, int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_vs<LENGTH, uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_vs<LENGTH, uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_vs<LENGTH, uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_vs<LENGTH, uint64_t>(X, Y, OUT); break; \
    }

#define DISPATCH_TYPED_VECTOR_TO_BOOL_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_BOOL: beam_##OP##_vv<LENGTH, bool>(X, Y, OUT); break; \
        case GLM_INT8: beam_##OP##_vv<LENGTH, int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_vv<LENGTH, int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_vv<LENGTH, int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_vv<LENGTH, int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_vv<LENGTH, uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_vv<LENGTH, uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_vv<LENGTH, uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_vv<LENGTH, uint64_t>(X, Y, OUT); break; \
        case GLM_FLOAT: beam_##OP##_vv<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vv<LENGTH, double>(X, Y, OUT); break; \
    }

#define DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(OP, LENGTH, X, Y, OUT) \
    switch (type) { \
        case GLM_INT8: beam_##OP##_vv<LENGTH, int8_t>(X, Y, OUT); break; \
        case GLM_INT16: beam_##OP##_vv<LENGTH, int16_t>(X, Y, OUT); break; \
        case GLM_INT32: beam_##OP##_vv<LENGTH, int32_t>(X, Y, OUT); break; \
        case GLM_INT64: beam_##OP##_vv<LENGTH, int64_t>(X, Y, OUT); break; \
        case GLM_UINT8: beam_##OP##_vv<LENGTH, uint8_t>(X, Y, OUT); break; \
        case GLM_UINT16: beam_##OP##_vv<LENGTH, uint16_t>(X, Y, OUT); break; \
        case GLM_UINT32: beam_##OP##_vv<LENGTH, uint32_t>(X, Y, OUT); break; \
        case GLM_UINT64: beam_##OP##_vv<LENGTH, uint64_t>(X, Y, OUT); break; \
        case GLM_FLOAT: beam_##OP##_vv<LENGTH, float>(X, Y, OUT); break; \
        case GLM_DOUBLE: beam_##OP##_vv<LENGTH, double>(X, Y, OUT); break; \
        default: return enif_make_badarg(env); \
    }

#define DEFINE_FLOAT_VECTOR_TO_SCALAR_UNARY_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int length; \
    if (!get_glm_length(env, argv[1], &length)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[2], &input_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    if (length == 2) { \
        DISPATCH_FLOAT_VECTOR_TO_SCALAR_UNARY(name, 2, input_bin, output_bin); \
    } else if (length == 3) { \
        DISPATCH_FLOAT_VECTOR_TO_SCALAR_UNARY(name, 3, input_bin, output_bin); \
    } else if (length == 4) { \
        DISPATCH_FLOAT_VECTOR_TO_SCALAR_UNARY(name, 4, input_bin, output_bin); \
    } else { \
        return enif_make_badarg(env); \
    } \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_VECTOR_TO_SCALAR_BINARY_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int length; \
    if (!get_glm_length(env, argv[1], &length)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input1_bin; \
    ErlNifBinary input2_bin; \
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    if (length == 2) { \
        DISPATCH_FLOAT_VECTOR_TO_SCALAR_BINARY(name, 2, input1_bin, input2_bin, output_bin); \
    } else if (length == 3) { \
        DISPATCH_FLOAT_VECTOR_TO_SCALAR_BINARY(name, 3, input1_bin, input2_bin, output_bin); \
    } else if (length == 4) { \
        DISPATCH_FLOAT_VECTOR_TO_SCALAR_BINARY(name, 4, input1_bin, input2_bin, output_bin); \
    } else { \
        return enif_make_badarg(env); \
    } \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_VECTOR_TO_VECTOR_UNARY_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int length; \
    if (!get_glm_length(env, argv[1], &length)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[2], &input_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    if (length == 2) { \
        DISPATCH_FLOAT_VECTOR_UNARY(name, 2, input_bin, output_bin); \
    } else if (length == 3) { \
        DISPATCH_FLOAT_VECTOR_UNARY(name, 3, input_bin, output_bin); \
    } else if (length == 4) { \
        DISPATCH_FLOAT_VECTOR_UNARY(name, 4, input_bin, output_bin); \
    } else { \
        return enif_make_badarg(env); \
    } \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_VECTOR_TO_VECTOR_BINARY_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int length; \
    if (!get_glm_length(env, argv[1], &length)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input1_bin; \
    ErlNifBinary input2_bin; \
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    if (length == 2) { \
        DISPATCH_FLOAT_VECTOR_TO_VECTOR_BINARY(name, 2, input1_bin, input2_bin, output_bin); \
    } else if (length == 3) { \
        DISPATCH_FLOAT_VECTOR_TO_VECTOR_BINARY(name, 3, input1_bin, input2_bin, output_bin); \
    } else if (length == 4) { \
        DISPATCH_FLOAT_VECTOR_TO_VECTOR_BINARY(name, 4, input1_bin, input2_bin, output_bin); \
    } else { \
        return enif_make_badarg(env); \
    } \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_VECTOR_TO_VECTOR_TERNARY_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int length; \
    if (!get_glm_length(env, argv[1], &length)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input1_bin; \
    ErlNifBinary input2_bin; \
    ErlNifBinary input3_bin; \
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin) || !enif_inspect_binary(env, argv[4], &input3_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    if (length == 2) { \
        DISPATCH_FLOAT_VECTOR_TO_VECTOR_TERNARY(name, 2, input1_bin, input2_bin, input3_bin, output_bin); \
    } else if (length == 3) { \
        DISPATCH_FLOAT_VECTOR_TO_VECTOR_TERNARY(name, 3, input1_bin, input2_bin, input3_bin, output_bin); \
    } else if (length == 4) { \
        DISPATCH_FLOAT_VECTOR_TO_VECTOR_TERNARY(name, 4, input1_bin, input2_bin, input3_bin, output_bin); \
    } else { \
        return enif_make_badarg(env); \
    } \
 \
    return enif_make_binary(env, &output_bin); \
}

static ERL_NIF_TERM nif_glm_is_multiple(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (pattern == 0) {
        if (input1_bin.size != glm_type_size(type) || input2_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(glm_type_size(GLM_BOOL), &output_bin)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_TO_BOOL_SCALAR_BINARY(is_multiple, input1_bin, input2_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_type_size(type)) {
                return enif_make_badarg(env);
            }
            if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
                return enif_make_badarg(env);
            }

            if (length == 2) {
                DISPATCH_INTEGER_VECTOR_SCALAR_TO_BOOL_VECTOR_BINARY(is_multiple, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_INTEGER_VECTOR_SCALAR_TO_BOOL_VECTOR_BINARY(is_multiple, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_INTEGER_VECTOR_SCALAR_TO_BOOL_VECTOR_BINARY(is_multiple, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
                return enif_make_badarg(env);
            }
            if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
                return enif_make_badarg(env);
            }

            if (length == 2) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(is_multiple, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(is_multiple, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(is_multiple, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_is_power_of_two(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (is_scalar_length(env, argv[1])) {
        if (input_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(glm_type_size(GLM_BOOL), &output_bin)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_TO_BOOL_SCALAR_UNARY(is_power_of_two, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_INTEGER_VECTOR_TO_BOOL_VECTOR_UNARY(is_power_of_two, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_INTEGER_VECTOR_TO_BOOL_VECTOR_UNARY(is_power_of_two, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_INTEGER_VECTOR_TO_BOOL_VECTOR_UNARY(is_power_of_two, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_next_multiple(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        if (input1_bin.size != glm_type_size(type) || input2_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_BINARY(next_multiple, input1_bin, input2_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_type_size(type)) {
                return enif_make_badarg(env);
            }

            if (length == 2) {
                DISPATCH_INTEGER_VECTOR_SCALAR_BINARY(next_multiple, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_INTEGER_VECTOR_SCALAR_BINARY(next_multiple, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_INTEGER_VECTOR_SCALAR_BINARY(next_multiple, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
                return enif_make_badarg(env);
            }

            if (length == 2) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(next_multiple, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(next_multiple, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(next_multiple, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_next_power_of_two(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
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

    if (is_scalar_length(env, argv[1])) {
        if (input_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_UNARY(next_power_of_two, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_INTEGER_VECTOR_UNARY(next_power_of_two, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_INTEGER_VECTOR_UNARY(next_power_of_two, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_INTEGER_VECTOR_UNARY(next_power_of_two, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_prev_multiple(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        if (input1_bin.size != glm_type_size(type) || input2_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_BINARY(prev_multiple, input1_bin, input2_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_type_size(type)) {
                return enif_make_badarg(env);
            }

            if (length == 2) {
                DISPATCH_INTEGER_VECTOR_SCALAR_BINARY(prev_multiple, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_INTEGER_VECTOR_SCALAR_BINARY(prev_multiple, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_INTEGER_VECTOR_SCALAR_BINARY(prev_multiple, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
                return enif_make_badarg(env);
            }

            if (length == 2) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(prev_multiple, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(prev_multiple, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_INTEGER_VECTOR_VECTOR_BINARY(prev_multiple, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_prev_power_of_two(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
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

    if (is_scalar_length(env, argv[1])) {
        if (input_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_UNARY(prev_power_of_two, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_INTEGER_VECTOR_UNARY(prev_power_of_two, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_INTEGER_VECTOR_UNARY(prev_power_of_two, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_INTEGER_VECTOR_UNARY(prev_power_of_two, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_abs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_FLOAT && type != GLM_DOUBLE) {
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

    if (is_scalar_length(env, argv[1])) {
        switch (type) {
            case GLM_INT8:
                beam_abs_s<int8_t>(input_bin, output_bin);
                break;
            case GLM_INT16:
                beam_abs_s<int16_t>(input_bin, output_bin);
                break;
            case GLM_INT32:
                beam_abs_s<int32_t>(input_bin, output_bin);
                break;
            case GLM_INT64:
                beam_abs_s<int64_t>(input_bin, output_bin);
                break;
            case GLM_FLOAT:
                beam_abs_s<float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_abs_s<double>(input_bin, output_bin);
                break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_INT8:
                    beam_abs_v<2, int8_t>(input_bin, output_bin);
                    break;
                case GLM_INT16:
                    beam_abs_v<2, int16_t>(input_bin, output_bin);
                    break;
                case GLM_INT32:
                    beam_abs_v<2, int32_t>(input_bin, output_bin);
                    break;
                case GLM_INT64:
                    beam_abs_v<2, int64_t>(input_bin, output_bin);
                    break;
                case GLM_FLOAT:
                    beam_abs_v<2, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_abs_v<2, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_INT8:
                    beam_abs_v<3, int8_t>(input_bin, output_bin);
                    break;
                case GLM_INT16:
                    beam_abs_v<3, int16_t>(input_bin, output_bin);
                    break;
                case GLM_INT32:
                    beam_abs_v<3, int32_t>(input_bin, output_bin);
                    break;
                case GLM_INT64:
                    beam_abs_v<3, int64_t>(input_bin, output_bin);
                    break;
                case GLM_FLOAT:
                    beam_abs_v<3, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_abs_v<3, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_INT8:
                    beam_abs_v<4, int8_t>(input_bin, output_bin);
                    break;
                case GLM_INT16:
                    beam_abs_v<4, int16_t>(input_bin, output_bin);
                    break;
                case GLM_INT32:
                    beam_abs_v<4, int32_t>(input_bin, output_bin);
                    break;
                case GLM_INT64:
                    beam_abs_v<4, int64_t>(input_bin, output_bin);
                    break;
                case GLM_FLOAT:
                    beam_abs_v<4, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_abs_v<4, double>(input_bin, output_bin);
                    break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_ceil(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
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

    if (is_scalar_length(env, argv[1])) {
        switch (type) {
            case GLM_FLOAT:
                beam_ceil_s<float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_ceil_s<double>(input_bin, output_bin);
                break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_FLOAT:
                    beam_ceil_v<2, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_ceil_v<2, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_FLOAT:
                    beam_ceil_v<3, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_ceil_v<3, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_FLOAT:
                    beam_ceil_v<4, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_ceil_v<4, double>(input_bin, output_bin);
                    break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
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

static ERL_NIF_TERM nif_glm_floor(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
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

    if (is_scalar_length(env, argv[1])) {
        switch (type) {
            case GLM_FLOAT:
                beam_floor_s<float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_floor_s<double>(input_bin, output_bin);
                break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_FLOAT:
                    beam_floor_v<2, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_floor_v<2, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_FLOAT:
                    beam_floor_v<3, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_floor_v<3, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_FLOAT:
                    beam_floor_v<4, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_floor_v<4, double>(input_bin, output_bin);
                    break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_float_bits_to_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT) {
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

    if (is_scalar_length(env, argv[1])) {
        beam_float_bits_to_int_s(input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_float_bits_to_int_v<2>(input_bin, output_bin);
        } else if (length == 3) {
            beam_float_bits_to_int_v<3>(input_bin, output_bin);
        } else if (length == 4) {
            beam_float_bits_to_int_v<4>(input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_float_bits_to_uint(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT) {
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

    if (is_scalar_length(env, argv[1])) {
        beam_float_bits_to_uint_s(input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_float_bits_to_uint_v<2>(input_bin, output_bin);
        } else if (length == 3) {
            beam_float_bits_to_uint_v<3>(input_bin, output_bin);
        } else if (length == 4) {
            beam_float_bits_to_uint_v<4>(input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_fract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
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

    if (is_scalar_length(env, argv[1])) {
        switch (type) {
            case GLM_FLOAT:
                beam_fract_s<float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_fract_s<double>(input_bin, output_bin);
                break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_FLOAT:
                    beam_fract_v<2, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_fract_v<2, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_FLOAT:
                    beam_fract_v<3, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_fract_v<3, double>(input_bin, output_bin);
                    break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_FLOAT:
                    beam_fract_v<4, float>(input_bin, output_bin);
                    break;
                case GLM_DOUBLE:
                    beam_fract_v<4, double>(input_bin, output_bin);
                    break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_frexp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary significand_bin;
    ErlNifBinary exponent_bin;

    if (is_scalar_length(env, argv[1])) {
        if (!enif_alloc_binary(input_bin.size, &significand_bin) || !enif_alloc_binary(sizeof(int32_t), &exponent_bin)) {
            return enif_make_badarg(env);
        }

        if (type == GLM_FLOAT) {
            beam_frexp_s<float>(input_bin, significand_bin, exponent_bin);
        } else {
            beam_frexp_s<double>(input_bin, significand_bin, exponent_bin);
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (!enif_alloc_binary(input_bin.size, &significand_bin) || !enif_alloc_binary(length * sizeof(int32_t), &exponent_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            if (type == GLM_FLOAT) {
                beam_frexp_v<2, float>(input_bin, significand_bin, exponent_bin);
            } else {
                beam_frexp_v<2, double>(input_bin, significand_bin, exponent_bin);
            }
        } else if (length == 3) {
            if (type == GLM_FLOAT) {
                beam_frexp_v<3, float>(input_bin, significand_bin, exponent_bin);
            } else {
                beam_frexp_v<3, double>(input_bin, significand_bin, exponent_bin);
            }
        } else if (length == 4) {
            if (type == GLM_FLOAT) {
                beam_frexp_v<4, float>(input_bin, significand_bin, exponent_bin);
            } else {
                beam_frexp_v<4, double>(input_bin, significand_bin, exponent_bin);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_tuple2(env, enif_make_binary(env, &significand_bin), enif_make_binary(env, &exponent_bin));
}

static ERL_NIF_TERM nif_glm_int_bits_to_float(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT32) {
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

    if (is_scalar_length(env, argv[1])) {
        beam_int_bits_to_float_s(input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_int_bits_to_float_v<2>(input_bin, output_bin);
        } else if (length == 3) {
            beam_int_bits_to_float_v<3>(input_bin, output_bin);
        } else if (length == 4) {
            beam_int_bits_to_float_v<4>(input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_is_inf(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
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
    if (!enif_alloc_binary(length, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (length == 2) {
        if (type == GLM_FLOAT) {
            beam_is_inf_v<2, float>(input_bin, output_bin);
        } else {
            beam_is_inf_v<2, double>(input_bin, output_bin);
        }
    } else if (length == 3) {
        if (type == GLM_FLOAT) {
            beam_is_inf_v<3, float>(input_bin, output_bin);
        } else {
            beam_is_inf_v<3, double>(input_bin, output_bin);
        }
    } else if (length == 4) {
        if (type == GLM_FLOAT) {
            beam_is_inf_v<4, float>(input_bin, output_bin);
        } else {
            beam_is_inf_v<4, double>(input_bin, output_bin);
        }
    } else {
        return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_is_nan(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
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
    if (!enif_alloc_binary(length, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (length == 2) {
        if (type == GLM_FLOAT) {
            beam_is_nan_v<2, float>(input_bin, output_bin);
        } else {
            beam_is_nan_v<2, double>(input_bin, output_bin);
        }
    } else if (length == 3) {
        if (type == GLM_FLOAT) {
            beam_is_nan_v<3, float>(input_bin, output_bin);
        } else {
            beam_is_nan_v<3, double>(input_bin, output_bin);
        }
    } else if (length == 4) {
        if (type == GLM_FLOAT) {
            beam_is_nan_v<4, float>(input_bin, output_bin);
        } else {
            beam_is_nan_v<4, double>(input_bin, output_bin);
        }
    } else {
        return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_ldexp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (is_scalar_length(env, argv[1])) {
        if (type == GLM_FLOAT) {
            beam_ldexp_s<float>(input1_bin, input2_bin, output_bin);
        } else {
            beam_ldexp_s<double>(input1_bin, input2_bin, output_bin);
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            if (type == GLM_FLOAT) {
                beam_ldexp_v<2, float>(input1_bin, input2_bin, output_bin);
            } else {
                beam_ldexp_v<2, double>(input1_bin, input2_bin, output_bin);
            }
        } else if (length == 3) {
            if (type == GLM_FLOAT) {
                beam_ldexp_v<3, float>(input1_bin, input2_bin, output_bin);
            } else {
                beam_ldexp_v<3, double>(input1_bin, input2_bin, output_bin);
            }
        } else if (length == 4) {
            if (type == GLM_FLOAT) {
                beam_ldexp_v<4, float>(input1_bin, input2_bin, output_bin);
            } else {
                beam_ldexp_v<4, double>(input1_bin, input2_bin, output_bin);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_max(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        DISPATCH_NUMERIC_SCALAR_BINARY(max, input1_bin, input2_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (length == 2) {
                DISPATCH_NUMERIC_VECTOR_SCALAR_BINARY(max, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_NUMERIC_VECTOR_SCALAR_BINARY(max, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_NUMERIC_VECTOR_SCALAR_BINARY(max, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (length == 2) {
                DISPATCH_NUMERIC_VECTOR_VECTOR_BINARY(max, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_NUMERIC_VECTOR_VECTOR_BINARY(max, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_NUMERIC_VECTOR_VECTOR_BINARY(max, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_min(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
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
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        DISPATCH_NUMERIC_SCALAR_BINARY(min, input1_bin, input2_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (length == 2) {
                DISPATCH_NUMERIC_VECTOR_SCALAR_BINARY(min, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_NUMERIC_VECTOR_SCALAR_BINARY(min, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_NUMERIC_VECTOR_SCALAR_BINARY(min, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (length == 2) {
                DISPATCH_NUMERIC_VECTOR_VECTOR_BINARY(min, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_NUMERIC_VECTOR_VECTOR_BINARY(min, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_NUMERIC_VECTOR_VECTOR_BINARY(min, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_mix(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    ErlNifBinary input3_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin) || !enif_inspect_binary(env, argv[5], &input3_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        DISPATCH_FLOAT_SCALAR_TERNARY(mix, input1_bin, input2_bin, input3_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (length == 2) {
                DISPATCH_FLOAT_VECTOR_SCALAR_TERNARY(mix, 2, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_FLOAT_VECTOR_SCALAR_TERNARY(mix, 3, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_FLOAT_VECTOR_SCALAR_TERNARY(mix, 4, input1_bin, input2_bin, input3_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (length == 2) {
                DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(mix, 2, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(mix, 3, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(mix, 4, input1_bin, input2_bin, input3_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_mod(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        switch (type) {
            case GLM_FLOAT:
                beam_mod_ss<float>(input1_bin, input2_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_mod_ss<double>(input1_bin, input2_bin, output_bin);
                break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (length == 2) {
                switch (type) {
                    case GLM_FLOAT: beam_mod_vs<2, float>(input1_bin, input2_bin, output_bin); break;
                    case GLM_DOUBLE: beam_mod_vs<2, double>(input1_bin, input2_bin, output_bin); break;
                }
            } else if (length == 3) {
                switch (type) {
                    case GLM_FLOAT: beam_mod_vs<3, float>(input1_bin, input2_bin, output_bin); break;
                    case GLM_DOUBLE: beam_mod_vs<3, double>(input1_bin, input2_bin, output_bin); break;
                }
            } else if (length == 4) {
                switch (type) {
                    case GLM_FLOAT: beam_mod_vs<4, float>(input1_bin, input2_bin, output_bin); break;
                    case GLM_DOUBLE: beam_mod_vs<4, double>(input1_bin, input2_bin, output_bin); break;
                }
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (length == 2) {
                switch (type) {
                    case GLM_FLOAT: beam_mod_vv<2, float>(input1_bin, input2_bin, output_bin); break;
                    case GLM_DOUBLE: beam_mod_vv<2, double>(input1_bin, input2_bin, output_bin); break;
                }
            } else if (length == 3) {
                switch (type) {
                    case GLM_FLOAT: beam_mod_vv<3, float>(input1_bin, input2_bin, output_bin); break;
                    case GLM_DOUBLE: beam_mod_vv<3, double>(input1_bin, input2_bin, output_bin); break;
                }
            } else if (length == 4) {
                switch (type) {
                    case GLM_FLOAT: beam_mod_vv<4, float>(input1_bin, input2_bin, output_bin); break;
                    case GLM_DOUBLE: beam_mod_vv<4, double>(input1_bin, input2_bin, output_bin); break;
                }
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_modf(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary fractional_bin;
    ErlNifBinary integral_bin;
    if (!enif_alloc_binary(input_bin.size, &fractional_bin) || !enif_alloc_binary(input_bin.size, &integral_bin)) {
        return enif_make_badarg(env);
    }

    if (is_scalar_length(env, argv[1])) {
        switch (type) {
            case GLM_FLOAT:
                beam_modf_s<float>(input_bin, fractional_bin, integral_bin);
                break;
            case GLM_DOUBLE:
                beam_modf_s<double>(input_bin, fractional_bin, integral_bin);
                break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_FLOAT: beam_modf_v<2, float>(input_bin, fractional_bin, integral_bin); break;
                case GLM_DOUBLE: beam_modf_v<2, double>(input_bin, fractional_bin, integral_bin); break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_FLOAT: beam_modf_v<3, float>(input_bin, fractional_bin, integral_bin); break;
                case GLM_DOUBLE: beam_modf_v<3, double>(input_bin, fractional_bin, integral_bin); break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_FLOAT: beam_modf_v<4, float>(input_bin, fractional_bin, integral_bin); break;
                case GLM_DOUBLE: beam_modf_v<4, double>(input_bin, fractional_bin, integral_bin); break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_tuple2(env, enif_make_binary(env, &fractional_bin), enif_make_binary(env, &integral_bin));
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

static ERL_NIF_TERM nif_glm_sign(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_FLOAT && type != GLM_DOUBLE) {
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

    if (is_scalar_length(env, argv[1])) {
        DISPATCH_SIGNED_OR_FLOAT_SCALAR_UNARY(sign, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_SIGNED_OR_FLOAT_VECTOR_UNARY(sign, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_SIGNED_OR_FLOAT_VECTOR_UNARY(sign, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_SIGNED_OR_FLOAT_VECTOR_UNARY(sign, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_smoothstep(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    ErlNifBinary input3_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin) || !enif_inspect_binary(env, argv[5], &input3_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    size_t output_size = pattern == 0 ? input1_bin.size : input3_bin.size;
    if (!enif_alloc_binary(output_size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        DISPATCH_FLOAT_SCALAR_TERNARY(smoothstep, input1_bin, input2_bin, input3_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (length == 2) {
                DISPATCH_FLOAT_SCALAR_VECTOR_TERNARY(smoothstep, 2, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_FLOAT_SCALAR_VECTOR_TERNARY(smoothstep, 3, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_FLOAT_SCALAR_VECTOR_TERNARY(smoothstep, 4, input1_bin, input2_bin, input3_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (length == 2) {
                DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(smoothstep, 2, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(smoothstep, 3, input1_bin, input2_bin, input3_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(smoothstep, 4, input1_bin, input2_bin, input3_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_step(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int pattern;
    if (!enif_get_int(env, argv[2], &pattern)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[3], &input1_bin) || !enif_inspect_binary(env, argv[4], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    size_t output_size = pattern == 0 ? input1_bin.size : input2_bin.size;
    if (!enif_alloc_binary(output_size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (pattern == 0) {
        DISPATCH_FLOAT_SCALAR_BINARY(step, input1_bin, input2_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (pattern == 1) {
            if (length == 2) {
                DISPATCH_FLOAT_SCALAR_VECTOR_BINARY(step, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_FLOAT_SCALAR_VECTOR_BINARY(step, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_FLOAT_SCALAR_VECTOR_BINARY(step, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else if (pattern == 2) {
            if (length == 2) {
                DISPATCH_FLOAT_VECTOR_VECTOR_BINARY(step, 2, input1_bin, input2_bin, output_bin);
            } else if (length == 3) {
                DISPATCH_FLOAT_VECTOR_VECTOR_BINARY(step, 3, input1_bin, input2_bin, output_bin);
            } else if (length == 4) {
                DISPATCH_FLOAT_VECTOR_VECTOR_BINARY(step, 4, input1_bin, input2_bin, output_bin);
            } else {
                return enif_make_badarg(env);
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_trunc(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
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

    if (is_scalar_length(env, argv[1])) {
        switch (type) {
            case GLM_FLOAT:
                beam_trunc_s<float>(input_bin, output_bin);
                break;
            case GLM_DOUBLE:
                beam_trunc_s<double>(input_bin, output_bin);
                break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_FLOAT: beam_trunc_v<2, float>(input_bin, output_bin); break;
                case GLM_DOUBLE: beam_trunc_v<2, double>(input_bin, output_bin); break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_FLOAT: beam_trunc_v<3, float>(input_bin, output_bin); break;
                case GLM_DOUBLE: beam_trunc_v<3, double>(input_bin, output_bin); break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_FLOAT: beam_trunc_v<4, float>(input_bin, output_bin); break;
                case GLM_DOUBLE: beam_trunc_v<4, double>(input_bin, output_bin); break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_uint_bits_to_float(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_UINT32) {
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

    if (is_scalar_length(env, argv[1])) {
        beam_uint_bits_to_float_s(input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_uint_bits_to_float_v<2>(input_bin, output_bin);
        } else if (length == 3) {
            beam_uint_bits_to_float_v<3>(input_bin, output_bin);
        } else if (length == 4) {
            beam_uint_bits_to_float_v<4>(input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

template<typename T>
using EaseFunc2 = void(*)(const ErlNifBinary&, ErlNifBinary&);

template<typename T>
using EaseFunc3 = void(*)(const ErlNifBinary&, const ErlNifBinary&, ErlNifBinary&);

template<typename T>
inline bool nif_glm_ease_argument_in_range(const ErlNifBinary& input);

template<typename T>
inline bool nif_glm_scalar_binary_matches_type(const ErlNifBinary& input);

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
            if (!nif_glm_ease_argument_in_range<float>(input_bin)) {
                return enif_make_badarg(env);
            }
            float_func(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            if (!nif_glm_ease_argument_in_range<double>(input_bin)) {
                return enif_make_badarg(env);
            }
            double_func(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

template<typename T>
inline bool nif_glm_ease_argument_in_range(const ErlNifBinary& input)
{
    if (input.size != sizeof(T)) {
        return false;
    }

    const T value = *(const T*)input.data;
    return value >= static_cast<T>(0) && value <= static_cast<T>(1);
}

template<typename T>
inline bool nif_glm_scalar_binary_matches_type(const ErlNifBinary& input)
{
    return input.size == sizeof(T);
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
            if (!nif_glm_ease_argument_in_range<float>(input1_bin) || !nif_glm_scalar_binary_matches_type<float>(input2_bin)) {
                return enif_make_badarg(env);
            }
            float_func(input1_bin, input2_bin, output_bin);
            break;
        case GLM_DOUBLE:
            if (!nif_glm_ease_argument_in_range<double>(input1_bin) || !nif_glm_scalar_binary_matches_type<double>(input2_bin)) {
                return enif_make_badarg(env);
            }
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
DEFINE_EASE_3ARG(backEaseIn)
DEFINE_EASE_2ARG(backEaseInOut)
DEFINE_EASE_3ARG(backEaseInOut)
DEFINE_EASE_2ARG(backEaseOut)
DEFINE_EASE_3ARG(backEaseOut)
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

#define DEFINE_FLOAT_UNARY_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[2], &input_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    if (is_scalar_length(env, argv[1])) { \
        DISPATCH_FLOAT_SCALAR_UNARY(name, input_bin, output_bin); \
    } else { \
        int length; \
        if (!get_glm_length(env, argv[1], &length)) { \
            return enif_make_badarg(env); \
        } \
 \
        if (length == 2) { \
            DISPATCH_FLOAT_VECTOR_UNARY(name, 2, input_bin, output_bin); \
        } else if (length == 3) { \
            DISPATCH_FLOAT_VECTOR_UNARY(name, 3, input_bin, output_bin); \
        } else if (length == 4) { \
            DISPATCH_FLOAT_VECTOR_UNARY(name, 4, input_bin, output_bin); \
        } else { \
            return enif_make_badarg(env); \
        } \
    } \
 \
    return enif_make_binary(env, &output_bin); \
}

DEFINE_FLOAT_UNARY_NIF(exp)
DEFINE_FLOAT_UNARY_NIF(exp2)
DEFINE_FLOAT_UNARY_NIF(inverse_sqrt)
DEFINE_FLOAT_UNARY_NIF(log)
DEFINE_FLOAT_UNARY_NIF(log2)
DEFINE_FLOAT_UNARY_NIF(sqrt)
DEFINE_FLOAT_UNARY_NIF(acos)
DEFINE_FLOAT_UNARY_NIF(acosh)
DEFINE_FLOAT_UNARY_NIF(asin)
DEFINE_FLOAT_UNARY_NIF(asinh)
DEFINE_FLOAT_UNARY_NIF(atan)
DEFINE_FLOAT_UNARY_NIF(atanh)
DEFINE_FLOAT_UNARY_NIF(cos)
DEFINE_FLOAT_UNARY_NIF(cosh)
DEFINE_FLOAT_UNARY_NIF(degrees)
DEFINE_FLOAT_UNARY_NIF(radians)
DEFINE_FLOAT_UNARY_NIF(sin)
DEFINE_FLOAT_UNARY_NIF(sinh)
DEFINE_FLOAT_UNARY_NIF(tan)
DEFINE_FLOAT_UNARY_NIF(tanh)

DEFINE_FLOAT_VECTOR_TO_SCALAR_UNARY_NIF(length)
DEFINE_FLOAT_VECTOR_TO_SCALAR_BINARY_NIF(distance)
DEFINE_FLOAT_VECTOR_TO_SCALAR_BINARY_NIF(dot)
DEFINE_FLOAT_VECTOR_TO_VECTOR_UNARY_NIF(normalize)
DEFINE_FLOAT_VECTOR_TO_VECTOR_BINARY_NIF(reflect)
DEFINE_FLOAT_VECTOR_TO_VECTOR_TERNARY_NIF(face_forward)

#define DEFINE_FLOAT_MATRIX_UNARY_SAME_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int shape; \
    if (!get_glm_matrix_shape(env, argv[1], &shape)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[2], &input_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    DISPATCH_FLOAT_MATRIX_SHAPE_UNARY(name, shape, input_bin, output_bin); \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_SQUARE_MATRIX_UNARY_TO_SCALAR_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int shape; \
    if (!get_glm_matrix_shape(env, argv[1], &shape)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[2], &input_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    DISPATCH_FLOAT_SQUARE_MATRIX_SHAPE_TO_SCALAR(name, shape, input_bin, output_bin); \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_SQUARE_MATRIX_UNARY_SAME_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int shape; \
    if (!get_glm_matrix_shape(env, argv[1], &shape)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[2], &input_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    DISPATCH_FLOAT_SQUARE_MATRIX_SHAPE_UNARY(name, shape, input_bin, output_bin); \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_AFFINE_MATRIX_UNARY_SAME_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int shape; \
    if (!get_glm_matrix_shape(env, argv[1], &shape)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[2], &input_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    DISPATCH_FLOAT_AFFINE_MATRIX_SHAPE_UNARY(name, shape, input_bin, output_bin); \
 \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_FLOAT_MATRIX_BINARY_SAME_NIF(name) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type)) { \
        return enif_make_badarg(env); \
    } \
    if (type != GLM_FLOAT && type != GLM_DOUBLE) { \
        return enif_make_badarg(env); \
    } \
 \
    int shape; \
    if (!get_glm_matrix_shape(env, argv[1], &shape)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input1_bin; \
    ErlNifBinary input2_bin; \
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    DISPATCH_FLOAT_MATRIX_SHAPE_BINARY(name, shape, input1_bin, input2_bin, output_bin); \
 \
    return enif_make_binary(env, &output_bin); \
}
DEFINE_FLOAT_AFFINE_MATRIX_UNARY_SAME_NIF(affine_inverse)
DEFINE_FLOAT_SQUARE_MATRIX_UNARY_TO_SCALAR_NIF(determinant)
DEFINE_FLOAT_SQUARE_MATRIX_UNARY_SAME_NIF(inverse)
DEFINE_FLOAT_SQUARE_MATRIX_UNARY_SAME_NIF(inverse_transpose)
DEFINE_FLOAT_MATRIX_BINARY_SAME_NIF(matrix_comp_mult)
DEFINE_FLOAT_MATRIX_UNARY_SAME_NIF(transpose)

static ERL_NIF_TERM nif_glm_column(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int shape;
    if (!get_glm_matrix_shape(env, argv[1], &shape)) {
        return enif_make_badarg(env);
    }

    int index;
    if (!enif_get_int(env, argv[2], &index)) {
        return enif_make_badarg(env);
    }

    int columns = glm_matrix_columns(shape);
    if (index < 1 || index > columns) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[3], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(type, glm_matrix_rows(shape)), &output_bin)) {
        return enif_make_badarg(env);
    }

    DISPATCH_FLOAT_MATRIX_SHAPE_COLUMN(shape, input_bin, index - 1, output_bin);

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_outer_product(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int shape;
    if (!get_glm_matrix_shape(env, argv[1], &shape)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, shape), &output_bin)) {
        return enif_make_badarg(env);
    }

    DISPATCH_FLOAT_MATRIX_SHAPE_OUTER_PRODUCT(shape, input1_bin, input2_bin, output_bin);

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_row(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int shape;
    if (!get_glm_matrix_shape(env, argv[1], &shape)) {
        return enif_make_badarg(env);
    }

    int index;
    if (!enif_get_int(env, argv[2], &index)) {
        return enif_make_badarg(env);
    }

    int rows = glm_matrix_rows(shape);
    if (index < 1 || index > rows) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[3], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(type, glm_matrix_columns(shape)), &output_bin)) {
        return enif_make_badarg(env);
    }

    DISPATCH_FLOAT_MATRIX_SHAPE_ROW(shape, input_bin, index - 1, output_bin);

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_pack_double_2x32(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type) || type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_vector_binary_size(GLM_UINT32, 2)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(sizeof(double), &output_bin)) {
        return enif_make_badarg(env);
    }

    beam_pack_double_2x32(input_bin, output_bin);
    return enif_make_binary(env, &output_bin);
}

#define DEFINE_PACK_FLOAT_TO_INTEGER_NIF(name, input_size, output_type) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type) || type != GLM_FLOAT) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != (input_size)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary(sizeof(output_type), &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    beam_##name(input_bin, output_bin); \
    return enif_make_binary(env, &output_bin); \
}

#define DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(name, input_type, output_size) \
static ERL_NIF_TERM nif_glm_##name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) \
{ \
    int type; \
    if (!get_glm_type(env, argv[0], &type) || type != GLM_FLOAT) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary input_bin; \
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != sizeof(input_type)) { \
        return enif_make_badarg(env); \
    } \
 \
    ErlNifBinary output_bin; \
    if (!enif_alloc_binary((output_size), &output_bin)) { \
        return enif_make_badarg(env); \
    } \
 \
    beam_##name(input_bin, output_bin); \
    return enif_make_binary(env, &output_bin); \
}

DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_half_1x16, sizeof(float), uint16_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_half_2x16, glm_vector_binary_size(GLM_FLOAT, 2), uint32_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_half_4x16, glm_vector_binary_size(GLM_FLOAT, 4), uint64_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_snorm_1x8, sizeof(float), uint8_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_snorm_2x8, glm_vector_binary_size(GLM_FLOAT, 2), uint16_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_snorm_1x16, sizeof(float), uint16_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_snorm_2x16, glm_vector_binary_size(GLM_FLOAT, 2), uint32_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_snorm_4x8, glm_vector_binary_size(GLM_FLOAT, 4), uint32_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_snorm_4x16, glm_vector_binary_size(GLM_FLOAT, 4), uint64_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_unorm_1x8, sizeof(float), uint8_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_unorm_2x8, glm_vector_binary_size(GLM_FLOAT, 2), uint16_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_unorm_1x16, sizeof(float), uint16_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_unorm_2x16, glm_vector_binary_size(GLM_FLOAT, 2), uint32_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_unorm_4x8, glm_vector_binary_size(GLM_FLOAT, 4), uint32_t)
DEFINE_PACK_FLOAT_TO_INTEGER_NIF(pack_unorm_4x16, glm_vector_binary_size(GLM_FLOAT, 4), uint64_t)

static ERL_NIF_TERM nif_glm_unpack_double_2x32(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type) || type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != sizeof(double)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_UINT32, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    beam_unpack_double_2x32(input_bin, output_bin);
    return enif_make_binary(env, &output_bin);
}

DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_half_1x16, uint16_t, sizeof(float))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_half_2x16, uint32_t, glm_vector_binary_size(GLM_FLOAT, 2))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_half_4x16, uint64_t, glm_vector_binary_size(GLM_FLOAT, 4))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_snorm_1x8, uint8_t, sizeof(float))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_snorm_2x8, uint16_t, glm_vector_binary_size(GLM_FLOAT, 2))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_snorm_1x16, uint16_t, sizeof(float))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_snorm_2x16, uint32_t, glm_vector_binary_size(GLM_FLOAT, 2))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_snorm_4x8, uint32_t, glm_vector_binary_size(GLM_FLOAT, 4))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_snorm_4x16, uint64_t, glm_vector_binary_size(GLM_FLOAT, 4))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_unorm_1x8, uint8_t, sizeof(float))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_unorm_2x8, uint16_t, glm_vector_binary_size(GLM_FLOAT, 2))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_unorm_1x16, uint16_t, sizeof(float))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_unorm_2x16, uint32_t, glm_vector_binary_size(GLM_FLOAT, 2))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_unorm_4x8, uint32_t, glm_vector_binary_size(GLM_FLOAT, 4))
DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF(unpack_unorm_4x16, uint64_t, glm_vector_binary_size(GLM_FLOAT, 4))

#undef DEFINE_PACK_FLOAT_TO_INTEGER_NIF
#undef DEFINE_UNPACK_FLOAT_FROM_INTEGER_NIF

#undef DEFINE_FLOAT_UNARY_NIF

static ERL_NIF_TERM nif_glm_cross(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length) || length != 3) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_cross_vv<float>(input1_bin, input2_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_cross_vv<double>(input1_bin, input2_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_refract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    ErlNifBinary input3_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin) || !enif_inspect_binary(env, argv[4], &input3_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (length == 2) {
        DISPATCH_FLOAT_VECTOR_VECTOR_SCALAR_TERNARY(refract, 2, input1_bin, input2_bin, input3_bin, output_bin);
    } else if (length == 3) {
        DISPATCH_FLOAT_VECTOR_VECTOR_SCALAR_TERNARY(refract, 3, input1_bin, input2_bin, input3_bin, output_bin);
    } else if (length == 4) {
        DISPATCH_FLOAT_VECTOR_VECTOR_SCALAR_TERNARY(refract, 4, input1_bin, input2_bin, input3_bin, output_bin);
    } else {
        return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_all(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int length;
    if (!get_glm_length(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_vector_binary_size(GLM_BOOL, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_type_size(GLM_BOOL), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_BOOL_VECTOR_REDUCTION(all, 2, input_bin, output_bin);
            break;
        case 3:
            DISPATCH_BOOL_VECTOR_REDUCTION(all, 3, input_bin, output_bin);
            break;
        case 4:
            DISPATCH_BOOL_VECTOR_REDUCTION(all, 4, input_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_any(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int length;
    if (!get_glm_length(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_vector_binary_size(GLM_BOOL, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_type_size(GLM_BOOL), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_BOOL_VECTOR_REDUCTION(any, 2, input_bin, output_bin);
            break;
        case 3:
            DISPATCH_BOOL_VECTOR_REDUCTION(any, 3, input_bin, output_bin);
            break;
        case 4:
            DISPATCH_BOOL_VECTOR_REDUCTION(any, 4, input_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_TYPED_VECTOR_TO_BOOL_VECTOR_BINARY(equal, 2, input1_bin, input2_bin, output_bin);
            break;
        case 3:
            DISPATCH_TYPED_VECTOR_TO_BOOL_VECTOR_BINARY(equal, 3, input1_bin, input2_bin, output_bin);
            break;
        case 4:
            DISPATCH_TYPED_VECTOR_TO_BOOL_VECTOR_BINARY(equal, 4, input1_bin, input2_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_greater_than(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type) || type == GLM_BOOL) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(greater_than, 2, input1_bin, input2_bin, output_bin);
            break;
        case 3:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(greater_than, 3, input1_bin, input2_bin, output_bin);
            break;
        case 4:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(greater_than, 4, input1_bin, input2_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_greater_than_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type) || type == GLM_BOOL) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(greater_than_equal, 2, input1_bin, input2_bin, output_bin);
            break;
        case 3:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(greater_than_equal, 3, input1_bin, input2_bin, output_bin);
            break;
        case 4:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(greater_than_equal, 4, input1_bin, input2_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_less_than(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type) || type == GLM_BOOL) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(less_than, 2, input1_bin, input2_bin, output_bin);
            break;
        case 3:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(less_than, 3, input1_bin, input2_bin, output_bin);
            break;
        case 4:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(less_than, 4, input1_bin, input2_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_less_than_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type) || type == GLM_BOOL) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(less_than_equal, 2, input1_bin, input2_bin, output_bin);
            break;
        case 3:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(less_than_equal, 3, input1_bin, input2_bin, output_bin);
            break;
        case 4:
            DISPATCH_ORDERED_VECTOR_TO_BOOL_VECTOR_BINARY(less_than_equal, 4, input1_bin, input2_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_not(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int length;
    if (!get_glm_length(env, argv[0], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_vector_binary_size(GLM_BOOL, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_BOOL_VECTOR_UNARY(not, 2, input_bin, output_bin);
            break;
        case 3:
            DISPATCH_BOOL_VECTOR_UNARY(not, 3, input_bin, output_bin);
            break;
        case 4:
            DISPATCH_BOOL_VECTOR_UNARY(not, 4, input_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_not_equal(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }

    int length;
    if (!get_glm_length(env, argv[1], &length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(GLM_BOOL, length), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (length) {
        case 2:
            DISPATCH_TYPED_VECTOR_TO_BOOL_VECTOR_BINARY(not_equal, 2, input1_bin, input2_bin, output_bin);
            break;
        case 3:
            DISPATCH_TYPED_VECTOR_TO_BOOL_VECTOR_BINARY(not_equal, 3, input1_bin, input2_bin, output_bin);
            break;
        case 4:
            DISPATCH_TYPED_VECTOR_TO_BOOL_VECTOR_BINARY(not_equal, 4, input1_bin, input2_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_frustum(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary left;
    ErlNifBinary right;
    ErlNifBinary bottom;
    ErlNifBinary top;
    ErlNifBinary near_value;
    ErlNifBinary far_value;
    if (!enif_inspect_binary(env, argv[1], &left) || !enif_inspect_binary(env, argv[2], &right) || !enif_inspect_binary(env, argv[3], &bottom) || !enif_inspect_binary(env, argv[4], &top) || !enif_inspect_binary(env, argv[5], &near_value) || !enif_inspect_binary(env, argv[6], &far_value)) {
        return enif_make_badarg(env);
    }
    if (left.size != glm_float_type_size(type) || right.size != glm_float_type_size(type) || bottom.size != glm_float_type_size(type) || top.size != glm_float_type_size(type) || near_value.size != glm_float_type_size(type) || far_value.size != glm_float_type_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_frustum<float>(left, right, bottom, top, near_value, far_value, output_bin);
            break;
        case GLM_DOUBLE:
            beam_frustum<double>(left, right, bottom, top, near_value, far_value, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_infinite_perspective(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary fovy;
    ErlNifBinary aspect;
    ErlNifBinary near_value;
    if (!enif_inspect_binary(env, argv[1], &fovy) || !enif_inspect_binary(env, argv[2], &aspect) || !enif_inspect_binary(env, argv[3], &near_value)) {
        return enif_make_badarg(env);
    }
    if (fovy.size != glm_float_type_size(type) || aspect.size != glm_float_type_size(type) || near_value.size != glm_float_type_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_infinite_perspective<float>(fovy, aspect, near_value, output_bin);
            break;
        case GLM_DOUBLE:
            beam_infinite_perspective<double>(fovy, aspect, near_value, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_look_at(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary eye;
    ErlNifBinary center;
    ErlNifBinary up;
    if (!enif_inspect_binary(env, argv[1], &eye) || !enif_inspect_binary(env, argv[2], &center) || !enif_inspect_binary(env, argv[3], &up)) {
        return enif_make_badarg(env);
    }
    if (eye.size != glm_vector_binary_size(type, 3) || center.size != glm_vector_binary_size(type, 3) || up.size != glm_vector_binary_size(type, 3)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_look_at<float>(eye, center, up, output_bin);
            break;
        case GLM_DOUBLE:
            beam_look_at<double>(eye, center, up, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_ortho(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary left;
    ErlNifBinary right;
    ErlNifBinary bottom;
    ErlNifBinary top;
    ErlNifBinary near_value;
    ErlNifBinary far_value;
    if (!enif_inspect_binary(env, argv[1], &left) || !enif_inspect_binary(env, argv[2], &right) || !enif_inspect_binary(env, argv[3], &bottom) || !enif_inspect_binary(env, argv[4], &top) || !enif_inspect_binary(env, argv[5], &near_value) || !enif_inspect_binary(env, argv[6], &far_value)) {
        return enif_make_badarg(env);
    }
    if (left.size != glm_float_type_size(type) || right.size != glm_float_type_size(type) || bottom.size != glm_float_type_size(type) || top.size != glm_float_type_size(type) || near_value.size != glm_float_type_size(type) || far_value.size != glm_float_type_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_ortho<float>(left, right, bottom, top, near_value, far_value, output_bin);
            break;
        case GLM_DOUBLE:
            beam_ortho<double>(left, right, bottom, top, near_value, far_value, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_perspective(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary fovy;
    ErlNifBinary aspect;
    ErlNifBinary near_value;
    ErlNifBinary far_value;
    if (!enif_inspect_binary(env, argv[1], &fovy) || !enif_inspect_binary(env, argv[2], &aspect) || !enif_inspect_binary(env, argv[3], &near_value) || !enif_inspect_binary(env, argv[4], &far_value)) {
        return enif_make_badarg(env);
    }
    if (fovy.size != glm_float_type_size(type) || aspect.size != glm_float_type_size(type) || near_value.size != glm_float_type_size(type) || far_value.size != glm_float_type_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_perspective<float>(fovy, aspect, near_value, far_value, output_bin);
            break;
        case GLM_DOUBLE:
            beam_perspective<double>(fovy, aspect, near_value, far_value, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_perspective_fov(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary fov;
    ErlNifBinary width;
    ErlNifBinary height;
    ErlNifBinary near_value;
    ErlNifBinary far_value;
    if (!enif_inspect_binary(env, argv[1], &fov) || !enif_inspect_binary(env, argv[2], &width) || !enif_inspect_binary(env, argv[3], &height) || !enif_inspect_binary(env, argv[4], &near_value) || !enif_inspect_binary(env, argv[5], &far_value)) {
        return enif_make_badarg(env);
    }
    if (fov.size != glm_float_type_size(type) || width.size != glm_float_type_size(type) || height.size != glm_float_type_size(type) || near_value.size != glm_float_type_size(type) || far_value.size != glm_float_type_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_perspective_fov<float>(fov, width, height, near_value, far_value, output_bin);
            break;
        case GLM_DOUBLE:
            beam_perspective_fov<double>(fov, width, height, near_value, far_value, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_project(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary object;
    ErlNifBinary model;
    ErlNifBinary projection;
    ErlNifBinary viewport;
    if (!enif_inspect_binary(env, argv[1], &object) || !enif_inspect_binary(env, argv[2], &model) || !enif_inspect_binary(env, argv[3], &projection) || !enif_inspect_binary(env, argv[4], &viewport)) {
        return enif_make_badarg(env);
    }
    if (object.size != glm_vector_binary_size(type, 3) || model.size != glm_matrix_binary_size(type, 2) || projection.size != glm_matrix_binary_size(type, 2) || viewport.size != glm_vector_binary_size(type, 4)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(type, 3), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_project<float>(object, model, projection, viewport, output_bin);
            break;
        case GLM_DOUBLE:
            beam_project<double>(object, model, projection, viewport, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_un_project(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary window;
    ErlNifBinary model;
    ErlNifBinary projection;
    ErlNifBinary viewport;
    if (!enif_inspect_binary(env, argv[1], &window) || !enif_inspect_binary(env, argv[2], &model) || !enif_inspect_binary(env, argv[3], &projection) || !enif_inspect_binary(env, argv[4], &viewport)) {
        return enif_make_badarg(env);
    }
    if (window.size != glm_vector_binary_size(type, 3) || model.size != glm_matrix_binary_size(type, 2) || projection.size != glm_matrix_binary_size(type, 2) || viewport.size != glm_vector_binary_size(type, 4)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(type, 3), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_un_project<float>(window, model, projection, viewport, output_bin);
            break;
        case GLM_DOUBLE:
            beam_un_project<double>(window, model, projection, viewport, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_rotate_transform(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary matrix;
    ErlNifBinary angle;
    ErlNifBinary axis;
    if (!enif_inspect_binary(env, argv[1], &matrix) || !enif_inspect_binary(env, argv[2], &angle) || !enif_inspect_binary(env, argv[3], &axis)) {
        return enif_make_badarg(env);
    }
    if (matrix.size != glm_matrix_binary_size(type, 2) || angle.size != glm_float_type_size(type) || axis.size != glm_vector_binary_size(type, 3)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_rotate_msv<float>(matrix, angle, axis, output_bin);
            break;
        case GLM_DOUBLE:
            beam_rotate_msv<double>(matrix, angle, axis, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_scale(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary matrix;
    ErlNifBinary vector;
    if (!enif_inspect_binary(env, argv[1], &matrix) || !enif_inspect_binary(env, argv[2], &vector)) {
        return enif_make_badarg(env);
    }
    if (matrix.size != glm_matrix_binary_size(type, 2) || vector.size != glm_vector_binary_size(type, 3)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_scale_mv<float>(matrix, vector, output_bin);
            break;
        case GLM_DOUBLE:
            beam_scale_mv<double>(matrix, vector, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_translate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary matrix;
    ErlNifBinary vector;
    if (!enif_inspect_binary(env, argv[1], &matrix) || !enif_inspect_binary(env, argv[2], &vector)) {
        return enif_make_badarg(env);
    }
    if (matrix.size != glm_matrix_binary_size(type, 2) || vector.size != glm_vector_binary_size(type, 3)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_translate_mv<float>(matrix, vector, output_bin);
            break;
        case GLM_DOUBLE:
            beam_translate_mv<double>(matrix, vector, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_angle(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_angle_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_angle_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_angle_axis(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary angle_bin;
    ErlNifBinary axis_bin;
    if (!enif_inspect_binary(env, argv[1], &angle_bin) || !enif_inspect_binary(env, argv[2], &axis_bin)) {
        return enif_make_badarg(env);
    }
    if (angle_bin.size != glm_float_type_size(type) || axis_bin.size != glm_vector_binary_size(type, 3)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_quat_binary_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_angle_axis_sv<float>(angle_bin, axis_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_angle_axis_sv<double>(angle_bin, axis_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_axis(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(type, 3), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_axis_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_axis_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_conjugate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_quat_binary_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_conjugate_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_conjugate_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_dot_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[1], &input1_bin) || !enif_inspect_binary(env, argv[2], &input2_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_quat_binary_size(type) || input2_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_dot_qq<float>(input1_bin, input2_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_dot_qq<double>(input1_bin, input2_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_euler_angles(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_vector_binary_size(type, 3), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_euler_angles_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_euler_angles_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_inverse_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_quat_binary_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_inverse_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_inverse_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_length_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_length_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_length_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_mat3_cast(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 1), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_mat3_cast_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_mat3_cast_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_mat4_cast(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_matrix_binary_size(type, 2), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_mat4_cast_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_mat4_cast_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_mix_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    ErlNifBinary input3_bin;
    if (!enif_inspect_binary(env, argv[1], &input1_bin) || !enif_inspect_binary(env, argv[2], &input2_bin) || !enif_inspect_binary(env, argv[3], &input3_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_quat_binary_size(type) || input2_bin.size != glm_quat_binary_size(type) || input3_bin.size != glm_float_type_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_quat_binary_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_mix_qqs<float>(input1_bin, input2_bin, input3_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_mix_qqs<double>(input1_bin, input2_bin, input3_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_normalize_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_quat_binary_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_normalize_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_normalize_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_pitch(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_pitch_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_pitch_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_quat_cast(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    int shape;
    if (!get_glm_matrix_shape(env, argv[1], &shape)) {
        return enif_make_badarg(env);
    }
    if (shape != 1 && shape != 2) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin) || input_bin.size != glm_matrix_binary_size(type, shape)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_quat_binary_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            if (shape == 1) {
                beam_quat_cast_m<3, float>(input_bin, output_bin);
            } else {
                beam_quat_cast_m<4, float>(input_bin, output_bin);
            }
            break;
        case GLM_DOUBLE:
            if (shape == 1) {
                beam_quat_cast_m<3, double>(input_bin, output_bin);
            } else {
                beam_quat_cast_m<4, double>(input_bin, output_bin);
            }
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_roll(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_roll_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_roll_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_rotate_quat(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary quat_bin;
    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &quat_bin) || !enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }
    if (quat_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (input_bin.size) {
        case sizeof(float) * 3:
            if (type != GLM_FLOAT) {
                return enif_make_badarg(env);
            }
            beam_rotate_qv3<float>(quat_bin, input_bin, output_bin);
            break;
        case sizeof(float) * 4:
            if (type != GLM_FLOAT) {
                return enif_make_badarg(env);
            }
            beam_rotate_qv4<float>(quat_bin, input_bin, output_bin);
            break;
        case sizeof(double) * 3:
            if (type != GLM_DOUBLE) {
                return enif_make_badarg(env);
            }
            beam_rotate_qv3<double>(quat_bin, input_bin, output_bin);
            break;
        case sizeof(double) * 4:
            if (type != GLM_DOUBLE) {
                return enif_make_badarg(env);
            }
            beam_rotate_qv4<double>(quat_bin, input_bin, output_bin);
            break;
        default:
            return enif_make_badarg(env);
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_slerp(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    ErlNifBinary input3_bin;
    if (!enif_inspect_binary(env, argv[1], &input1_bin) || !enif_inspect_binary(env, argv[2], &input2_bin) || !enif_inspect_binary(env, argv[3], &input3_bin)) {
        return enif_make_badarg(env);
    }
    if (input1_bin.size != glm_quat_binary_size(type) || input2_bin.size != glm_quat_binary_size(type) || input3_bin.size != glm_float_type_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_quat_binary_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_slerp_qqs<float>(input1_bin, input2_bin, input3_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_slerp_qqs<double>(input1_bin, input2_bin, input3_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_yaw(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[1], &input_bin) || input_bin.size != glm_quat_binary_size(type)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(glm_float_type_size(type), &output_bin)) {
        return enif_make_badarg(env);
    }

    switch (type) {
        case GLM_FLOAT:
            beam_yaw_q<float>(input_bin, output_bin);
            break;
        case GLM_DOUBLE:
            beam_yaw_q<double>(input_bin, output_bin);
            break;
    }

    return enif_make_binary(env, &output_bin);
}

// Placeholder NIF inventory for upcoming operational families.
//
// These names mirror the intended raw Erlang entrypoints. They are not wired
// yet on purpose; each family will be implemented and registered incrementally.

static ERL_NIF_TERM nif_glm_bit_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (is_scalar_length(env, argv[1])) {
        if (input_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(sizeof(int32_t), &output_bin)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_TO_INT32_SCALAR_UNARY(bit_count, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(glm_vector_binary_size(GLM_INT32, length), &output_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(bit_count, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(bit_count, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(bit_count, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_bitfield_extract(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    ErlNifBinary value_bin;
    ErlNifBinary offset_bin;
    ErlNifBinary bits_bin;
    if (!enif_inspect_binary(env, argv[2], &value_bin) || !enif_inspect_binary(env, argv[3], &offset_bin) || !enif_inspect_binary(env, argv[4], &bits_bin)) {
        return enif_make_badarg(env);
    }
    if (offset_bin.size != sizeof(int32_t) || bits_bin.size != sizeof(int32_t)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(value_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (is_scalar_length(env, argv[1])) {
        if (value_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }

        switch (type) {
            case GLM_INT8: beam_bitfield_extract_s<int8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_INT16: beam_bitfield_extract_s<int16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_INT32: beam_bitfield_extract_s<int32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_INT64: beam_bitfield_extract_s<int64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT8: beam_bitfield_extract_s<uint8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT16: beam_bitfield_extract_s<uint16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT32: beam_bitfield_extract_s<uint32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT64: beam_bitfield_extract_s<uint64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (value_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_INT8: beam_bitfield_extract_v<2, int8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT16: beam_bitfield_extract_v<2, int16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT32: beam_bitfield_extract_v<2, int32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT64: beam_bitfield_extract_v<2, int64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT8: beam_bitfield_extract_v<2, uint8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT16: beam_bitfield_extract_v<2, uint16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT32: beam_bitfield_extract_v<2, uint32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT64: beam_bitfield_extract_v<2, uint64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_INT8: beam_bitfield_extract_v<3, int8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT16: beam_bitfield_extract_v<3, int16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT32: beam_bitfield_extract_v<3, int32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT64: beam_bitfield_extract_v<3, int64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT8: beam_bitfield_extract_v<3, uint8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT16: beam_bitfield_extract_v<3, uint16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT32: beam_bitfield_extract_v<3, uint32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT64: beam_bitfield_extract_v<3, uint64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_INT8: beam_bitfield_extract_v<4, int8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT16: beam_bitfield_extract_v<4, int16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT32: beam_bitfield_extract_v<4, int32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT64: beam_bitfield_extract_v<4, int64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT8: beam_bitfield_extract_v<4, uint8_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT16: beam_bitfield_extract_v<4, uint16_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT32: beam_bitfield_extract_v<4, uint32_t>(value_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT64: beam_bitfield_extract_v<4, uint64_t>(value_bin, offset_bin, bits_bin, output_bin); break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_bitfield_insert(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    ErlNifBinary base_bin;
    ErlNifBinary insert_bin;
    ErlNifBinary offset_bin;
    ErlNifBinary bits_bin;
    if (!enif_inspect_binary(env, argv[2], &base_bin) || !enif_inspect_binary(env, argv[3], &insert_bin) || !enif_inspect_binary(env, argv[4], &offset_bin) || !enif_inspect_binary(env, argv[5], &bits_bin)) {
        return enif_make_badarg(env);
    }
    if (offset_bin.size != sizeof(int32_t) || bits_bin.size != sizeof(int32_t)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(base_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (is_scalar_length(env, argv[1])) {
        if (base_bin.size != glm_type_size(type) || insert_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }

        switch (type) {
            case GLM_INT8: beam_bitfield_insert_s<int8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_INT16: beam_bitfield_insert_s<int16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_INT32: beam_bitfield_insert_s<int32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_INT64: beam_bitfield_insert_s<int64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT8: beam_bitfield_insert_s<uint8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT16: beam_bitfield_insert_s<uint16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT32: beam_bitfield_insert_s<uint32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            case GLM_UINT64: beam_bitfield_insert_s<uint64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
        }
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (base_bin.size != glm_vector_binary_size(type, length) || insert_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            switch (type) {
                case GLM_INT8: beam_bitfield_insert_v<2, int8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT16: beam_bitfield_insert_v<2, int16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT32: beam_bitfield_insert_v<2, int32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT64: beam_bitfield_insert_v<2, int64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT8: beam_bitfield_insert_v<2, uint8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT16: beam_bitfield_insert_v<2, uint16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT32: beam_bitfield_insert_v<2, uint32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT64: beam_bitfield_insert_v<2, uint64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            }
        } else if (length == 3) {
            switch (type) {
                case GLM_INT8: beam_bitfield_insert_v<3, int8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT16: beam_bitfield_insert_v<3, int16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT32: beam_bitfield_insert_v<3, int32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT64: beam_bitfield_insert_v<3, int64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT8: beam_bitfield_insert_v<3, uint8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT16: beam_bitfield_insert_v<3, uint16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT32: beam_bitfield_insert_v<3, uint32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT64: beam_bitfield_insert_v<3, uint64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            }
        } else if (length == 4) {
            switch (type) {
                case GLM_INT8: beam_bitfield_insert_v<4, int8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT16: beam_bitfield_insert_v<4, int16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT32: beam_bitfield_insert_v<4, int32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_INT64: beam_bitfield_insert_v<4, int64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT8: beam_bitfield_insert_v<4, uint8_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT16: beam_bitfield_insert_v<4, uint16_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT32: beam_bitfield_insert_v<4, uint32_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
                case GLM_UINT64: beam_bitfield_insert_v<4, uint64_t>(base_bin, insert_bin, offset_bin, bits_bin, output_bin); break;
            }
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_bitfield_reverse(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
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

    if (is_scalar_length(env, argv[1])) {
        if (input_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_UNARY(bitfield_reverse, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_INTEGER_VECTOR_UNARY(bitfield_reverse, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_INTEGER_VECTOR_UNARY(bitfield_reverse, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_INTEGER_VECTOR_UNARY(bitfield_reverse, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_find_lsb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (is_scalar_length(env, argv[1])) {
        if (input_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(sizeof(int32_t), &output_bin)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_TO_INT32_SCALAR_UNARY(find_lsb, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(glm_vector_binary_size(GLM_INT32, length), &output_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(find_lsb, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(find_lsb, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(find_lsb, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_find_msb(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT8 && type != GLM_INT16 && type != GLM_INT32 && type != GLM_INT64 && type != GLM_UINT8 && type != GLM_UINT16 && type != GLM_UINT32 && type != GLM_UINT64) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input_bin;
    if (!enif_inspect_binary(env, argv[2], &input_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (is_scalar_length(env, argv[1])) {
        if (input_bin.size != glm_type_size(type)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(sizeof(int32_t), &output_bin)) {
            return enif_make_badarg(env);
        }

        DISPATCH_INTEGER_SCALAR_TO_INT32_SCALAR_UNARY(find_msb, input_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(glm_vector_binary_size(GLM_INT32, length), &output_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(find_msb, 2, input_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(find_msb, 3, input_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_INTEGER_VECTOR_TO_INT32_VECTOR_UNARY(find_msb, 4, input_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_imul_extended(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_INT32) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary msb_bin;
    ErlNifBinary lsb_bin;

    if (is_scalar_length(env, argv[1])) {
        if (input1_bin.size != sizeof(int32_t) || input2_bin.size != sizeof(int32_t)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(sizeof(int32_t), &msb_bin) || !enif_alloc_binary(sizeof(int32_t), &lsb_bin)) {
            return enif_make_badarg(env);
        }

        beam_imul_extended_s(input1_bin, input2_bin, msb_bin, lsb_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(input1_bin.size, &msb_bin) || !enif_alloc_binary(input1_bin.size, &lsb_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_imul_extended_v<2>(input1_bin, input2_bin, msb_bin, lsb_bin);
        } else if (length == 3) {
            beam_imul_extended_v<3>(input1_bin, input2_bin, msb_bin, lsb_bin);
        } else if (length == 4) {
            beam_imul_extended_v<4>(input1_bin, input2_bin, msb_bin, lsb_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_tuple2(env, enif_make_binary(env, &msb_bin), enif_make_binary(env, &lsb_bin));
}

static ERL_NIF_TERM nif_glm_uadd_carry(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_UINT32) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary result_bin;
    ErlNifBinary carry_bin;

    if (is_scalar_length(env, argv[1])) {
        if (input1_bin.size != sizeof(uint32_t) || input2_bin.size != sizeof(uint32_t)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(sizeof(uint32_t), &result_bin) || !enif_alloc_binary(sizeof(uint32_t), &carry_bin)) {
            return enif_make_badarg(env);
        }

        beam_uadd_carry_s(input1_bin, input2_bin, result_bin, carry_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(input1_bin.size, &result_bin) || !enif_alloc_binary(input1_bin.size, &carry_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_uadd_carry_v<2>(input1_bin, input2_bin, result_bin, carry_bin);
        } else if (length == 3) {
            beam_uadd_carry_v<3>(input1_bin, input2_bin, result_bin, carry_bin);
        } else if (length == 4) {
            beam_uadd_carry_v<4>(input1_bin, input2_bin, result_bin, carry_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_tuple2(env, enif_make_binary(env, &result_bin), enif_make_binary(env, &carry_bin));
}

static ERL_NIF_TERM nif_glm_umul_extended(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_UINT32) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary msb_bin;
    ErlNifBinary lsb_bin;

    if (is_scalar_length(env, argv[1])) {
        if (input1_bin.size != sizeof(uint32_t) || input2_bin.size != sizeof(uint32_t)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(sizeof(uint32_t), &msb_bin) || !enif_alloc_binary(sizeof(uint32_t), &lsb_bin)) {
            return enif_make_badarg(env);
        }

        beam_umul_extended_s(input1_bin, input2_bin, msb_bin, lsb_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(input1_bin.size, &msb_bin) || !enif_alloc_binary(input1_bin.size, &lsb_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_umul_extended_v<2>(input1_bin, input2_bin, msb_bin, lsb_bin);
        } else if (length == 3) {
            beam_umul_extended_v<3>(input1_bin, input2_bin, msb_bin, lsb_bin);
        } else if (length == 4) {
            beam_umul_extended_v<4>(input1_bin, input2_bin, msb_bin, lsb_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_tuple2(env, enif_make_binary(env, &msb_bin), enif_make_binary(env, &lsb_bin));
}

static ERL_NIF_TERM nif_glm_usub_borrow(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_UINT32) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary result_bin;
    ErlNifBinary borrow_bin;

    if (is_scalar_length(env, argv[1])) {
        if (input1_bin.size != sizeof(uint32_t) || input2_bin.size != sizeof(uint32_t)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(sizeof(uint32_t), &result_bin) || !enif_alloc_binary(sizeof(uint32_t), &borrow_bin)) {
            return enif_make_badarg(env);
        }

        beam_usub_borrow_s(input1_bin, input2_bin, result_bin, borrow_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }
        if (input1_bin.size != glm_vector_binary_size(type, length) || input2_bin.size != glm_vector_binary_size(type, length)) {
            return enif_make_badarg(env);
        }
        if (!enif_alloc_binary(input1_bin.size, &result_bin) || !enif_alloc_binary(input1_bin.size, &borrow_bin)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            beam_usub_borrow_v<2>(input1_bin, input2_bin, result_bin, borrow_bin);
        } else if (length == 3) {
            beam_usub_borrow_v<3>(input1_bin, input2_bin, result_bin, borrow_bin);
        } else if (length == 4) {
            beam_usub_borrow_v<4>(input1_bin, input2_bin, result_bin, borrow_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_tuple2(env, enif_make_binary(env, &result_bin), enif_make_binary(env, &borrow_bin));
}

static ERL_NIF_TERM nif_glm_fma(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    ErlNifBinary input3_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin) || !enif_inspect_binary(env, argv[4], &input3_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (is_scalar_length(env, argv[1])) {
        DISPATCH_FLOAT_SCALAR_TERNARY(fma, input1_bin, input2_bin, input3_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(fma, 2, input1_bin, input2_bin, input3_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(fma, 3, input1_bin, input2_bin, input3_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY(fma, 4, input1_bin, input2_bin, input3_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}

static ERL_NIF_TERM nif_glm_pow(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int type;
    if (!get_glm_type(env, argv[0], &type)) {
        return enif_make_badarg(env);
    }
    if (type != GLM_FLOAT && type != GLM_DOUBLE) {
        return enif_make_badarg(env);
    }

    ErlNifBinary input1_bin;
    ErlNifBinary input2_bin;
    if (!enif_inspect_binary(env, argv[2], &input1_bin) || !enif_inspect_binary(env, argv[3], &input2_bin)) {
        return enif_make_badarg(env);
    }

    ErlNifBinary output_bin;
    if (!enif_alloc_binary(input1_bin.size, &output_bin)) {
        return enif_make_badarg(env);
    }

    if (is_scalar_length(env, argv[1])) {
        DISPATCH_FLOAT_SCALAR_BINARY(pow, input1_bin, input2_bin, output_bin);
    } else {
        int length;
        if (!get_glm_length(env, argv[1], &length)) {
            return enif_make_badarg(env);
        }

        if (length == 2) {
            DISPATCH_FLOAT_VECTOR_VECTOR_BINARY(pow, 2, input1_bin, input2_bin, output_bin);
        } else if (length == 3) {
            DISPATCH_FLOAT_VECTOR_VECTOR_BINARY(pow, 3, input1_bin, input2_bin, output_bin);
        } else if (length == 4) {
            DISPATCH_FLOAT_VECTOR_VECTOR_BINARY(pow, 4, input1_bin, input2_bin, output_bin);
        } else {
            return enif_make_badarg(env);
        }
    }

    return enif_make_binary(env, &output_bin);
}
//
// glm_common:
// nif_glm_abs, nif_glm_ceil, nif_glm_float_bits_to_int,
// nif_glm_float_bits_to_uint, nif_glm_floor, nif_glm_fma, nif_glm_fract,
// nif_glm_frexp, nif_glm_int_bits_to_float, nif_glm_is_inf, nif_glm_is_nan,
// nif_glm_ldexp, nif_glm_max, nif_glm_min, nif_glm_mix, nif_glm_mod,
// nif_glm_modf, nif_glm_sign, nif_glm_smoothstep, nif_glm_step,
// nif_glm_trunc, nif_glm_uint_bits_to_float
//
// glm_angle:
// nif_glm_acos, nif_glm_acosh, nif_glm_asin, nif_glm_asinh, nif_glm_atan,
// nif_glm_atanh, nif_glm_cos, nif_glm_cosh, nif_glm_degrees,
// nif_glm_radians, nif_glm_sin, nif_glm_sinh, nif_glm_tan, nif_glm_tanh
//
// glm_integer:
// nif_glm_bit_count, nif_glm_bitfield_extract, nif_glm_bitfield_insert,
// nif_glm_bitfield_reverse, nif_glm_find_lsb, nif_glm_find_msb,
// nif_glm_imul_extended, nif_glm_is_multiple, nif_glm_is_power_of_two,
// nif_glm_next_multiple, nif_glm_next_power_of_two, nif_glm_prev_multiple,
// nif_glm_prev_power_of_two, nif_glm_uadd_carry, nif_glm_umul_extended,
// nif_glm_usub_borrow
//
// glm_matrix:
// nif_glm_affine_inverse, nif_glm_column, nif_glm_determinant,
// nif_glm_inverse, nif_glm_inverse_transpose, nif_glm_matrix_comp_mult,
// nif_glm_outer_product, nif_glm_row, nif_glm_transpose
//
// glm_packing:
// nif_glm_pack_double_2x32, nif_glm_pack_half_2x16,
// nif_glm_pack_snorm_2x16, nif_glm_pack_snorm_4x8,
// nif_glm_pack_unorm_2x16, nif_glm_pack_unorm_4x8,
// nif_glm_unpack_double_2x32, nif_glm_unpack_half_2x16,
// nif_glm_unpack_snorm_2x16, nif_glm_unpack_snorm_4x8,
// nif_glm_unpack_unorm_2x16, nif_glm_unpack_unorm_4x8
//
// glm_quat:
// nif_glm_angle, nif_glm_angle_axis, nif_glm_axis, nif_glm_conjugate,
// nif_glm_dot, nif_glm_euler_angles, nif_glm_length, nif_glm_mat3_cast,
// nif_glm_mat4_cast, nif_glm_mix, nif_glm_normalize, nif_glm_pitch,
// nif_glm_quat_cast, nif_glm_roll, nif_glm_rotate, nif_glm_slerp,
// nif_glm_yaw
//
// glm_relational:
// nif_glm_all, nif_glm_any, nif_glm_equal, nif_glm_greater_than,
// nif_glm_greater_than_equal, nif_glm_less_than, nif_glm_less_than_equal,
// nif_glm_not, nif_glm_not_equal
//
// glm_transform:
// nif_glm_frustum, nif_glm_infinite_perspective, nif_glm_look_at,
// nif_glm_ortho, nif_glm_perspective, nif_glm_perspective_fov,
// nif_glm_project, nif_glm_rotate, nif_glm_scale, nif_glm_translate,
// nif_glm_un_project
//
// glm_vector:
// nif_glm_cross, nif_glm_distance, nif_glm_dot, nif_glm_face_forward,
// nif_glm_length, nif_glm_normalize, nif_glm_reflect, nif_glm_refract

static ErlNifFunc nif_functions[] = {
    {"all_raw", 2, nif_glm_all, 0},
    {"any_raw", 2, nif_glm_any, 0},
    {"angle_raw", 2, nif_glm_angle, 0},
    {"angle_axis_raw", 3, nif_glm_angle_axis, 0},
    {"axis_raw", 2, nif_glm_axis, 0},
    {"affine_inverse_raw", 3, nif_glm_affine_inverse, 0},
    {"acos_raw", 3, nif_glm_acos, 0},
    {"acosh_raw", 3, nif_glm_acosh, 0},
    {"asin_raw", 3, nif_glm_asin, 0},
    {"asinh_raw", 3, nif_glm_asinh, 0},
    {"atan_raw", 3, nif_glm_atan, 0},
    {"atanh_raw", 3, nif_glm_atanh, 0},
    {"abs_raw", 3, nif_glm_abs, 0},
    {"bit_count_raw", 3, nif_glm_bit_count, 0},
    {"bitfield_extract_raw", 5, nif_glm_bitfield_extract, 0},
    {"bitfield_insert_raw", 6, nif_glm_bitfield_insert, 0},
    {"bitfield_reverse_raw", 3, nif_glm_bitfield_reverse, 0},
    {"ceil_raw", 3, nif_glm_ceil, 0},
    {"column_raw", 4, nif_glm_column, 0},
    {"conjugate_raw", 2, nif_glm_conjugate, 0},
    {"cross_raw", 4, nif_glm_cross, 0},
    {"cos_raw", 3, nif_glm_cos, 0},
    {"cosh_raw", 3, nif_glm_cosh, 0},
    {"determinant_raw", 3, nif_glm_determinant, 0},
    {"distance_raw", 4, nif_glm_distance, 0},
    {"degrees_raw", 3, nif_glm_degrees, 0},
    {"dot_raw", 3, nif_glm_dot_quat, 0},
    {"dot_raw", 4, nif_glm_dot, 0},
    {"equal_raw", 4, nif_glm_equal, 0},
    {"clamp_raw", 6, nif_glm_clamp, 0},
    {"euler_angles_raw", 2, nif_glm_euler_angles, 0},
    {"exp_raw", 3, nif_glm_exp, 0},
    {"exp2_raw", 3, nif_glm_exp2, 0},
    {"face_forward_raw", 5, nif_glm_face_forward, 0},
    {"find_lsb_raw", 3, nif_glm_find_lsb, 0},
    {"find_msb_raw", 3, nif_glm_find_msb, 0},
    {"float_bits_to_int_raw", 3, nif_glm_float_bits_to_int, 0},
    {"float_bits_to_uint_raw", 3, nif_glm_float_bits_to_uint, 0},
    {"fma_raw", 5, nif_glm_fma, 0},
    {"fract_raw", 3, nif_glm_fract, 0},
    {"frexp_raw", 3, nif_glm_frexp, 0},
    {"floor_raw", 3, nif_glm_floor, 0},
    {"greater_than_raw", 4, nif_glm_greater_than, 0},
    {"greater_than_equal_raw", 4, nif_glm_greater_than_equal, 0},
    {"inverse_raw", 2, nif_glm_inverse_quat, 0},
    {"inverse_sqrt_raw", 3, nif_glm_inverse_sqrt, 0},
    {"inverse_raw", 3, nif_glm_inverse, 0},
    {"inverse_transpose_raw", 3, nif_glm_inverse_transpose, 0},
    {"imul_extended_raw", 4, nif_glm_imul_extended, 0},
    {"int_bits_to_float_raw", 3, nif_glm_int_bits_to_float, 0},
    {"is_inf_raw", 3, nif_glm_is_inf, 0},
    {"is_multiple_raw", 5, nif_glm_is_multiple, 0},
    {"is_nan_raw", 3, nif_glm_is_nan, 0},
    {"is_power_of_two_raw", 3, nif_glm_is_power_of_two, 0},
    {"ldexp_raw", 4, nif_glm_ldexp, 0},
    {"length_raw", 2, nif_glm_length_quat, 0},
    {"length_raw", 3, nif_glm_length, 0},
    {"log_raw", 3, nif_glm_log, 0},
    {"log2_raw", 3, nif_glm_log2, 0},
    {"max_raw", 5, nif_glm_max, 0},
    {"mat3_cast_raw", 2, nif_glm_mat3_cast, 0},
    {"mat4_cast_raw", 2, nif_glm_mat4_cast, 0},
    {"matrix_comp_mult_raw", 4, nif_glm_matrix_comp_mult, 0},
    {"min_raw", 5, nif_glm_min, 0},
    {"mix_raw", 4, nif_glm_mix_quat, 0},
    {"mix_raw", 6, nif_glm_mix, 0},
    {"mod_raw", 5, nif_glm_mod, 0},
    {"modf_raw", 3, nif_glm_modf, 0},
    {"less_than_raw", 4, nif_glm_less_than, 0},
    {"less_than_equal_raw", 4, nif_glm_less_than_equal, 0},
    {"not_raw", 2, nif_glm_not, 0},
    {"not_equal_raw", 4, nif_glm_not_equal, 0},
    {"normalize_raw", 2, nif_glm_normalize_quat, 0},
    {"normalize_raw", 3, nif_glm_normalize, 0},
    {"next_multiple_raw", 5, nif_glm_next_multiple, 0},
    {"next_power_of_two_raw", 3, nif_glm_next_power_of_two, 0},
    {"outer_product_raw", 4, nif_glm_outer_product, 0},
    {"pitch_raw", 2, nif_glm_pitch, 0},
    {"pow_raw", 4, nif_glm_pow, 0},
    {"quat_cast_raw", 3, nif_glm_quat_cast, 0},
    {"radians_raw", 3, nif_glm_radians, 0},
    {"reflect_raw", 4, nif_glm_reflect, 0},
    {"refract_raw", 5, nif_glm_refract, 0},
    {"roll_raw", 2, nif_glm_roll, 0},
    {"frustum_raw", 7, nif_glm_frustum, 0},
    {"infinite_perspective_raw", 4, nif_glm_infinite_perspective, 0},
    {"look_at_raw", 4, nif_glm_look_at, 0},
    {"ortho_raw", 7, nif_glm_ortho, 0},
    {"perspective_raw", 5, nif_glm_perspective, 0},
    {"perspective_fov_raw", 6, nif_glm_perspective_fov, 0},
    {"prev_multiple_raw", 5, nif_glm_prev_multiple, 0},
    {"prev_power_of_two_raw", 3, nif_glm_prev_power_of_two, 0},
    {"project_raw", 5, nif_glm_project, 0},
    {"rotate_raw", 3, nif_glm_rotate_quat, 0},
    {"rotate_raw", 4, nif_glm_rotate_transform, 0},
    {"row_raw", 4, nif_glm_row, 0},
    {"round_raw", 3, nif_glm_round, 0},
    {"round_even_raw", 3, nif_glm_round_even, 0},
    {"scale_raw", 3, nif_glm_scale, 0},
    {"pack_double_2x32_raw", 2, nif_glm_pack_double_2x32, 0},
    {"pack_half_1x16_raw", 2, nif_glm_pack_half_1x16, 0},
    {"pack_half_2x16_raw", 2, nif_glm_pack_half_2x16, 0},
    {"pack_half_4x16_raw", 2, nif_glm_pack_half_4x16, 0},
    {"pack_snorm_1x8_raw", 2, nif_glm_pack_snorm_1x8, 0},
    {"pack_snorm_2x8_raw", 2, nif_glm_pack_snorm_2x8, 0},
    {"pack_snorm_1x16_raw", 2, nif_glm_pack_snorm_1x16, 0},
    {"pack_snorm_2x16_raw", 2, nif_glm_pack_snorm_2x16, 0},
    {"pack_snorm_4x8_raw", 2, nif_glm_pack_snorm_4x8, 0},
    {"pack_snorm_4x16_raw", 2, nif_glm_pack_snorm_4x16, 0},
    {"pack_unorm_1x8_raw", 2, nif_glm_pack_unorm_1x8, 0},
    {"pack_unorm_2x8_raw", 2, nif_glm_pack_unorm_2x8, 0},
    {"pack_unorm_1x16_raw", 2, nif_glm_pack_unorm_1x16, 0},
    {"pack_unorm_2x16_raw", 2, nif_glm_pack_unorm_2x16, 0},
    {"pack_unorm_4x8_raw", 2, nif_glm_pack_unorm_4x8, 0},
    {"pack_unorm_4x16_raw", 2, nif_glm_pack_unorm_4x16, 0},
    {"sin_raw", 3, nif_glm_sin, 0},
    {"sinh_raw", 3, nif_glm_sinh, 0},
    {"sign_raw", 3, nif_glm_sign, 0},
    {"slerp_raw", 4, nif_glm_slerp, 0},
    {"smoothstep_raw", 6, nif_glm_smoothstep, 0},
    {"sqrt_raw", 3, nif_glm_sqrt, 0},
    {"step_raw", 5, nif_glm_step, 0},
    {"tan_raw", 3, nif_glm_tan, 0},
    {"tanh_raw", 3, nif_glm_tanh, 0},
    {"translate_raw", 3, nif_glm_translate, 0},
    {"transpose_raw", 3, nif_glm_transpose, 0},
    {"trunc_raw", 3, nif_glm_trunc, 0},
    {"uadd_carry_raw", 4, nif_glm_uadd_carry, 0},
    {"unpack_double_2x32_raw", 2, nif_glm_unpack_double_2x32, 0},
    {"unpack_half_1x16_raw", 2, nif_glm_unpack_half_1x16, 0},
    {"unpack_half_2x16_raw", 2, nif_glm_unpack_half_2x16, 0},
    {"unpack_half_4x16_raw", 2, nif_glm_unpack_half_4x16, 0},
    {"unpack_snorm_1x8_raw", 2, nif_glm_unpack_snorm_1x8, 0},
    {"unpack_snorm_2x8_raw", 2, nif_glm_unpack_snorm_2x8, 0},
    {"unpack_snorm_1x16_raw", 2, nif_glm_unpack_snorm_1x16, 0},
    {"unpack_snorm_2x16_raw", 2, nif_glm_unpack_snorm_2x16, 0},
    {"unpack_snorm_4x8_raw", 2, nif_glm_unpack_snorm_4x8, 0},
    {"unpack_snorm_4x16_raw", 2, nif_glm_unpack_snorm_4x16, 0},
    {"un_project_raw", 5, nif_glm_un_project, 0},
    {"umul_extended_raw", 4, nif_glm_umul_extended, 0},
    {"unpack_unorm_1x8_raw", 2, nif_glm_unpack_unorm_1x8, 0},
    {"unpack_unorm_2x8_raw", 2, nif_glm_unpack_unorm_2x8, 0},
    {"unpack_unorm_1x16_raw", 2, nif_glm_unpack_unorm_1x16, 0},
    {"unpack_unorm_2x16_raw", 2, nif_glm_unpack_unorm_2x16, 0},
    {"unpack_unorm_4x8_raw", 2, nif_glm_unpack_unorm_4x8, 0},
    {"unpack_unorm_4x16_raw", 2, nif_glm_unpack_unorm_4x16, 0},
    {"uint_bits_to_float_raw", 3, nif_glm_uint_bits_to_float, 0},
    {"usub_borrow_raw", 4, nif_glm_usub_borrow, 0},
    {"yaw_raw", 2, nif_glm_yaw, 0},
    {"back_ease_in_raw", 2, nif_glm_backEaseIn_2, 0},
    {"back_ease_in_raw", 3, nif_glm_backEaseIn_3, 0},
    {"back_ease_in_out_raw", 2, nif_glm_backEaseInOut_2, 0},
    {"back_ease_in_out_raw", 3, nif_glm_backEaseInOut_3, 0},
    {"back_ease_out_raw", 2, nif_glm_backEaseOut_2, 0},
    {"back_ease_out_raw", 3, nif_glm_backEaseOut_3, 0},
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
