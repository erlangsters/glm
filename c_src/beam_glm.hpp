//
// Copyright (c) 2026, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#include <glm/glm.hpp>
#include <glm/ext/matrix_clip_space.hpp>
#include <glm/ext/matrix_projection.hpp>
#include <glm/ext/matrix_transform.hpp>
#include <glm/ext/scalar_integer.hpp>
#include <glm/ext/vector_integer.hpp>
#include <glm/integer.hpp>
#include <glm/packing.hpp>
#include <glm/gtc/packing.hpp>
#include <glm/gtc/matrix_inverse.hpp>
#include <glm/gtc/quaternion.hpp>
#define GLM_ENABLE_EXPERIMENTAL
#include <glm/gtx/easing.hpp>
#include <glm/gtx/quaternion.hpp>
#include <cmath>
#include <erl_nif.h>
#include <type_traits>

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

template<typename T>
inline void beam_clamp_sss(const ErlNifBinary& input_x, const ErlNifBinary& input_min, const ErlNifBinary& input_max, ErlNifBinary& output) {
    *(T*)output.data = glm::clamp(*(const T*)input_x.data, *(const T*)input_min.data, *(const T*)input_max.data);
}

template<int L, typename T>
inline void beam_clamp_vss(const ErlNifBinary& input_x, const ErlNifBinary& input_min, const ErlNifBinary& input_max, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::clamp(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const T*)input_min.data, *(const T*)input_max.data);
}

template<int L, typename T>
inline void beam_clamp_vvv(const ErlNifBinary& input_x, const ErlNifBinary& input_min, const ErlNifBinary& input_max, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::clamp(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const glm::vec<L, T, glm::defaultp>*)input_min.data, *(const glm::vec<L, T, glm::defaultp>*)input_max.data);
}

template<typename T>
inline void beam_abs_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::abs(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_abs_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::abs(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_ceil_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::ceil(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_ceil_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::ceil(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_floor_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::floor(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_floor_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::floor(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_fract_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::fract(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_fract_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::fract(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_frexp_s(const ErlNifBinary& input, ErlNifBinary& significand_output, ErlNifBinary& exponent_output) {
    int32_t exponent;
    *(T*)significand_output.data = std::frexp(*(const T*)input.data, &exponent);
    *(int32_t*)exponent_output.data = exponent;
}

template<int L, typename T>
inline void beam_frexp_v(const ErlNifBinary& input, ErlNifBinary& significand_output, ErlNifBinary& exponent_output) {
    const auto& in = *(const glm::vec<L, T, glm::defaultp>*)input.data;
    auto& significand = *(glm::vec<L, T, glm::defaultp>*)significand_output.data;
    auto& exponent = *(glm::vec<L, int32_t, glm::defaultp>*)exponent_output.data;

    for (int i = 0; i < L; ++i) {
        significand[i] = std::frexp(in[i], &exponent[i]);
    }
}

inline void beam_float_bits_to_int_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(int32_t*)output.data = glm::floatBitsToInt(*(const float*)input.data);
}

template<int L>
inline void beam_float_bits_to_int_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, int32_t, glm::defaultp>*)output.data = glm::floatBitsToInt(*(const glm::vec<L, float, glm::defaultp>*)input.data);
}

inline void beam_float_bits_to_uint_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint32_t*)output.data = glm::floatBitsToUint(*(const float*)input.data);
}

template<int L>
inline void beam_float_bits_to_uint_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, uint32_t, glm::defaultp>*)output.data = glm::floatBitsToUint(*(const glm::vec<L, float, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_fma_sss(const ErlNifBinary& input_a, const ErlNifBinary& input_b, const ErlNifBinary& input_c, ErlNifBinary& output) {
    *(T*)output.data = std::fma(*(const T*)input_a.data, *(const T*)input_b.data, *(const T*)input_c.data);
}

template<int L, typename T>
inline void beam_fma_vvv(const ErlNifBinary& input_a, const ErlNifBinary& input_b, const ErlNifBinary& input_c, ErlNifBinary& output) {
    const auto& a = *(const glm::vec<L, T, glm::defaultp>*)input_a.data;
    const auto& b = *(const glm::vec<L, T, glm::defaultp>*)input_b.data;
    const auto& c = *(const glm::vec<L, T, glm::defaultp>*)input_c.data;
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = std::fma(a[i], b[i], c[i]);
    }
}

inline void beam_int_bits_to_float_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(float*)output.data = glm::intBitsToFloat(*(const int32_t*)input.data);
}

template<int L>
inline void beam_int_bits_to_float_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, float, glm::defaultp>*)output.data = glm::intBitsToFloat(*(const glm::vec<L, int32_t, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_ldexp_s(const ErlNifBinary& input_x, const ErlNifBinary& input_exp, ErlNifBinary& output) {
    *(T*)output.data = std::ldexp(*(const T*)input_x.data, *(const int32_t*)input_exp.data);
}

template<int L, typename T>
inline void beam_ldexp_v(const ErlNifBinary& input_x, const ErlNifBinary& input_exp, ErlNifBinary& output) {
    const auto& in = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& exponent = *(const glm::vec<L, int32_t, glm::defaultp>*)input_exp.data;
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = std::ldexp(in[i], exponent[i]);
    }
}

#define DEFINE_BEAM_STD_UNARY(OP, FN) \
template<typename T> \
inline void beam_##OP##_s(const ErlNifBinary& input, ErlNifBinary& output) { \
    *(T*)output.data = static_cast<T>((FN)(*(const T*)input.data)); \
} \
 \
template<int L, typename T> \
inline void beam_##OP##_v(const ErlNifBinary& input, ErlNifBinary& output) { \
    const auto& in = *(const glm::vec<L, T, glm::defaultp>*)input.data; \
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data; \
 \
    for (int i = 0; i < L; ++i) { \
        out[i] = static_cast<T>((FN)(in[i])); \
    } \
}

DEFINE_BEAM_STD_UNARY(exp, std::exp)
DEFINE_BEAM_STD_UNARY(exp2, std::exp2)
DEFINE_BEAM_STD_UNARY(log, std::log)
DEFINE_BEAM_STD_UNARY(log2, std::log2)
DEFINE_BEAM_STD_UNARY(sqrt, std::sqrt)
DEFINE_BEAM_STD_UNARY(acos, std::acos)
DEFINE_BEAM_STD_UNARY(acosh, std::acosh)
DEFINE_BEAM_STD_UNARY(asin, std::asin)
DEFINE_BEAM_STD_UNARY(asinh, std::asinh)
DEFINE_BEAM_STD_UNARY(atan, std::atan)
DEFINE_BEAM_STD_UNARY(atanh, std::atanh)
DEFINE_BEAM_STD_UNARY(cos, std::cos)
DEFINE_BEAM_STD_UNARY(cosh, std::cosh)
DEFINE_BEAM_STD_UNARY(sin, std::sin)
DEFINE_BEAM_STD_UNARY(sinh, std::sinh)
DEFINE_BEAM_STD_UNARY(tan, std::tan)
DEFINE_BEAM_STD_UNARY(tanh, std::tanh)

#undef DEFINE_BEAM_STD_UNARY

template<typename T>
inline void beam_frustum(const ErlNifBinary& left, const ErlNifBinary& right, const ErlNifBinary& bottom, const ErlNifBinary& top, const ErlNifBinary& near_value, const ErlNifBinary& far_value, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::frustum(
        *(const T*)left.data,
        *(const T*)right.data,
        *(const T*)bottom.data,
        *(const T*)top.data,
        *(const T*)near_value.data,
        *(const T*)far_value.data
    );
}

template<typename T>
inline void beam_infinite_perspective(const ErlNifBinary& fovy, const ErlNifBinary& aspect, const ErlNifBinary& near_value, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::infinitePerspective(
        *(const T*)fovy.data,
        *(const T*)aspect.data,
        *(const T*)near_value.data
    );
}

template<typename T>
inline void beam_look_at(const ErlNifBinary& eye, const ErlNifBinary& center, const ErlNifBinary& up, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::lookAt(
        *(const glm::vec<3, T, glm::defaultp>*)eye.data,
        *(const glm::vec<3, T, glm::defaultp>*)center.data,
        *(const glm::vec<3, T, glm::defaultp>*)up.data
    );
}

template<typename T>
inline void beam_ortho(const ErlNifBinary& left, const ErlNifBinary& right, const ErlNifBinary& bottom, const ErlNifBinary& top, const ErlNifBinary& near_value, const ErlNifBinary& far_value, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::ortho(
        *(const T*)left.data,
        *(const T*)right.data,
        *(const T*)bottom.data,
        *(const T*)top.data,
        *(const T*)near_value.data,
        *(const T*)far_value.data
    );
}

template<typename T>
inline void beam_perspective(const ErlNifBinary& fovy, const ErlNifBinary& aspect, const ErlNifBinary& near_value, const ErlNifBinary& far_value, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::perspective(
        *(const T*)fovy.data,
        *(const T*)aspect.data,
        *(const T*)near_value.data,
        *(const T*)far_value.data
    );
}

template<typename T>
inline void beam_perspective_fov(const ErlNifBinary& fov, const ErlNifBinary& width, const ErlNifBinary& height, const ErlNifBinary& near_value, const ErlNifBinary& far_value, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::perspectiveFov(
        *(const T*)fov.data,
        *(const T*)width.data,
        *(const T*)height.data,
        *(const T*)near_value.data,
        *(const T*)far_value.data
    );
}

template<typename T>
inline void beam_project(const ErlNifBinary& object, const ErlNifBinary& model, const ErlNifBinary& projection, const ErlNifBinary& viewport, ErlNifBinary& output) {
    *(glm::vec<3, T, glm::defaultp>*)output.data = glm::project(
        *(const glm::vec<3, T, glm::defaultp>*)object.data,
        *(const glm::mat<4, 4, T, glm::defaultp>*)model.data,
        *(const glm::mat<4, 4, T, glm::defaultp>*)projection.data,
        *(const glm::vec<4, T, glm::defaultp>*)viewport.data
    );
}

template<typename T>
inline void beam_rotate_msv(const ErlNifBinary& matrix, const ErlNifBinary& angle, const ErlNifBinary& axis, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::rotate(
        *(const glm::mat<4, 4, T, glm::defaultp>*)matrix.data,
        *(const T*)angle.data,
        *(const glm::vec<3, T, glm::defaultp>*)axis.data
    );
}

template<typename T>
inline void beam_scale_mv(const ErlNifBinary& matrix, const ErlNifBinary& vector, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::scale(
        *(const glm::mat<4, 4, T, glm::defaultp>*)matrix.data,
        *(const glm::vec<3, T, glm::defaultp>*)vector.data
    );
}

template<typename T>
inline void beam_translate_mv(const ErlNifBinary& matrix, const ErlNifBinary& vector, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::translate(
        *(const glm::mat<4, 4, T, glm::defaultp>*)matrix.data,
        *(const glm::vec<3, T, glm::defaultp>*)vector.data
    );
}

template<typename T>
inline void beam_un_project(const ErlNifBinary& window, const ErlNifBinary& model, const ErlNifBinary& projection, const ErlNifBinary& viewport, ErlNifBinary& output) {
    *(glm::vec<3, T, glm::defaultp>*)output.data = glm::unProject(
        *(const glm::vec<3, T, glm::defaultp>*)window.data,
        *(const glm::mat<4, 4, T, glm::defaultp>*)model.data,
        *(const glm::mat<4, 4, T, glm::defaultp>*)projection.data,
        *(const glm::vec<4, T, glm::defaultp>*)viewport.data
    );
}

constexpr long double BEAM_GLM_PI = 3.141592653589793238462643383279502884L;

template<typename T>
inline T beam_radians_value(T value) {
    return value * static_cast<T>(BEAM_GLM_PI / 180.0L);
}

template<typename T>
inline T beam_degrees_value(T value) {
    return value * static_cast<T>(180.0L / BEAM_GLM_PI);
}

#define DEFINE_BEAM_VALUE_UNARY(OP, FN) \
template<typename T> \
inline void beam_##OP##_s(const ErlNifBinary& input, ErlNifBinary& output) { \
    *(T*)output.data = (FN)(*(const T*)input.data); \
} \
 \
template<int L, typename T> \
inline void beam_##OP##_v(const ErlNifBinary& input, ErlNifBinary& output) { \
    const auto& in = *(const glm::vec<L, T, glm::defaultp>*)input.data; \
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data; \
 \
    for (int i = 0; i < L; ++i) { \
        out[i] = (FN)(in[i]); \
    } \
}

DEFINE_BEAM_VALUE_UNARY(degrees, beam_degrees_value<T>)
DEFINE_BEAM_VALUE_UNARY(radians, beam_radians_value<T>)

#undef DEFINE_BEAM_VALUE_UNARY

template<typename T>
inline void beam_inverse_sqrt_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = static_cast<T>(1) / static_cast<T>(std::sqrt(*(const T*)input.data));
}

template<int L, typename T>
inline void beam_inverse_sqrt_v(const ErlNifBinary& input, ErlNifBinary& output) {
    const auto& in = *(const glm::vec<L, T, glm::defaultp>*)input.data;
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = static_cast<T>(1) / static_cast<T>(std::sqrt(in[i]));
    }
}

template<typename T>
inline void beam_pow_ss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = static_cast<T>(std::pow(*(const T*)input_x.data, *(const T*)input_y.data));
}

template<int L, typename T>
inline void beam_pow_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    const auto& x = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& y = *(const glm::vec<L, T, glm::defaultp>*)input_y.data;
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = static_cast<T>(std::pow(x[i], y[i]));
    }
}

template<typename T>
inline void beam_cross_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<3, T, glm::defaultp>*)output.data = glm::cross(
        *(const glm::vec<3, T, glm::defaultp>*)input_x.data,
        *(const glm::vec<3, T, glm::defaultp>*)input_y.data
    );
}

template<int L, typename T>
inline void beam_distance_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::distance(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_y.data
    );
}

template<int L, typename T>
inline void beam_dot_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::dot(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_y.data
    );
}

template<int L, typename T>
inline void beam_face_forward_vvv(const ErlNifBinary& input_n, const ErlNifBinary& input_i, const ErlNifBinary& input_nref, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::faceforward(
        *(const glm::vec<L, T, glm::defaultp>*)input_n.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_i.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_nref.data
    );
}

template<int L, typename T>
inline void beam_length_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::length(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<int L, typename T>
inline void beam_normalize_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::normalize(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<int L, typename T>
inline void beam_reflect_vv(const ErlNifBinary& input_i, const ErlNifBinary& input_n, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::reflect(
        *(const glm::vec<L, T, glm::defaultp>*)input_i.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_n.data
    );
}

template<int L, typename T>
inline void beam_refract_vvs(const ErlNifBinary& input_i, const ErlNifBinary& input_n, const ErlNifBinary& input_eta, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::refract(
        *(const glm::vec<L, T, glm::defaultp>*)input_i.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_n.data,
        *(const T*)input_eta.data
    );
}

template<int N, typename T>
inline void beam_affine_inverse_m(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::mat<N, N, T, glm::defaultp>*)output.data = glm::affineInverse(*(const glm::mat<N, N, T, glm::defaultp>*)input.data);
}

template<int C, int R, typename T>
inline void beam_column_m(const ErlNifBinary& input, int index, ErlNifBinary& output) {
    const auto& in = *(const glm::mat<C, R, T, glm::defaultp>*)input.data;
    *(glm::vec<R, T, glm::defaultp>*)output.data = in[index];
}

template<int N, typename T>
inline void beam_determinant_m(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::determinant(*(const glm::mat<N, N, T, glm::defaultp>*)input.data);
}

template<int N, typename T>
inline void beam_inverse_m(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::mat<N, N, T, glm::defaultp>*)output.data = glm::inverse(*(const glm::mat<N, N, T, glm::defaultp>*)input.data);
}

template<int N, typename T>
inline void beam_inverse_transpose_m(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::mat<N, N, T, glm::defaultp>*)output.data = glm::transpose(glm::inverse(*(const glm::mat<N, N, T, glm::defaultp>*)input.data));
}

template<int C, int R, typename T>
inline void beam_matrix_comp_mult_m(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::mat<C, R, T, glm::defaultp>*)output.data = glm::matrixCompMult(
        *(const glm::mat<C, R, T, glm::defaultp>*)input_x.data,
        *(const glm::mat<C, R, T, glm::defaultp>*)input_y.data
    );
}

template<int C, int R, typename T>
inline void beam_outer_product_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::mat<C, R, T, glm::defaultp>*)output.data = glm::outerProduct(
        *(const glm::vec<R, T, glm::defaultp>*)input_x.data,
        *(const glm::vec<C, T, glm::defaultp>*)input_y.data
    );
}

template<int C, int R, typename T>
inline void beam_row_m(const ErlNifBinary& input, int index, ErlNifBinary& output) {
    const auto& in = *(const glm::mat<C, R, T, glm::defaultp>*)input.data;
    auto& out = *(glm::vec<C, T, glm::defaultp>*)output.data;

    for (int column = 0; column < C; ++column) {
        out[column] = in[column][index];
    }
}

template<int C, int R, typename T>
inline void beam_transpose_m(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::mat<R, C, T, glm::defaultp>*)output.data = glm::transpose(*(const glm::mat<C, R, T, glm::defaultp>*)input.data);
}

inline void beam_pack_double_2x32(const ErlNifBinary& input, ErlNifBinary& output) {
    *(double*)output.data = glm::packDouble2x32(*(const glm::vec<2, uint32_t, glm::defaultp>*)input.data);
}

inline void beam_pack_half_1x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint16_t*)output.data = glm::packHalf1x16(*(const float*)input.data);
}

inline void beam_pack_half_2x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint32_t*)output.data = glm::packHalf2x16(*(const glm::vec<2, float, glm::defaultp>*)input.data);
}

inline void beam_pack_half_4x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint64_t*)output.data = glm::packHalf4x16(*(const glm::vec<4, float, glm::defaultp>*)input.data);
}

inline void beam_pack_snorm_1x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint8_t*)output.data = glm::packSnorm1x8(*(const float*)input.data);
}

inline void beam_pack_snorm_2x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint16_t*)output.data = glm::packSnorm2x8(*(const glm::vec<2, float, glm::defaultp>*)input.data);
}

inline void beam_pack_snorm_1x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint16_t*)output.data = glm::packSnorm1x16(*(const float*)input.data);
}

inline void beam_pack_snorm_2x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint32_t*)output.data = glm::packSnorm2x16(*(const glm::vec<2, float, glm::defaultp>*)input.data);
}

inline void beam_pack_snorm_4x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint32_t*)output.data = glm::packSnorm4x8(*(const glm::vec<4, float, glm::defaultp>*)input.data);
}

inline void beam_pack_snorm_4x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint64_t*)output.data = glm::packSnorm4x16(*(const glm::vec<4, float, glm::defaultp>*)input.data);
}

inline void beam_pack_unorm_1x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint8_t*)output.data = glm::packUnorm1x8(*(const float*)input.data);
}

inline void beam_pack_unorm_2x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint16_t*)output.data = glm::packUnorm2x8(*(const glm::vec<2, float, glm::defaultp>*)input.data);
}

inline void beam_pack_unorm_1x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint16_t*)output.data = glm::packUnorm1x16(*(const float*)input.data);
}

inline void beam_pack_unorm_2x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint32_t*)output.data = glm::packUnorm2x16(*(const glm::vec<2, float, glm::defaultp>*)input.data);
}

inline void beam_pack_unorm_4x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint32_t*)output.data = glm::packUnorm4x8(*(const glm::vec<4, float, glm::defaultp>*)input.data);
}

inline void beam_pack_unorm_4x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint64_t*)output.data = glm::packUnorm4x16(*(const glm::vec<4, float, glm::defaultp>*)input.data);
}

inline void beam_unpack_double_2x32(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<2, uint32_t, glm::defaultp>*)output.data = glm::unpackDouble2x32(*(const double*)input.data);
}

inline void beam_unpack_half_1x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(float*)output.data = glm::unpackHalf1x16(*(const uint16_t*)input.data);
}

inline void beam_unpack_half_2x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<2, float, glm::defaultp>*)output.data = glm::unpackHalf2x16(*(const uint32_t*)input.data);
}

inline void beam_unpack_half_4x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<4, float, glm::defaultp>*)output.data = glm::unpackHalf4x16(*(const uint64_t*)input.data);
}

inline void beam_unpack_snorm_1x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(float*)output.data = glm::unpackSnorm1x8(*(const uint8_t*)input.data);
}

inline void beam_unpack_snorm_2x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<2, float, glm::defaultp>*)output.data = glm::unpackSnorm2x8(*(const uint16_t*)input.data);
}

inline void beam_unpack_snorm_1x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(float*)output.data = glm::unpackSnorm1x16(*(const uint16_t*)input.data);
}

inline void beam_unpack_snorm_2x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<2, float, glm::defaultp>*)output.data = glm::unpackSnorm2x16(*(const uint32_t*)input.data);
}

inline void beam_unpack_snorm_4x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<4, float, glm::defaultp>*)output.data = glm::unpackSnorm4x8(*(const uint32_t*)input.data);
}

inline void beam_unpack_snorm_4x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<4, float, glm::defaultp>*)output.data = glm::unpackSnorm4x16(*(const uint64_t*)input.data);
}

inline void beam_unpack_unorm_1x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(float*)output.data = glm::unpackUnorm1x8(*(const uint8_t*)input.data);
}

inline void beam_unpack_unorm_2x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<2, float, glm::defaultp>*)output.data = glm::unpackUnorm2x8(*(const uint16_t*)input.data);
}

inline void beam_unpack_unorm_1x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(float*)output.data = glm::unpackUnorm1x16(*(const uint16_t*)input.data);
}

inline void beam_unpack_unorm_2x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<2, float, glm::defaultp>*)output.data = glm::unpackUnorm2x16(*(const uint32_t*)input.data);
}

inline void beam_unpack_unorm_4x8(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<4, float, glm::defaultp>*)output.data = glm::unpackUnorm4x8(*(const uint32_t*)input.data);
}

inline void beam_unpack_unorm_4x16(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<4, float, glm::defaultp>*)output.data = glm::unpackUnorm4x16(*(const uint64_t*)input.data);
}

template<typename T>
inline void beam_angle_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::angle(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_angle_axis_sv(const ErlNifBinary& input_angle, const ErlNifBinary& input_axis, ErlNifBinary& output) {
    *(glm::qua<T, glm::defaultp>*)output.data = glm::angleAxis(
        *(const T*)input_angle.data,
        *(const glm::vec<3, T, glm::defaultp>*)input_axis.data
    );
}

template<typename T>
inline void beam_axis_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<3, T, glm::defaultp>*)output.data = glm::axis(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_conjugate_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::qua<T, glm::defaultp>*)output.data = glm::conjugate(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_dot_qq(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::dot(
        *(const glm::qua<T, glm::defaultp>*)input_x.data,
        *(const glm::qua<T, glm::defaultp>*)input_y.data
    );
}

template<typename T>
inline void beam_euler_angles_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<3, T, glm::defaultp>*)output.data = glm::eulerAngles(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_inverse_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::qua<T, glm::defaultp>*)output.data = glm::inverse(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_length_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::length(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_mat3_cast_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::mat<3, 3, T, glm::defaultp>*)output.data = glm::mat3_cast(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_mat4_cast_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::mat<4, 4, T, glm::defaultp>*)output.data = glm::mat4_cast(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_mix_qqs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, const ErlNifBinary& input_a, ErlNifBinary& output) {
    *(glm::qua<T, glm::defaultp>*)output.data = glm::mix(
        *(const glm::qua<T, glm::defaultp>*)input_x.data,
        *(const glm::qua<T, glm::defaultp>*)input_y.data,
        *(const T*)input_a.data
    );
}

template<typename T>
inline void beam_normalize_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::qua<T, glm::defaultp>*)output.data = glm::normalize(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_pitch_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::pitch(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<int N, typename T>
inline void beam_quat_cast_m(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::qua<T, glm::defaultp>*)output.data = glm::quat_cast(*(const glm::mat<N, N, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_roll_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::roll(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_rotate_qv3(const ErlNifBinary& input_q, const ErlNifBinary& input_v, ErlNifBinary& output) {
    *(glm::vec<3, T, glm::defaultp>*)output.data = glm::rotate(
        *(const glm::qua<T, glm::defaultp>*)input_q.data,
        *(const glm::vec<3, T, glm::defaultp>*)input_v.data
    );
}

template<typename T>
inline void beam_rotate_qv4(const ErlNifBinary& input_q, const ErlNifBinary& input_v, ErlNifBinary& output) {
    const auto& q = *(const glm::qua<T, glm::defaultp>*)input_q.data;
    const auto& in = *(const glm::vec<4, T, glm::defaultp>*)input_v.data;
    const glm::vec<3, T, glm::defaultp> rotated = glm::rotate(q, glm::vec<3, T, glm::defaultp>(in.x, in.y, in.z));

    *(glm::vec<4, T, glm::defaultp>*)output.data = glm::vec<4, T, glm::defaultp>(rotated.x, rotated.y, rotated.z, in.w);
}

template<typename T>
inline void beam_slerp_qqs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, const ErlNifBinary& input_a, ErlNifBinary& output) {
    *(glm::qua<T, glm::defaultp>*)output.data = glm::slerp(
        *(const glm::qua<T, glm::defaultp>*)input_x.data,
        *(const glm::qua<T, glm::defaultp>*)input_y.data,
        *(const T*)input_a.data
    );
}

template<typename T>
inline void beam_yaw_q(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::yaw(*(const glm::qua<T, glm::defaultp>*)input.data);
}

template<int L, typename T>
inline void beam_is_inf_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, bool, glm::defaultp>*)output.data = glm::isinf(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<int L, typename T>
inline void beam_is_nan_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, bool, glm::defaultp>*)output.data = glm::isnan(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_bit_count_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(int32_t*)output.data = glm::bitCount(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_bit_count_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, int32_t, glm::defaultp>*)output.data = glm::bitCount(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline T beam_bitfield_extract_value(T value, int32_t offset, int32_t bits) {
    using U = typename std::make_unsigned<T>::type;
    constexpr int width = sizeof(U) * 8;

    if (bits <= 0) {
        return static_cast<T>(0);
    }

    U mask = bits >= width ? static_cast<U>(~static_cast<U>(0)) : static_cast<U>((static_cast<U>(1) << bits) - 1);
    U extracted = static_cast<U>(static_cast<U>(value) >> offset) & mask;

    if (std::is_signed<T>::value && bits < width) {
        U sign_bit = static_cast<U>(1) << (bits - 1);
        if ((extracted & sign_bit) != 0) {
            extracted |= static_cast<U>(~mask);
        }
    }

    return static_cast<T>(extracted);
}

template<typename T>
inline T beam_bitfield_insert_value(T base, T insert, int32_t offset, int32_t bits) {
    using U = typename std::make_unsigned<T>::type;
    constexpr int width = sizeof(U) * 8;

    if (bits <= 0) {
        return base;
    }

    U mask = bits >= width ? static_cast<U>(~static_cast<U>(0)) : static_cast<U>((static_cast<U>(1) << bits) - 1);
    U shifted_mask = bits >= width ? mask : static_cast<U>(mask << offset);
    U base_value = static_cast<U>(base);
    U insert_value = static_cast<U>(insert);

    return static_cast<T>((base_value & static_cast<U>(~shifted_mask)) | ((insert_value << offset) & shifted_mask));
}

template<typename T>
inline void beam_bitfield_extract_s(const ErlNifBinary& input_value, const ErlNifBinary& input_offset, const ErlNifBinary& input_bits, ErlNifBinary& output) {
    *(T*)output.data = beam_bitfield_extract_value(
        *(const T*)input_value.data,
        *(const int32_t*)input_offset.data,
        *(const int32_t*)input_bits.data
    );
}

template<int L, typename T>
inline void beam_bitfield_extract_v(const ErlNifBinary& input_value, const ErlNifBinary& input_offset, const ErlNifBinary& input_bits, ErlNifBinary& output) {
    const auto& in = *(const glm::vec<L, T, glm::defaultp>*)input_value.data;
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data;
    int32_t offset = *(const int32_t*)input_offset.data;
    int32_t bits = *(const int32_t*)input_bits.data;

    for (int component = 0; component < L; ++component) {
        out[component] = beam_bitfield_extract_value(in[component], offset, bits);
    }
}

template<typename T>
inline void beam_bitfield_insert_s(const ErlNifBinary& input_base, const ErlNifBinary& input_insert, const ErlNifBinary& input_offset, const ErlNifBinary& input_bits, ErlNifBinary& output) {
    *(T*)output.data = beam_bitfield_insert_value(
        *(const T*)input_base.data,
        *(const T*)input_insert.data,
        *(const int32_t*)input_offset.data,
        *(const int32_t*)input_bits.data
    );
}

template<int L, typename T>
inline void beam_bitfield_insert_v(const ErlNifBinary& input_base, const ErlNifBinary& input_insert, const ErlNifBinary& input_offset, const ErlNifBinary& input_bits, ErlNifBinary& output) {
    const auto& base = *(const glm::vec<L, T, glm::defaultp>*)input_base.data;
    const auto& insert = *(const glm::vec<L, T, glm::defaultp>*)input_insert.data;
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data;
    int32_t offset = *(const int32_t*)input_offset.data;
    int32_t bits = *(const int32_t*)input_bits.data;

    for (int component = 0; component < L; ++component) {
        out[component] = beam_bitfield_insert_value(base[component], insert[component], offset, bits);
    }
}

template<typename T>
inline void beam_bitfield_reverse_s(const ErlNifBinary& input, ErlNifBinary& output) {
    using U = typename std::make_unsigned<T>::type;

    U value = static_cast<U>(*(const T*)input.data);
    U reversed = 0;
    for (size_t i = 0; i < sizeof(U) * 8; ++i) {
        reversed = static_cast<U>((reversed << 1) | (value & static_cast<U>(1)));
        value = static_cast<U>(value >> 1);
    }

    *(T*)output.data = static_cast<T>(reversed);
}

template<int L, typename T>
inline void beam_bitfield_reverse_v(const ErlNifBinary& input, ErlNifBinary& output) {
    using U = typename std::make_unsigned<T>::type;

    const auto& in = *(const glm::vec<L, T, glm::defaultp>*)input.data;
    auto& out = *(glm::vec<L, T, glm::defaultp>*)output.data;

    for (int component = 0; component < L; ++component) {
        U value = static_cast<U>(in[component]);
        U reversed = 0;
        for (size_t i = 0; i < sizeof(U) * 8; ++i) {
            reversed = static_cast<U>((reversed << 1) | (value & static_cast<U>(1)));
            value = static_cast<U>(value >> 1);
        }
        out[component] = static_cast<T>(reversed);
    }
}

template<typename T>
inline void beam_find_lsb_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(int32_t*)output.data = glm::findLSB(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_find_lsb_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, int32_t, glm::defaultp>*)output.data = glm::findLSB(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_find_msb_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(int32_t*)output.data = glm::findMSB(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_find_msb_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, int32_t, glm::defaultp>*)output.data = glm::findMSB(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

inline void beam_imul_extended_s(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& msb_output, ErlNifBinary& lsb_output) {
    glm::imulExtended(
        *(const int32_t*)input_x.data,
        *(const int32_t*)input_y.data,
        *(int32_t*)msb_output.data,
        *(int32_t*)lsb_output.data
    );
}

template<int L>
inline void beam_imul_extended_v(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& msb_output, ErlNifBinary& lsb_output) {
    glm::imulExtended(
        *(const glm::vec<L, int32_t, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, int32_t, glm::defaultp>*)input_y.data,
        *(glm::vec<L, int32_t, glm::defaultp>*)msb_output.data,
        *(glm::vec<L, int32_t, glm::defaultp>*)lsb_output.data
    );
}

inline void beam_uadd_carry_s(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& result_output, ErlNifBinary& carry_output) {
    uint32_t carry;
    *(uint32_t*)result_output.data = glm::uaddCarry(*(const uint32_t*)input_x.data, *(const uint32_t*)input_y.data, carry);
    *(uint32_t*)carry_output.data = carry;
}

template<int L>
inline void beam_uadd_carry_v(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& result_output, ErlNifBinary& carry_output) {
    *(glm::vec<L, uint32_t, glm::defaultp>*)result_output.data = glm::uaddCarry(
        *(const glm::vec<L, uint32_t, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, uint32_t, glm::defaultp>*)input_y.data,
        *(glm::vec<L, uint32_t, glm::defaultp>*)carry_output.data
    );
}

inline void beam_umul_extended_s(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& msb_output, ErlNifBinary& lsb_output) {
    glm::umulExtended(
        *(const uint32_t*)input_x.data,
        *(const uint32_t*)input_y.data,
        *(uint32_t*)msb_output.data,
        *(uint32_t*)lsb_output.data
    );
}

template<int L>
inline void beam_umul_extended_v(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& msb_output, ErlNifBinary& lsb_output) {
    glm::umulExtended(
        *(const glm::vec<L, uint32_t, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, uint32_t, glm::defaultp>*)input_y.data,
        *(glm::vec<L, uint32_t, glm::defaultp>*)msb_output.data,
        *(glm::vec<L, uint32_t, glm::defaultp>*)lsb_output.data
    );
}

inline void beam_usub_borrow_s(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& result_output, ErlNifBinary& borrow_output) {
    uint32_t borrow;
    *(uint32_t*)result_output.data = glm::usubBorrow(*(const uint32_t*)input_x.data, *(const uint32_t*)input_y.data, borrow);
    *(uint32_t*)borrow_output.data = borrow;
}

template<int L>
inline void beam_usub_borrow_v(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& result_output, ErlNifBinary& borrow_output) {
    *(glm::vec<L, uint32_t, glm::defaultp>*)result_output.data = glm::usubBorrow(
        *(const glm::vec<L, uint32_t, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, uint32_t, glm::defaultp>*)input_y.data,
        *(glm::vec<L, uint32_t, glm::defaultp>*)borrow_output.data
    );
}

template<typename T>
inline void beam_is_multiple_ss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(uint8_t*)output.data = glm::isMultiple(*(const T*)input_x.data, *(const T*)input_y.data) ? 1 : 0;
}

template<int L, typename T>
inline void beam_is_multiple_vs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, bool, glm::defaultp>*)output.data = glm::isMultiple(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const T*)input_y.data
    );
}

template<int L, typename T>
inline void beam_is_multiple_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, bool, glm::defaultp>*)output.data = glm::isMultiple(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_y.data
    );
}

template<typename T>
inline void beam_is_power_of_two_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint8_t*)output.data = glm::isPowerOfTwo(*(const T*)input.data) ? 1 : 0;
}

template<int L, typename T>
inline void beam_is_power_of_two_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, bool, glm::defaultp>*)output.data = glm::isPowerOfTwo(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_next_multiple_ss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::nextMultiple(*(const T*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_next_multiple_vs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::nextMultiple(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const T*)input_y.data
    );
}

template<int L, typename T>
inline void beam_next_multiple_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::nextMultiple(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_y.data
    );
}

template<typename T>
inline void beam_next_power_of_two_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::nextPowerOfTwo(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_next_power_of_two_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::nextPowerOfTwo(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_prev_multiple_ss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::prevMultiple(*(const T*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_prev_multiple_vs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::prevMultiple(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const T*)input_y.data
    );
}

template<int L, typename T>
inline void beam_prev_multiple_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::prevMultiple(
        *(const glm::vec<L, T, glm::defaultp>*)input_x.data,
        *(const glm::vec<L, T, glm::defaultp>*)input_y.data
    );
}

template<typename T>
inline void beam_prev_power_of_two_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::prevPowerOfTwo(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_prev_power_of_two_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::prevPowerOfTwo(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_max_ss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::max(*(const T*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_max_vs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::max(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_max_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::max(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const glm::vec<L, T, glm::defaultp>*)input_y.data);
}

template<typename T>
inline void beam_min_ss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::min(*(const T*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_min_vs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::min(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_min_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::min(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const glm::vec<L, T, glm::defaultp>*)input_y.data);
}

template<typename T>
inline void beam_mix_sss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, const ErlNifBinary& input_a, ErlNifBinary& output) {
    *(T*)output.data = glm::mix(*(const T*)input_x.data, *(const T*)input_y.data, *(const T*)input_a.data);
}

template<int L, typename T>
inline void beam_mix_vvs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, const ErlNifBinary& input_a, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::mix(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const glm::vec<L, T, glm::defaultp>*)input_y.data, *(const T*)input_a.data);
}

template<int L, typename T>
inline void beam_mix_vvv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, const ErlNifBinary& input_a, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::mix(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const glm::vec<L, T, glm::defaultp>*)input_y.data, *(const glm::vec<L, T, glm::defaultp>*)input_a.data);
}

template<typename T>
inline void beam_mod_ss(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(T*)output.data = glm::mod(*(const T*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_mod_vs(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::mod(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const T*)input_y.data);
}

template<int L, typename T>
inline void beam_mod_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::mod(*(const glm::vec<L, T, glm::defaultp>*)input_x.data, *(const glm::vec<L, T, glm::defaultp>*)input_y.data);
}

template<typename T>
inline void beam_modf_s(const ErlNifBinary& input, ErlNifBinary& fractional_output, ErlNifBinary& integral_output) {
    T integral;
    *(T*)fractional_output.data = glm::modf(*(const T*)input.data, integral);
    *(T*)integral_output.data = integral;
}

template<int L, typename T>
inline void beam_modf_v(const ErlNifBinary& input, ErlNifBinary& fractional_output, ErlNifBinary& integral_output) {
    glm::vec<L, T, glm::defaultp> integral;
    *(glm::vec<L, T, glm::defaultp>*)fractional_output.data = glm::modf(*(const glm::vec<L, T, glm::defaultp>*)input.data, integral);
    *(glm::vec<L, T, glm::defaultp>*)integral_output.data = integral;
}

template<int L, typename T>
inline void beam_round(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::round(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<int L, typename T>
inline void beam_round_even(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::roundEven(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_sign_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::sign(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_sign_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::sign(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<typename T>
inline void beam_smoothstep_sss(const ErlNifBinary& input_edge0, const ErlNifBinary& input_edge1, const ErlNifBinary& input_x, ErlNifBinary& output) {
    *(T*)output.data = glm::smoothstep(*(const T*)input_edge0.data, *(const T*)input_edge1.data, *(const T*)input_x.data);
}

template<int L, typename T>
inline void beam_smoothstep_ssv(const ErlNifBinary& input_edge0, const ErlNifBinary& input_edge1, const ErlNifBinary& input_x, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::smoothstep(*(const T*)input_edge0.data, *(const T*)input_edge1.data, *(const glm::vec<L, T, glm::defaultp>*)input_x.data);
}

template<int L, typename T>
inline void beam_smoothstep_vvv(const ErlNifBinary& input_edge0, const ErlNifBinary& input_edge1, const ErlNifBinary& input_x, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::smoothstep(*(const glm::vec<L, T, glm::defaultp>*)input_edge0.data, *(const glm::vec<L, T, glm::defaultp>*)input_edge1.data, *(const glm::vec<L, T, glm::defaultp>*)input_x.data);
}

template<typename T>
inline void beam_step_ss(const ErlNifBinary& input_edge, const ErlNifBinary& input_x, ErlNifBinary& output) {
    *(T*)output.data = glm::step(*(const T*)input_edge.data, *(const T*)input_x.data);
}

template<int L, typename T>
inline void beam_step_sv(const ErlNifBinary& input_edge, const ErlNifBinary& input_x, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::step(*(const T*)input_edge.data, *(const glm::vec<L, T, glm::defaultp>*)input_x.data);
}

template<int L, typename T>
inline void beam_step_vv(const ErlNifBinary& input_edge, const ErlNifBinary& input_x, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::step(*(const glm::vec<L, T, glm::defaultp>*)input_edge.data, *(const glm::vec<L, T, glm::defaultp>*)input_x.data);
}

template<typename T>
inline void beam_trunc_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(T*)output.data = glm::trunc(*(const T*)input.data);
}

template<int L, typename T>
inline void beam_trunc_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::trunc(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

inline void beam_uint_bits_to_float_s(const ErlNifBinary& input, ErlNifBinary& output) {
    *(float*)output.data = glm::uintBitsToFloat(*(const uint32_t*)input.data);
}

template<int L>
inline void beam_uint_bits_to_float_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, float, glm::defaultp>*)output.data = glm::uintBitsToFloat(*(const glm::vec<L, uint32_t, glm::defaultp>*)input.data);
}

template<int L>
inline void beam_all_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint8_t*)output.data = glm::all(*(const glm::vec<L, bool, glm::defaultp>*)input.data) ? 1 : 0;
}

template<int L>
inline void beam_any_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(uint8_t*)output.data = glm::any(*(const glm::vec<L, bool, glm::defaultp>*)input.data) ? 1 : 0;
}

template<int L, typename T>
inline void beam_equal_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    const auto& x = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& y = *(const glm::vec<L, T, glm::defaultp>*)input_y.data;
    auto& out = *(glm::vec<L, bool, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = (x[i] == y[i]);
    }
}

template<int L, typename T>
inline void beam_greater_than_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    const auto& x = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& y = *(const glm::vec<L, T, glm::defaultp>*)input_y.data;
    auto& out = *(glm::vec<L, bool, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = (x[i] > y[i]);
    }
}

template<int L, typename T>
inline void beam_greater_than_equal_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    const auto& x = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& y = *(const glm::vec<L, T, glm::defaultp>*)input_y.data;
    auto& out = *(glm::vec<L, bool, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = (x[i] >= y[i]);
    }
}

template<int L, typename T>
inline void beam_less_than_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    const auto& x = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& y = *(const glm::vec<L, T, glm::defaultp>*)input_y.data;
    auto& out = *(glm::vec<L, bool, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = (x[i] < y[i]);
    }
}

template<int L, typename T>
inline void beam_less_than_equal_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    const auto& x = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& y = *(const glm::vec<L, T, glm::defaultp>*)input_y.data;
    auto& out = *(glm::vec<L, bool, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = (x[i] <= y[i]);
    }
}

template<int L>
inline void beam_not_v(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, bool, glm::defaultp>*)output.data = glm::not_(*(const glm::vec<L, bool, glm::defaultp>*)input.data);
}

template<int L, typename T>
inline void beam_not_equal_vv(const ErlNifBinary& input_x, const ErlNifBinary& input_y, ErlNifBinary& output) {
    const auto& x = *(const glm::vec<L, T, glm::defaultp>*)input_x.data;
    const auto& y = *(const glm::vec<L, T, glm::defaultp>*)input_y.data;
    auto& out = *(glm::vec<L, bool, glm::defaultp>*)output.data;

    for (int i = 0; i < L; ++i) {
        out[i] = (x[i] != y[i]);
    }
}

#define DEFINE_BEAM_EASE_FUNCTION_2ARG(name) \
template<typename T> \
inline void beam_##name(const ErlNifBinary& input, ErlNifBinary& output) { \
    *(T*)output.data = glm::name(*(const T*)input.data); \
}

#define DEFINE_BEAM_EASE_FUNCTION_3ARG(name) \
template<typename T> \
inline void beam_##name(const ErlNifBinary& input1, const ErlNifBinary& input2, ErlNifBinary& output) { \
    *(T*)output.data = glm::name(*(const T*)input1.data, *(const T*)input2.data); \
}

DEFINE_BEAM_EASE_FUNCTION_2ARG(backEaseIn)
DEFINE_BEAM_EASE_FUNCTION_3ARG(backEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(backEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_3ARG(backEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(backEaseOut)
DEFINE_BEAM_EASE_FUNCTION_3ARG(backEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(bounceEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(bounceEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(bounceEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(circularEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(circularEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(circularEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(cubicEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(cubicEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(cubicEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(elasticEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(elasticEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(elasticEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(exponentialEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(exponentialEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(exponentialEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(linearInterpolation)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quadraticEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quadraticEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quadraticEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quarticEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quarticEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quarticEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quinticEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quinticEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(quinticEaseOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(sineEaseIn)
DEFINE_BEAM_EASE_FUNCTION_2ARG(sineEaseInOut)
DEFINE_BEAM_EASE_FUNCTION_2ARG(sineEaseOut)

// Placeholder native inventory for upcoming operational families.
//
// These are intentionally listed before implementation so the native layer can
// be completed family by family without rediscovering the target entrypoints.
//
// glm_common:
// beam_abs, beam_ceil, beam_float_bits_to_int, beam_float_bits_to_uint,
// beam_floor, beam_fma, beam_fract, beam_frexp, beam_int_bits_to_float,
// beam_is_inf, beam_is_nan, beam_ldexp, beam_max, beam_min, beam_mix,
// beam_mod, beam_modf, beam_sign, beam_smoothstep, beam_step, beam_trunc,
// beam_uint_bits_to_float
//
// glm_angle:
// beam_acos, beam_acosh, beam_asin, beam_asinh, beam_atan, beam_atanh,
// beam_cos, beam_cosh, beam_degrees, beam_radians, beam_sin, beam_sinh,
// beam_tan, beam_tanh
//
// glm_integer:
// beam_bit_count, beam_bitfield_extract, beam_bitfield_insert,
// beam_bitfield_reverse, beam_find_lsb, beam_find_msb, beam_imul_extended,
// beam_is_multiple, beam_is_power_of_two, beam_next_multiple,
// beam_next_power_of_two, beam_prev_multiple, beam_prev_power_of_two,
// beam_uadd_carry, beam_umul_extended, beam_usub_borrow
//
// glm_matrix:
// beam_affine_inverse, beam_column, beam_determinant, beam_inverse,
// beam_inverse_transpose, beam_matrix_comp_mult, beam_outer_product,
// beam_row, beam_transpose
//
// glm_packing:
// beam_pack_double_2x32, beam_pack_half_2x16, beam_pack_snorm_2x16,
// beam_pack_snorm_4x8, beam_pack_unorm_2x16, beam_pack_unorm_4x8,
// beam_unpack_double_2x32, beam_unpack_half_2x16, beam_unpack_snorm_2x16,
// beam_unpack_snorm_4x8, beam_unpack_unorm_2x16, beam_unpack_unorm_4x8
//
// glm_quat:
// beam_angle, beam_angle_axis, beam_axis, beam_conjugate, beam_dot,
// beam_euler_angles, beam_length, beam_mat3_cast, beam_mat4_cast, beam_mix,
// beam_normalize, beam_pitch, beam_quat_cast, beam_roll, beam_rotate,
// beam_slerp, beam_yaw
//
// glm_relational:
// beam_all, beam_any, beam_equal, beam_greater_than,
// beam_greater_than_equal, beam_less_than, beam_less_than_equal, beam_not,
// beam_not_equal
//
// glm_transform:
// beam_frustum, beam_infinite_perspective, beam_look_at, beam_ortho,
// beam_perspective, beam_perspective_fov, beam_project, beam_rotate,
// beam_scale, beam_translate, beam_un_project
//
// glm_vector:
// beam_cross, beam_distance, beam_dot, beam_face_forward, beam_length,
// beam_normalize, beam_reflect, beam_refract
