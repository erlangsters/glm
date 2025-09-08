//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#include <glm/glm.hpp>
#define GLM_ENABLE_EXPERIMENTAL
#include <glm/gtx/easing.hpp>
#include <erl_nif.h>

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

template<int L, typename T>
inline void beam_round(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::round(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

template<int L, typename T>
inline void beam_round_even(const ErlNifBinary& input, ErlNifBinary& output) {
    *(glm::vec<L, T, glm::defaultp>*)output.data = glm::roundEven(*(const glm::vec<L, T, glm::defaultp>*)input.data);
}

#define DEFINE_BEAM_EASE_FUNCTION_2ARG(name) \
template<typename T> \
inline void beam_##name(const ErlNifBinary& input, ErlNifBinary& output) { \
    *(T*)output.data = glm::name(*(const T*)input.data); \
}

#define DEFINE_BEAM_EASE_FUNCTION_3ARG(name) \
template<typename T> \
inline void beam_##name(const ErlNifBinary& input1, const ErlNifBinary& input2, ErlNifBinary& output) { \
    *(T*)output.data = glm::name(*(const T*)input1.data, (const T*)input2.data); \
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
