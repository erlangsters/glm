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
#include <erl_nif.h>

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
