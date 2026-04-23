---
description: "Use when editing c_src/beam_glm.cpp or c_src/beam_glm.hpp, adding NIF entrypoints, helper templates, dispatch macros, or native GLM/std math bindings. Covers beam_glm C++ layer conventions and cross-layer synchronization with glm_raw."
applyTo: "c_src/beam_glm.cpp, c_src/beam_glm.hpp"
---
# beam_glm C++ Layer

- Treat `beam_glm.cpp` and `beam_glm.hpp` as a single unit with `src/glm_raw.erl`. New native functionality is not complete until the helper templates, NIF handler, `nif_functions[]` registration, and raw Erlang declarations all agree on arity and shape.
- Keep dispatch naming aligned with the existing macro conventions. If a handler uses `DISPATCH_FLOAT_SCALAR_TERNARY`, the helper must match names like `beam_<op>_sss`; if it uses `DISPATCH_FLOAT_VECTOR_VECTOR_TERNARY`, the helper must match `beam_<op>_vvv`.
- Preserve the current type and shape encoding model: Erlang passes GLM type tags, optional vector length, and compact pattern integers. Do not invent a second dispatch scheme unless the existing one genuinely cannot express the operation.
- Prefer component-wise implementations when GLM template entrypoints are awkward or unstable for the NIF build. If `glm::` overloads introduce linker or template-resolution issues, use `std::` math or explicit per-component loops rather than forcing the GLM call.
- Be precise about binary sizes. Scalar outputs use the input scalar byte width, vector outputs use the full vector binary size, bool vectors use one byte per component, and tuple-returning operations must allocate each output independently.
- Keep handlers defensive and predictable: validate type, validate length when needed, reject unsupported patterns with `badarg`, and avoid silent coercions between float, double, signed int, unsigned int, or bool.
- When adding tuple-returning functions such as `modf` or `frexp`, keep the BEAM-facing return shape in mind and allocate binaries so the raw layer can return wrapped values without post-processing hacks.
