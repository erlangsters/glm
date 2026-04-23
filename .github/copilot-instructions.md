# GLM Copilot Guidelines

- `glm` is a native binding in the `graphics-stack` family.
- Treat it as a near-release binding. Prefer focused edits that preserve API mapping and the stable BEAM-facing math shapes.
- The native layer uses upstream GLM headers through CMake `FetchContent` and links `glm::glm-header-only`; do not add a system package or Rebar dependency for GLM itself unless the build strategy is intentionally changing.
- For CI and workflow work, treat GLM as self-contained apart from Erlang/OTP and CMake. Linux should not need a `libglm-dev` package just to build this repository.
- The `glm` binding is not expected to mirror 100% of upstream GLM. Prefer reasonable coverage over exhaustive coverage.
- Reasonable coverage means prioritizing the common graphics math path: core scalar, vector, matrix, transform, and quaternion primitives and the most broadly useful operations on top of them.
- Prefer APIs that map cleanly to a small number of stable Erlang shapes over exposing upstream alias noise, qualifier variants, or template artifacts.
- For the public API, prefer first-class support for these primitives: scalar, vectors of length 2 to 4, matrices with columns and rows 2 to 4, and quaternions.
- Keep primitives and their constructors/accessors in `glm.erl`.
- Split operational APIs into grouped modules such as `glm_common`, `glm_vector`, `glm_matrix`, `glm_transform`, `glm_angle`, `glm_quat`, and `glm_easing` instead of growing `glm.erl` into a catch-all module.
- Keep native and Erlang layers in sync: when adding or changing NIF-backed functionality, update the safe API, raw API, tests, and docs together rather than landing only one layer.
- Deprioritize exotic or low-value surface area unless there is a clear Erlang use case, a clean type shape, and a plan to support the safe API, raw API, tests, and docs together.