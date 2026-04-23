---
description: "Use when editing src/glm*.erl files in the glm binding, including glm.erl, glm_raw.erl, and family modules such as glm_common, glm_vector, glm_matrix, glm_transform, glm_angle, glm_quat, and glm_easing. Covers module boundaries, wrapped shapes, and safe/raw layer conventions."
applyTo: "src/glm*.erl"
---
# glm Erlang Layer

- Keep `glm.erl` focused on primitives: scalar, vector, matrix, quaternion constructors, accessors, and shared types. Do not grow it into a catch-all operational API module.
- Put operational functions in family modules such as `glm_common`, `glm_vector`, `glm_matrix`, `glm_transform`, `glm_angle`, `glm_quat`, and `glm_easing`. Prefer semantic grouping over upstream GLM header names or extension noise.
- Preserve stable BEAM shapes. Public functions should consume and return wrapped Erlang values like `{scalar, T, Bin}`, `{vec, L, T, Bin}`, `{mat, C, R, T, Bin}`, and tuple-returning wrappers when the GLM operation naturally has multiple outputs.
- When introducing a new public function, carry it through all relevant layers together: family module, `glm_raw.erl`, native layer, tests, and docs if the user-facing surface changed. Do not leave a public export pointing at placeholder raw behavior.
- In `glm_raw.erl`, keep the raw API thin and mechanical: convert Erlang-side pattern atoms to compact integers, map types through `?GLM_TYPE`, and let the NIF layer own the actual computation. Be careful when editing spec blocks; malformed insertions here are easy to miss and break compile.
- Match function acceptance rules to the chosen BEAM shape, not just to what upstream GLM technically exposes. Prefer a smaller, coherent API over exposing every overload if it complicates Erlang typing or return conventions.
- Reuse established conventions from this repo: `modf/1` and `frexp/1` return wrapped tuples, float bit-casts map to `{int, 32}` and `{uint, 32}`, and vector exponent outputs for floating operations use `vec(L, {int, 32})` when the operation is component-wise.
- Keep tests family-local. New `glm_common` behavior belongs in `test/glm_common_test.erl`, and validation should rely on `rebar3 compile` and focused `rebar3 eunit -m <module>` runs.