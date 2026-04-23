# GLM Binding Surface

This file is the internal scope note for what the GLM binding exposes today,
what remains reasonable to add later, and what is intentionally outside the
first public surface.

The public-facing mapping document lives in `docs/api-mapping.md`. This file is
the authoritative internal inventory.

## Status vocabulary

- `implemented`: present in the repository and part of the current surface.
- `planned`: intended for the public surface but not landed yet.
- `deferred`: reasonable later, but not part of the first release target.
- `not planned`: intentionally out of scope unless a concrete use case changes
  the decision.

## Scope rules

- Keep primitives, constructors, accessors, and primitive mutation helpers in
  `glm.erl`.
- Keep `glm_raw.erl` as the thin raw and binary-oriented layer beneath the safe
  wrappers.
- Group operational APIs by BEAM semantics, not by upstream GLM header names.
- Prefer stable wrapped shapes over exposing qualifier variants, alias noise,
  and template artifacts directly.
- When adding NIF-backed functionality, update the safe API, raw API, tests,
  and docs together.

## Module inventory

| Module | Upstream provenance | Status | Notes |
| --- | --- | --- | --- |
| `glm` | Core primitive types and constructors | implemented | First-class scalar, vector, matrix, and quaternion layer plus public accessors and primitive helpers. |
| `glm_raw` | Internal raw bridge across the implemented surface | implemented | Binary-oriented layer beneath the safe wrappers. It is available, but it is not the default public entry point. |
| `glm_common` | Core common functions plus selected scalar and vector common helpers | implemented | Includes clamp, rounding, interpolation, decomposition-style helpers such as `frexp` and `modf`, classification, and floating-point bit casts. |
| `glm_angle` | Core angle and trigonometric functions | implemented | Covers `radians`, `degrees`, trigonometric, inverse-trigonometric, and hyperbolic functions. |
| `glm_exponential` | Core exponential functions | implemented | Covers `pow`, `exp`, `exp2`, `log`, `log2`, `sqrt`, and `inverse_sqrt`. |
| `glm_vector` | Core geometric functions plus selected vector-focused extensions | implemented | Covers `length`, `distance`, `dot`, `cross`, `normalize`, `face_forward`, `reflect`, and `refract`. |
| `glm_matrix` | Core matrix functions plus selected GTC and GTX helpers | implemented | Covers determinant, inverse, inverse transpose, transpose, component-wise matrix multiply, outer product, row and column access, and selected matrix helpers that fit the wrapped matrix surface. |
| `glm_transform` | `GLM_EXT_matrix_clip_space`, `GLM_EXT_matrix_projection`, `GLM_EXT_matrix_transform`, `GLM_GTX_transform` | implemented | Covers `frustum`, `infinite_perspective`, `look_at`, `ortho`, `perspective`, `perspective_fov`, `project`, `un_project`, `rotate`, `scale`, and `translate`. |
| `glm_quat` | `GLM_GTC_quaternion` plus selected quaternion extensions | implemented | Covers quaternion construction, decomposition, interpolation, matrix casts, Euler-angle helpers, and vector rotation. |
| `glm_relational` | Core relational functions plus bool-vector reductions | implemented | Covers vector comparisons and bool-vector reduction helpers such as `all`, `any`, and `not`. |
| `glm_integer` | Core integer functions plus selected integer extensions | implemented | Covers bit counting and bitfield helpers, `find_lsb`, `find_msb`, multiple and power-of-two helpers, and carry, borrow, and extended multiply helpers. |
| `glm_packing` | Core packing functions plus selected GTC packing helpers | implemented | Covers the core surface plus half-float and normalized 8-bit and 16-bit packing and unpacking helpers. |
| `glm_easing` | Easing extensions | implemented | Dedicated easing-function module on wrapped `float` and `double` scalars. |

## Deferred but reasonable follow-ons

These families remain plausible additions, but they are not part of the current
near-release target.

| Upstream provenance | Likely module | Status | Rationale |
| --- | --- | --- | --- |
| `GLM_GTC_constants`, `GLM_EXT_scalar_constants` | `glm_constants` | deferred | Useful, but lower priority than operational math. |
| `GLM_GTC_epsilon` | `glm_epsilon` | deferred | Useful once more comparison conventions are settled. |
| Additional `GLM_GTC_packing` formats beyond the current half-float and normalized helpers | `glm_packing` | deferred | Worth adding case by case rather than importing the entire format matrix. |
| Extra matrix access and inverse helpers | `glm_matrix` | deferred | Start with the common helpers first. |
| `GLM_EXT_matrix_integer`, `GLM_EXT_matrix_relational` | `glm_matrix` | deferred | Reasonable, but not required for the first release. |
| `GLM_EXT_quaternion_relational` | `glm_quat` | deferred | Revisit after comparison conventions are clearer. |
| `GLM_EXT_scalar_common`, `GLM_EXT_vector_common` beyond the current implemented subset | `glm_common`, `glm_vector` | deferred | Evaluate case by case rather than importing extension families wholesale. |
| `GLM_EXT_scalar_relational`, `GLM_EXT_vector_relational` beyond the current implemented subset | `glm_relational` | deferred | Keep the first release surface small and coherent. |
| `GLM_GTX_vector_angle`, `GLM_GTX_rotate_vector`, `GLM_GTX_rotate_normalized_axis`, `GLM_GTX_projection` | `glm_vector` | deferred | Plausible follow-on vector utilities. |
| `GLM_GTX_transform2`, `GLM_GTX_matrix_transform_2d` | `glm_transform` | deferred | Useful when a clear 2D transform use case appears. |
| `GLM_GTX_matrix_query`, `GLM_GTX_matrix_operation`, `GLM_GTX_matrix_interpolation`, `GLM_GTX_matrix_cross_product`, `GLM_GTX_orthonormalize`, `GLM_GTX_matrix_decompose` | `glm_matrix` | deferred | Valuable, but not first-pass surface. |

## Not planned

These families are intentionally outside the first public surface.

| Upstream provenance | Status | Notes |
| --- | --- | --- |
| `GLM_GTC_random` | not planned | No clear need for GLM-backed randomness on the BEAM. |
| `GLM_GTC_noise` | not planned | Large surface with weak immediate value for this binding. |
| `GLM_GTC_color_space` | not planned | Outside the intended core graphics-math path. |
| `GLM_GTC_ulp`, `GLM_EXT_scalar_ulp`, `GLM_EXT_vector_ulp` | not planned | Specialized floating-point tooling with limited payoff for the first release. |
| `GLM_EXT_scalar_reciprocal` | not planned | Large niche extension with weak immediate value. |
| `GLM_GTX_vector_query` | not planned | Low-value surface for the initial binding. |
| `GLM_GTX_texture` | not planned | Texture wrap helpers do not justify a dedicated BEAM surface. |
| `GLM_GTX_string_cast` | not planned | String formatting is better handled natively in Erlang. |
| `GLM_GTX_spline` | not planned | Outside the first release target until a concrete use case appears. |
| `GLM_GTX_polar_coordinates` | not planned | Niche enough to leave out for now. |
| `GLM_GTX_pca` | not planned | Outside the intended scope of this binding. |
| `GLM_GTX_optimum_pow` | not planned | Too specialized for the first public surface. |
| `GLM_GTX_normalize_dot`, `GLM_GTX_norm`, `GLM_GTX_normal`, `GLM_GTX_mixed_product` | not planned | Deprioritized low-value extras. |
| `GLM_GTX_matrix_major_storage`, `GLM_GTX_matrix_factorisation` | not planned | Too specialized relative to the intended scope. |
| `GLM_GTX_log_base` | not planned | Not worth a dedicated surface right now. |
| `GLM_GTX_intersect` | not planned | Broad geometric query family outside the first release target. |