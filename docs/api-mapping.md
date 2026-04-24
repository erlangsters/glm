# API Mapping

The GLM binding does not mirror the upstream C++ API one symbol at a time.
Instead, it maps the template-heavy GLM surface to a small set of stable
BEAM-facing shapes and groups operations into semantic Erlang modules.

This document describes that public mapping.

## Mapping rules

- `glm` owns the wrapped primitives used throughout the binding:
  `{scalar, T, Bin}`, `{vec, L, T, Bin}`, `{mat, C, R, T, Bin}`, and
  `{quat, T, Bin}`.
- Public modules are grouped by BEAM-facing behavior, not by upstream header
  names. A single Erlang module may draw from several GLM headers when that
  produces a cleaner public surface.
- Upstream camelCase names become snake_case Erlang functions.
- C++ overload families collapse onto wrapped-value dispatch. The binding
  prefers one coherent public function over exposing qualifier, alias, and
  template noise directly.
- The first public surface focuses on scalars, vectors of length 2 to 4,
  matrices of size 2 to 4, quaternions, and the most broadly useful graphics
  math operations on top of them.
- `glm_raw` remains available as the lower-level binary-oriented layer, but it
  is not the primary public API described here.

## Public module mapping

| Upstream provenance | BEAM module | Public scope |
| --- | --- | --- |
| Core primitive types and constructors | `glm` | Wrapped scalar, vector, matrix, and quaternion constructors, accessors, and shape-level helpers. |
| Core common functions plus selected scalar and vector common helpers | `glm_common` | `abs`, `ceil`, `clamp`, `floor`, `fma`, `fract`, `frexp`, `ldexp`, `max`, `min`, `mix`, `mod`, `modf`, `round`, `round_even`, `sign`, `smoothstep`, `step`, `trunc`, floating-point bit casts, `is_inf`, and `is_nan`. |
| Core angle and trigonometric functions | `glm_angle` | `radians`, `degrees`, trigonometric, inverse-trigonometric, and hyperbolic functions on wrapped scalars and vectors. |
| Core exponential functions | `glm_exponential` | `pow`, `exp`, `exp2`, `log`, `log2`, `sqrt`, and `inverse_sqrt`. |
| Core geometric functions plus selected vector-focused extensions | `glm_vector` | `length`, `distance`, `dot`, `cross`, `normalize`, `face_forward`, `reflect`, and `refract`. |
| Core matrix functions plus selected GTC and GTX helpers | `glm_matrix` | Determinant, inverse, inverse transpose, transpose, component-wise matrix multiply, outer product, row and column access, and selected matrix helpers that fit the wrapped matrix surface. |
| `GLM_EXT_matrix_clip_space`, `GLM_EXT_matrix_projection`, `GLM_EXT_matrix_transform`, and `GLM_GTX_transform` | `glm_transform` | `frustum`, `infinite_perspective`, `look_at`, `ortho`, `perspective`, `perspective_fov`, `project`, `un_project`, `rotate`, `scale`, and `translate`. |
| `GLM_GTC_quaternion` plus selected quaternion extensions | `glm_quat` | Quaternion construction, decomposition, interpolation, matrix casts, Euler-angle helpers, and vector rotation. |
| Core relational functions plus bool-vector reductions | `glm_relational` | `equal`, `not_equal`, ordering predicates, `all`, `any`, and `not` for wrapped vectors and bool vectors. |
| Core integer functions plus selected integer extensions | `glm_integer` | Bit counting and bitfield helpers, `find_lsb`, `find_msb`, power-of-two and multiple helpers, and the carry, borrow, and extended-multiply operations. |
| Core packing functions plus selected GTC packing helpers | `glm_packing` | Core pack and unpack operations together with half-float and normalized 8-bit and 16-bit packers and unpackers. |
| Easing extensions | `glm_easing` | Easing curves on wrapped `float` and `double` scalars. |

## Collapsed upstream variants

Some upstream GLM families are intentionally collapsed rather than exposed as a
one-to-one copy of the C++ API.

- Qualifier variants and alias noise are not surfaced. The BEAM-facing type
  metadata already carries the information needed for dispatch.
- Left-handed, right-handed, `NO`, and `ZO` projection variants are not exposed
  as separate public functions today. `glm_transform` keeps the smaller default
  transform surface that maps cleanly to the current binding.
- Template-only distinctions that do not improve the BEAM API are kept behind
  the wrapped dispatch layer instead of becoming separate public symbols.

## Deferred but plausible follow-ons

These upstream families remain reasonable future additions, but they are not
part of the first public surface.

| Upstream provenance | Likely module | Current status |
| --- | --- | --- |
| `GLM_GTC_constants`, `GLM_EXT_scalar_constants` | `glm_constants` | Deferred. |
| `GLM_GTC_epsilon` | `glm_epsilon` | Deferred. |
| Additional `GLM_GTC_packing` formats beyond the current half-float and normalized surface | `glm_packing` | Deferred. |
| Extra matrix access and inverse helpers | `glm_matrix` | Deferred. |
| `GLM_EXT_matrix_integer`, `GLM_EXT_matrix_relational` | `glm_matrix` | Deferred. |
| `GLM_EXT_quaternion_relational` | `glm_quat` | Deferred. |
| `GLM_EXT_scalar_relational`, `GLM_EXT_vector_relational` beyond the current comparison surface | `glm_relational` | Deferred. |
| `GLM_GTX_vector_angle`, `GLM_GTX_rotate_vector`, `GLM_GTX_rotate_normalized_axis`, `GLM_GTX_projection` | `glm_vector` | Deferred. |
| `GLM_GTX_transform2`, `GLM_GTX_matrix_transform_2d` | `glm_transform` | Deferred. |
| `GLM_GTX_matrix_query`, `GLM_GTX_matrix_operation`, `GLM_GTX_matrix_interpolation`, `GLM_GTX_matrix_cross_product`, `GLM_GTX_orthonormalize`, `GLM_GTX_matrix_decompose` | `glm_matrix` | Deferred. |

## Intentionally out of scope for now

The following upstream families are not part of the planned near-release
surface unless a concrete Erlang use case changes the decision.

- `GLM_GTC_random`
- `GLM_GTC_noise`
- `GLM_GTC_color_space`
- `GLM_GTC_ulp`, `GLM_EXT_scalar_ulp`, `GLM_EXT_vector_ulp`
- `GLM_EXT_scalar_reciprocal`
- `GLM_GTX_vector_query`
- `GLM_GTX_texture`
- `GLM_GTX_string_cast`
- `GLM_GTX_spline`
- `GLM_GTX_polar_coordinates`
- `GLM_GTX_pca`
- `GLM_GTX_optimum_pow`
- `GLM_GTX_normalize_dot`, `GLM_GTX_norm`, `GLM_GTX_normal`, `GLM_GTX_mixed_product`
- `GLM_GTX_matrix_major_storage`, `GLM_GTX_matrix_factorisation`
- `GLM_GTX_log_base`
- `GLM_GTX_intersect`