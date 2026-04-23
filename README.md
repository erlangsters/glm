# OpenGL Mathematics for the BEAM

[![Erlangsters Repository](https://img.shields.io/badge/erlangsters-glm-%23a90432)](https://github.com/erlangsters/glm)
![Supported Erlang/OTP Versions](https://img.shields.io/badge/erlang%2Fotp-28-%23a90432)
![Current Version](https://img.shields.io/badge/version-0.1.0-%23354052)
![License](https://img.shields.io/github/license/erlangsters/glm)
[![Build Status](https://img.shields.io/github/actions/workflow/status/erlangsters/glm/build.yml)](https://github.com/erlangsters/glm/actions/workflows/build.yml)
[![Documentation Link](https://img.shields.io/badge/documentation-available-yellow)](http://erlangsters.github.io/glm/)

The binding of [GLM](https://github.com/g-truc/glm) for the Erlang and Elixir
programming language. Just like GLM itself, it is not limited to OpenGL and
can be used as a general-purpose math library.

```erlang
Min = glm:float(0.0).
Max = glm:float(1.0).
Result = glm_common:clamp(glm:vec2(float, -1.0, 2.0), Min, Max).
0.0 = glm:vec2_x(Result).
1.0 = glm:vec2_y(Result).
```

Because arithmetic operations in a graphical application are often performance
critical, this binding is designed to be efficient. It keeps the BEAM-facing
API small and explicit while storing the underlying values in binaries so the
same data can move cleanly between BEAM code, native code, and graphics APIs.

> [!NOTE]
> Even if keeping data in binaries is less ergonomic, they will almost always
> end up being passed down to the graphics card in the binary form.

> [!NOTE]
> Another strong argument in favor of keeping data in binaries is that the BEAM
> does not implement floating point numbers according to the IEEE 754 standard
> in a way that maps directly to the native layouts expected by graphics code.
> At some point the exact bit representation matters, whether it is for SIMD,
> packing helpers, NIF calls, or uploading data to the graphics card.

Most importantly, it provides a safe API, with constructors and runtime checks
that keep the wrapped data structures valid before any operation is performed.
With that said, it also provides a raw API that bypasses those checks when you
intentionally want the lower-level binary-oriented layer.

Written by the Erlangsters [community](https://about.erlangsters.org/) and released under the MIT [license](https://opensource.org/license/mit).

## Getting started

The `glm` module is the entry point. It defines the wrapped scalar, vector,
matrix, and quaternion values used throughout the binding, together with the
constructors and accessors that convert between regular BEAM numbers and the
binary-backed GLM representation.

```erlang
Scalar = glm:scalar(float, 42.0).
42.0 = glm:scalar_value(Scalar).

Direction = glm_vector:normalize(glm:vec3(double, 3.0, 4.0, 0.0)).
0.6 = glm:vec3_x(Direction).
0.8 = glm:vec3_y(Direction).

Projection = glm_transform:perspective(
    glm:double(math:pi() / 2.0),
    glm:double(16.0 / 9.0),
    glm:double(0.1),
    glm:double(100.0)
).
```

At the BEAM boundary you still work with regular `integer()` and `float()`
values. The constructors and accessors in `glm` handle the conversion to and
from the wrapped binary-backed representation for you.

The wrapper shape is explicit because GLM is a C++ library built around
templates and overloads, while the BEAM has neither. Each wrapped value keeps
just enough metadata to dispatch to the right implementation.

```erlang
-type scalar(Type) :: {scalar, Type, binary()}.
-type vec(Length, Type) :: {vec, Length, Type, binary()}.
-type mat(Columns, Rows, Type) :: {mat, Columns, Rows, Type, binary()}.
-type quat(Type) :: {quat, Type, binary()}.
```

The public API is organized by operation family rather than by upstream GLM
header layout:

| Module | Focus |
| --- | --- |
| [glm](src/glm.erl) | Constructors, accessors, and public wrapped types |
| [glm_common](src/glm_common.erl) | Common scalar and vector helpers such as `clamp/3`, rounding, and bit reinterpretation helpers |
| [glm_angle](src/glm_angle.erl) | Angle conversion, trigonometric, inverse-trigonometric, and hyperbolic functions |
| [glm_exponential](src/glm_exponential.erl) | Power, exponential, logarithmic, and root functions |
| [glm_vector](src/glm_vector.erl) | Vector geometry such as `dot/2`, `cross/2`, `normalize/1`, and `reflect/2` |
| [glm_matrix](src/glm_matrix.erl) | Matrix operations such as `transpose/1`, `determinant/1`, and `inverse/1` |
| [glm_transform](src/glm_transform.erl) | Projection, view, and matrix transform helpers |
| [glm_quat](src/glm_quat.erl) | Quaternion construction, conversion, interpolation, and rotation helpers |
| [glm_integer](src/glm_integer.erl) | Integer bit operations, integer predicates, and extended arithmetic helpers |
| [glm_relational](src/glm_relational.erl) | Component-wise comparison and bool-vector reduction helpers |
| [glm_packing](src/glm_packing.erl) | Packing and unpacking helpers for compact scalar representations |
| [glm_easing](src/glm_easing.erl) | Easing functions |

The safe API is the default choice. It keeps the wrapped values consistent and
raises clear errors when a value does not match the requested GLM shape. For
instance, `glm:uint8(256)` fails immediately, and so does a call that mixes
incompatible wrapped operands.

## Module guide

The `glm` module stays focused on the wrapped primitives themselves. It is the
place to construct scalars, vectors, matrices, and quaternions, to read their
components back, and to understand the public BEAM-facing shapes used
throughout the binding.

The `glm_common`, `glm_angle`, and `glm_exponential` modules cover the scalar
and vector helpers that tend to be used everywhere. `glm_common` contains the
small arithmetic and rounding helpers, `glm_angle` contains the angle,
trigonometric, and hyperbolic functions, and `glm_exponential` contains powers,
logarithms, roots, and inverse roots.

The `glm_vector`, `glm_matrix`, `glm_transform`, and `glm_quat` modules cover
the core graphics math workflow. `glm_vector` handles vector geometry,
`glm_matrix` handles matrix inspection and matrix operations,
`glm_transform` builds projection, view, and model-space transforms, and
`glm_quat` handles quaternion construction, interpolation, conversion, and
rotation helpers.

The `glm_integer`, `glm_relational`, `glm_packing`, and `glm_easing` modules
cover the more specialized helpers that still fit cleanly into the safe API.
`glm_integer` contains integer-specific bit and multiple operations,
`glm_relational` contains comparison and bool-vector reduction helpers,
`glm_packing` contains compact scalar packing and unpacking helpers, and
`glm_easing` contains easing curves on wrapped floating-point values.

Together these modules form the public safe surface. Most code should stay at
this level.

## The raw layer

The [glm_raw](src/glm_raw.erl) module sits beneath the safe API. It is the thin
binary-oriented layer used by the wrapper modules themselves.

It exists for code that already controls the binary layout and intentionally
wants to skip the safe-layer validation. It is lower level, more mechanical,
and intentionally less ergonomic. Most callers should avoid it unless they are
measuring a real need and are prepared to manage the unchecked representation
directly.

Not every upstream GLM function is exposed. The binding prioritizes the common
graphics math path: scalars, vectors of length 2 to 4, matrices of size 2 to 4,
quaternions, and the operations that map cleanly to stable Erlang shapes.
