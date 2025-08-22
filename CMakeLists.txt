cmake_minimum_required(VERSION 3.11)
project(beam_glm LANGUAGES CXX)

set(CMAKE_C_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# We let Rebar3 inform CMake where to find the Erlang/OTP location. Therefore,
# it requires the ERLANG_ERTS_DIR and ERL_INTERFACE_DIR values specified with
# the -D flags.
if(NOT DEFINED ERLANG_ERTS_DIR)
    message(FATAL_ERROR "ERLANG_ERTS_DIR must be defined (pass -DERLANG_ERTS_DIR=...)")
endif()
if(NOT DEFINED ERL_INTERFACE_DIR)
    message(FATAL_ERROR "ERL_INTERFACE_DIR must be defined (pass -DERL_INTERFACE_DIR=...)")
endif()

set(ERTS_INCLUDE_DIR "${ERLANG_ERTS_DIR}/include")
set(ERL_INTERFACE_INCLUDE_DIR "${ERL_INTERFACE_DIR}/include")
set(ERL_INTERFACE_LIB_DIR "${ERL_INTERFACE_DIR}/lib")

include_directories(
    ${ERTS_INCLUDE_DIR}
    ${ERL_INTERFACE_INCLUDE_DIR}
)
link_directories(${ERL_INTERFACE_LIB_DIR})

include(FetchContent)
FetchContent_Declare(
  glm
  GIT_REPOSITORY https://github.com/g-truc/glm.git
  GIT_TAG 1.0.1
)
FetchContent_MakeAvailable(glm)

add_library(beam-glm SHARED c_src/beam_glm.cpp)
set_target_properties(beam-glm PROPERTIES
    OUTPUT_NAME "beam-glm"
    PREFIX ""
)

# The Rebar3 build system (as configured in rebar.config) does not make use
# of the "install" step. Instead, it produces the NIF library in the priv/
# folder directly during the "build" step.
set_target_properties(beam-glm PROPERTIES
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/priv"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/priv"
)

# On macOS, we need to add special linker flags to avoid issues with symbol
# resolution.
if(APPLE)
    set_target_properties(beam-glm PROPERTIES
        LINK_FLAGS "-flat_namespace -undefined suppress"
        SUFFIX ".so"
    )
endif()

target_link_libraries(beam-glm PRIVATE glm::glm-header-only)
