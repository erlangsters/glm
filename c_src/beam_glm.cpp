#include <stdio.h>
#include <stdint.h>
#include <erl_nif.h>
#include <glm/glm.hpp>

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    (void)priv_data;
    (void)arg;

    return 0;
}

static void nif_module_unload(ErlNifEnv* caller_env, void* priv_data)
{
    (void)caller_env;
    (void)priv_data;
}

static ERL_NIF_TERM nif_glm_foo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_functions[] = {
    {"foo", 0, nif_glm_foo, 0}
};

ERL_NIF_INIT(
    glm_raw,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
