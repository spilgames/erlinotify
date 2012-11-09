#include "erl_nif.h"

static ErlNifResourceType* erlinotify_nif_RESOURCE = NULL;

typedef struct
{
} erlinotify_nif_handle;

// Prototypes
static ERL_NIF_TERM erlinotify_nif_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM erlinotify_nif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, erlinotify_nif_new},
    {"myfunction", 1, erlinotify_nif_myfunction}
};

static ERL_NIF_TERM erlinotify_nif_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    erlinotify_nif_handle* handle = enif_alloc_resource(erlinotify_nif_RESOURCE,
                                                    sizeof(erlinotify_nif_handle));
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM erlinotify_nif_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void erlinotify_nif_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in erlinotify_nif_handle */
    /* erlinotify_nif_handle* handle = (erlinotify_nif_handle*)arg; */
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "erlinotify_nif_resource",
                                                     &erlinotify_nif_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    erlinotify_nif_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(erlinotify_nif, nif_funcs, &on_load, NULL, NULL, NULL);
