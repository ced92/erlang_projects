#include "erl_nif.h"
#include <math.h>

static ERL_NIF_TERM abs1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    //Declare variables
    int length; // length of a tuple
    long re_long,im_long;
    double re_double,im_double,ret1_double,ret2_double;
    const ERL_NIF_TERM** tuple;

    //Load from erlang to C
    enif_get_tuple(env, argv[0], &length, &tuple);

    if(enif_get_long(env, tuple[0], &re_long)){
        re_double = (double) re_long;
    } else {
        enif_get_double(env, tuple[0], &re_double);
    }

    if(enif_get_long(env, tuple[1], &im_long)){
        im_double = (double) im_long;
    } else {
        enif_get_double(env, tuple[1], &im_double);
    }

    ret1_double = re_double*re_double + im_double*im_double;
    ret2_double = sqrt(ret1_double);
    return enif_make_double(env, ret2_double);
}

//argc is the arity of the function i.e. number of parameters
//argv contains the parameters passed to the function
//NIFs also take an environment argument that serves as an opaque handle that 
//is needed to be passed on to most API functions
static ERL_NIF_TERM sqr(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    //Declare variables
    int length; // length of tuple
    long re_long,im_long;
    double re_double,im_double,ret1_double,ret2_double;
    const ERL_NIF_TERM** tuple;

    //Load from erlang to C
    enif_get_tuple(env, argv[0], &length, &tuple);

    if(enif_get_long(env, tuple[0], &re_long)){
        re_double = (double) re_long;
    } else {
        enif_get_double(env, tuple[0], &re_double);
    }

    if(enif_get_long(env, tuple[1], &im_long)){
        im_double = (double) im_long;
    } else {
        enif_get_double(env, tuple[1], &im_double);
    }

    ret1_double = re_double*re_double - im_double*im_double;
    ret2_double = 2*re_double*im_double;
    return enif_make_tuple2(env, enif_make_double(env, ret1_double), enif_make_double(env, ret2_double));
}

static ErlNifFunc nif_funcs[] = {
    {"sqr", 1, sqr},
    {"abs1", 1, abs1}
};

ERL_NIF_INIT(cmplx_optimized, nif_funcs, NULL, NULL, NULL, NULL)
