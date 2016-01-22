#include "diffr.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"diffr", (DL_FUNC) &DIFFR_diffr, 2},
  {NULL, NULL, 0}
};

void R_init_diffr(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
