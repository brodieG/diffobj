#include "diffobj.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"diffobj", (DL_FUNC) &DIFFOBJ_diffobj, 2},
  {NULL, NULL, 0}
};

void R_init_diffobj(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
