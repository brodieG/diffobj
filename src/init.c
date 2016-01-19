#include "diffr.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"alike_ext", (DL_FUNC) &ALIKEC_alike_ext, 4},
  {"alike_fast1", (DL_FUNC) &ALIKEC_alike_fast1, 4},
  {"alike_fast2", (DL_FUNC) &ALIKEC_alike_fast2, 2},
  {"typeof", (DL_FUNC) &ALIKEC_typeof, 1},
  {"type_alike", (DL_FUNC) &ALIKEC_type_alike, 4},
  {"compare_attributes", (DL_FUNC) &ALIKEC_compare_attributes, 3},
  {"test", (DL_FUNC) &ALIKEC_test, 1},
  {"test2", (DL_FUNC) &ALIKEC_test2, 2},
  {"is_valid_name_ext", (DL_FUNC) &ALIKEC_is_valid_name_ext, 1},
  {"is_dfish", (DL_FUNC) &ALIKEC_is_dfish_ext, 1},
  {"compare_names", (DL_FUNC) &ALIKEC_compare_special_char_attrs, 2},
  {"compare_dimnames", (DL_FUNC) &ALIKEC_compare_dimnames_ext, 2},
  {"compare_class", (DL_FUNC) &ALIKEC_compare_class_ext, 2},
  {"compare_dims", (DL_FUNC) &ALIKEC_compare_dim_ext, 5},
  {"compare_ts", (DL_FUNC) &ALIKEC_compare_ts_ext, 2},
  {"lang_alike", (DL_FUNC) &ALIKEC_lang_alike_ext, 3},
  {"lang_alike_chr", (DL_FUNC) &ALIKEC_lang_alike_chr_ext, 3},
  {"fun_alike", (DL_FUNC) &ALIKEC_fun_alike_ext, 2},
  {"deparse", (DL_FUNC) &ALIKEC_deparse_ext, 2},
  {"deparse_oneline", (DL_FUNC) &ALIKEC_deparse_oneline_ext, 3},
  {"pad", (DL_FUNC) &ALIKEC_pad_ext, 3},
  {"match_call", (DL_FUNC) &ALIKEC_match_call, 3},
  {"abstract_ts", (DL_FUNC) &ALIKEC_abstract_ts, 2},
  {"env_track", (DL_FUNC) &ALIKEC_env_track_test, 2},
  {NULL, NULL, 0}
};

void R_init_diffr(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_RegisterCCallable("alike", "ALIKEC_alike_ext", (DL_FUNC) ALIKEC_alike_ext);
}
