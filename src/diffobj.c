/*
Code based on libmba-0.9.1/examples/strdiff.c
*/

#include <stdlib.h>
#include "diffobj.h"

SEXP DIFFOBJ_diffobj(SEXP a, SEXP b, SEXP max) {
  int n, m, d;
  int sn, i;
  /* allocate max possible size for edit script; wasteful, but this greatly
   * simplifies code since we don't need any of the variable array logic and
   * besides is just an (M + N) allocation
   */
  n = XLENGTH(a);
  m = XLENGTH(b);
  if(
    TYPEOF(max) != INTSXP || XLENGTH(max) != 1L || asInteger(max) == NA_INTEGER
  )
    error("Logic Error: `max` not integer(1L) and not NA");

  int max_i = asInteger(max);
  if(max < 0) max = 0;

  struct diff_edit *ses = (struct diff_edit *)
    R_alloc(n + m + 1, sizeof(struct diff_edit));

  d = diff(a, 0, n, b, 0, m, NULL, max_i, ses, &sn);

  SEXP res = PROTECT(allocVector(VECSXP, 3));
  SEXP type = PROTECT(allocVector(INTSXP, sn));
  SEXP count = PROTECT(allocVector(INTSXP, sn));
  SEXP offs = PROTECT(allocVector(INTSXP, sn));

  for (i = 0; i < sn; i++) {
    struct diff_edit *e = ses + i;

    switch (e->op) {
      case DIFF_MATCH:
        INTEGER(type)[i] = 1;
        break;
      case DIFF_INSERT:
        INTEGER(type)[i] = 2;
        break;
      case DIFF_DELETE:
        INTEGER(type)[i] = 3;
        break;
    }
    INTEGER(count)[i] = e->len;
    INTEGER(offs)[i] = e->off;
  }
  SET_VECTOR_ELT(res, 0, type);
  SET_VECTOR_ELT(res, 1, count);
  SET_VECTOR_ELT(res, 2, offs);
  UNPROTECT(4);

  return res;
}

