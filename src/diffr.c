/*
Code based on libmba-0.9.1/examples/strdiff.c
*/

#include <stdlib.h>
#include "diffr.h"

SEXP DIFFR_diffr(SEXP a, SEXP b) {
  int n, m, d;
  int sn, i;
  /* allocate max possible size for edit script; wasteful, but this greatly
   * simplifies code since we don't need any of the variable array logic and
   * besides is just an (M + N) allocation
   */
  n = XLENGTH(a);
  m = XLENGTH(b);

  struct diff_edit *ses = (struct diff_edit *)
    R_alloc(n + m + 1, sizeof(struct diff_edit));

  d = diff(a, 0, n, b, 0, m, NULL, 0, ses, &sn);

  Rprintf("d=%d sn=%d\n", d, sn);
  SEXP res = PROTECT(allocVector(VECSXP, 2));
  SEXP type = PROTECT(allocVector(INTSXP, sn));
  SEXP count = PROTECT(allocVector(INTSXP, sn));
  SEXP offs = PROTECT(allocVector(INTSXP, sn));

  for (i = 0; i < sn; i++) {
    struct diff_edit *e = ses + i;

    switch (e->op) {
      case DIFF_MATCH:
        INTEGER(type)[i] = 0;
        break;
      case DIFF_INSERT:
        INTEGER(type)[i] = 1;
        break;
      case DIFF_DELETE:
        INTEGER(type)[i] = 2;
        break;
    }
    INTEGER(count)[i] = e->len;
    INTEGER(offs)[i] = e->off;
  }
  SET_VECTOR_ELT(res, 0, type);
  SET_VECTOR_ELT(res, 1, count);
  SET_VECTOR_ELT(res, 2, offs);
  UNPROTECT(3);

  return res;
}

