/*
 * Copyright (C) 2019 Brodie Gaslam
 *
 * This file is part of "diffobj - Diffs for R Objects"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
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
    error("Logic Error: `max` not integer(1L) and not NA"); // nocov

  int max_i = asInteger(max);
  if(max_i < 0) max_i = -1;

  struct diff_edit *ses = (struct diff_edit *)
    R_alloc(n + m + 1, sizeof(struct diff_edit));

  d = diff(a, 0, n, b, 0, m, NULL, max_i, ses, &sn);

  SEXP res = PROTECT(allocVector(VECSXP, 4));
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
  SET_VECTOR_ELT(res, 3, ScalarInteger(d));
  UNPROTECT(4);

  return res;
}

