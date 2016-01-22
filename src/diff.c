/* This an adaptation of the Michael B. Allen implementation of the
 * Myers diff algorithm (see below for details)
 *
 * The main changes are adapting to use in an R context (memory allocations with
 * R_alloc) and simplifying code by removing the variable arrays and the
 * ability to specify custom comparison functions.
 */

/* diff - compute a shortest edit script (SES) given two sequences
 * Copyright (c) 2004 Michael B. Allen <mba2000 ioplex.com>
 *
 * The MIT License
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 */

/* This algorithm is basically Myers' solution to SES/LCS with
 * the Hirschberg linear space refinement as described in the
 * following publication:
 *
 *   E. Myers, ``An O(ND) Difference Algorithm and Its Variations,''
 *   Algorithmica 1, 2 (1986), 251-266.
 *   http://www.cs.arizona.edu/people/gene/PAPERS/diff.ps
 *
 * This is the same algorithm used by GNU diff(1).
 */

#include <stdlib.h>
#include "diffr.h"

#define FV(k) _v(ctx, (k), 0)
#define RV(k) _v(ctx, (k), 1)
/* we've abandonned the use of varray for both ses and buf, instead we
 * pre-allocate both the edit script and the buffer to the maximum possible size
 * of m + n + 1 recongnizing that this is wasteful but greatly reduces the
 * amount of code needed
 */
struct _ctx {
  void *context;
  int *buf;               // used to be varray
  struct diff_edit *ses;  // used to be varray
  int si;
  int dmax;
};

struct middle_snake {
  int x, y, u, v;
};

  static void
_setv(struct _ctx *ctx, int k, int r, int val)
{
  int j;
  int *i;
  /* Pack -N to N into 0 to N * 2
  */
  j = k <= 0 ? -k * 4 + r : k * 4 + (r - 2);

  i = ctx->buf + j;
  *i = val;
}
  static int
_v(struct _ctx *ctx, int k, int r)
{
  int j;

  j = k <= 0 ? -k * 4 + r : k * 4 + (r - 2);

  return *(ctx->buf + j);
}

  static int
_find_middle_snake(SEXP a, int aoff, int n,
    SEXP b, int boff, int m,
    struct _ctx *ctx,
    struct middle_snake *ms)
{
  int delta, odd, mid, d;

  delta = n - m;
  odd = delta & 1;
  mid = (n + m) / 2;
  mid += odd;

  _setv(ctx, 1, 0, 0);
  _setv(ctx, delta - 1, 1, n);

  for (d = 0; d <= mid; d++) {
    int k, x, y;

    if ((2 * d - 1) >= ctx->dmax) {
      return ctx->dmax;
    }

    for (k = d; k >= -d; k -= 2) {
      if (k == -d || (k != d && FV(k - 1) < FV(k + 1))) {
        x = FV(k + 1);
      } else {
        x = FV(k - 1) + 1;
      }
      y = x - k;

      ms->x = x;
      ms->y = y;
      while(
        x < n && y < m && STRING_ELT(a, aoff + x) == STRING_ELT(b, boff + y)
      ) {
        x++; y++;
      }
      _setv(ctx, k, 0, x);

      if (odd && k >= (delta - (d - 1)) && k <= (delta + (d - 1))) {
        if (x >= RV(k)) {
          ms->u = x;
          ms->v = y;
          return 2 * d - 1;
        }
      }
    }
    for (k = d; k >= -d; k -= 2) {
      int kr = (n - m) + k;

      if (k == d || (k != -d && RV(kr - 1) < RV(kr + 1))) {
        x = RV(kr - 1);
      } else {
        x = RV(kr + 1) - 1;
      }
      y = x - kr;

      ms->u = x;
      ms->v = y;
      while (
        x > 0 && y > 0 && STRING_ELT(a, aoff + x) == STRING_ELT(b, boff + y)
      ) {
        x--; y--;
      }
      _setv(ctx, kr, 1, x);

      if (!odd && kr >= -d && kr <= d) {
        if (x <= FV(kr)) {
          ms->x = x;
          ms->y = y;
          return 2 * d;
        }
      }
    }
  }
  error("Logic Error: failed finding middle snake, contact maintainer");
}

/*
 * Update edit script atom with newest info, we record the operation, and the
 * offset and length so we can recover the values from the original vector
 */
  static void
_edit(struct _ctx *ctx, int op, int off, int len)
{
  struct diff_edit *e;

  if (len == 0 || ctx->ses == NULL) {
    return;
  }               /* Add an edit to the SES (or
                   * coalesce if the op is the same)
                   */
  e = ctx->ses + ctx->si;
  if (e->op != op) {
    if (e->op) {
      ctx->si++;
      e = ctx->ses + ctx->si;
    }
    e->op = op;
    e->off = off;
    e->len = len;
  } else {
    e->len += len;
  }
}
/* Generate shortest edit script
 *
 */
  static int
_ses(SEXP a, int aoff, int n,
    SEXP b, int boff, int m,
    struct _ctx *ctx)
{
  struct middle_snake ms;
  int d;

  if (n == 0) {
    _edit(ctx, DIFF_INSERT, boff, m);
    d = m;
  } else if (m == 0) {
    _edit(ctx, DIFF_DELETE, aoff, n);
    d = n;
  } else {
    /* Find the middle "snake" around which we
     * recursively solve the sub-problems.  Note this modifies `ms` by ref to
     * set the end points of both the forward and reverse paths
     */
    d = _find_middle_snake(a, aoff, n, b, boff, m, ctx, &ms);
    if (d == -1) {
      return -1;
    } else if (d >= ctx->dmax) {
      return ctx->dmax;
    } else if (ctx->ses == NULL) {
      return d;
    } else if (d > 1) {
      if (_ses(a, aoff, ms.x, b, boff, ms.y, ctx) == -1) {
        return -1;
      }

      _edit(ctx, DIFF_MATCH, aoff + ms.x, ms.u - ms.x);

      aoff += ms.u;
      boff += ms.v;
      n -= ms.u;
      m -= ms.v;
      if (_ses(a, aoff, n, b, boff, m, ctx) == -1) {
        return -1;
      }
    } else {
      int x = ms.x;
      int u = ms.u;

      /* There are only 4 base cases when the
       * edit distance is 1.
       *
       * n > m   m > n
       *
       *   -       |
       *    \       \    x != u
       *     \       \
       *
       *   \       \
       *    \       \    x == u
       *     -       |
       */

      if (m > n) {
        if (x == u) {
          _edit(ctx, DIFF_MATCH, aoff, n);
          _edit(ctx, DIFF_INSERT, boff + (m - 1), 1);
        } else {
          _edit(ctx, DIFF_INSERT, boff, 1);
          _edit(ctx, DIFF_MATCH, aoff, n);
        }
      } else {
        if (x == u) {
          _edit(ctx, DIFF_MATCH, aoff, m);
          _edit(ctx, DIFF_DELETE, aoff + (n - 1), 1);
        } else {
          _edit(ctx, DIFF_DELETE, aoff, 1);
          _edit(ctx, DIFF_MATCH, aoff + 1, m);
        }
      }
    }
  }

  return d;
}
/*
 * - ses is a pointer to an array of diff_edit structs that is initialized
 *   outside of this call
 * - aoff and boff are how far we've moved across the strings, used mostly in the
 *   context of recursion for _ses
 * - n is the lenght of a, m the length of b
 */
  int
diff(SEXP a, int aoff, int n,
    SEXP b, int boff, int m,
    void *context, int dmax,
    struct diff_edit *ses, int *sn)
{
  struct _ctx ctx;
  int d, x, y;
  struct diff_edit *e = NULL;
  int *tmp = (int *) R_alloc(n + m + 1, sizeof(int));

  ctx.context = context;

  /* initialize buffer
   */
  ctx.buf = tmp;
  ctx.ses = ses;
  ctx.si = 0;
  ctx.dmax = dmax ? dmax : INT_MAX;

  /* initialize first ses edit struct*/
  if (ses && sn) {
    if ((e = ses) == NULL) {
      error("Logic Error: specifying sn, but ses is NULL, contact maintainer.");
    }
    e->op = 0;
  }

  /* The _ses function assumes the SES will begin or end with a delete
   * or insert. The following will insure this is true by eating any
   * beginning matches. This is also a quick to process sequences
   * that match entirely.
   */
  x = y = 0;
  while (x < n && y < m && STRING_ELT(a, aoff + x) == STRING_ELT(b, boff + y)) {
    x++; y++;
    if(boff + y < boff + y - 1 || aoff + x < aoff + x - 1)
      error("Logic Error: exceeded int size 45823; contact maintainer");
  }
  _edit(&ctx, DIFF_MATCH, aoff, x);

  d = _ses(a, aoff + x, n - x, b, boff + y, m - y, &ctx);
  if (ses && sn) {
    *sn = e->op ? ctx.si + 1 : 0;
  }
  return d;
}

