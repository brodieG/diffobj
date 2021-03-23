/*
 * This file is part of a program that contains a heavily modified version of
 * Michael B. Allen implementation of the Myers diff algorithm.  This
 * implementation is not compatible with the original one.  See next
 * comment blocks for original copyright and license information.
 *
 * Copyright (C) 2021 Brodie Gaslam
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
/* ORIGINAL COPYRIGHT NOTICE:
 *
 * diff - compute a shortest edit script (SES) given two sequences
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
 * This is the same algorithm used by GNU diff(1).  Please note that the
 * code in this file is heavily modified from the original source code
 * that is available at:
 *
 *   <http://www.ioplex.com/~miallen/libmba/dl/libmba-0.9.1.tar.gz>
 *
 * This implementation is not compatible with the original libmba library.
 */
/* The following is a list of the modifications made to the original Michael
 * B. Allen code:
 *
 * Here is a list of changes from the original implementation:
 * - Switch memory allocation and error handling to R specific functions
 * - Removing variable arrays in favor of fixed sized buffers to simplify code;
 *   this results in potential overallocation of memory since we pre-allocate a
 *   4 * (n + m + abs(n - m)) + 1 vector which is wasteful but still linear so
 *   should be okay.
 * - Removing ability to specify custom comparison functions
 * - Adding a failover result if diffs exceed maximum allowable diffs whereby
 *   we failover into a linear time algorithm to produce a sub optimal edit
 *   script rather than simply saying the shortest edit script is longer than
 *   allowable diffs; this is all the `faux_snake` stuff.  This algorithm tries
 *   to salvage whatever the myers algo computed up to the point of max diffs
 * - Comments.
 */
/*
 * Terms
 *
 * * A: first string
 * * B: second string
 * * N: length of A
 * * M: length of B
 * * K: grid-diagonal, numbered from -M to N.  For each grid-diagonal K,
 *   X - Y == K, i.e. (3, 1) is on diagonal K == 2.
 * * D: Number of differences in a path.
 * * Snake: sequence of diagonal moves (i.e. matching substrings).  An edit
 *   script (path) will combine snakes with (possibly zero) right/down moves.  A
 *   D-path may have up to D + 1 snakes, some or all zero length.
 * * Middle Snake: Possibly zero length snake in which the forward and reverse
 *   paths meet in the Linear Space Refinement algorithm.  Defined in terms of
 *   (x, y, u, v) where (x, y) are the coordinates from (0, 0), and (u, v) those
 *   from (M, N).
 */


#include <stdlib.h>
#include "diffobj.h"

// See _v and _setv for details

#define FV(k) _v(ctx, (k), 0)
#define RV(k) _v(ctx, (k), 1)

// We can't reach some branches through tests so they are untested, they may not
// be reachable so we mark them as no-cov; we really should figure the logic out
// to make sure they are unreachable, but don't have time for that now.

static const char * err_msg_ubrnch =
  "Internal Error: reached theoretically unreachable branch %d, contact maintainer.";

/* we've abandonned the use of varray for both ses and buf, instead we
 * pre-allocate both the edit script and the buffer to the maximum possible size
 */
struct _ctx {
  void *context;
  int *buf;               // used to be varray
  int bufmax;
  struct diff_edit *ses;  // used to be varray
  int si;
  int simax;
  int dmax;
  int dmaxhit;
};

struct middle_snake {
  int x, y, u, v;
};
/* debugging util */
/*
const char * _op_to_chr(diff_op op) {
    switch (op) {
      case DIFF_MATCH: return "match"; break;
      case DIFF_DELETE: return "delete"; break;
      case DIFF_INSERT: return "insert"; break;
      case DIFF_NULL: return "NULL"; break;
      default:
        error("Logic Error: unexpected faux snake instruction; contact maintainer");
    }
}
*/
/*
 * For each diagonal k, we only store the path that up to this point has gotten
 * furthest on it (well, two really, one from the forward and one from the
 * reverse directins)
 *
 * @param k = diagonal number
 * @param val = x value
 * @param r = 0 for forward snake, 1 for backward snake
 */
  static void
_setv(struct _ctx *ctx, int k, int r, int val)
{
  int j;
  int *i;
  /* Pack -N to N into 0 to N * 2, but also pack reverse and forward snakes
   * in that same space which is why we need the * 4
   */
  j = k <= 0 ? -k * 4 + r : k * 4 + (r - 2);

  if(j > ctx->bufmax || j < 0) {
    // nocov start
    error(
      "Logic Error: exceeded buffer size (%d vs %d); contact maintainer.",
      j, ctx->bufmax
    );
    // nocov end
  }
  i = ctx->buf + j;
  *i = val;
}
/*
 * For any given `k` diagonal, return the x coordinate of the furthest reaching
 * path we've found.
 *
 * See `_set_v`.
 */
  static int
_v(struct _ctx *ctx, int k, int r)
{
  int j;

  j = k <= 0 ? -k * 4 + r : k * 4 + (r - 2);
  if(j > ctx->bufmax || j < 0) {
    // nocov start
    error(
      "Logic Error: exceeded buffer 2 size (%d vs %d); contact maintainer.",
      j, ctx->bufmax
    );
    // nocov end
  }
  int bufval = *(ctx->buf + j);
  return bufval;
}
/* Compare character vector values
 *
 * Needs to account for special case when indeces are oob for the strings.  The
 * oob checks seem to be necessary since algo is requesting reads past length
 * of string (maybe this worked with the varrays).  As the current algo is
 * written, it does not seem to be the case that we even attempt oob reads.
 */
int _comp_chr(SEXP a, int aidx, SEXP b, int bidx) {
  int alen = XLENGTH(a);
  int blen = XLENGTH(b);
  int comp;
  if(aidx >= alen && bidx >= blen) {
    // nocov start
    error(err_msg_ubrnch, 1);
    comp = 1;
    // nocov end
  } else if(aidx >= alen || bidx >= blen) {
    comp = 0;  // nocov
  } else comp = STRING_ELT(a, aidx) == STRING_ELT(b, bidx);
  return(comp);
}
/*
 * Handle cases where differences exceed maximum allowable differences
 *
 * General logic is to try to connect the prior furthest points by naively
 * incrementing in each dimension and hoping for some diagonal runs.
 *
 * @param a character vector
 * @param b character vector
 * @param d number of differences associated with the backward snake implicit in
 *   the ms.u/v values.
 * forward loops in difference seeking; this is not the same as the
 *   number of differences as in each loop we find a forward and backward
 *   difference.
 */
static int
_find_faux_snake(
  SEXP a, int aoff, int n, SEXP b, int boff, int m,
  struct middle_snake *ms, diff_op ** faux_snake, int d
) {
  int x = ms->x;
  int y = ms->y;
  // Should switch to unsigned int...
  if(x < 0 || y < 0 || ms->u < 0 || ms->v < 0) 
      error("Internal Error: fake snake with -ve start; contact maintainer.");  // nocov

  int steps = 0;
  int diffs = 0;    // only diffs from fake snake
  int step_dir = 1; /* last direction we moved in, 1 is down */

  if(x > ms->u || y > ms->v) {
    // Overshot backward snake, e.g. you hit a long diagonal run that overshoots
    // the prior backward closest point.  In this case toss backward snake.
    ms->u = n;
    ms->v = m;
    diffs -= d;  // we're also tossing accrued differences from back snake
    if(x > ms->u || y > ms->v)
      error("Internal Error: can't correct fwd snake overshoot; contact maintainer"); // nocov
  }
  if(ms->u > INT_MAX - ms->v - 1) // x/y positive, so this is conservative
    error("Logic Error: fake snake step overflow? Contact maintainer."); // nocov

  int max_steps;
  max_steps = (ms->u - x) + (ms->v - y) + 1;

  diff_op * faux_snake_tmp = (diff_op*) R_alloc(max_steps, sizeof(diff_op));
  for(int i = 0; i < max_steps; i++) *(faux_snake_tmp + i) = DIFF_NULL;
  while((x < ms->u) || (y < ms->v)) {
    if(
      x < ms->u && y < ms->v &&
      _comp_chr(a, aoff + x, b, boff + y)
    ) {
      x++; y++;
      *(faux_snake_tmp + steps) = DIFF_MATCH;
    } else if (x < ms->u && (step_dir || y >= ms->v)) {
      x++;
      diffs++;
      step_dir = !step_dir;
      *(faux_snake_tmp + steps) = DIFF_DELETE;
    } else if (y < ms->v && (!step_dir || x >= ms->u)) {
      y++;
      diffs++;
      *(faux_snake_tmp + steps) = DIFF_INSERT;
      step_dir = !step_dir;
    } else {
      error("Logic Error: unexpected outcome in snake creation process; contact maintainer"); // nocov
    }
    steps++;
  }
  if(x != ms->u || y != ms->v || steps >= max_steps) {
    error("Logic Error: faux snake process failed; contact maintainer."); // nocov
  }
  *faux_snake = faux_snake_tmp;
  return diffs;
}

/*
 * Advance from both ends of the diff graph toward center until we reach
 * up to half of the maximum possible number of differences between
 * a and b (note that `n` is net of `aoff`).  As we process this we record the
 * end points of each path we explored in the `ctx` structure.  Once we reach
 * the maximum number of differences, we return to `_ses` with the number of
 * differences found.  `_ses` will then attempt to stitch back the snakes
 * together.
 *
 * @param ms tracks beginning (x,y) and end (u,v) coords of the middle snake
 */
  static int
_find_middle_snake(
  SEXP a, int aoff, int n, SEXP b, int boff, int m, struct _ctx *ctx,
  struct middle_snake *ms, diff_op ** faux_snake
) {
  int delta, odd, mid, d;
  int x_max, y_max, v_max, u_max;
  ms->x = x_max = 0;
  ms->y = y_max = 0;
  ms->u = u_max = n;
  ms->v = v_max = m;
  double dist = (x_max - u_max) * (x_max - u_max) +
    (y_max - v_max) * (y_max - v_max);

  delta = n - m;
  odd = delta & 1;
  mid = (n + m) / 2;  // we check in `diff` that this won't overflow int
  mid += odd;

  _setv(ctx, 1, 0, 0);
  _setv(ctx, delta - 1, 1, n);

  /* For each number of differences `d`, compute the farthest reaching paths
   * from both the top left and bottom right of the edit graph
   *
   * First loop does NOT actually find a difference, which makes all the
   * difference calculations weird.
   */
  for (d = 0; d <= mid; d++) {
    int k, x, y;

    /* Reached maximum allowable differences before real exit condition.
     * Each loop iteration finds up to 2 d differences (one forward, one
     * backward).
     *
     * We know there is going to be at least one more difference because there
     * must be at least one for us to get here, and there might be two if the
     * extra forward difference doesn't find the end.
     */
    if (2 * (d - 1) > ctx->dmax - 1) {
      // So far we've found 2*(d - 1) differences
      ctx->dmaxhit = 1;
      ms->x = x_max; ms->y = y_max; ms->u = u_max; ms->v = v_max;
      return 2 * (d - 1) + _find_faux_snake(
        a, aoff, n, b, boff, m, ms, faux_snake, d - 1
      );
    }
    /* Forward (from top left) paths */

    // // Alternate looping picks path closest to middle diagonal.  If we change
    // // this we also should change it for backward paths.  This leads to more
    // // compact diffs, but TBD whether this is good IRL so we abandon it for
    // // now to avoid introducing behavior change.
    // int ki = 0;
    // k = d % 2 ? 1 : 0;
    // for (;
    //   k >= -d && k <= d;
    //   ki++, k += 2 * ki * (ki % 2 ? -1 : 1)
    // ) {
    for (k = d; k >= -d; k -= 2) {
      // If at lowest possible diag, or not at highest and next diag up is
      // further along in x, move to the right, otherwise move down.
      if (k == -d || (k != d && FV(k - 1) < FV(k + 1))) {
        x = FV(k + 1);      // move to the right, effectively
      } else {
        x = FV(k - 1) + 1;  // move down, effectively
      }
      y = x - k;

      ms->x = x;
      ms->y = y;
      while(x < n && y < m && _comp_chr(a, aoff + x, b, boff + y)) {
        x++; y++;  /* matching characters, just walk down diagonal */
      }
      double dist_new = (x - u_max) * (x - u_max) + (y - v_max) * (y - v_max);
      if(x <= n && y <= m && dist_new < dist) {
        dist = dist_new;
        x_max = x;
        y_max = y;
      }
      _setv(ctx, k, 0, x);

      /* For this diagonal k we are now at farthest reaching point for a given
       * `d`.  Then return if:
       *
       * - We're at the edge of the addressable part of the graph
       * - The reverse snakes are already overlapping in the `x` coordinate
       *
       * For the backward snake we reverse xy and uv so that the matching snake
       * is defined ` as starting at `ms.(xy)` and ending at `ms.(uv)`
       */
      if (odd && k >= (delta - (d - 1)) && k <= (delta + (d - 1))) {
        if (x >= RV(k)) {
          ms->u = x;
          ms->v = y;
          return 2 * d - 1;
        }
      }
    }
    // Check again if we'd go over by engaging the reverse snake
    if (2 * d > ctx->dmax) {
      // So far we've found 2*(d - 1) differences
      ctx->dmaxhit = 1;
      ms->x = x_max; ms->y = y_max; ms->u = u_max; ms->v = v_max;
      return 2 * (d - 1) + 1 + _find_faux_snake(
        a, aoff, n, b, boff, m, ms, faux_snake, d - 1
      );
    }
    /* Backwards (from bottom right) paths (see forward loop).  The two loops
     * are very similar so it is tempting to fold them into each other now, but
     * would require some work + ensuring no performance degradation.
     */
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

      while (x > 0 && y > 0 && _comp_chr(a, aoff + x - 1, b, boff + y - 1)) {
        /* matching characters, just walk up diagonal */
        x--; y--;
      }
      double dist_new = (x_max - x) * (x_max - x) + (y_max - y) * (y_max - y);
      if(x >= 0 && y >= 0 && dist_new < dist) {
        dist = dist_new;
        u_max = x;
        v_max = y;
      }
      _setv(ctx, kr, 1, x);

      /* see comments in forward section */
      if (!odd && kr >= -d && kr <= d) {
        if (x <= FV(kr)) {
          ms->x = x;
          ms->y = y;
          return 2 * d;
        }
      }
    }
  }
  error("Logic Error: failed finding middle snake, contact maintainer"); // nocov
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
  if(ctx->si > ctx->simax)
    error("Logic Error: exceed edit script length; contact maintainer."); // nocov
  if (e->op != op) {
    if (e->op) {
      ctx->si++;
      if(ctx->si > ctx->simax)
        error("Logic Error: exceed edit script length; contact maintainer."); // nocov
      e = ctx->ses + ctx->si;
    }
    e->op = op;
    e->off = off;
    e->len = len;
  } else {
    e->len += len;
  }
}
/*
 * Update edit script with the faux diff data
 */
  static void
_edit_faux(struct _ctx *ctx, diff_op * faux_snake, int aoff, int boff) {
  int i = 0, off;
  diff_op op;
  while((op = *(faux_snake + i++)) != DIFF_NULL) {
    switch (op) {
      case DIFF_MATCH: {
        boff++;  /* note no break here */
      }
      case DIFF_DELETE: off = aoff++;
        break;
      case DIFF_INSERT: off = boff++;
        break;
      default:
        error("Logic Error: unexpected faux snake instruction; contact maintainer"); // nocov
    }
    /* use x (aoff) offset for MATCH and DELETE, y offset for INSERT */
    _edit(ctx, op, off, 1);
  }
}
/* Generate shortest edit script
 *
 */
  static int
_ses(
  SEXP a, int aoff, int n, SEXP b, int boff, int m, struct _ctx *ctx
) {
  R_CheckUserInterrupt();
  struct middle_snake ms;
  int d;

  if (n == 0) {
    _edit(ctx, DIFF_INSERT, boff, m);
    d = m;
  } else if (m == 0) {
    _edit(ctx, DIFF_DELETE, aoff, n);
    d = n;
  } else {
    /* Find the middle "snake" around which we recursively solve the
     * sub-problems.  Note this modifies `ms` by ref to set the beginning and
     * end coordinates of the snake of the furthest reaching path.  The
     * beginning is always the top left part of the snake, irrespective of
     * whether it was found on a forward or reverse path as f_m_s will flip the
     * coordinates when appropriately when recording them in `ms`
     *
     * Additionally, if diffs exceed max.diffs, then `faux.snake` will also
     * be set.  `faux_snake` is a pointer to a pointer that points to a the
     * beginning of an array of match/delete/insert instructions generated
     * to connect the top left and bottom right paths. _fms() repoints the
     * pointer to an updated edit list if needed via (_ffs())
     */
    diff_op fsv = DIFF_NULL;
    diff_op * faux_snake;
    faux_snake = &fsv;

    d = _find_middle_snake(a, aoff, n, b, boff, m, ctx, &ms, &faux_snake);
    if (d == -1) {
      // nocov start
      error(
        "Logic error: failed trying to find middle snake, contact maintainer."
      );
      // nocov end
    } else if (ctx->ses == NULL) {
      // nocov start
      error(err_msg_ubrnch, 6);
      return d;
      // nocov end
    } else if (d != 1) {
      /* in this case we have something along the lines of (note the non-
       * diagonal bits are just non-diagonal, we're making no claims about
       * whether they should or could be of the horizontal variety)
       * ... -
       *      \
       *       \
       *        \- ...
       * so we will record the snake (diagonal) in the middle, and recurse
       * on the stub at the beginning and on the stub at the end separately
       *
       * Also have d == 0 case which can happen when the backward snake only has
       * matches.
       */

      /* Beginning stub */

      if (_ses(a, aoff, ms.x, b, boff, ms.y, ctx) == -1) {
        // nocov start
        error("Logic error: failed trying to run ses; contact maintainer.");
        // nocov end
      }
      /* Now record middle snake
       *
       * u should be x coord of end of snake of longest path
       * v should be y coord of end of snake
       * x, y should be coord of begining of snake
       *
       * record that there is a matching section between the beginning of the
       * middle snake and the end
       *
       * if faux_snake is defined it means that there were too many differences
       * to complete algorigthm normally so we need to record the faux snake
       */
      if(*faux_snake) {
        /* for faux snake length of snake will most likely not be ms.u - ms.x
         * since it will not be a diagonal
         */
        _edit_faux(ctx, faux_snake, aoff + ms.x, boff + ms.y);
      } else {
        _edit(ctx, DIFF_MATCH, aoff + ms.x, ms.u - ms.x);
      }
      /* Now recurse into the second stub */
      aoff += ms.u;
      boff += ms.v;
      n -= ms.u;
      m -= ms.v;
      if(_ses(a, aoff, n, b, boff, m, ctx) == -1) {
        // nocov start
        error("Logic error: failed trying to run ses 2; contact maintainer.");
        // nocov end
      }
    } else if (d == 1) {
      int x = ms.x;
      int u = ms.u;

      /* There are only 4 base cases when the edit distance is 1.  Having a hard
       * time finding cases that trigger the x == u, possibly because the algo
       * eats leading matches, although apparently we do achieve it somewhere in
       * the test suite.
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
      } else if (m < n) {
        if (x == u) {
          _edit(ctx, DIFF_MATCH, aoff, m);
          _edit(ctx, DIFF_DELETE, aoff + (n - 1), 1);
        } else {
          _edit(ctx, DIFF_DELETE, aoff, 1);
          _edit(ctx, DIFF_MATCH, aoff + 1, m);
        }
      } else {
        // Should never get here since this should be a D 2 case
        // nocov start
        error(
          "%s d %d n %d m %d aoff %d boff %d u %d; contact maintainer\n",
          "Logic Error: special case", d, n, m, aoff, boff, ms.u
        );
        // nocov end
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
diff(SEXP a, int aoff, int n, SEXP b, int boff, int m,
  void *context, int dmax, struct diff_edit *ses, int *sn
) {
  if(n < 0 || m < 0)
    error("Logic Error: negative lengths; contact maintainer.");  // nocov
  if(n > INT_MAX - m)
    error("Combined length of diffed vectors exeeds INT_MAX (%d)", INT_MAX);  // nocov
  struct _ctx ctx;
  int d, x, y;
  struct diff_edit *e = NULL;
  int delta = n - m;
  if(delta < 0) delta = -delta;
  if(n + m > INT_MAX - delta)
    error("Logic Error: exceeded max allowable combined string length.");  // nocov
  if(n + m + delta > INT_MAX / 4 - 1)
    error("Logic Error: exceeded max allowable combined string length.");  // nocov
  int bufmax = 4 * (n + m + delta) + 1;  // see _setv

  int *tmp = (int *) R_alloc(bufmax, sizeof(int));
  for(int i = 0; i < bufmax; i++) *(tmp + i) = 0;

  ctx.context = context;

  /* initialize buffer
   */
  ctx.buf = tmp;
  ctx.bufmax = bufmax;
  ctx.ses = ses;
  ctx.si = 0;
  ctx.simax = n + m;
  ctx.dmax = dmax >= 0 ? dmax : INT_MAX;
  ctx.dmaxhit = 0;

  /* initialize first ses edit struct*/
  if (ses && sn) {
    if ((e = ses) == NULL) {
      error("Logic Error: specifying sn, but ses is NULL, contact maintainer.");  // nocov
    }
    e->op = 0;
  }
  /* The _ses function assumes the SES will begin or end with a delete
   * or insert. The following will ensure this is true by eating any
   * beginning matches. This is also a quick to process sequences
   * that match entirely.
   */
  x = y = 0;
  if(boff > INT_MAX - m || aoff > INT_MAX - n)
      error("Internal error: overflow for a/boff; contact maintainer"); //nocov

  while (x < n && y < m) {
    if(!_comp_chr(a, aoff + x, b, boff + y)) break;
    x++; y++;
  }
  _edit(&ctx, DIFF_MATCH, aoff, x);

  d = _ses(a, aoff + x, n - x, b, boff + y, m - y, &ctx);
  if (ses && sn) {
    *sn = e->op ? ctx.si + 1 : 0;
  }
  return d * (ctx.dmaxhit ? -1 : 1);
}

