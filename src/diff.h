/*
 * This file is part of a program that contains a heavily modified version of
 * Michael B. Allen implementation of the Myers diff algorithm.  This
 * implementation is not compatible with the original one.  See next
 * comment blocks for original copyright and license information.
 *
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

#ifndef DIFFOBJ_DIFF_H
#define DIFFOBJ_DIFF_H

/* diff - compute a shortest edit script (SES) given two sequences
 */

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
        DIFF_NULL = 0,
	DIFF_MATCH,
	DIFF_DELETE,
	DIFF_INSERT
} diff_op;

struct diff_edit {
	short op;
	int off; /* off into s1 if MATCH or DELETE but s2 if INSERT */
	int len;
};

/* consider alternate behavior for each NULL parameter
 */
int diff(SEXP a, int aoff, int n,
  SEXP b, int boff, int m,
  void *context, int dmax,
  struct diff_edit *ses, int *sn
);

#ifdef __cplusplus
}
#endif

#endif
