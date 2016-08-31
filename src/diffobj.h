/*
 * diffobj - Diffs for R Objects
 * Copyright (C) 2016  Brodie Gaslam
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses/GPL-3> for a copy of the license.
 */

#ifndef DIFFR_H_
#define DIFFR_H_

#include <R.h>
#include <Rinternals.h>
#include "diff.h"

SEXP DIFFOBJ_diffobj(SEXP a, SEXP b, SEXP max);

#endif

