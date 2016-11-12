/*
 * Copyright (C) 2016  Brodie Gaslam
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

#include "diffobj.h"
#include <R_ext/Rdynload.h>

static const
R_CallMethodDef callMethods[] = {
  {"diffobj", (DL_FUNC) &DIFFOBJ_diffobj, 3},
  {NULL, NULL, 0}
};

void R_init_diffobj(DllInfo *info)
{
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}
