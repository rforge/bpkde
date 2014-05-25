/*
 *  Part of R package KernSmooth
 *  Copyright (C) 2005-2007  B. D. Ripley
 *
 *  Unlimited use and distribution (see LICENCE).
 */

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

void F77_SUB(f_linbin)(double *x, int *n, double *a,
		     double *b, int *m, int *trun, double *gcounts);

void F77_SUB(f_lbtwod)(double *x, int *n, double *a1,
		     double *a2, double *b1, double *b2, int *m1,
		     int *m2, double *gcounts);


void R_init_bpkde(DllInfo *dll)
{
  R_FortranMethodDef FortEntries[] = {
    {"linbin", (DL_FUNC) &F77_SUB(f_linbin),  7},
    {"lbtwod", (DL_FUNC) &F77_SUB(f_lbtwod),  9},
    {NULL, NULL, 0}
  };

  R_registerRoutines(dll, NULL, NULL, FortEntries, NULL);
}


