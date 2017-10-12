#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP _srnadiff_rcpp_buildHmm(SEXP, SEXP, SEXP);
extern SEXP _srnadiff_rcpp_viterbi(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _srnadiff_rcpp_clustering(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_srnadiff_rcpp_buildHmm",   (DL_FUNC) &_srnadiff_rcpp_buildHmm,   3},
    {"_srnadiff_rcpp_viterbi",    (DL_FUNC) &_srnadiff_rcpp_viterbi,    8},
    {"_srnadiff_rcpp_clustering", (DL_FUNC) &_srnadiff_rcpp_clustering, 7},
    {NULL, NULL, 0}
};

void R_init_srnadiff(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
