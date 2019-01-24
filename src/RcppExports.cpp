// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// rcpp_buildHmm
IntegerMatrix rcpp_buildHmm(ListOf < ListOf < IntegerVector > >& lengths, ListOf < ListOf < IntegerVector > >& values, IntegerVector& chromosomeSizes, int minDepth);
RcppExport SEXP _srnadiff_rcpp_buildHmm(SEXP lengthsSEXP, SEXP valuesSEXP, SEXP chromosomeSizesSEXP, SEXP minDepthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type lengths(lengthsSEXP);
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type chromosomeSizes(chromosomeSizesSEXP);
    Rcpp::traits::input_parameter< int >::type minDepth(minDepthSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_buildHmm(lengths, values, chromosomeSizes, minDepth));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_viterbi
DataFrame rcpp_viterbi(IntegerVector& chromosomeSizes, NumericMatrix& transitions, NumericMatrix& emissions, float emissionThreshold, NumericVector& starts, IntegerVector& counts, NumericVector& pvalues, ListOf < ListOf < IntegerVector > >& lengths, ListOf < ListOf < IntegerVector > >& values, int minDepth, int minSize, int maxSize);
RcppExport SEXP _srnadiff_rcpp_viterbi(SEXP chromosomeSizesSEXP, SEXP transitionsSEXP, SEXP emissionsSEXP, SEXP emissionThresholdSEXP, SEXP startsSEXP, SEXP countsSEXP, SEXP pvaluesSEXP, SEXP lengthsSEXP, SEXP valuesSEXP, SEXP minDepthSEXP, SEXP minSizeSEXP, SEXP maxSizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector& >::type chromosomeSizes(chromosomeSizesSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type transitions(transitionsSEXP);
    Rcpp::traits::input_parameter< NumericMatrix& >::type emissions(emissionsSEXP);
    Rcpp::traits::input_parameter< float >::type emissionThreshold(emissionThresholdSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type starts(startsSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type counts(countsSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type pvalues(pvaluesSEXP);
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type lengths(lengthsSEXP);
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< int >::type minDepth(minDepthSEXP);
    Rcpp::traits::input_parameter< int >::type minSize(minSizeSEXP);
    Rcpp::traits::input_parameter< int >::type maxSize(maxSizeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_viterbi(chromosomeSizes, transitions, emissions, emissionThreshold, starts, counts, pvalues, lengths, values, minDepth, minSize, maxSize));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_naive
DataFrame rcpp_naive(ListOf < ListOf < IntegerVector > >& lengths, ListOf < ListOf < IntegerVector > >& values, IntegerVector& chromosomeSizes, NumericVector normalizationFactors, int depth, int distance, int size);
RcppExport SEXP _srnadiff_rcpp_naive(SEXP lengthsSEXP, SEXP valuesSEXP, SEXP chromosomeSizesSEXP, SEXP normalizationFactorsSEXP, SEXP depthSEXP, SEXP distanceSEXP, SEXP sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type lengths(lengthsSEXP);
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type chromosomeSizes(chromosomeSizesSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type normalizationFactors(normalizationFactorsSEXP);
    Rcpp::traits::input_parameter< int >::type depth(depthSEXP);
    Rcpp::traits::input_parameter< int >::type distance(distanceSEXP);
    Rcpp::traits::input_parameter< int >::type size(sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_naive(lengths, values, chromosomeSizes, normalizationFactors, depth, distance, size));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_normalization
NumericVector rcpp_normalization(ListOf < ListOf < IntegerVector > >& lengths, ListOf < ListOf < IntegerVector > >& values, IntegerVector& chromosomeSizes, IntegerVector& librarySizes);
RcppExport SEXP _srnadiff_rcpp_normalization(SEXP lengthsSEXP, SEXP valuesSEXP, SEXP chromosomeSizesSEXP, SEXP librarySizesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type lengths(lengthsSEXP);
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type chromosomeSizes(chromosomeSizesSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type librarySizes(librarySizesSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_normalization(lengths, values, chromosomeSizes, librarySizes));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_slice
List rcpp_slice(ListOf < ListOf < IntegerVector > >& lengths, ListOf < ListOf < IntegerVector > >& values, IntegerVector& chromosomeSizes, int minDepth, int minSize, int maxSize, int minDifference);
RcppExport SEXP _srnadiff_rcpp_slice(SEXP lengthsSEXP, SEXP valuesSEXP, SEXP chromosomeSizesSEXP, SEXP minDepthSEXP, SEXP minSizeSEXP, SEXP maxSizeSEXP, SEXP minDifferenceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type lengths(lengthsSEXP);
    Rcpp::traits::input_parameter< ListOf < ListOf < IntegerVector > >& >::type values(valuesSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type chromosomeSizes(chromosomeSizesSEXP);
    Rcpp::traits::input_parameter< int >::type minDepth(minDepthSEXP);
    Rcpp::traits::input_parameter< int >::type minSize(minSizeSEXP);
    Rcpp::traits::input_parameter< int >::type maxSize(maxSizeSEXP);
    Rcpp::traits::input_parameter< int >::type minDifference(minDifferenceSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_slice(lengths, values, chromosomeSizes, minDepth, minSize, maxSize, minDifference));
    return rcpp_result_gen;
END_RCPP
}
