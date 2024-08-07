// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// ismis_dotlist
LogicalVector ismis_dotlist(List dots);
RcppExport SEXP _offarray_ismis_dotlist(SEXP dotsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type dots(dotsSEXP);
    rcpp_result_gen = Rcpp::wrap(ismis_dotlist(dots));
    return rcpp_result_gen;
END_RCPP
}
// indfix
List indfix(List dots, LogicalVector dotmiss, IntegerVector dimx, IntegerVector offs, LogicalVector slice, LogicalVector nooff);
RcppExport SEXP _offarray_indfix(SEXP dotsSEXP, SEXP dotmissSEXP, SEXP dimxSEXP, SEXP offsSEXP, SEXP sliceSEXP, SEXP nooffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type dots(dotsSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type dotmiss(dotmissSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type dimx(dimxSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type offs(offsSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type slice(sliceSEXP);
    Rcpp::traits::input_parameter< LogicalVector >::type nooff(nooffSEXP);
    rcpp_result_gen = Rcpp::wrap(indfix(dots, dotmiss, dimx, offs, slice, nooff));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_offarray_ismis_dotlist", (DL_FUNC) &_offarray_ismis_dotlist, 1},
    {"_offarray_indfix", (DL_FUNC) &_offarray_indfix, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_offarray(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
