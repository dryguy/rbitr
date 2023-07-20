// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// parse_engine_line_cpp
CharacterVector parse_engine_line_cpp(std::string engine_line, CharacterVector tag_names);
RcppExport SEXP _rbitr_parse_engine_line_cpp(SEXP engine_lineSEXP, SEXP tag_namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type engine_line(engine_lineSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type tag_names(tag_namesSEXP);
    rcpp_result_gen = Rcpp::wrap(parse_engine_line_cpp(engine_line, tag_names));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_rbitr_parse_engine_line_cpp", (DL_FUNC) &_rbitr_parse_engine_line_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_rbitr(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
