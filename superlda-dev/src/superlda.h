#ifndef SUPERLDA_H
#define SUPERLDA_H

#include <RcppArmadillo.h>
using namespace arma;

// Declare arma-related functions
mat flatten_spmat(arma::sp_mat DTM, int n, int ntokens);
Rcpp::List rand_init_label(vec WS, vec DS, mat TAGSET, int nunique, int ndocs, int ntokens, int T);

#endif
