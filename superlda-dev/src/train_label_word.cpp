//#include <RcppArmadillo.h>
#include "superlda.h"
using namespace arma;

// [[Rcpp::export]]
Rcpp::List train_label_word(arma::sp_mat DTM, arma::sp_mat DLM, int ndocs, int nunique, int ntokens, int nelem, int niter, double BETA, double ALPHA) {
  /* Trains the label-word distribution (phi) via collapsed Gibbs sampling

   Inputs
   ------
   DTM     = sparse, document-term matrix
   DLM     = sparse, document-label matrix
   ndocs   = number of documents
   nunique = number of "unique" tokens
   ntokens = total number of tokens
   nelem   = number of elements in sparse DTM
   niter   = number of iterations for the Gibbs sampler
   BETA    = hyperparameter for the token distribution
   ALPHA   = hyperparameter for the document distribution

   Outputs
   -------
   WP     = Normalized probability matrix for words (tokens x labels)
   DP     = Normalized probability matrix for documents (docs x labels)

   */

  // Parse word (WS) and document (DS) vectors
  Rprintf("Generating input data...\n");
  mat WSDS = flatten_spmat(DTM, nelem, ntokens);
  vec WS = WSDS.col(0);
  vec DS = WSDS.col(1);

  // Convert DLM to dense matrix
  mat TAGSET(DLM);
  int T = TAGSET.n_cols;    // lookup total number of tags

  // Setup locals
  int i, ii, j, wi, di, topic, iter, navail;
  double WBETA, totprob, cumprob, r;
  uvec TAVAIL, TAVAIL_rand; // vector to hold available tags
  // Setup matrices
  mat WP_prob;
  mat DP_prob;
  vec probs;
  vec probs_norm;
  vec wpbeta;              // used when normalizing WP matrix

  // Random initialization
  Rcpp::List ivalues = rand_init_label(WS, DS, TAGSET, nunique, ndocs, ntokens, T);
  mat WP = ivalues[0];
  mat DP = ivalues[1];
  ivec ztot = ivalues[2];
  ivec Z = ivalues[3];

  Rprintf("Start Gibbs sampling...\n");
  // Determine random order update sequence
  ivec order = shuffle(regspace<ivec>(0, 1, ntokens-1));

  // Gibbs update
  WBETA = nunique*BETA; // "words" x BETA
  for (iter = 1; iter <= niter; iter++){
    printf("\titer = %d\n" , iter);
    if ((iter % 10)==0) Rprintf("\tIteration %d of %d\n" , iter , niter);

    for (ii = 0; ii < ntokens; ii++){ // DEBUG -- change back to ntokens
      i = order[ii]; // get random token index from order vec
      wi = WS(i);    // get the word
      di = DS(i);    // get the document
      topic = Z(i);  // current topic assignment

      // All tokens up to, but not including the token under consideration
      ztot(topic)--;
      WP(wi, topic)--;
      DP(di, topic)--;

      // Find the available tags for document "di"
      TAVAIL = find(TAGSET.row(di));

      // Collapsed sampler update
      totprob = 0;
      navail = TAVAIL.n_elem;
      probs = zeros(navail);
      probs_norm = zeros(navail);

      for (j = 0; j < navail; j++) {
        topic = TAVAIL(j);
        probs(j) = ((WP(wi,topic) + BETA)/(ztot[topic] + WBETA))*(DP(di,topic) + (ALPHA/navail));
        totprob += probs(j);
      }

      // Normalize to transform into probability
      probs_norm = probs/totprob;

      // Sample from multinomial distribution (cummulative method)
      r = randu();
      cumprob = probs_norm(0);
      j = 0;
      while (r > cumprob){
        j++;
        cumprob += probs_norm(j);
      }
      // Return sampled topic
      topic = TAVAIL(j);

      // Update topic assignment
      Z(i) = topic;    // assign current word token i to topic
      WP(wi,topic)++;  // and update counts
      DP(di,topic)++;
      ztot(topic)++;
    }
  }

  Rprintf("Normalizing word and document matrices...\n");
  // Normalize the WP matrix
  for (i = 0; i < T; i++){
    // Word x label probability matrix
    wpbeta = WP.col(i) + BETA;
    WP_prob.insert_cols(i, wpbeta/sum(wpbeta));
  }

  // Normalize the DP matrix
  for (i = 0; i < ndocs; i++){
    // Doc x label probability matrix
    DP_prob.insert_rows(i, DP.row(i)/sum(DP.row(i)));
  }

  Rprintf("Returning results to R...\n");
  // Return word and topic matrices
  return Rcpp::List::create(Rcpp::Named("word_probs") = WP_prob,
                            Rcpp::Named("doc_probs") = DP_prob);
                            //Rcpp::Named("ztot") = ztot,
                            //Rcpp::Named("worder") = order,
                            //Rcpp::Named("Z") = Z);
}
