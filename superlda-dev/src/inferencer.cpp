#include "superlda.h"
using namespace arma;

// [[Rcpp::export]]
Rcpp::List infer_labels(arma::sp_mat DTM, int ndocs, int nunique, int ntokens, int nelem, int T, int niter, double BETA, double ALPHA, arma::mat PCW) {
  /* Infer unlabeled documents for"flat" LDA via collapsed Gibbs sampling.

   Inputs
   ------
   DTM     = list of token ID vectors for each document
   ndocs   = number of documents
   nunique = number of "unique" tokens
   ntokens = total number of tokens
   nelem   = number of elements in sparse DTM
   T       = number of labels
   niter   = number of iterations for the Gibbs sampler
   BETA    = hyperparameter for the token distribution (scalar)
   ALPHA   = hyperparameter for the document distribution (scalar)
   PWC     = (Normalized) Token x label probablity matirx

   Outputs
   -------
   DP     = Normalized probability matrix for documents (docs x labels)

   */

  // Parse word (WS) and document (DS) vectors
  Rcpp::Rcout << "Generating input data...\n" << std::endl;
  mat WSDS = flatten_spmat(DTM, nelem, ntokens);
  vec WS = WSDS.col(0);
  vec DS = WSDS.col(1);

  // Get hyperparameter for labels
  vec cdalpha = (ones(T) * ALPHA)/T;

  // Setup locals
  int i, ii, j, wi, di, topic, iter, navail;
  double totprob, cumprob, r;

  // Setup matrices
  mat DP = zeros(ndocs, T);
  ivec Z = zeros<ivec>(ntokens);
  mat DP_prob;
  vec probs;
  vec probs_norm;

  Rprintf("Starting random initialization...\n");
  for (i = 0; i < ntokens; i++){
    wi = WS(i);      // lookup word
    di = DS(i);      // lookup document

    // Assign tag and update counters
    topic = randi(1, distr_param(0,T-1))(0,0); // randomly sample topic
    Z(i) = topic;                              // assign word to intial, random topic
    DP(di, topic)++;                           // increment dp count matrix
  }

  Rprintf("Start Gibbs sampling...\n");
  // Determine random order update sequence
  ivec order = shuffle(regspace<ivec>(0, 1, ntokens-1));

  // Gibbs update
  for (iter = 1; iter <= niter; iter++){
    Rprintf("\titer = %d\n" , iter);
    if ((iter % 10)==0) Rprintf("\tIteration %d of %d\n" , iter , niter);

    for (ii = 0; ii < ntokens; ii++){
      i = order[ii]; // get random token index from order vec
      wi = WS(i);    // get the word
      di = DS(i);    // get the document
      topic = Z(i);  // current topic assignment

      // All docs up to, but not including the doc under consideration;
      DP(di, topic)--;

      // Collapsed sampler update
      totprob = 0;
      probs = zeros(T);
      probs_norm = zeros(T);

      for (j = 0; j < T; j++) {
        probs(j) = PCW(wi,j)*(DP(di,j) + cdalpha(j));
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
      topic = j;

      // Update topic assignment
      Z(i) = topic;    // assign current word token i to topic
      DP(di,topic)++;  // update counter
    }
  }

  // Normalize the DP matrix
  for (i = 0; i < ndocs; i++){
    // Doc x label probability matrix
    DP_prob.insert_rows(i, DP.row(i)/sum(DP.row(i)));
  }

  // Return list for debugging
  return Rcpp::List::create(Rcpp::Named("doc_probs") = DP_prob);
}

// [[Rcpp::export]]
Rcpp::List infer_topics(arma::sp_mat DTM, int ndocs, int nunique, int ntokens, int nelem, int T, int niter, double BETA, arma::vec ALPHA, arma::mat PCW) {
  /* Infer topics from a fitted LDA model with asymetric hyperparameters for the topic-word
   * distribution (alpha). 
  
  Inputs
  ------
  DTM     = list of token ID vectors for each document
  ndocs   = number of documents
  nunique = number of "unique" tokens
  ntokens = total number of tokens
  nelem   = number of elements in sparse DTM
  T       = number of topics
  niter   = number of iterations for the Gibbs sampler
  BETA    = hyperparameter for the token distribution (scalar)
  ALPHA   = hyperparameters for the document distribution (vector)
  PWC     = (Normalized) Token x label probablity matirx
  
  Outputs
  -------
  DP     = Normalized probability matrix for documents (docs x labels)
  
  */
  
  // Parse word (WS) and document (DS) vectors
  Rcpp::Rcout << "Generating input data...\n" << std::endl;
  mat WSDS = flatten_spmat(DTM, nelem, ntokens);
  vec WS = WSDS.col(0);
  vec DS = WSDS.col(1);
  
  // Get hyperparameter for topics
  vec cdalpha = ALPHA;
  
  // Setup locals
  int i, ii, j, wi, di, topic, iter, navail;
  double totprob, cumprob, r;
  
  // Setup matrices
  mat DP = zeros(ndocs, T);
  ivec Z = zeros<ivec>(ntokens);
  mat DP_prob;
  vec probs;
  vec probs_norm;
  
  Rprintf("Starting random initialization...\n");
  for (i = 0; i < ntokens; i++){
    wi = WS(i);      // lookup word
    di = DS(i);      // lookup document
    
    // Assign topic and update counters
    topic = randi(1, distr_param(0,T-1))(0,0); // randomly sample topic
    Z(i) = topic;                              // assign word to intial, random topic
    DP(di, topic)++;                           // increment dp count matrix
  }
  
  Rprintf("Start Gibbs sampling...\n");
  // Determine random order update sequence
  ivec order = shuffle(regspace<ivec>(0, 1, ntokens-1));
  
  // Gibbs update
  for (iter = 1; iter <= niter; iter++){
    Rprintf("\titer = %d\n" , iter);
    if ((iter % 10)==0) Rprintf("\tIteration %d of %d\n" , iter , niter);
    
    for (ii = 0; ii < ntokens; ii++){
      i = order[ii]; // get random token index from order vec
      wi = WS(i);    // get the word
      di = DS(i);    // get the document
      topic = Z(i);  // current topic assignment
      
      // All docs up to, but not including the doc under consideration;
      DP(di, topic)--;
      
      // Collapsed sampler update
      totprob = 0;
      probs = zeros(T);
      probs_norm = zeros(T);
      
      for (j = 0; j < T; j++) {
        probs(j) = PCW(wi,j)*(DP(di,j) + cdalpha(j));
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
      topic = j;
      
      // Update topic assignment
      Z(i) = topic;    // assign current word token i to topic
      DP(di,topic)++;  // update counter
    }
  }
  
  // Normalize the DP matrix
  for (i = 0; i < ndocs; i++){
    // Doc x label probability matrix
    DP_prob.insert_rows(i, DP.row(i)/sum(DP.row(i)));
  }
  
  // Return list for debugging
  return Rcpp::List::create(Rcpp::Named("doc_probs") = DP_prob);
}
