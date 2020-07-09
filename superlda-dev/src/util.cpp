#include <RcppArmadillo.h>
using namespace arma;

mat flatten_spmat(sp_mat DTM, int n, int ntokens) {
  /* Flattens a list of vectors holding the token IDs for each document.

   Inputs
   ------
   DTM     = sparse, document-topic matrix
   nelem   = total number of elements in the DTM (i.e., total number of tokens
             counts).
   ntokens = total number of tokens

   Outputs
   -------
   WSDS  = Matrix holding flattened token (WS) and document (DS) IDs (n x 2)

   */

  // Preallocate matrix to hold results
  mat WSDS = zeros(ntokens, 2);

  //----------------------------------------------
  // Flatten sparse matrix

  // Find begining and end of nonzero elements
  sp_mat::const_iterator start = DTM.begin();
  sp_mat::const_iterator end   = DTM.end();

  // Setup counters
  int i = 0;
  int j;

  for(sp_mat::const_iterator it = start; it != end; ++it){
    // Parse dtm element
    int doc = it.row();
    int token = it.col();
    int freq = (*it);

    // Repeat elements based on frequency
    for (j = 0; j < freq; j++){
      WSDS(i,0) = token;
      WSDS(i,1) = doc;
      i++;
    }
  }
  return(WSDS);
}

Rcpp::List rand_init_label(vec WS, vec DS, mat TAGSET, int nunique, int ndocs, int ntokens, int T){
  /* Random intitialization for the labeled LDA

   Inputs
   ------
   WS      = vector of token IDs for the entire corpus (ntokens x 1)
   DS      = vector of document IDs for the entire corpus (ntokens x 1)
   TAGSET  = (binary) matrix of tags for each document (ndocs x T)
   nunique = number of "unique" tokens
   ndocs   = number of documents
   ntokens = total number of tokens
   T       = number of topics

   Outputs
   -------
   WP      = matrix of label counts for tokens (nunique x T)
   DP      = matrix of label counts for documents (ndocs x T)
   ztot    = vector of total counts for Z assignments (T x 1)
   Z       = vector of Z assignments (ntokens x 1)

   */
  int i, wi, di, topic;
  uvec TAVAIL, TAVAIL_rand; // vector to hold available tags
  mat WP = zeros(nunique, T);
  mat DP = zeros(ndocs, T);
  ivec Z = zeros<ivec>(ntokens);
  ivec ztot = zeros<ivec>(T);
  Rprintf("Starting random initialization...\n");
  for (i = 0; i < ntokens; i++){
    wi = WS(i);      // lookup word
    di = DS(i);      // lookup document

    // Find the available tags for document "di"
    TAVAIL = find(TAGSET.row(di));

    // Sample from available tags
    TAVAIL_rand = shuffle(TAVAIL);
    topic = TAVAIL_rand(0);

    // Assign tag and update counters
    Z(i) = topic;    // assign word to intial, random topic
    WP(wi, topic)++; // increment wp count matrix
    DP(di, topic)++; // increment dp count matrix
    ztot(topic)++;   // increment ztot matrix
  }
  return Rcpp::List::create(Rcpp::Named("WP") = WP,
                            Rcpp::Named("DP") = DP,
                            Rcpp::Named("ztot") = ztot,
                            Rcpp::Named("Z") = Z);
}
