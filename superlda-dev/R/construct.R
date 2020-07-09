# Functions to convert corpus data tables (or frames) into
# useable data objects for superlda.

create_dfm <- function(corpus_df, vname){
  # Tokenize
  it = itoken(corpus_df[[vname]],
              tokenizer = word_tokenizer,
              progressbar = FALSE)

  # Create a vocabulary mapping
  vocab = create_vocabulary(it)

  # Vectorize
  vectorizer = vocab_vectorizer(vocab)

  # Create a document-feature-matrix
  return(list("dfm" = create_dtm(it, vectorizer, type = "dgTMatrix"),
              "vectorizer" = vectorizer))
}

transform_dfm <- function(corpus_df, vname, vectorizer){
  # Tokenize
  it = itoken(corpus_df[[vname]],
              tokenizer = word_tokenizer,
              progressbar = FALSE)

  return(create_dtm(it, vectorizer, type = "dgTMatrix"))
}

#' Construct a superlda data object
#'
#' Creates a superlda data object from a data.table (or data.frame) using the text2vec package. This returns a list with the following elements:
#' * *vocab* -- A character vector of the unique *tokens*
#' * *token_vectorizer* -- The text2vec vectorizer used to generate the document-token matrix
#' * *dtm* -- A sparse document-token matrix (dgTMatrix)
#' * *labels* -- A character vector of the unique *labels*
#' * *label_vectorizer* -- The text2vec vectorizer used to generate the document-label matrix
#' * *dlm* -- A sparse document-term matrix (dgTMatrix)
#' @param corpus_df The data.table (or data.frame) used to construct the superlda data object
#' @param text Name of the column holding the text data.
#' @param labels Name of the column holding the label data.
#' @param verbose Print progress (useful for large datasets).
#' @return superlda data object
#' @author Travis G. Coan <t.coan@exeter.ac.uk>
#' @examples
#' # Load data table
#' dat = load("election_media.Rdata")
#'
#' # Construct superlda data object using defaults:
#' slda_obj = construct_superlda(dat)
#' @export
construct_superobj <- function(corpus_df, text = "text", labels = "labels", verbose = FALSE){

  # Get document-term matrix
  if (verbose == TRUE){
    cat("Generating document-term matrix...\n")
  }

  dtm_list = create_dfm(corpus_df, text)
  dtm = dtm_list$dfm
  token_vectorizer = dtm_list$vectorizer

  # Get document-label matrix
  if (verbose == TRUE){
    cat("Generating document-label matrix...")
  }

  dlm_list = create_dfm(corpus_df, labels)
  dlm = dlm_list$dfm
  label_vectorizer = dlm_list$vectorizer

  # Extract vocab and label vectors
  vocab = dtm@Dimnames[[2]]
  labels = dlm@Dimnames[[2]]

  return(list("vocab" = vocab,
              "token_vectorizer" = token_vectorizer,
              "dtm" = dtm,
              "labels" = labels,
              "label_vectorizer" = label_vectorizer,
              "dlm" = dlm))
}

#' Make data consistent with an existing superlda data object
#'
#' Transforms a data.table (or data.frame) such at the returned features are consistent with an exisitng superlda data object. This returns a list with the following elements:
#' * *vocab* -- A character vector of the unique *tokens*
#' * *token_vectorizer* -- The text2vec vectorizer used to generate the document-token matrix
#' * *dtm* -- A sparse document-token matrix (dgTMatrix)
#' * *labels* -- A character vector of the unique *labels*
#' * *label_vectorizer* -- The text2vec vectorizer used to generate the document-label matrix
#' * *dlm* -- A sparse document-term matrix (dgTMatrix)
#' @param corpus_df The data.table (or data.frame) used to construct the superlda data object
#' @param text Name of the column holding the text data.
#' @param labels Name of the column holding the label data.
#' @param verbose Print progress (useful for large datasets).
#' @return superlda data object
#' @author Travis G. Coan <t.coan@exeter.ac.uk>
#' @examples
#' # Load data table
#' dat = load("election_media.Rdata")
#'
#' # Construct superlda data object using defaults:
#' slda_obj = construct_superlda(dat)
#' @export
transform_superobj <- function(corpus_df, slda_obj, text = "text"){

  # Get document-term matrix
  dtm = transform_dfm(corpus_df, text, slda_obj$token_vectorizer)

  return(list("vocab" = slda_obj$vocab,
              "token_vectorizer" = slda_obj$token_vectorizer,
              "dtm" = dtm,
              "labels" = slda_obj$labels,
              "label_vectorizer" = slda_obj$label_vectorizer,
              "dlm" = NA))
}

