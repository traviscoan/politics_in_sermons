# Utility functions for interacting with models and corpus objects

# lookup "top" n items
top_n <- function(x, lookup, n = 10){
  idx <- order(x, decreasing=TRUE)[1:n]
  return(lookup[idx])
}

#' Return token names for a vector token indexes
#'
#' Returns the token names associated with a vector of token indexes.
#' @param token_ids Vector of token indices
#' @param slda_obj A superlda data object (see \link{convert_superlda} for more information on superlda data objects).
#' @return Returns a character vector of token names.
#' @author Travis G. Coan <t.coan@exeter.ac.uk>
#' @export
get_token_name <- function(token_ids, slda_obj){
  return(slda_obj$vocab[token_ids])
}

#' Return label names for a vector label indexes
#'
#' Returns the label names associated with a vector of label indexes.
#' @param token_ids Vector of label indices
#' @param slda_obj A superlda data object (see \link{convert_superlda} for more information on superlda data objects).
#' @return Returns a character vector of label names.
#' @author Travis G. Coan <t.coan@exeter.ac.uk>
#' @export
get_label_name <- function(label_ids, slda_obj){
  return(slda_obj$labels[label_ids])
}

#' Return the most probable tokens for each label
#'
#' Returns the "n" most probable tokens (or "keywords) for each label
#' @param word_probs Word (or token) probability matrix from an estimated model
#' @param slda_obj A superlda data object (see \link{convert_superlda} for more information on superlda data objects).
#' @param n Number of keywords to return (default: n = 10)
#' @author Travis G. Coan <t.coan@exeter.ac.uk>
#' @export
get_top_keywords <- function(word_probs, slda_obj, n = 10){
  keywords <- as.data.frame(apply(word_probs, 2, top_n, lookup = slda_obj$vocab, n = n))
  names(keywords) <- slda_obj$labels
  return(keywords)
}

#' Return the most probable labels for each document
#'
#' Returns the "n" most probable labels for each document
#' @param doc_probs Document probability matrix from an estimated model
#' @param slda_obj A superlda data object (see \link{convert_superlda} for more information on superlda data objects).
#' @param n Number of labels to return (default: n = 3)
#' @author Travis G. Coan <t.coan@exeter.ac.uk>
#' @export
get_top_labels <- function(doc_probs, corpus, n = 3, rtype = "label"){
  if (rtype == "label") {
    top_labels <- as.data.frame(t(apply(doc_probs, 1, top_n, lookup = slda_obj$labels, n = n)))
    lnames <- apply(as.matrix(seq(n)), 1, function(x) paste("label", x, sep = ''))
    names(top_labels) <- lnames
    return(top_labels)
  } else if (rtype == "prob"){
      top_labels <- as.data.frame(t(apply(doc_probs, 1, function(x, n) x[order(x, decreasing=TRUE)[1:n]], n = n)))
      lnames <- apply(as.matrix(seq(n)), 1, function(x) paste("label", x, sep = ''))
      names(top_labels) <- lnames
      return(top_labels)
  } else {
    stop("Please enter a valid return type (rtype).")
  }
}



