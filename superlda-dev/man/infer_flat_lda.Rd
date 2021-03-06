% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation.R
\name{infer_flat_lda}
\alias{infer_flat_lda}
\title{Infer labels for unseen documents via the "flat" (or labeled) LDA model}
\usage{
infer_flat_lda(slda_obj, word_probs, beta = 1, alpha = 100,
  niter = 10)
}
\arguments{
\item{corpus}{A corpus list object generated by the read_corpus() function}
}
\description{
Infers the labels for unseen documents using a trained "flat" (or labeled) LDA model.
The inferencer uses collapsed Gibbs sampling, but fixes the word-label distributions
to the trained values.
}
\examples{
read_corpus("data/text_to_read.txt")
}
\author{
Travis G. Coan \href{mailto:t.coan@exeter.ac.uk}{t.coan@exeter.ac.uk}
}
