% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimation.R
\name{train_flat_lda}
\alias{train_flat_lda}
\title{Train a labeled (or "flat") LDA model}
\usage{
train_flat_lda(slda_obj, beta = 0.01, alpha = 50, niter = 10)
}
\arguments{
\item{slda_obj}{A superlda data object (see \link{convert_superlda} for more information on superlda data objects).}

\item{beta}{Hyperparameter for the token distribution (see Details for more information; defaults to beta = 0.01).}

\item{alpha}{Hyperparameter for the document distribution (see Details for more information; defaults to alpha = 50).}

\item{niter}{Number of iterations to use for the Gibbs sampler (defaults to niter = 10).}
}
\value{
Returns a list with the two matrices of interest:
\itemize{
\item \code{word_probs} -- Normalized probability matrix for words (tokens x labels)
\item \code{doc_probs} -- Normalized probability matrix for documents (docs x labels)
}
}
\description{
Trains the "flat" LDA model via collapsed Gibbs sampling.
}
\examples{
# Load data table
dat = load("election_media.Rdata")

# Construct superlda data object using defaults:
slda_obj = construct_superlda(dat)

# Estimate a labeled (or flat) LDA using the defaults:
fit = train_flat_lda
}
\references{
Daniel Ramage, David Hall, Ramesh Nallapati, and Christopher D. Manning. 2009. Labeled LDA: a supervised topic model for credit attribution in multi-labeled corpora.
In Proceedings of the 2009 Conference on Empirical Methods in Natural Language Processing: Volume 1 - Volume 1 (EMNLP '09), Vol. 1.
Association for Computational Linguistics, Stroudsburg, PA, USA, 248-256.

Timothy Rubin, America Chambers, Padhraic Smyth, and Mark Steyvers. 2012. Statistical Topic Models for Multi-Label Document Classification.
\emph{Jounral of Machine Learning} 88: 157-208.
}
\author{
Travis G. Coan \href{mailto:t.coan@exeter.ac.uk}{t.coan@exeter.ac.uk}
}
