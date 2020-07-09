# Political speech in sermons

### Summary

This repository provides the code used in:

* "[Political Speech in Religious Sermons](https://www.cambridge.org/core/journals/politics-and-religion/article/political-speech-in-religious-sermons/53583EA3BD5F4223B5E31AA279698563)." Constantine Boussalis, Travis G. Coan, and Mirya R. Holman. *Politics and Religion*, 2020. doi:10.1017/S1755048320000334.


### The `superlda` package

#### Installation

I wrote a custom `R` package (`superlda`) to estimate the Labelled Latent Dirchlet Allocation (L-LDA) model described in the main text (see the section on "Learning About Political Speech From Community-Generated Labels").

After cloning the repository, you will need to build the `superlda` package from source. Open up the terminal and `cd` into the politics_in_sermons root directory. Then type:

```console
R CMD build superlda-dev
R CMD INSTALL superlda_0.1.tar.gz
```

For more information on `superlda`, including the package contents, type `help(package=superlda)`.

#### Usage

To illustate how to use `superlda`, we can the `sermons` data frame that is automatically loaded
with the package.

```{r}
print(nrow(sermons))
```

The `sermons` data frame provides a sample of 10,000 sermons from the sermons central data set. The data frame has 3 variables: a sermon id, a list of labels, and the tokens used by the L-LDA. The first step is to contruct a `superlda` object, which takes the raw data frame and transforms it into a structure suitable for estimation (type `help(construct_superobj)' for more information):

```{r}
slda_obj = construct_superobj(dat_train, verbose = T)
```

We then train the "flat" L-LDA model and view the top keywords as follows:

```{r}
# Fit a labeled  LDA
fit1 = train_flat_lda(slda_obj, niter = 5)

# Examine fit results
keys = get_top_keywords(fit1$word_probs, slda_obj)
View(keys)
```

Lastly, we can infer "unseen" documents as follows:

```{r}
## Inferring the topic probabilities for unknown labels ##
slda_obj_test = transform_superobj(sermons_unseen, slda_obj)
fit2 = infer_flat_lda(slda_obj_test, fit1$word_probs, niter = 5)
doc_labels = get_top_labels(fit2$doc_probs, rtype = 'label')
View(doc_labels)
```


