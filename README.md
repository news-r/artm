
<!-- README.md is generated from README.Rmd. Please edit that file -->
artm
====

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental) <!-- badges: end -->

The goal of artm is to integrate by integrating [BigARTM](http://bigartm.org/) (**A**dditive **R**egularization of **T**opic **M**odels) with R.

Installation
------------

Install the package using `remotes`.

``` r
# install.packages("remotes")
remotes::install_github("news-r/artm")
```

To install the dependency follow the instructions given on the [official documentation](http://docs.bigartm.org/en/stable/installation/index.html).

Example
-------

Use data from another [news-r](https://news-r.org) package.

``` r
# remotes::install_github("news-r/nethoser")
data("webhoser", package = "nethoser")
```

Preprocess.

``` r
library(artm)

# Preprocess returns path to file
(file <- preprocess(webhoser, text, uuid))

batch_vectorizer(file)
```
