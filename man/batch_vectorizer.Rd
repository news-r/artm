% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batches-utils.R
\name{batch_vectorizer}
\alias{batch_vectorizer}
\title{Batch Vectorizer}
\usage{
batch_vectorizer(preprocessed_file, batches = NULL,
  collection_name = NULL, data_format = c("vowpal_wabbit", "batches",
  "bow_uci", "bow_n_wd"), target_folder = NULL, batch_size = 1000,
  batch_name_type = "code", data_weight = 1, n_wd = NULL,
  vocabulary = NULL, gather_dictionary = TRUE, class_ids = NULL,
  process_in_memory_model = NULL)
}
\description{
Batch Vectorizer
}
