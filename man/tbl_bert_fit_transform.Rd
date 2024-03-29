% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bertopic.R
\name{tbl_bert_fit_transform}
\alias{tbl_bert_fit_transform}
\title{BERTopic Fit and Transform from tibble.}
\usage{
tbl_bert_fit_transform(
  obj,
  data = NULL,
  text_field = NULL,
  class_fields = NULL,
  embeddings = NULL,
  images = NULL
)
}
\arguments{
\item{obj}{BERTopic Object}

\item{data}{a tibble}

\item{text_field}{name of text field}

\item{class_fields}{if not `NULL` `y` or `class` fields.}

\item{embeddings}{If not `NULL` Pre-trained document embeddings. These can be used instead of the sentence-transformer model.}

\item{images}{}
}
\description{
Fit the models on a collection of documents, generate topics, and return the docs with topics.
}
