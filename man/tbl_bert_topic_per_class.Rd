% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tbl_bert_topic_per_class}
\alias{tbl_bert_topic_per_class}
\title{Find Topics from Class via Tibble}
\usage{
tbl_bert_topic_per_class(
  data,
  topic_model,
  document_name = NULL,
  class_name = NULL,
  global_tuning = TRUE,
  sort_by_topic = TRUE
)
}
\arguments{
\item{data}{Data with text and features}

\item{topic_model}{BERT topic model object}

\item{document_name}{name of the text feature}

\item{class_name}{name of the class feature(s)}

\item{global_tuning}{Fine-tune each topic representation for class c t by averaging its c-TF-IDF matrix with the global c-TF-IDF matrix. Turn this off if you want to prevent words in topic representations that could not be found in the documents for class c.  Default `TRUE`}
}
\description{
Find Topics from Class via Tibble
}
