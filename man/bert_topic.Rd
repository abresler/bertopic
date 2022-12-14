% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bertopic.R
\name{bert_topic}
\alias{bert_topic}
\title{Initiate BERT Topic Model}
\usage{
bert_topic(
  language = "english",
  top_n_words = 10L,
  use_key_phrase_vectorizer = F,
  is_lower_case = T,
  n_gram_range = list(1L, 3L),
  keyphrase_ngram_range = list(1L, 1L),
  min_topic_size = 10L,
  umap_model = NULL,
  hdbscan_model = NULL,
  vectorizer_model = NULL,
  embedding_model = NULL,
  ctfidf_model = NULL,
  nr_topics = NULL,
  low_memory = F,
  exclude_stop_words = T,
  extra_stop_words = NULL,
  calculate_probabilities = T,
  diversity = NULL,
  min_df = 1L,
  max_df = 1L,
  pos_pattern = "<J.*>*<N.*>+",
  seed_topic_list = NULL,
  verbose = T,
  vocabulary = NULL
)
}
\arguments{
\item{vocabulary}{}
}
\value{
python object
}
\description{
Functions for unsupervised clustering algorithms.
}
\details{

}
\examples{
import_bertopic()
data <- sklearn::sk_datasets()
docs_all <- data$fetch_20newsgroups(subset = 'all', remove = c('headers', 'footers', 'quotes'))
docs <- docs_all["data"]
tm <- bert_topic()
topic_model <- tm$fit_transform(documents = docs)


}
