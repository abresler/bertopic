% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{bert_update_topics}
\alias{bert_update_topics}
\title{Updates the topic representation by recalculating c-TF-IDF with the new parameters as defined in this function.}
\usage{
bert_update_topics(
  obj,
  docs = NULL,
  topics = NULL,
  top_n_words = 10,
  images = NULL,
  n_gram_range = list(1L, 1L),
  use_empty_vectorizer = FALSE,
  vectorizer_model = NULL,
  ctfidf_model = NULL,
  representation_model = NULL,
  language = "english",
  use_key_phrase_vectorizer = F,
  use_sklearn_vectorizer = F,
  is_lower_case = T,
  keyphrase_ngram_range = list(1L, 1L),
  exclude_stop_words = T,
  stopword_package_sources = NULL,
  extra_stop_words = NULL,
  min_df = 1L,
  max_df = 1L,
  pos_pattern = "<J.*>*<N.*>+",
  seed_topic_list = NULL,
  decay = NULL,
  delete_min_df = NULL,
  workers = 6L,
  vocabulary = NULL
)
}
\arguments{
\item{obj}{BERTopic Object}

\item{docs}{The documents you used when calling either fit or fit_transform}

\item{topics}{A list of topics where each topic is related to a document in docs. Use this variable to change or map the topics. NOTE: Using a custom list of topic assignments may lead to errors if topic reduction techniques are used afterwards. Make sure that manually assigning topics is the last step in the pipeline.  Default `NULL`}

\item{top_n_words}{The number of words per topic to extract. Setting this too high can negatively impact topic embeddings as topics are typically best represented by at most 10 words.  Default `10`}

\item{n_gram_range}{The n-gram range for the CountVectorizer. Default `NULL`}

\item{vectorizer_model}{Pass in your own CountVectorizer from scikit-learn.  Default `NULL`}

\item{ctfidf_model}{Pass in your own c-TF-IDF model to update the representations.  Default `NULL`}

\item{representation_model}{Pass in a model that fine-tunes the topic representations calculated through c-TF-IDF. Models from bertopic.representation are supported. Default `NULL`}

\item{language}{The main language used in your documents. The default sentence-transformers model for "english" is all-MiniLM-L6-v2. For a full overview of supported languages see bertopic.backend.languages. Select "multilingual" to load in the paraphrase-multilingual-MiniLM-L12-v2 sentence-tranformers model that supports 50+ languages. Default `english`}

\item{use_key_phrase_vectorizer}{if `TRUE` uses a keyphrase vectorizer}

\item{use_sklearn_vectorizer}{If `TRUE` uses SKLearn vectorizer}

\item{is_lower_case}{if `TRUE` lower case}

\item{keyphrase_ngram_range}{If not `NULL` range for keyphrase}

\item{exclude_stop_words}{if `TRUE` excludes basic stopwords}

\item{stopword_package_sources}{options if not `NULL` `c("snowball", "stopwords-iso", "smart", "nltk")`}

\item{extra_stop_words}{vector of other stopwords}

\item{min_df}{During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `1L`}

\item{pos_pattern}{Position patter for keyphrase.  Defaults to `pos_pattern = "<J.*>*<N.*>+",`}

\item{seed_topic_list}{A list of seed words per topic to converge around.  Default is `NULL`}

\item{workers}{}
}
\description{
Updates the topic representation by recalculating c-TF-IDF with the new parameters as defined in this function.
}
