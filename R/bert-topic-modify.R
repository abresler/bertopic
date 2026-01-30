# Topic modification operations

#' Merge Topics
#'
#' @param obj bertopic model object
#' @param docs The documents you used when calling either fit or fit_transform
#' @param topics_to_merge Either a list of topics or a list of list of topics to merge. For example: `list(1,2,3)` will merge topics 1, 2 and 3 `list(list(1,2), list(3,4))` will merge topics 1 and 2, and separately merge topics 3 and 4.
#'
#' @return
#' @export
#'
#' @examples
bert_merge_topics <-
  function(obj, docs,
           topics_to_merge = NULL) {
    if (length(topics_to_merge) == 0) {
      "Enter a list or list of lists of topics to merge" |> message()
      return(obj)
    }
    not_list <- class(topics_to_merge) != "list"
    if (not_list) {
      topics_to_merge <- as.integer(topics_to_merge)
      topics_to_merge <- list(topics_to_merge)
    }
    obj <-
      obj$merge_topics(docs = docs, topics_to_merge = topics_to_merge)
    obj
  }

#' Reduce Bertopic Outliers
#'
#' @param obj
#' @param docs
#' @param topics The topics that correspond to the documents.  IF `NULL` pulls from bertopic object
#' @param images
#' @param strategy  When using HDBSCAN, DBSCAN, or OPTICS, a number of outlier documents might be createdthat do not fall within any of the created topics. These are labeled as -1. This function allows the user to match outlier documents with their nearest topic using one of the following strategies using the `strategy`. parameter: #' \itemize{
#' \item{
#' `probabilities` This uses the soft-clustering as performed by HDBSCAN to find the best matching topic for each outlier document. To use this, make sure to calculate the `probabilities` beforehand by instantiating BERTopic with `calculate_probabilities=True`.
#' }
#' \item{
#' `distributions`  Use the topic distributions, as calculated with `.approximate_distribution` to find the most frequent topic in each outlier document. You can use the `distributions_params` variable to tweak the parameters of`.approximate_distribution`.
#' }
#' \item{
#' `c-tf-idf` Calculate the c-TF-IDF representation for each outlier document and find the best matching c-TF-IDF topic representation using cosine similarity.
#' }
#' \item{
#' `embeddings` Using the embeddings of each outlier documents, find the best matching topic embedding using cosine similarity.
#' }
#' }
#' @param probabilities
#' @param threshold The threshold for assigning topics to outlier documents. This value represents the minimum probability when `strategy="probabilities"`. For all other strategies, it represents the minimum similarity.
#' @param embeddings The pre-computed embeddings to be used when `strategy="embeddings"`. If this is None, then it will compute the embeddings for the outlier documents.
#' @param distributions_params The parameters used in `.approximate_distribution` when using the strategy `"distributions"`.
#' @param document_name if not `NULL` new name for document column
#' @param update_topics If `TRUE` updates topics
#'
#' @return
#' @export
#'
#' @examples
bert_reduce_outliers <-
  function(obj,
           docs,
           document_name = NULL,
           topics = NULL,
           images = NULL,
           strategy = "distributions",
           probabilities = NULL,
           threshold = 0L,
           embeddings = NULL,
           update_topics = TRUE,
           distributions_params = NULL) {
    if (length(docs) == 0) {
      "Enter documents" |> message()
      return(invisible())
    }

    if (length(topics) == 0) {
      topics <- as.integer(obj$topics_)
    }


    out <-
      obj$reduce_outliers(
        documents = docs,
        topics = topics,
        images = images,
        strategy = strategy,
        probabilities = probabilities,
        threshold = threshold,
        embeddings = embeddings,
        distributions_params = distributions_params
      )

    new_topics <- out |> as.numeric()



    dat <- tibble(topic_bert_reduced = new_topics) |>
      mutate(
        number_document = 1:n(),
        strategy
      ) |>
      select(number_document, strategy, everything())

    dat_old <- obj |>
      bert_document_info(docs = docs) |>
      mutate(number_document = 1:n())

    dat <-
      dat |>
      left_join(
        dat_old |> distinct(
          topic_bert_reduced = topic_bert,
          label_bertopic_reduced = label_bertopic
        ),
        by = "topic_bert_reduced"
      ) |>
      left_join(dat_old, by = "number_document") |>
      mutate(
        is_reduced_topic = label_bertopic_reduced != label_bertopic,
        .after = "strategy"
      )

    if (length(document_name) > 0) {
      dat <- dat |>
        rename(UQ(document_name) := document)
    }

    if (update_topics) {
      message("Updating topics inside topic model")
      obj$update_topics(docs = docs, topics = new_topics)
    }

    dat
  }

#' Reduce BERTopics
#'
#' @param obj BERTopic Topic Model
#' @param docs Vector of Documents
#' @param number_topics Number of topics to reduce to
#' @param number_words Top n words per topic to use.  Default is 4
#' @param separator The string with which the words and topic prefix will be separated. Underscores are the default but a nice alternative is ", ".  Default `_`
#' @param word_length The maximum length of each word in the topic label. Some words might be relatively long and setting this value helps to make sure that all labels have relatively similar lengths.
#' @param topic_prefix  Whether to use the topic ID as a prefix. If set to True, the topic ID will be separated using the separator
#' @param append_number_words Append the number of words
#' @param update_bert_labels If `TRUE` updates actual topic labels
#' @param update_topic_model_labels if `TRUE` updates new label into a custom field
#' @param images
#'
#' @return
#' @export
#'
#' @examples
bert_reduce_topics <-
  function(obj,
           docs = NULL,
           number_topics = NULL,
           update_bert_labels = TRUE,
           number_words = 1L,
           separator = "_",
           word_length = NULL,
           update_topic_model_labels = TRUE,
           append_number_words = FALSE,
           images = NULL,
           topic_prefix = FALSE) {
    if (length(number_topics) == 0) {
      "Enter number of reduced topics" |> message()
      return(obj)
    }
    if (length(docs) == 0) {
      "Enter Documents" |> message()
      return(obj)
    }
    obj$reduce_topics(docs = docs, nr_topics = as.integer(number_topics), images = images)

    if (!update_bert_labels) {
      return(obj)
    }
    message("Updating BERTopic Labels")
    obj <- bert_topic_labels(
      obj = obj,
      number_words = number_words,
      separator = separator,
      word_length = word_length,
      update_topic_model_labels = update_topic_model_labels,
      append_number_words = append_number_words,
      topic_prefix = topic_prefix
    )

    obj
  }

#' Updates the topic representation by recalculating c-TF-IDF with the new parameters as defined in this function.
#'
#' @param obj BERTopic Object
#' @param docs The documents you used when calling either fit or fit_transform
#' @param topics  A list of topics where each topic is related to a document in docs. Use this variable to change or map the topics. NOTE: Using a custom list of topic assignments may lead to errors if topic reduction techniques are used afterwards. Make sure that manually assigning topics is the last step in the pipeline.  Default `NULL`
#' @param top_n_words The number of words per topic to extract. Setting this too high can negatively impact topic embeddings as topics are typically best represented by at most 10 words.  Default `10`
#' @param n_gram_range The n-gram range for the CountVectorizer. Default `NULL`
#' @param vectorizer_model Pass in your own CountVectorizer from scikit-learn.  Default `NULL`
#' @param ctfidf_model Pass in your own c-TF-IDF model to update the representations.  Default `NULL`
#' @param representation_model  Pass in a model that fine-tunes the topic representations calculated through c-TF-IDF. Models from bertopic.representation are supported. Default `NULL`
#' @param language The main language used in your documents. The default sentence-transformers model for "english" is all-MiniLM-L6-v2. For a full overview of supported languages see bertopic.backend.languages. Select "multilingual" to load in the paraphrase-multilingual-MiniLM-L12-v2 sentence-tranformers model that supports 50+ languages. Default `english`
#' @param use_key_phrase_vectorizer if `TRUE` uses a keyphrase vectorizer
#' @param use_sklearn_vectorizer If `TRUE` uses SKLearn vectorizer
#' @param is_lower_case if `TRUE` lower case
#' @param keyphrase_ngram_range If not `NULL` range for keyphrase
#' @param exclude_stop_words if `TRUE` excludes basic stopwords
#' @param stopword_package_sources options if not `NULL` `c("snowball", "stopwords-iso", "smart", "nltk")`
#' @param extra_stop_words vector of other stopwords
#' @param min_df During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `1L`
#' @param pos_pattern Position patter for keyphrase.  Defaults to `pos_pattern = "<J.*>*<N.*>+",`
#' @param seed_topic_list A list of seed words per topic to converge around.  Default is `NULL`
#' @param max_df
#' @param vocabulary
#' @param images
#' @param use_empty_vectorizer
#' @param decay
#' @param delete_min_df
#' @param workers
#'
#' @return
#' @export
#'
#' @examples
bert_update_topics <-
  function(obj,
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
           keyphrase_ngram_range = list(
             1L,
             1L
           ),
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
           vocabulary = NULL) {
    if (length(docs) == 0) {
      "Enter documents to fit" |> message()
      return(obj)
    }
    if (length(n_gram_range) > 0) {
      n_gram_range <- reticulate::tuple(n_gram_range)
    }

    if (use_empty_vectorizer) {
      "Using empty vectorizer" |> message()
      obj$update_topics(
        docs = docs,
        topics = topics,
        top_n_words = as.integer(top_n_words),
        n_gram_range = n_gram_range,
        vectorizer_model = NULL,
        representation_model = representation_model,
        ctfidf_model = ctfidf_model,
        images = images
      )
      return(obj)
    }

    has_override_vectorizer <- length(vectorizer_model) > 0

    if (has_override_vectorizer) {
      override_vectorizer_model <- vectorizer_model
    }

    if (use_key_phrase_vectorizer & !has_override_vectorizer) {
      "Using keyphrase vectorizer" |> message()
      vectorizer_model <-
        keyphrase_vectorizer(
          min_df = min_df,
          max_df = max_df,
          exclude_stop_words = exclude_stop_words,
          language = language,
          pos_pattern = pos_pattern,
          extra_stop_words = extra_stop_words,
          workers = workers,
          decay = decay,
          delete_min_df = delete_min_df
        )
    }

    if (!use_key_phrase_vectorizer &
      length(vectorizer_model) == 0 & !has_override_vectorizer) {
      "Using sklearn vectorizer" |> message()
      use_sklearn_vectorizer <- T
      vectorizer_model <-
        sklearn_vectorizer(
          min_df = min_df,
          max_df = max_df,
          ngram_range = n_gram_range,
          vocabulary = vocabulary,
          language = language,
          exclude_stop_words = exclude_stop_words,
          extra_stop_words = extra_stop_words
        )
    }
    uses_vectorizer <-
      use_sklearn_vectorizer |
        use_key_phrase_vectorizer |
        has_override_vectorizer

    if (has_override_vectorizer) {
      "Using overriden vectorizer" |> message()
      vectorizer_model <- override_vectorizer_model
      vectorizer_model$min_df <- as.integer(min_df)
      vectorizer_model$max_df <- as.integer(max_df)
      vectorizer_model$decay <- decay
    }

    if (uses_vectorizer & exclude_stop_words) {
      "Adding stopwords" |> message()
      stop_words <-
        bert_stopwords(
          language = language,
          is_lower_case = is_lower_case,
          extra_stop_words = extra_stop_words,
          stopword_package_sources = stopword_package_sources
        )
      vectorizer_model$stop_words <-
        c(stop_words) |> unique()
    }

    obj$update_topics(
      docs = docs,
      topics = topics,
      top_n_words = as.integer(top_n_words),
      n_gram_range = n_gram_range,
      vectorizer_model = vectorizer_model,
      representation_model = representation_model,
      ctfidf_model = ctfidf_model,
      images = images
    )

    # obj <-
    #   set_bert_attributes(obj = obj, representation_model = representation_model)

    obj
  }

#' Set Topic Labals
#'
#' @param obj
#' @param topic_labels
#'
#' @return
#' @export
#'
#' @examples
bert_set_topic_labels <-
  function(obj, topic_labels) {
    obj$set_topic_labels(topic_labels = topic_labels)
  }
