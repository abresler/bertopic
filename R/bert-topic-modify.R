# Topic modification operations

#' Merge Topics
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents used in model fitting.
#' @param topics_to_merge List. Either a list of topic IDs or a list of lists of topic IDs to merge. For example: `list(1,2,3)` merges topics 1, 2, and 3; `list(list(1,2), list(3,4))` merges 1 and 2 separately from 3 and 4.
#'
#' @returns The updated BERTopic model object with merged topics.
#' @export
#'

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

#' Reduce BERTopic Outliers
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents used in model fitting.
#' @param topics Integer vector. Topics corresponding to documents. If `NULL`, pulls from model object.
#' @param images List. Optional images associated with documents. Default `NULL`.
#' @param strategy Character. Strategy for assigning outliers. Options: `"probabilities"` (uses HDBSCAN soft-clustering), `"distributions"` (uses topic distributions), `"c-tf-idf"` (uses c-TF-IDF cosine similarity), `"embeddings"` (uses embedding cosine similarity). Default `"distributions"`.
#' @param probabilities Matrix. Pre-computed probabilities for strategy `"probabilities"`. Default `NULL`.
#' @param threshold Numeric. Minimum probability or similarity threshold for topic assignment. Default `0L`.
#' @param embeddings Matrix. Pre-computed embeddings for strategy `"embeddings"`. If `NULL`, computes them.
#' @param distributions_params List. Parameters for `.approximate_distribution` when using strategy `"distributions"`. Default `NULL`.
#' @param document_name Character. If not `NULL`, new name for document column.
#' @param update_topics Logical. If `TRUE`, updates topics in model. Default `TRUE`.
#'
#' @returns A tibble with columns: `number_document`, `strategy`, `is_reduced_topic`, and topic information before/after reduction.
#' @export
#'

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

#' Reduce BERTopic Topics
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents used in model fitting.
#' @param number_topics Integer. Target number of topics to reduce to.
#' @param update_bert_labels Logical. If `TRUE`, updates actual topic labels. Default `TRUE`.
#' @param number_words Integer. Number of top words per topic for labeling. Default `1L`.
#' @param separator Character. Separator between words and topic prefix. Default `"_"`.
#' @param word_length Integer or `NULL`. Maximum length of each word in topic label. Default `NULL`.
#' @param update_topic_model_labels Logical. If `TRUE`, updates custom label field. Default `TRUE`.
#' @param append_number_words Logical. If `TRUE`, appends number of words to label. Default `FALSE`.
#' @param images List. Optional images associated with documents. Default `NULL`.
#' @param topic_prefix Logical. If `TRUE`, uses topic ID as prefix. Default `FALSE`.
#'
#' @returns The updated BERTopic model object with reduced topics.
#' @export
#'

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

#' Update BERTopic Topic Representations
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents used in model fitting.
#' @param topics Integer vector. Topics mapping for documents. Default `NULL`.
#' @param top_n_words Integer. Number of words per topic to extract. Default `10`.
#' @param n_gram_range List. N-gram range for CountVectorizer as `list(min, max)`. Default `list(1L, 1L)`.
#' @param vectorizer_model Callable. Custom CountVectorizer from scikit-learn. Default `NULL`.
#' @param ctfidf_model Callable. Custom c-TF-IDF model. Default `NULL`.
#' @param representation_model Callable. Model to fine-tune c-TF-IDF representations. Default `NULL`.
#' @param language Character. Language code. Default `"english"`.
#' @param use_key_phrase_vectorizer Logical. If `TRUE`, uses keyphrase vectorizer. Default `FALSE`.
#' @param use_sklearn_vectorizer Logical. If `TRUE`, uses sklearn vectorizer. Default `FALSE`.
#' @param is_lower_case Logical. If `TRUE`, converts to lowercase. Default `TRUE`.
#' @param keyphrase_ngram_range List. N-gram range for keyphrase vectorizer. Default `list(1L, 1L)`.
#' @param exclude_stop_words Logical. If `TRUE`, excludes stopwords. Default `TRUE`.
#' @param stopword_package_sources Character vector. Stopword sources: `"snowball"`, `"stopwords-iso"`, `"smart"`, `"nltk"`. Default `NULL`.
#' @param extra_stop_words Character vector. Additional stopwords. Default `NULL`.
#' @param min_df Integer. Minimum document frequency for keyphrases. Default `1L`.
#' @param pos_pattern Character. POS pattern for keyphrase vectorizer. Default `"<J.*>*<N.*>+"`.
#' @param seed_topic_list List. Seed words per topic. Default `NULL`.
#' @param max_df Integer. Maximum document frequency. Default `1L`.
#' @param vocabulary List. Custom vocabulary. Default `NULL`.
#' @param images List. Optional images. Default `NULL`.
#' @param use_empty_vectorizer Logical. If `TRUE`, uses empty vectorizer. Default `FALSE`.
#' @param decay Numeric or `NULL`. Decay parameter. Default `NULL`.
#' @param delete_min_df Logical or `NULL`. Delete min_df parameter. Default `NULL`.
#' @param workers Integer. Number of workers for processing. Default `6L`.
#'
#' @returns The updated BERTopic model object.
#' @export
#'

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

#' Set Topic Labels
#'
#' @param obj BERTopic model object.
#' @param topic_labels Character vector or list. Custom labels for topics.
#'
#' @returns The updated BERTopic model object with custom labels set.
#' @export
#'

bert_set_topic_labels <-
  function(obj, topic_labels) {
    obj$set_topic_labels(topic_labels = topic_labels)
  }
