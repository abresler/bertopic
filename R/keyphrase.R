#' Import Keyphrase Vectorizers
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns the keyphrase module to the global environment. Default `TRUE`.
#' @param path Character. Optional path to Python executable. If `NULL`, uses default Python.
#'
#' @returns A Python keyphrase_vectorizers module object.
#' @export
#'

import_keyphrase <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("keyphrase_vectorizers")
    if (assign_to_environment) {
      assign('keyphrase', obj, envir = .GlobalEnv)
    }
    obj
  }


#' Build a Keyphrase Vectorizer
#'
#' @param obj Keyphrase module object. If `NULL`, imports automatically.
#' @param language Character. Stopwords language. Default `"english"`.
#' @param exclude_stop_words Logical. If `TRUE`, excludes stopwords. Default `TRUE`.
#' @param extra_stop_words Character vector. Additional stopwords to exclude. Default `NULL`.
#' @param is_lower_case Logical. If `TRUE`, converts keyphrases to lowercase. Default `TRUE`.
#' @param spacy_exclude List. spaCy pipeline components to exclude. Default `list('parser', 'attribute_ruler', 'lemmatizer', 'ner')`.
#' @param max_df Integer or `NULL`. Maximum document frequency threshold. Default `NULL`.
#' @param min_df Integer or `NULL`. Minimum document frequency threshold. Default `NULL`.
#' @param workers Integer. Number of spaCy workers. Default `1`.
#' @param pos_pattern Character. POS pattern for keyphrase extraction. Default `"<J.*>*<N.*>+"`.
#' @param spacy_pipeline Character. spaCy model. Default `"en_core_web_sm"`.
#' @param custom_pos_tagger Callable or `NULL`. Custom POS tagger function. Default `NULL`.
#' @param binary Logical. If `TRUE`, all non-zero counts are set to 1. Default `FALSE`.
#' @param stopword_package_sources Character vector. External stopword sources. Default `NULL`.
#' @param decay Numeric or `NULL`. Decay parameter. Default `NULL`.
#' @param delete_min_df Logical or `NULL`. Delete min_df parameter. Default `NULL`.
#'
#' @returns A Python KeyphraseCountVectorizer object configured with specified parameters.
#' @export
#'
#' @examples
#' vectorizer_model <- keyphrase_vectorizer(pos_pattern = "<J.*>*<N.*>+")
#' docs <- c('This is the first document.', 'This document is the second document.', 'And this is the third one.', 'Is this the first document?')
#' vectorizer_model$fit_transform(raw_documents = docs)
#' vectorizer_model$get_feature_names_out()
#' vectorizer_model$keyphrases

keyphrase_vectorizer <-
  function(obj = NULL,
           language = "english",
           stopword_package_sources = NULL,
           exclude_stop_words = T,
           extra_stop_words = NULL,
           is_lower_case = TRUE,
           spacy_exclude = list('parser', 'attribute_ruler', 'lemmatizer', 'ner'),
           max_df = NULL,
           min_df = NULL,
           workers = 1,
           pos_pattern = "<J.*>*<N.*>+",
           spacy_pipeline = "en_core_web_sm",
           custom_pos_tagger = NULL,
           binary = FALSE,
           decay = NULL,
           delete_min_df = NULL
           )  {
    if (length(obj) == 0) {
      obj <- import_keyphrase(assign_to_environment = F)
    }

    vectorizer_model <-
      obj$KeyphraseCountVectorizer()


    vectorizer_model$lowercase <- is_lower_case
    if (!is.null(max_df)) vectorizer_model$max_df <- as.integer(max_df)
    if (!is.null(min_df)) vectorizer_model$min_df <- as.integer(min_df)
    vectorizer_model$workers <- as.integer(workers)
    vectorizer_model$spacy_pipeline <- spacy_pipeline
    vectorizer_model$pos_pattern <- pos_pattern
    vectorizer_model$spacy_exclude <- spacy_exclude
    vectorizer_model$custom_pos_tagger <- custom_pos_tagger
    vectorizer_model$binary <- binary
    vectorizer_model$decay <- decay
    vectorizer_model$delete_min_df <- delete_min_df

    if (exclude_stop_words | length(extra_stop_words) > 0) {
      all_stop <-
        bert_stopwords(
          language = language,
          is_lower_case = is_lower_case,
          extra_stop_words = extra_stop_words,
          stopword_package_sources = stopword_package_sources
        )
      vectorizer_model$stop_words <- all_stop
    } else {
      vectorizer_model$stop_words <- NULL
    }

    vectorizer_model
  }

#' Keyphrase TF-IDF Vectorizer
#'
#' @param obj Keyphrase module object. If `NULL`, imports automatically.
#' @param language Character. Stopwords language. Default `"english"`.
#' @param exclude_stop_words Logical. If `TRUE`, excludes stopwords. Default `TRUE`.
#' @param extra_stop_words Character vector. Additional stopwords. Default `NULL`.
#' @param is_lower_case Logical. If `TRUE`, converts keyphrases to lowercase. Default `TRUE`.
#' @param spacy_exclude List. spaCy pipeline components to exclude. Default `NULL`.
#' @param max_df Integer or `NULL`. Maximum document frequency. Default `NULL`.
#' @param min_df Integer or `NULL`. Minimum document frequency. Default `NULL`.
#' @param workers Integer. Number of spaCy workers. Default `1L`.
#' @param pos_pattern Character. POS pattern. Default `"<J.*>*<N.*>+"`.
#' @param spacy_pipeline Character. spaCy model. Default `"en_core_web_sm"`.
#' @param custom_pos_tagger Callable or `NULL`. Custom POS tagger. Default `NULL`.
#' @param norm Character. Norm type for TF-IDF. Default `"l2"`.
#' @param use_idf Logical. If `TRUE`, enables inverse document frequency re-weighting. Default `TRUE`.
#' @param smooth_idf Logical. If `TRUE`, adds one to document frequencies. Default `TRUE`.
#' @param sublinear_tf Logical. If `TRUE`, applies sublinear TF scaling. Default `FALSE`.
#' @param binary Logical. If `TRUE`, all non-zero counts are set to 1. Default `FALSE`.
#' @param stopword_package_sources Character vector. External stopword sources. Default `NULL`.
#'
#' @returns A Python KeyphraseTfidfVectorizer object configured with specified parameters.
#' @export
#'
#' @examples
#' docs <- c('This is the first document.', 'This document is the second document.', 'And this is the third one.', 'Is this the first document?')
#' tfidf <- keyphrase_tf_idf()
#' tfidf$fit_transform(raw_documents = docs)
#'
keyphrase_tf_idf <-
  function(obj = NULL,
           language = "english",
           stopword_package_sources = NULL,
           exclude_stop_words = T,
           extra_stop_words = NULL,
           is_lower_case = TRUE,
           spacy_exclude = NULL,
           max_df = NULL,
           min_df = NULL,
           workers = 1L,
           pos_pattern = "<J.*>*<N.*>+",
           spacy_pipeline = "en_core_web_sm",
           custom_pos_tagger = NULL,
           norm = "l2",
           use_idf = TRUE,
           smooth_idf = TRUE,
           sublinear_tf = FALSE,
           binary = FALSE) {
    if (length(obj) == 0) {
      obj <- import_keyphrase(assign_to_environment = F)
    }

    vectorizer_model <-
      obj$KeyphraseTfidfVectorizer()


    vectorizer_model$lowercase <- is_lower_case
    if (!is.null(max_df)) vectorizer_model$max_df <- as.integer(max_df)
    if (!is.null(min_df)) vectorizer_model$min_df <- as.integer(min_df)
    vectorizer_model$workers <- as.integer(workers)
    vectorizer_model$spacy_pipeline <- spacy_pipeline
    vectorizer_model$pos_pattern <- pos_pattern
    vectorizer_model$spacy_exclude <- spacy_exclude
    vectorizer_model$custom_pos_tagger <- custom_pos_tagger
    vectorizer_model$binary <- binary
    vectorizer_model$norm <- norm
    vectorizer_model$use_idf <- use_idf
    vectorizer_model$smooth_idf  <- smooth_idf
    vectorizer_model$sublinear_tf <- sublinear_tf

    if (exclude_stop_words) {
      all_stop <-
        bert_stopwords(
          language = language,
          is_lower_case = is_lower_case,
          extra_stop_words = extra_stop_words,
          stopword_package_sources = stopword_package_sources
        )
      vectorizer_model$stop_words <- all_stop

    }

    vectorizer_model
  }

#' Keyphrase TF-IDF to Tibble
#'
#' @param tfidf Python KeyphraseTfidfVectorizer object.
#' @param docs Character vector of documents to vectorize.
#' @param return_wide Logical. If `TRUE`, returns wide format; if `FALSE`, returns long format. Default `TRUE`.
#'
#' @returns A tibble with columns: `number_document`, `text`, and keyphrase TF-IDF scores (wide or long format).
#' @export
#'
#' @examples
#' docs <- c('This is the first document.', 'This document is the second document.', 'And this is the third one.', 'Is this the first document?')
#' tfidf <- keyphrase_tf_idf()
#' keyphrase_tf_idf_to_tibble(tfidf, docs)

keyphrase_tf_idf_to_tibble <-
  function(tfidf,
           docs,
           return_wide = T) {
    mat <- tfidf$fit_transform(raw_documents = docs)
    phrases <- tfidf$keyphrases
    mat <- as.matrix(mat)
    data <-
      as_tibble(mat) |> setNames(janitor::make_clean_names(phrases)) |>
      mutate(text = docs,
             number_document = 1:n()) |>
      select(number_document, text, everything())

    if (!return_wide) {
      data <- data |>
        tidyr::pivot_longer(
          cols = -c(number_document, text),
          names_to = "phrase",
          values_to = "score"
        ) |>
        filter(score != 0)
    }

    data

  }
