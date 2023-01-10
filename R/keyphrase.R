#' Import Keyphrase
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_keyphrase <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    keyphrase <- reticulate::import("keyphrase_vectorizers")
    ! 'keyphrase' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('keyphrase', keyphrase, envir = .GlobalEnv)
    }
    keyphrase
  }


#' Build a Keyphrase Vectorizer
#'
#' @param obj keyphrase modeule object
#' @param language stop words language; default is `English`
#' @param exclude_stop_words if `TRUE` excludes stopwords
#' @param extra_stop_words if `TRUE` other stopwords to exclude
#' @param is_lower_case  Whether the returned keyphrases should be converted to lowercase.
#' @param spacy_exclude
#' @param max_df During fitting ignore keyphrases that have a document frequency strictly higher than the given threshold. Default `NULL`
#' @param min_df During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `NULL`
#' @param workers How many workers to use for spaCy part-of-speech tagging. If set to -1, use all available worker threads of the machine. SpaCy uses the specified number of cores to tag documents with part-of-speech. Depending on the platform, starting many processes with multiprocessing can add a lot of overhead. In particular, the default start method spawn used in macOS/OS X (as of Python 3.8) and in Windows can be slow. Therefore, carefully consider whether this option is really necessary.
#' @param pos_pattern
#' @param spacy_pipeline A list of spaCy \itemize{
#' \item \href{https://spacy.io/usage/processing-pipelines#built-in}Spacy Pipeline options}
#' } components that should be excluded during the POS-tagging. Removing not needed pipeline components can sometimes make a big difference and improve loading and inference speed.
#' @param custom_pos_tagger  A callable function which expects a list of strings in a ‘raw_documents’ parameter and returns a list of (word token, POS-tag) tuples. If this parameter is not None, the custom tagger function is used to tag words with parts-of-speech, while the spaCy pipeline is ignored.
#' @param binary If True, all non zero counts are set to 1. This is useful for discrete probabilistic models that model binary events rather than integer counts.
#'
#' @return
#' @export
#'
#' @examples
#' vectorizer_model <- keyphrase_vectorizer()
#' docs <- c('This is the first document.', 'This document is the second document.', 'And this is the third one.', 'Is this the first document?')
#' vectorizer_model$fit_transform(raw_documents = docs)
#' vectorizer_model$get_feature_names_out()

keyphrase_vectorizer <-
  function(obj = NULL,
           language = "english",
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
           binary = FALSE
           )  {
  if (length(obj) == 0) {
    obj <- import_keyphrase(assign_to_environment = F)
  }

  vectorizer_model <-
    obj$KeyphraseCountVectorizer()


  vectorizer_model$lowercase <- is_lower_case
  vectorizer_model$max_df <- max_df
  vectorizer_model$min_df <- min_df
  vectorizer_model$workers <- workers
  vectorizer_model$spacy_pipeline <- spacy_pipeline
  vectorizer_model$pos_pattern <- pos_pattern
  vectorizer_model$spacy_exclude <- spacy_exclude
  vectorizer_model$custom_pos_tagger <- custom_pos_tagger
  vectorizer_model$binary <- binary

  if (exclude_stop_words) {
    all_stop <- bert_stopwords(language = language, is_lower_case = is_lower_case, extra_stop_words = extra_stop_words)
    vectorizer_model$stop_words <- all_stop

  }

  vectorizer_model
}

#' Keyphrase TFIDF
#'
#' @param obj
#' @param language
#' @param exclude_stop_words
#' @param extra_stop_words
#' @param is_lower_case
#' @param spacy_exclude
#' @param max_df
#' @param min_df
#' @param workers
#' @param pos_pattern
#' @param spacy_pipeline
#' @param custom_pos_tagger
#' @param norm
#' @param use_idf
#' @param smooth_idf
#' @param sublinear_tf
#' @param binary
#'
#' @return
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
    vectorizer_model$max_df <- max_df
    vectorizer_model$min_df <- min_df
    vectorizer_model$workers <- workers
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
      all_stop <- bert_stopwords(language = language, is_lower_case = is_lower_case, extra_stop_words = extra_stop_words)
      vectorizer_model$stop_words <- all_stop

    }

    vectorizer_model
  }

#' Keyphrase TFIDF to Tibble
#'
#' @param tfidf
#' @param raw_documents
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
#' docs <- c('This is the first document.', 'This document is the second document.', 'And this is the third one.', 'Is this the first document?')
#' tfidf <- keyphrase_tf_idf()
#' keyphrase_tf_idf_to_tibble(tfidf, docs)

keyphrase_tf_idf_to_tibble <-
  function(tfidf, raw_documents = docs,
         return_wide = T) {
  mat <- tfidf$fit_transform(raw_documents = docs)
  phrases <- tfidf$keyphrases
  mat <- as.matrix(mat)
  data <-
    as_tibble(mat) |> setNames(janitor::make_clean_names(phrases)) |>
    mutate(number_document = 1:n()) |>
    select(number_document, everything())

  if (!return_wide) {
  data <- data |>
      gather(phrase, score, -number_document) |>
      filter(score != 0)
  }

  data

}
