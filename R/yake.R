#' Import yake
#'
#' Imports the YAKE (Yet Another Keyword Extractor) Python module via reticulate.
#'
#' @param assign_to_environment Logical. If TRUE, assigns the yake module to the global environment. Default TRUE.
#' @param path Optional path to Python executable. If NULL, uses default Python.
#'
#' @return A Python yake module object.
#' @export
#'
#' @examples
#' \dontrun{
#' yake <- import_yake()
#' }
import_yake <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("yake")
    ! 'yake' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('yake', obj, envir = .GlobalEnv)
    }
    obj
  }

.ke_doc_extractor <-
  function(ke, docs) {
    data <-
      seq_along(docs) |>
      map_dfr(function(x) {
        out <- ke$extract_keywords(docs[[x]]) |> unlist()
        keywords <- out[c(T, F)]
        score <- out[c(F, T)] |> as.numeric()
        tibble(
          number_document = x,
          text = docs[[x]],
          keyword_yake = keywords,
          score_yake = score
        )
      })

    data
  }

#' Yake Keyword Extractor
#'
#' Extract keywords from documents using YAKE (Yet Another Keyword Extractor).
#'
#' @param docs Character vector of documents to extract keywords from. If NULL, returns the KeywordExtractor object.
#' @param obj Optional yake module object. If NULL, imports yake automatically.
#' @param text_column Optional name to rename the text column in output.
#' @param top_features Integer. Number of top keywords to extract per document. Default 10.
#' @param language Character. Language of the documents. Default "english".
#' @param assign_to_environment Logical. If TRUE, assigns the KeywordExtractor to global environment. Default TRUE.
#' @param max_ngram_size Integer. Maximum n-gram size for keywords. Default 2.
#' @param deduplication_thresold Numeric. Threshold for deduplication (0-1). Default 0.9.
#' @param deduplication_algo Character. Deduplication algorithm. Default "seqm".
#' @param return_summary Logical. If TRUE, returns summarized keywords per document. Default TRUE.
#' @param window_size Integer. Window size for keyword extraction. Default 1.
#'
#' @return A tibble with extracted keywords per document, or a KeywordExtractor object if docs is NULL.
#' @export
#'
#' @examples
#' \dontrun{
#' # Extract keywords from documents
#' docs <- c("Machine learning is a subset of artificial intelligence.",
#'           "Natural language processing enables text analysis.")
#' keywords <- yake_keyword_extractor(docs = docs, top_features = 5)
#' }
yake_keyword_extractor <-
  function(docs = NULL,
           obj = NULL,
           text_column = NULL,
           top_features = 10,
           language = "english",
           assign_to_environment = T,
           max_ngram_size = 2,
           deduplication_thresold = 0.9,
           deduplication_algo = 'seqm',
           return_summary = TRUE,
           window_size = 1) {
    if (length(obj) == 0) {
      obj <- import_yake(assign_to_environment = F)
    }
    ke <-
      obj$KeywordExtractor(
        top = as.integer(top_features),
        lan = language,
        n = as.integer(max_ngram_size),
        dedupLim = deduplication_thresold,
        dedupFunc = deduplication_algo,
        windowsSize = as.integer(window_size)
      )

    if (length(docs) == 0) {
      return(ke)
    }
    if (assign_to_environment) {
      assign('ke', ke, envir = .GlobalEnv)
    }
    dat <- .ke_doc_extractor(ke = ke, docs = docs)

    if (return_summary) {
      dat <- dat |>
        group_by(number_document, text) |>
        summarise(keyword_yake = keyword_yake |> str_flatten_comma(last = " and ")) |>
        ungroup()

    }

    if (length(text_column) > 0) {
      dat <- dat |>
        rename(UQ(text_column) := text)
    }

    dat
  }
