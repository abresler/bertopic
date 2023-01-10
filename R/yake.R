#' Import yake
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_yake <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    yake <- reticulate::import("yake")
    ! 'yake' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('yake', yake, envir = .GlobalEnv)
    }
    yake
  }

.ke_doc_extractor <-
  function(ke, docs) {
    1:length(docs) |>
      map_dfr(function(x){
        out <- ke$extract_keywords(docs[[x]]) |> unlist()
        keywords <- out[c(T,F)]
        score <- out[c(F,T)] |> as.numeric()
        tibble(number_document = x, keyword_yake = keywords, score_yake = score)
      })
  }

#' Yake Keyword Extractor
#'
#' @param obj
#' @param docs
#' @param top_features
#' @param language
#' @param assign_to_environment
#' @param max_ngram_size
#' @param deduplication_thresold
#' @param deduplication_algo
#' @param window_size
#'
#' @return
#' @export
#'
#' @examples
yake_keyword_extractor <-
  function(docs = NULL,
           obj = NULL,
           top_features = 10,
           language = "english",
           assign_to_environment = T,
           max_ngram_size = 2,
           deduplication_thresold = 0.9,
           deduplication_algo = 'seqm',
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
    .ke_doc_extractor(ke = ke, docs = docs)
  }
