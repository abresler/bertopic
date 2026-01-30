# Model I/O, stopwords, and Python setup functions

# load_save ---------------------------------------------------------------

#' Load BERTopic Model
#'
#' @param model_path
#' @param obj
#' @param embedding_model
#' @param numba_threads
#' @param use_token_parallel
#'
#' @return
#' @export
#'
#' @examples
bert_load <-
  function(model_path = NULL,
           obj = NULL,
           numba_threads = 1,
           use_token_parallel = TRUE,
           embedding_model = NULL) {
    if (length(model_path) == 0) {
      stop("Enter Path")
    }
    if (length(obj) == 0) {
      obj <- import_bertopic(
        numba_threads = numba_threads,
        use_token_parallel = use_token_parallel
      )
    }

    oldwd <- getwd()

    setwd("~")

    out <-
      obj$BERTopic$load(path = model_path, embedding_model = embedding_model)

    if (oldwd != getwd()) {
      setwd(oldwd)
    }

    out
  }

#' Save BERTopic Model
#'
#' @param obj BERTopic Object
#' @param model_path Path where object is saved
#' @param file_name name of the BERTOPIC Model
#' @param save_embedding_model if `TRUE` If serialization pickle, then you can choose to skip saving the embedding model. If serialization safetensors or pytorch, this variable can be used as a string pointing towards a huggingface model.
#' @param serialization  If pickle, the entire model will be pickled. If safetensors or pytorch the model will be saved without the embedding, dimensionality reduction, and clustering algorithms. This is a very efficient format and typically advised.  Default `safetensors`
#' @param save_ctfidf
#'
#' @return
#' @export
#'
#' @examples
bert_save <-
  function(obj,
           model_path = NULL,
           file_name = "bert_model",
           serialization = "safetensors",
           save_ctfidf = TRUE,
           save_embedding_model = TRUE) {
    if (length(model_path) == 0) {
      stop("Enter Path")
    }

    oldwd <- getwd()

    setwd("~")

    model_path <- model_path |> str_remove_all("/$")

    bert_model_path <-
      glue::glue("{model_path}/{file_name}")

    obj$save(
      path = bert_model_path,
      save_embedding_model = save_embedding_model,
      serialization = serialization
    )

    if (getwd() != oldwd) {
      setwd(oldwd)
    }

    glue::glue("Saved {file_name} BERTopic Model to {model_path}") |> message()

    return(invisible())
  }


# stopwords ---------------------------------------------------------------


#' NLTK Stopwords
#'
#' @param language language defaults to `english`
#'
#' @return
#' @export
#'
#' @examples
#' dictionary_nltk_stopwords()
dictionary_nltk_stopwords <-
  function(language = "english") {
    nltk <- reticulate::import("nltk.corpus")
    sw_eng <-
      nltk$stopwords$words(language)
    sw_eng
  }

#' Stopword Package Sources
#'
#' @param sources
#'
#' @return
#' @export
#'
#' @examples
stopwords_sources <-
  function(sources = c(
             "snowball",
             "stopwords-iso",
             "smart",
             "nltk"
           )) {
    1:length(sources) |>
      map(function(x) {
        out <- stopwords::stopwords(source = sources[x])
        out
      }) |>
      flatten_chr() |>
      unique()
  }

#' Stopword List Generator
#'
#' @param language
#' @param is_lower_case
#' @param extra_stop_words
#' @param stopword_package_sources
#'
#' @return
#' @export
#'
#' @examples
bert_stopwords <-
  function(language = "english",
           is_lower_case = T,
           stopword_package_sources = NULL,
           extra_stop_words = NULL) {
    sw <-
      dictionary_nltk_stopwords(language = language)
    if (length(extra_stop_words) > 0) {
      extra_stop_words <-
        case_when(
          is_lower_case ~ str_to_lower(extra_stop_words),
          TRUE ~ extra_stop_words
        )

      sw <- c(sw, extra_stop_words) |> unique()
    }

    if (length(stopword_package_sources) > 0) {
      sw_sources <- stopwords_sources(stopword_package_sources)
      sw <- c(sw, sw_sources) |> unique()
    }

    sw
  }


# python ------------------------------------------------------------------

#' Select Correct Python
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
select_correct_python <-
  function(path = "/opt/miniconda3/bin/python3.12") {
    if (length(path) == 0) {
      return(invisible())
    }
    reticulate::use_python(python = path)
  }
