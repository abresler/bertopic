# Model I/O, stopwords, and Python setup functions

# load_save ---------------------------------------------------------------

#' Load BERTopic Model
#'
#' @param model_path Character. File path where the BERTopic model is stored.
#' @param obj BERTopic module object. If `NULL`, imports BERTopic automatically.
#' @param embedding_model Character or callable. Embedding model to use. If `NULL`, uses model's default.
#' @param numba_threads Integer. Number of Numba threads to use. Default `1`.
#' @param use_token_parallel Logical. Whether to use token-level parallelization. Default `TRUE`.
#'
#' @returns The loaded BERTopic model object.
#' @export
#'

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
#' @param obj BERTopic model object.
#' @param model_path Character. Directory path where the model will be saved.
#' @param file_name Character. Name for the BERTopic model file. Default `"bert_model"`.
#' @param save_embedding_model Logical or character. If `TRUE`, saves the embedding model. If character, points to HuggingFace model. Default `TRUE`.
#' @param serialization Character. Serialization format: `"pickle"`, `"safetensors"`, or `"pytorch"`. Default `"safetensors"`.
#' @param save_ctfidf Logical. Whether to save c-TF-IDF model. Default `TRUE`.
#'
#' @returns Called for side effects; returns invisibly.
#' @export
#'

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
#' @param language Character. Language code. Default `"english"`.
#'
#' @returns A character vector of stopwords in the specified language.
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
#' @param sources Character vector. Stopword sources to include. Options: `"snowball"`, `"stopwords-iso"`, `"smart"`, `"nltk"`.
#'
#' @returns A character vector of unique stopwords combined from specified sources.
#' @export
#'

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
#' @param language Character. Language code. Default `"english"`.
#' @param is_lower_case Logical. If `TRUE`, converts stopwords to lowercase. Default `TRUE`.
#' @param extra_stop_words Character vector. Additional stopwords to include. Default `NULL`.
#' @param stopword_package_sources Character vector. External stopword sources to combine. Default `NULL`.
#'
#' @returns A character vector of unique stopwords.
#' @export
#'

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
#' @param path Character. Path to Python executable. If `NULL`, uses default Python.
#'
#' @returns Called for side effects; returns invisibly.
#' @export
#'

select_correct_python <-
  function(path = "/opt/miniconda3/bin/python3.12") {
    if (length(path) == 0) {
      return(invisible())
    }
    reticulate::use_python(python = path)
  }
