.import_python_module <-
  function(module = "umap",
           python_path = NULL) {
    select_correct_python(path = python_path)
    obj <- reticulate::import(module)
    obj
  }

#' Import Python Modules
#'
#' @param modules Character vector of Python module names to import.
#' @param assign_to_environment Logical. If `TRUE`, assigns modules to global environment.
#' @param python_path Character. Path to Python installation directory.
#'
#' @returns The Python module objects, invisibly.
#' @export
#'
#' @examples
#' import_python_modules(modules = c("umap", "hdbscan"))
#' hdbscan
#' umap
import_python_modules <-
  function(modules = NULL,
           python_path = NULL,
           assign_to_environment = FALSE) {
    modules |>
      walk(function(x) {
        obj <- .import_python_module(module = x, python_path = python_path)

        if (assign_to_environment) {
          module_clean <- janitor::make_clean_names(x)
          assign(module_clean, obj, envir = .GlobalEnv)
        }
        obj
      })
  }

# nltk --------------------------------------------------------------------

#' Import NLTK
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python NLTK module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' nltk <- import_nltk(assign_to_environment = FALSE)
#' }
import_nltk <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("nltk")
    if (assign_to_environment) {
      assign('nltk', obj, envir = .GlobalEnv)
    }
    obj
  }


#' Import NLTK Corpus
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python NLTK corpus module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' nltk_corpus <- import_nltk_corpus(assign_to_environment = FALSE)
#' }
import_nltk_corpus <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("nltk.corpus")
    if (assign_to_environment) {
      assign('nltk_corpus', obj, envir = .GlobalEnv)
    }
    obj
  }


# sklearn -----------------------------------------------------------------



#' Import Scikit Learn
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python scikit-learn module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' sklearn <- import_sklearn(assign_to_environment = FALSE)
#' }
import_sklearn <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("sklearn")
    if (assign_to_environment) {
      assign('sklearn', obj, envir = .GlobalEnv)
    }
    obj
  }

#' SKLearn Word Vectorizer
#'
#' @param obj Scikit-learn library object or `NULL`.
#' @param language Character. Language for stopword filtering. Default `"english"`.
#' @param ngram_range List of two integers. The lower and upper boundary of the range of n-values for different word n-grams to be extracted. For example `list(1L, 2L)` means unigrams and bigrams.
#' @param analyzer Character. Whether the feature should be made of word n-grams or character n-grams. Options: `"word"`, `"char"`, or `"char_wb"`. Default `"word"`.
#' @param stopword_package_sources Character vector or `NULL`. Sources for stopword packages.
#' @param strip_accents Character or `NULL`. Remove accents: `"ascii"` or `"unicode"`. Default `NULL`.
#' @param exclude_stop_words Logical. If `TRUE`, excludes stopwords. Default `TRUE`.
#' @param extra_stop_words Character vector or `NULL`. Additional stopwords to exclude.
#' @param token_pattern Character. Regular expression for tokenization. Default `"(?u)\\b\\w\\w+\\b"`.
#' @param vocabulary List or `NULL`. Pre-defined vocabulary mapping.
#' @param is_lower_case Logical. If `TRUE`, converts text to lowercase. Default `TRUE`.
#' @param max_df Numeric. Maximum document frequency threshold. Default `1`.
#' @param min_df Numeric. Minimum document frequency threshold. Default `1`.
#' @param max_features Integer or `NULL`. Maximum number of features to consider.
#' @param binary Logical. If `TRUE`, all non-zero counts are set to 1. Default `FALSE`.
#'
#' @returns A Python CountVectorizer object configured with the specified parameters.
#' @export
#'
#' @examples
#' \dontrun{
#' vectorizer_model <- sklearn_vectorizer(ngram_range = list(1L, 3L))
#' docs <- c('This is the first document.', 'This document is the second document.',
#'           'And this is the third one.', 'Is this the first document?')
#' vectorizer_model$fit_transform(raw_documents = docs)
#' vectorizer_model$get_feature_names_out()
#' }
sklearn_vectorizer <-
  function(obj = NULL,
           language = "english",
           ngram_range = list(1L, 1L),
           analyzer = "word",
           stopword_package_sources = NULL,
           strip_accents = NULL,
           exclude_stop_words = T,
           extra_stop_words = NULL,
           token_pattern = "(?u)\\b\\w\\w+\\b",
           vocabulary = NULL,
           is_lower_case = TRUE,
           max_df = 1,
           min_df = 1,
           max_features = NULL,
           binary = FALSE)  {
    if (length(obj) == 0) {
      obj <- import_sklearn(assign_to_environment = F)
    }

    vectorizer_model <-
      obj$feature_extraction$text$CountVectorizer()


    vectorizer_model$lowercase <- is_lower_case


    if (min_df != 1 | max_df != 1) {
      vectorizer_model$min_df <- as.numeric(min_df)
      vectorizer_model$max_df <- as.numeric(max_df)
    }


    vectorizer_model$binary <- binary
    vectorizer_model$strip_accents <- strip_accents
    vectorizer_model$token_pattern <- token_pattern
    vectorizer_model$analyzer <- analyzer
    vectorizer_model$ngram_range <- reticulate::tuple(ngram_range)
    vectorizer_model$max_features <- max_features
    vectorizer_model$vocabulary <- vocabulary

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


# umap --------------------------------------------------------------------

#' Import UMAP
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python UMAP module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' umap <- import_umap(assign_to_environment = FALSE)
#' }
import_umap <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("umap")
    if (assign_to_environment) {
      assign('umap', obj, envir = .GlobalEnv)
    }
    obj
  }



# hdbscan -----------------------------------------------------------------


#' Import hdbscan
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python hdbscan module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' hdbscan <- import_hdbscan(assign_to_environment = FALSE)
#' }
import_hdbscan <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("hdbscan")
    if (assign_to_environment) {
      assign('hdbscan', obj, envir = .GlobalEnv)
    }
    obj
  }





# gensim ------------------------------------------------------------------

#' Import Gensim Module
#'
#' \itemize{
#' \item \href{https://radimrehurek.com/gensim/}{GENSIM}
#' }
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python Gensim module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' gensim <- import_gensim(assign_to_environment = FALSE)
#' }
import_gensim <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("gensim")
    if (assign_to_environment) {
      assign('gensim', obj, envir = .GlobalEnv)
    }
    obj
  }


# spacy -------------------------------------------------------------------

#' Import Spacy
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python Spacy module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' spacy <- import_spacy(assign_to_environment = FALSE)
#' }
import_spacy <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("spacy")
    if (assign_to_environment) {
      assign('spacy', obj, envir = .GlobalEnv)
    }
    obj
  }

# use ---------------------------------------------------------------------



#' Import use
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python use module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' use <- import_use(assign_to_environment = FALSE)
#' }
import_use <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("use")
    if (assign_to_environment) {
      assign('use', obj, envir = .GlobalEnv)
    }
    obj
  }


#' Import OpenAI
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python OpenAI module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' openai <- import_openai(assign_to_environment = FALSE)
#' }
import_openai <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("openai")
    if (assign_to_environment) {
      assign('openai', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Import Llama CPP
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python llama-cpp module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' llama_cpp <- import_llama_cpp(assign_to_environment = FALSE)
#' }
import_llama_cpp <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("llama_cpp")
    if (assign_to_environment) {
      assign('llama_cpp', obj, envir = .GlobalEnv)
    }
    obj
  }
