.import_python_module <-
  function(module = "umap",
           python_path = NULL) {
    select_correct_python(path = python_path)
    obj <- reticulate::import(module)
    obj
  }

#' Import Python Modules
#'
#' @param modules
#' @param assign_to_environment
#' @param python_path
#'
#' @return
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
    if (length(modules) > 1) {
      assign_to_environment <- T
    }

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

#' import NLTK
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_nltk <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("nltk")
    ! 'nltk' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('nltk', obj, envir = .GlobalEnv)
    }
    obj
  }


#' Import NLTK Corpus
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_nltk_corpus <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("nltk.corpus")
    ! 'nltk_corpus' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('nltk_corpus', nltk_cobjorpus, envir = .GlobalEnv)
    }
    obj
  }


# sklearn -----------------------------------------------------------------



#' Import Scikit Learn
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_sklearn <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("sklearn")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('sklearn', obj, envir = .GlobalEnv)
    }
    obj
  }

#' SKLearn Word Vectorizer
#'
#' @param obj
#' @param language
#' @param ngram_range list The lower and upper boundary of the range of n-values for different word n-grams or char n-grams to be extracted. All values of n such such that min_n <= n <= max_n will be used. For example an ngram_range of (1, 1) means only unigrams, (1, 2) means unigrams and bigrams, and (2, 2) means only bigrams. Only applies if analyzer is not callab
#' @param analyzer Remove accents and perform other character normalization during the preprocessing step. ‘ascii’ is a fast method that only works on characters that have a direct ASCII mapping. ‘unicode’ is a slightly slower method that works on any characters. None (default) does nothing.
#' @param strip_accents
#' @param exclude_stop_words if `TRUE` excludes stopwords
#' @param extra_stop_words if `TRUE` other stopwords to exclude

#' @param token_pattern  Regular expression denoting what constitutes a “token”, only used if analyzer == 'word'. The default regexp select tokens of 2 or more alphanumeric characters (punctuation is completely ignored and always treated as a token separator).
#'
#' @param is_lower_case if `TRUE` all to lower case
#' @param max_df During fitting ignore keyphrases that have a document frequency strictly higher than the given threshold. Default `NULL`
#' @param min_df During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `NULL`
#' @param max_features  If not None, build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.
#' @param binary If True, all non zero counts are set to 1. This is useful for discrete probabilistic models that model binary events rather than integer counts.
#' @param obj
#' @param language
#' @param ngram_range
#' @param analyzer
#' @param stopword_package_sources
#' @param strip_accents
#' @param exclude_stop_words
#' @param extra_stop_words
#' @param vocabulary
#'
#' @return
#' @export
#'
#' @examples
#' vectorizer_model <- sklearn_vectorizer(ngram_range = list(1L, 3L))
#' docs <- c('This is the first document.', 'This document is the second document.', 'And this is the third one.', 'Is this the first document?')
#' vectorizer_model$fit_transform(raw_documents = docs)
#'vectorizer_model$get_feature_names_out()
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
    vectorizer_model$max_df <- as.integer(max_df)
    vectorizer_model$min_df <- as.integer(min_df)
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
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_umap <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("umap")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('umap', obj, envir = .GlobalEnv)
    }
    obj
  }



# hdbscan -----------------------------------------------------------------


#' Import hdbscan
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_hdbscan <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("hdbscan")
    ! 'obj' %>% exists() & assign_to_environment
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
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_gensim <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("gensim")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('gensim', obj, envir = .GlobalEnv)
    }
    obj
  }


# spacy -------------------------------------------------------------------

#' Import Spacy
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_spacy <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("spacy")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('spacy', obj, envir = .GlobalEnv)
    }
    obj
  }

# use ---------------------------------------------------------------------



#' Import use
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_use <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("use")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('use', obj, envir = .GlobalEnv)
    }
    obj
  }


#' OpenAI
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_openai <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("openai")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('openai', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Import Llama CPP
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_llama_cpp <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("llama_cpp")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('llama_cpp', obj, envir = .GlobalEnv)
    }
    obj
  }
