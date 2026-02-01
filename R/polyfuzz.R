


# https://maartengr.github.io/PolyFuzz/


# utils -------------------------------------------------------------------


#' Import Polyfuzz
#' Polyfuzz module \url{https://maartengr.github.io/PolyFuzz/}
#'
#' @param assign_to_environment Logical. If `TRUE` assigns the PolyFuzz module to the global environment.
#' @param path Character. Path to Python environment or `NULL` for default.
#'
#' @returns A Python PolyFuzz module object.
#' @export
#'

import_polyfuzz <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("polyfuzz")
    ! 'polyfuzz' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('polyfuzz', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Import RapidFuzz Module
#'
#' RapidFuzz module \url{https://maxbachmann.github.io/RapidFuzz/}
#'
#' @param assign_to_environment Logical. If `TRUE` assigns the RapidFuzz module to the global environment.
#' @param path Character. Path to Python environment or `NULL` for default.
#'
#' @returns A Python RapidFuzz module object.
#' @export
#'

import_rapidfuzz <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("rapidfuzz")
    ! 'fuzz' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('fuzz', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Import RapidFuzz Fuzz Options
#'
#' @returns A Python RapidFuzz fuzz module object for string matching algorithms.
#' @export
#'

import_fuzz <-
  function() {
    obj <- import_rapidfuzz(assign_to_environment = F)
    obj$fuzz
  }

#' Import Jellyfish
#'
#' Jelly fish module \url{https://jamesturk.github.io/jellyfish/}
#'
#' @param assign_to_environment Logical. If `TRUE` assigns the Jellyfish module to the global environment.
#' @param path Character. Path to Python environment or `NULL` for default.
#'
#' @returns A Python Jellyfish module object for string comparison.
#' @export
#'

import_jellyfish <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("jellyfish")
    ! 'jellyfish' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('jellyfish', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Save Polyfuzz Model
#'
#' @param obj A PolyFuzz model object.
#' @param model_path Character. Path to directory where model will be saved.
#' @param file_name Character. Name of the file. Defaults to polyfuzz.
#'
#' @returns Called for side effects; returns invisibly. Saves model to specified path.
#' @export
#'

polyfuzz_save <-
  function(obj,
           model_path = NULL,
           file_name = "polyfuzz") {
    if (length(model_path) == 0) {
      stop("Enter Path")
    }

    oldwd <- getwd()

    setwd("~")

    model_path <- model_path |> str_remove_all("/$")

    bert_model_path <-
      glue::glue("{model_path}/{file_name}")

    obj$save(path = bert_model_path)

    if (getwd() != oldwd) {
      setwd(oldwd)
    }

    glue::glue("Saved {file_name} Polyfuzz Model to {model_path}") |> message()

    return(invisible())

  }

# model -------------------------------------------------------------------



#' Polyfuzz Model
#'
#' @param obj A PolyFuzz module object or `NULL` for default import.
#' @param method Character. The method used for matching. Options: `EditDistance`, `TF-IDF`, or `Embeddings`. Default is `TF-IDF`.
#' @param verbose Logical. If `TRUE` prints results during fitting.
#' @param model_object A pretrained model object from polyfuzz or `NULL` for default.
#' @param from_list Character vector. Words to match from, or `NULL`.
#' @param to_list Character vector. Words to match to, or `NULL`.
#'
#' @returns A Python PolyFuzz model object.
#' @export
#'

polyfuzz_model <-
  function(method = "TF-IDF",
           model_object = NULL,
           obj = NULL,
           verbose = TRUE,
           from_list = NULL,
           to_list = NULL) {
    if (length(obj) == 0) {
      obj <- import_polyfuzz(assign_to_environment = F)
    }
    if (length(model_object) > 0) {
      out <- obj$PolyFuzz(model_object)
    }
    if (length(model_object) == 0) {
      out <- obj$PolyFuzz(method = method, verbose = verbose)
    }

    if (length(from_list) > 0) {
      message("Fitting lists")
      out |> polyfuzz_fit(from_list = from_list, to_list = to_list)
    }

    out
  }

#' Match Lists of Words
#'
#' @param obj A PolyFuzz model object.
#' @param from_list Character vector. The list from which you want mappings. If mapping items within a list, supply only `from_list` and ignore `to_list`.
#' @param to_list Character vector. The list where you want to map to, or `NULL`.
#' @param top_n_matches Integer. The number of matches to return. Currently implemented for TFIDF and Embeddings methods. Default is 1.
#' @param model_id Character. The model ID if multiple models are specified, or `NULL`.
#' @param exclude_unmatched Logical. If `TRUE` excludes unmatched strings.
#' @param group_model A PolyFuzz grouper model or `NULL` for no grouping.
#' @param link_min_similarity Numeric. Minimum similarity for linkage-based grouping. Default is 0.75.
#' @param group_all_strings Logical. If `TRUE` compares a list with itself and clusters strings; otherwise only clusters strings that were mapped to.
#'
#' @returns A tibble containing fuzzy matches with columns: `from`, `to`, `similarity`, and `has_match`.
#' @export
#'

polyfuzz_match <-
  function(obj,
           from_list = NULL,
           to_list = NULL,
           top_n_matches = 1,
           group_model = NULL,
           link_min_similarity = .75,
           group_all_strings = FALSE,
           model_id = NULL,
           exclude_unmatched = FALSE) {
    if (length(from_list) == 0) {
      stop("Enter From List")
    }
    out <-
      obj$match(
        from_list = from_list,
        to_list = to_list,
        top_n = as.integer(top_n_matches)
      )

    if (length(link_min_similarity) > 0) {
      glue::glue("Using group linkage at {link_min_similarity} min similarity") |> message()
      obj$group(
        model = group_model,
        link_min_similarity = link_min_similarity,
        group_all_strings = group_all_strings
      )
    }


    dat <-
      out$get_matches(model_id = model_id) |> janitor::clean_names() |> as_tibble()
    tbl_from <- dat |> distinct(from)
    dat <-
      tbl_from |> left_join(dat |>
                              tidyr::unnest(), by = "from") |>
      mutate(has_match = !is.na(similarity))

    if (exclude_unmatched) {
      dat <- dat |> filter(has_match)
    }

    dat
  }

#' A character based n-gram TF-IDF to approximate edit distance
#' We turn a string into, typically of length 3, n-grams. For example, using 3-grams of the "hotel" we get ['hot', 'ote', 'tel']. These are then used as input for a TfidfVectorizer in order to create a vector for each word. Then, we simply apply cosine similarity through k-NN
#'
#' @param n_gram_range List of integers. The n-gram range on a character-level. Default is `list(3L, 3L)`.
#' @param clean_string Logical. If `TRUE` cleans the string to keep only alphanumeric characters. Default is `TRUE`.
#' @param min_similarity Numeric. Minimum similarity between strings, otherwise return 0. Default is 0.75.
#' @param top_n Integer. The number of matches you want returned.
#' @param cosine_method Character. Method for calculating cosine similarity. Options: `sparse`, `sklearn`, `knn`. Default is `sparse`.
#' @param model_id Character. Name of the particular instance, used when comparing models.
#'
#' @returns A Python PolyFuzz TFIDF model object.
#' @export
#'

polyfuzz_tfidf <-
  function(n_gram_range = list(3L, 3L),
           clean_string = TRUE,
           min_similarity = .75,
           top_n = 1,
           cosine_method = 'sparse',
           model_id = NULL) {
    obj <- import_polyfuzz()
    obj$models$TFIDF(
      n_gram_range = n_gram_range,
      clean_string = clean_string,
      min_similarity = min_similarity,
      top_n = as.integer(top_n),
      cosine_method = cosine_method,
      model_id = model_id
    )
  }

#' Calculate the Edit Distance between lists of strings using any distance/similarity based scorer
#'
#' @param n_jobs Integer. Number of parallel jobs. Default is 1.
#' @param scorer A Python function for calculating edit distance. Should return a float between 0 and 1. If `NULL` uses jellyfish `jaro_winkler_similarity`.
#' @param model_id Character. Name of the particular instance, used when comparing models.
#' @param normalize Logical. Whether to normalize scores. Default is `TRUE`.
#'
#' @returns A Python PolyFuzz EditDistance model object.
#' @export
#'

polyfuzz_edit_distance <- function(n_jobs = 1,
                                   scorer = NULL,
                                   model_id = NULL,
                                   normalize = TRUE) {
  obj <- import_polyfuzz()

  if (length(scorer) == 0) {
    message("Using jaro_winkler_similarity")
    jelly <- import_jellyfish()
    scorer <- jelly$jaro_winkler_similarity
  }
  obj$models$EditDistance(
    n_jobs = as.integer(n_jobs),
    scorer = scorer,
    model_id = model_id,
    normalize = normalize
  )
}

#' Calculate the Edit Distance between lists of strings using RapidFuzz's process function
#'
#' We are using RapidFuzz instead of FuzzyWuzzy since it is much faster and does not require the more restrictive GPL license
#'
#' @param n_jobs Integer. Number of parallel processes; use -1 for all cores. Default is 1.
#' @param score_cutoff Numeric. Minimum similarity for a good match, between 0 and 1. Default is 0.5.
#' @param scorer A Python scorer function. Options: `fuzz.ratio`, `fuzz.partial_ratio`, `fuzz.token_sort_ratio`, `fuzz.partial_token_sort_ratio`, `fuzz.token_set_ratio`, `fuzz.partial_token_set_ratio`, `fuzz.token_ratio`, `fuzz.partial_token_ratio`, `fuzz.WRatio`, `fuzz.QRatio`. If `NULL` uses WRatio.
#' @param model_id Character. Name of the particular instance, used when comparing models.
#'
#' @returns A Python PolyFuzz RapidFuzz model object.
#' @export
#'

polyfuzz_rapidmatcher <-
  function(n_jobs = 1,
           score_cutoff = 0.5,
           scorer = NULL,
           model_id = NULL) {
    obj <- import_polyfuzz()

    if (length(scorer) == 0) {
      rf <- import_rapidfuzz()
      message("Using Rapid Fuzz WRatio")
      scorer <- rf$fuzz$WRatio
    }
    obj$models$RapidFuzz(
      n_jobs = as.integer(n_jobs),
      score_cutoff = score_cutoff,
      scorer = scorer,
      model_id = model_id
    )
  }

#' Embedding Method
#'
#' @param embedding_method A Flair embeddings object or `NULL` for default.
#' @param min_similarity Numeric. Minimum similarity between strings, otherwise return 0. Default is 0.75.
#' @param top_n Integer. The number of best matches to return.
#' @param cosine_method Character. Method for calculating cosine similarity. Options: `sparse`, `sklearn`, `knn`. Default is `sparse`.
#' @param model_id Character. Name of the model instance.
#'
#' @returns A Python PolyFuzz Embeddings model object.
#' @export
#'

polyfuzz_embeddings <-
  function(embedding_method = NULL,
           min_similarity = .75,
           top_n = 1,
           cosine_method = 'sparse',
           model_id = NULL) {
    obj <- import_polyfuzz()


    obj$models$Embeddings(
      embedding_method = embedding_method,
      min_similarity = min_similarity,
      top_n = as.integer(top_n),
      cosine_method = cosine_method,
      model_id = model_id
    )
  }

#' Embed words into vectors and use cosine similarity to find
#' the best matches between two lists of strings
#'
#' @param embedding_model A Sentence Transformer model. Can be either a string or model object.
#' @param min_similarity Numeric. Minimum similarity between strings, otherwise return 0. Default is 0.5.
#' @param top_n Integer. The number of best matches to return.
#' @param cosine_method Character. Method for calculating cosine similarity. Options: `sparse`, `sklearn`, `knn`. Default is `sparse`.
#' @param model_id Character. Name of the model instance.
#'
#' @returns A Python PolyFuzz SentenceEmbeddings model object.
#' @export
#'

polyfuzz_sentence_embeddings <-
  function(embedding_model = NULL,
           min_similarity = .5,
           top_n = 1,
           cosine_method = 'sparse',
           model_id = NULL) {
    obj <- import_polyfuzz()


    obj$models$SentenceEmbeddings(
      embedding_model = embedding_model,
      min_similarity = min_similarity,
      top_n = as.integer(top_n),
      cosine_method = cosine_method,
      model_id = model_id
    )
  }

#' Gensim Embeddings
#'
#' @param embedding_model A Gensim model. Can be either a string or model object.
#' @param min_similarity Numeric. Minimum similarity between strings, otherwise return 0. Default is 0.5.
#' @param top_n Integer. The number of best matches to return.
#' @param cosine_method Character. Method for calculating cosine similarity. Options: `sparse`, `sklearn`, `knn`. Default is `sparse`.
#' @param model_id Character. Name of the model instance.
#'
#' @returns A Python PolyFuzz GensimEmbeddings model object.
#' @export
#'

polyfuzz_gensim_embeddings <-
  function(embedding_model = NULL,
           min_similarity = .5,
           top_n = 1,
           cosine_method = 'sparse',
           model_id = NULL) {
    obj <- import_polyfuzz()


    obj$models$GensimEmbeddings(
      embedding_model = embedding_model,
      min_similarity = min_similarity,
      top_n = as.integer(top_n),
      cosine_method = cosine_method,
      model_id = model_id
    )
  }

#' Spacy Embedder
#'
#' @param embedding_model A Spacy model. Can be either a string or model object.
#' @param min_similarity Numeric. Minimum similarity between strings, otherwise return 0. Default is 0.5.
#' @param top_n Integer. The number of best matches to return.
#' @param cosine_method Character. Method for calculating cosine similarity. Options: `sparse`, `sklearn`, `knn`. Default is `sparse`.
#' @param model_id Character. Name of the model instance.
#'
#' @returns A Python PolyFuzz SpacyEmbeddings model object.
#' @export
#'

polyfuzz_spacy_embeddings <-
  function(embedding_model = NULL,
           min_similarity = .5,
           top_n = 1,
           cosine_method = 'sparse',
           model_id = NULL) {
    obj <- import_polyfuzz()


    obj$models$SpacyEmbeddings(
      embedding_model = embedding_model,
      min_similarity = min_similarity,
      top_n = as.integer(top_n),
      cosine_method = cosine_method,
      model_id = model_id
    )
  }

#' USE model
#'
#' @param embedding_model Character. URL to the Universal Sentence Encoder model. Default is `"https://tfhub.dev/google/universal-sentence-encoder/4"`.
#' @param min_similarity Numeric. Minimum similarity between strings, otherwise return 0. Default is 0.5.
#' @param top_n Integer. The number of best matches to return.
#' @param cosine_method Character. Method for calculating cosine similarity. Options: `sparse`, `sklearn`, `knn`. Default is `sparse`.
#' @param model_id Character. Name of the model instance.
#'
#' @returns A Python PolyFuzz USEEmbeddings model object.
#' @export
#'

polyfuzz_use_embeddings <-
  function(embedding_model = "https://tfhub.dev/google/universal-sentence-encoder/4",
           min_similarity = .5,
           top_n = 1,
           cosine_method = 'sparse',
           model_id = NULL) {
    obj <- import_polyfuzz()


    obj$models$USEEmbeddings(
      embedding_model = embedding_model,
      min_similarity = min_similarity,
      top_n = as.integer(top_n),
      cosine_method = cosine_method,
      model_id = model_id
    )
  }

# fit_transform -----------------------------------------------------------


#' Transform Unseen List
#'
#' @param obj A PolyFuzz model object.
#' @param from_list Character vector of unseen words to transform.
#'
#' @returns A tibble with transformed matches including `from`, `to`, and `similarity` columns, plus `type_model` column.
#' @export
#'

polyfuzz_transform <-
  function(obj, from_list) {
    out <- obj$transform(from_list = from_list)
    type_model <- names(out)
    out[[1]] |> as_tibble() |> janitor::clean_names() |>
      mutate(type_model) |>
      select(type_model, everything())
  }

#' Fit PolyFuzz Model
#'
#' @param obj A PolyFuzz model object.
#' @param from_list Character vector. Words to match from.
#' @param to_list Character vector. Words to match to, or `NULL`.
#'
#' @returns The PolyFuzz model object (invisibly) after fitting to the provided lists.
#' @export
#'

polyfuzz_fit <-
  function(obj,
           from_list = NULL,
           to_list = NULL) {
    if (length(from_list) == 0) {
      message("Requires a from list") |> message()
      return(obj)
    }
    obj$fit(from_list = from_list, to_list = to_list)
  }


#' Polyfuzz matches in a clean tibble
#'
#' @param obj A PolyFuzz model object.
#'
#' @returns A tibble containing matches with columns: `from`, `to`, and `similarity`.
#' @export
#'

tbl_polyfuzz_matches <-
  function(obj) {
    obj$get_matches() |> janitor::clean_names() |> as_tibble() |> unnest()
  }
