


# https://maartengr.github.io/PolyFuzz/


# utils -------------------------------------------------------------------


#' Import Polyfuzz
#' Polyfuzz module \url{https://maartengr.github.io/PolyFuzz/}
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
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
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
import_fuzz <-
  function() {
    obj <- import_rapidfuzz(assign_to_environment = F)
    obj$fuzz
  }

#' Import Jellyfish
#'
#' Jelly fish module \url{https://jamesturk.github.io/jellyfish/}
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
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
#' @param obj Polyfuzz Object
#' @param model_path Path to save
#' @param file_name Name of the file.  Defaults to polyfuzz
#'
#' @return
#' @export
#'
#' @examples
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
#' @param obj Polyfuzz Object
#' @param method  the method(s) used for matching. For quick selection of models select one of the following: `EditDistance`, `TF-IDF` or `Embeddings`. If you want more control over the models above, pass in a model from polyfuzz.models. For examples, see usage below.  Default `TF-iDF`
#' @param verbose If `TRUE` prints results
#' @param model_object A pretrained model object
#' @param from_list If not `NULL` vector of from list
#' @param to_list If not `NULL` vector of to list
#'
#' @return
#' @export
#'
#' @examples
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
#' @param obj Polyfuzz Object
#' @param from_list The list from which you want mappings. If you want to map items within a list, and not map the items to themselves, you can supply only the from_list and ignore the to_list.
#' @param to_list The list where you want to map to
#' @param top_n_matches The number of matches you want returned. This is currently only implemented for polyfuzz.models.TFIDF and polyfuzz.models.Embeddings as they can computationally handle more comparisons.
#' @param model_id the model id of the model if you have specified multiple models
#' @param exclude_unmatched If `TRUE` excludes unmatched
#' @param group_model you can choose one of the models in polyfuzz.models to be used as a grouper default `NONE`
#' @param link_min_similarity If not `NULL` the minimum similarity between strings before they are grouped in a single linkage fashion.  Default `.75`
#' @param group_all_strings  if you want to compare a list of strings with itself and then cluster those strings, set this to True. Otherwise, only the strings that were mapped To are clustered.
#'
#' @return
#' @export
#'
#' @examples
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
#' @param n_gram_range The n_gram_range on a character-level default `list(3L, 3L)`
#' @param clean_string  Whether to clean the string such that only alphanumerical characters are kept.  Default `TRUE`
#' @param min_similarity  The minimum similarity between strings, otherwise return 0 similarity.  Default `.75`
#' @param top_n  The number of matches you want returned
#' @param cosine_method The method/package for calculating the cosine similarity. Options: `sparse` `sklearn` and `knn`.  Default is `sparse`
#' @param model_id The name of the particular instance, used when comparing models
#'
#' @return
#' @export
#'
#' @examples
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
#' @param n_jobs
#' @param scorer The scorer function to be used to calculate the edit distance. This function should give back a float between 0 and 1, and work as follows: scorer("string_one", "string_two").  If `NULL` uses jellyfish `jaro_winkler_similarity`
#' @param model_id The name of the particular instance, used when comparing models
#'
#' @return
#' @export
#'
#' @examples
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
#' @param n_jobs Nr of parallel processes, use -1 to use all cores.  Default `1L`
#' @param score_cutoff  The minimum similarity for which to return a good match. Should be between 0 and 1.  Default .5
#' @param scorer The scorer function to be used to calculate the edit distance Options: `fuzz.ratio` `fuzz.partial_ratio`  `fuzz.token_sort_ratio` `fuzz.partial_token_sort_ratio` `fuzz.token_set_ratio ` `fuzz.partial_token_set_ratio` `fuzz.token_ratio` `fuzz.partial_token_ratio` `fuzz.WRation` `fuzz.QRatio` See https://maxbachmann.github.io/rapidfuzz/usage/fuzz/ for an extensive description of the scoring methods.  If NULL uses WRatio
#' @param model_id The name of the particular instance, used when comparing models
#'
#' @return
#' @export
#'
#' @examples
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
#' @param embedding_method list of Flair embeddings to use Default is NULL
#' @param min_similarity The minimum similarity between strings, otherwise return 0 similarity
#' @param top_n The number of best matches you want returned
#' @param cosine_method The method/package for calculating the cosine similarity. Options: `sparse` `sklearn` and `knn`.  Default is `sparse`
#' @param model_id  model id
#'
#' @return
#' @export
#'
#' @examples
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
#' @param embedding_model  The Gensim model to use, this can be either a string or the model directly model to use, this can be either a string or the model directly
#' @param min_similarity The minimum similarity between strings, otherwise return 0 similarity
#' @param top_n The number of best matches you want returned
#' @param cosine_method The method/package for calculating the cosine similarity. Options: `sparse` `sklearn` and `knn`.  Default is `sparse`
#' @param model_id  model id
#'
#' @return
#' @export
#'
#' @examples
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
#' @param embedding_model
#' @param min_similarity The minimum similarity between strings, otherwise return 0 similarity
#' @param top_n The number of best matches you want returned
#' @param cosine_method The method/package for calculating the cosine similarity. Options: `sparse` `sklearn` and `knn`.  Default is `sparse`
#' @param model_id  model id
#'
#' @return
#' @export
#'
#' @examples
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
#' @param embedding_model
#' @param min_similarity The minimum similarity between strings, otherwise return 0 similarity
#' @param top_n The number of best matches you want returned
#' @param cosine_method The method/package for calculating the cosine similarity. Options: `sparse` `sklearn` and `knn`.  Default is `sparse`
#' @param model_id  model id
#'
#' @return
#' @export
#'
#' @examples
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
#' @param embedding_model Defaut `"https://tfhub.dev/google/universal-sentence-encoder/4"`
#' @param min_similarity The minimum similarity between strings, otherwise return 0 similarity
#' @param top_n The number of best matches you want returned
#' @param cosine_method The method/package for calculating the cosine similarity. Options: `sparse` `sklearn` and `knn`.  Default is `sparse`
#' @param model_id  model id
#'
#' @return
#' @export
#'
#' @examples
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
#' @param obj Polyfuzz Model Object
#' @param from_list Vector of Unseen Words
#'
#' @return
#' @export
#'
#' @examples
polyfuzz_transform <-
  function(obj, from_list) {
    out <- obj$transform(from_list = from_list)
    type_model <- names(out)
    out[[1]] |> as_tibble() |> janitor::clean_names() |>
      mutate(type_model) |>
      select(type_model, everything())
  }

#' Title
#'
#' @param obj
#' @param from_list
#' @param to_list
#'
#' @return
#' @export
#'
#' @examples
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
#' @param obj polyfuzz objet
#'
#' @return
#' @export
#'
#' @examples
tbl_polyfuzz_matches <-
  function(obj) {
    obj$get_matches() |> janitor::clean_names() |> as_tibble() |> unnest()
  }
