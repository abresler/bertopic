# https://maartengr.github.io/PolyFuzz/


#' Import Polyfuzz
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

#' Polyfuzz Model
#'
#' @param obj Polyfuzz Object
#' @param method  the method(s) used for matching. For quick selection of models select one of the following: "EditDistance", "TF-IDF" or "Embeddings". If you want more control over the models above, pass in a model from polyfuzz.models. For examples, see usage below.  Default `TF-iDF`
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
polyfuzz_model <-
  function(obj = NULL, method = "TF-IDF", verbose = TRUE) {
    if (length(obj) == 0) {
      obj <- import_polyfuzz(assign_to_environment = F)
    }
    out <- obj$PolyFuzz(method = method, verbose = verbose)
    out
  }

#' Match Lists of Words
#'
#' @param obj Polyfuzz Object
#' @param from_list The list from which you want mappings. If you want to map items within a list, and not map the items to themselves, you can supply only the from_list and ignore the to_list.


#' @param to_list The list where you want to map t

#' @param top_n_matches The number of matches you want returned. This is currently only implemented for polyfuzz.models.TFIDF and polyfuzz.models.Embeddings as they can computationally handle more comparisons.


#' @param model_id the model id of the model if you have specified multiple models


#' @param exclude_unmatched If `TRUE` excludes unmatched
#'
#' @return
#' @export
#'
#' @examples
polyfuzz_match <-
  function(obj, from_list = NULL, to_list = NULL, top_n_matches = 1, model_id = NULL, exclude_unmatched = FALSE) {

    if (length(from_list) == 0) {
      stop("Enter From List")
    }
    out <- obj$match(from_list = from_list, to_list = to_list, top_n = as.integer(top_n_matches))
    dat <- out$get_matches(model_id = model_id) |> janitor::clean_names()
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
