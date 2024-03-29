
# flair -------------------------------------------------------------------

#' Import Flair Module
#'
#' Flair module \url{https://flair.readthedocs.io/en/latest/modules.html}
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_flair <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("flair")
    ! 'obj' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('flair', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Title
#'
#' @return
#' @export
#'
#' @examples
flair_embeddings <-
  function() {
    obj <- import_flair(assign_to_environment = F)
    obj$embeddings
  }

#' Roberta Embeddings
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
roberta_embeddings <-
  function(obj = NULL, is_token_embedding = T) {
  if (length(obj) == 0) {
    obj <- import_flair(assign_to_environment = F)
  }
  e <- obj$embeddings$TransformerDocumentEmbeddings(is_token_embedding = is_token_embedding)
  e
}



# sentence_transformer ----------------------------------------------------


#' Sentence Transformer Module
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_sentence_transformers <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("sentence_transformers")
    ! 'sentence_transformers' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('sentence_transformers', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Sentence Transformer
#'
#' @param obj
#' @param model_name transformer model name default `all-MiniLM-L6-v2` other options
#' \item \href{https://www.sbert.net/docs/pretrained_models.html}{sbert}
#' } and it defaults to `all-MiniLM-L6-v2`
#'
#' @return
#' @export
#'
#' @examples
sentence_transformer <-
  function(obj = NULL, model_name = "all-MiniLM-L6-v2",...) {
    if (length(obj) == 0) {
      obj <- import_sentence_transformers(assign_to_environment = F)
    }
    obj$SentenceTransformer(model_name_or_path = model_name, ...)
  }


# transformers ------------------------------------------------------------

#' Import Hugging Face Transformers
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_transformers <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("transformers")
    ! 'transformers' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('transformers', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Distilbert Transformer
#'
#' @param obj
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
distilbert_embedding_transformer <-
  function(obj = NULL,...) {
    if (length(obj) == 0) {
      obj <- import_transformers(assign_to_environment = F)
    }
    obj$pipelines$pipeline(task = "feature-extraction", model="distilbert-base-cased", ...)
  }


#' Hugging Face Transformers
#'
#' @param obj
#' @param task
#' @param model model name options
#' \itemize{
#' \item \href{https://huggingface.co/models}{hugging face models}
#' }
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
hugging_face_transformers <-
  function(obj = NULL, task = "feature-extraction", model = "distilbert-base-cased" ,...) {
    if (length(obj) == 0) {
      obj <- import_transformers(assign_to_environment = F)
    }
    obj$pipelines$pipeline(task = "feature-extraction", model="distilbert-base-cased", ...)
  }



# base --------------------------------------------------------------------

#' Embed Vector of Words
#'
#' @param obj Topic Model Object
#' @param words Vector of Words
#' @param verbose if `TRUE` verbose
#'
#' @return
#' @export
#'
#' @examples
bert_embed_words <-
  function(obj, words, return_tibble = T,
           verbose = TRUE) {
    dat <- topic_model$embedding_model$embed_words(words = words, verbose = verbose)

    if (!return_tibble) {
      rownames(dat) <- words
      return(dat)
    }
    dat |>
      as_tibble() |>
      janitor::clean_names() |>
      mutate(word = words) |>
      select(word, everything())
}

#' Embed Documents
#'
#' @param obj
#' @param docs
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
bert_embed_documents <-
  function(obj, docs, return_tibble = TRUE, verbose = TRUE) {
    dat <- topic_model$embedding_model$embed_documents(document = docs, verbose = verbose)

    if (!return_tibble) {
      rownames(dat) <- docs
      return(dat)
    }

    dat |>
      as_tibble() |>
      janitor::clean_names() |>
      mutate(document = docs) |>
      select(document, everything())
  }
