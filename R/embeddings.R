
# flair -------------------------------------------------------------------

#' Import Flair Module
#'
#' Flair module \url{https://flair.readthedocs.io/en/latest/modules.html}
#'
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python Flair module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' flair <- import_flair(assign_to_environment = FALSE)
#' }
import_flair <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("flair")
    if (assign_to_environment) {
      assign('flair', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Access Flair Embeddings
#'
#' @returns The Flair embeddings module object.
#' @export
#'
#' @examples
#' \dontrun{
#' embeddings <- flair_embeddings()
#' }
flair_embeddings <-
  function() {
    obj <- import_flair(assign_to_environment = F)
    obj$embeddings
  }

#' Roberta Embeddings
#'
#' @param obj Flair module object or `NULL`. If `NULL`, imports the module.
#' @param is_token_embedding Logical. If `TRUE`, uses token-level embeddings. Default `TRUE`.
#'
#' @returns A Python TransformerDocumentEmbeddings object for RoBERTa embeddings.
#' @export
#'
#' @examples
#' \dontrun{
#' roberta <- roberta_embeddings()
#' }
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
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python sentence-transformers module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' st <- import_sentence_transformers(assign_to_environment = FALSE)
#' }
import_sentence_transformers <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("sentence_transformers")
    if (assign_to_environment) {
      assign('sentence_transformers', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Sentence Transformer
#'
#' @param obj Sentence-transformers module object or `NULL`. If `NULL`, imports the module.
#' @param model_name Character. Transformer model name. Default `"all-MiniLM-L6-v2"`. See \href{https://www.sbert.net/docs/pretrained_models.html}{sbert documentation} for other options.
#' @param ... Additional arguments passed to `SentenceTransformer()`.
#'
#' @returns A Python SentenceTransformer model object for generating embeddings.
#' @export
#'
#' @examples
#' \dontrun{
#' model <- sentence_transformer(model_name = "all-MiniLM-L6-v2")
#' }
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
#' @param assign_to_environment Logical. If `TRUE`, assigns module to global environment.
#' @param path Character. Path to Python installation directory.
#'
#' @returns The Python transformers module object, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' transformers <- import_transformers(assign_to_environment = FALSE)
#' }
import_transformers <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("transformers")
    if (assign_to_environment) {
      assign('transformers', obj, envir = .GlobalEnv)
    }
    obj
  }

#' Distilbert Transformer
#'
#' @param obj Transformers module object or `NULL`. If `NULL`, imports the module.
#' @param ... Additional arguments passed to `pipeline()`.
#'
#' @returns A Python feature-extraction pipeline using DistilBERT model.
#' @export
#'
#' @examples
#' \dontrun{
#' pipeline <- distilbert_embedding_transformer()
#' }
distilbert_embedding_transformer <-
  function(obj = NULL,...) {
    if (length(obj) == 0) {
      obj <- import_transformers(assign_to_environment = F)
    }
    obj$pipelines$pipeline(task = "feature-extraction", model="distilbert-base-cased", ...)
  }


#' Hugging Face Transformers
#'
#' @param obj Transformers module object or `NULL`. If `NULL`, imports the module.
#' @param task Character. The task for the pipeline. Default `"feature-extraction"`.
#' @param model Character. Model name from Hugging Face. Default `"distilbert-base-cased"`. See \href{https://huggingface.co/models}{Hugging Face models} for other options.
#' @param ... Additional arguments passed to `pipeline()`.
#'
#' @returns A Python transformers pipeline object for the specified task and model.
#' @export
#'
#' @examples
#' \dontrun{
#' pipeline <- hugging_face_transformers(task = "feature-extraction", model = "distilbert-base-cased")
#' }
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
#' @param obj BERTopic topic model object.
#' @param words Character vector. Words to embed.
#' @param return_tibble Logical. If `TRUE`, returns a tibble; if `FALSE`, returns a matrix. Default `TRUE`.
#' @param verbose Logical. If `TRUE`, displays verbose output. Default `TRUE`.
#'
#' @returns A tibble (if `return_tibble = TRUE`) or matrix with embeddings for each word. Columns include embedding dimensions and a `word` column.
#' @export
#'
#' @examples
#' \dontrun{
#' embeddings <- bert_embed_words(obj = topic_model, words = c("hello", "world"))
#' }
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
#' @param obj BERTopic topic model object.
#' @param docs Character vector. Documents to embed.
#' @param return_tibble Logical. If `TRUE`, returns a tibble; if `FALSE`, returns a matrix. Default `TRUE`.
#' @param verbose Logical. If `TRUE`, displays verbose output. Default `TRUE`.
#'
#' @returns A tibble (if `return_tibble = TRUE`) or matrix with embeddings for each document. Columns include embedding dimensions and a `document` column.
#' @export
#'
#' @examples
#' \dontrun{
#' embeddings <- bert_embed_documents(obj = topic_model, docs = c("First document.", "Second document."))
#' }
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
