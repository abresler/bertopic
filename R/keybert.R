







# https://github.com/MaartenGr/KeyBERT


# import ------------------------------------------------------------------



#' Import Keybert Module
#'
#' @param assign_to_environment Logical. If `TRUE` assigns to environment.
#' @param path Character. Python path to use for imports.
#'
#' @returns A Python KeyBERT module object.
#' @export
#'
#' @examples
#' library(bertopic)
#' import_keybert()
import_keybert <-
  function(assign_to_environment = T,
           use_token_parallel = TRUE,
           path = NULL) {
    select_correct_python(path = path)
    os <- reticulate::import("os")

    os$environ["TOKENIZERS_PARALLELISM"] = as.character(str_to_lower(use_token_parallel))
    obj <- reticulate::import("keybert")
    if (assign_to_environment) {
      assign('keybert', obj, envir = .GlobalEnv)
    }
    obj
  }


.extract_document_keywords <- function(obj,
                                       doc,
                                       candidates =  NULL,
                                       keyphrase_ngram_range = list(1L, 1L),
                                       stop_words,
                                       min_df,
                                       use_maxsum,
                                       word_embeddings,
                                       doc_embeddings,
                                       seed_keywords,
                                       highlight,
                                       vectorizer_model,
                                       use_mmr,
                                       diversity,
                                       nr_candidates,
                                       top_n_words = 5,
                                       threshold = NULL) {
  obj$extract_keywords(
    docs = doc,
    candidates = candidates,
    keyphrase_ngram_range = reticulate::tuple(keyphrase_ngram_range),
    stop_words = stop_words,
    top_n = as.integer(top_n_words),
    min_df = min_df,
    use_maxsum = use_maxsum,
    word_embeddings = word_embeddings,
    doc_embeddings = doc_embeddings,
    seed_keywords = seed_keywords,
    highlight = highlight,
    vectorizer = vectorizer_model,
    use_mmr = use_mmr,
    diversity = diversity,
    nr_candidates = nr_candidates

  )

}

# model -------------------------------------------------------------------


#' Initiate Keybert model
#'
#' Runs a keybert model.
#'
#' @param keybert Python KeyBERT module. If `NULL`, imports KeyBERT automatically.
#' @param model Character. Sentence transformer model to use. Default is `all-MiniLM-L6-v2`. See \href{https://www.sbert.net/docs/pretrained_models.html}{sbert documentation} for available models.
#'
#' @returns A Python KeyBERT model object.
#' @export
#'
#' @examples
#' kb <- keybert_model()
keybert_model <-
  function(keybert = NULL, model = "all-MiniLM-L6-v2") {
    if (length(keybert) == 0) {
      keybert <- import_keybert(assign_to_environment = F)
    }
    obj <- keybert$KeyBERT(model = model)

    obj

  }


#' Keybert Keywords
#'
#' @param docs The document(s) for which to extract keywords/keyphrases.   This is simply your text! It's the document (or a list of documents) from which you want to extract keywords.
#' @param candidates Candidate keywords/keyphrases to use instead of extracting them from the document(s) NOTE: This is not used if you passed a vectorizer.  If you already have a specific list of words or phrases you think might be keywords, you can give that list to KeyBERT here. KeyBERT will then only choose the best keywords from your provided list that are relevant to your document.
#' @param keyphrase_ngram_range Length, in words, of the extracted keywords/keyphrases. NOTE: This is not used if you passed a vectorizer.  This tells KeyBERT how long the keywords should be in terms of the number of words.
#' @param min_df Minimum document frequency of a word across all documents if keywords for multiple documents need to be extracted. NOTE: This is not used if you passed a vectorizer.  (Mostly relevant when KeyBERT generates candidates itself and you're processing multiple documents). "A word must appear in at least this many documents to even be considered as a candidate keyword." If you're processing just one document, this usually doesn't have much effect unless set higher than 1 (which would mean no words are considered).
#' @param top_n_words Number of top words to return.  This is simply "how many of the best keywords do you want me to give you?" If you set it to 5, you'll get the top 5 most relevant keywords. If 10, you get the top 10.

#' @param use_maxsum if `TRUE` Calculate Max Sum Distance for extraction of keywords We take the 2 x top_n most similar words/phrases to the document. Then, we take all top_n combinations from the 2 x top_n words and extract the combination that are the least similar to each other by cosine similarity. This is O(n^2) and therefore not advised if you use a large top_n.  If set to True, KeyBERT tries to pick keywords that are not too similar to each other. It aims for a diverse set of keywords that cover different aspects of your document, rather than picking several keywords that mean almost the same thing.
#' @param use_mmr if `TRUE` Calculate Maximal Marginal Relevance (MMR) between candidate keywords and the document. and default is `TRUE`.  Stands for Maximal Marginal Relevance. Similar to use_maxsum, this also tries to give you a diverse set of keywords. It works by picking keywords that are relevant to the document but also different from the keywords already selected.
#' @param diversity How diverse the select keywords/keyphrases are. Values between 0 and 1 with 0 being not diverse at all and 1 being most diverse. Default is `0.5`.  : Only used if use_mmr is True. This number (between 0 and 1) controls how much diversity you want.  A higher number (e.g., 0.7-0.9) means you want keywords that are more different from each other, even if they are slightly less relevant to the overall document.  A lower number (e.g., 0.1-0.3) means you prioritize relevance more, and diversity less.
#' @param nr_candidates The number of candidates to consider default `20L`.  When use_maxsum or use_mmr is active (for diversity), KeyBERT first considers a larger pool of potential keywords (this many) and then selects the final top_n diverse set from this pool.
#' @param seed_keywords Seed keywords that may guide the extraction of keywords by steering the similarities towards the seeded keywords. Default `NULL`. : If you have some specific words or topics you are particularly interested in, you can list them here. KeyBERT will then try to find keywords in your document that are semantically related or similar in meaning to these "seed" words. It's like giving KeyBERT a hint about what kind of keywords to look for.
#' @param doc_embeddings The embeddings of each document. Default `NULL`.   KeyBERT works by turning your document and candidate keywords into lists of numbers (called "embeddings") to compare them. If you've already done this conversion for your main document(s) using the same method KeyBERT would use, you can provide those numbers here.
#' @param word_embeddings Word embeddings to use if not `NULL`.   Similar to doc_embeddings, but for the candidate keywords. If you have pre-computed the numerical representations (embeddings) for your candidate keywords (either those you provided with candidates or those KeyBERT might generate), you can pass them here.
#' @param obj Keybert Object
#' @param model Use a custom embedding model. The following backends are currently supported: * SentenceTransformers * 🤗 Transformers * Flair * Spacy * Gensim * USE (TF-Hub) You can also pass in a string that points to one of the following sentence-transformers models: * https://www.sbert.net/docs/pretrained_models.html
#' @param exclude_stop_words if `TRUE` excludes stop words
#' @param use_key_phrase_vectorizer if `TRUE` uses kephrase vectorizer
#' @param use_yake_candidates if `TRUE` uses Yake Keyword Extractor.  Default  `FALSE`
#' @param is_lower_case if  `TRUE` lower cases
#' @param extra_stop_words vector of extra stop words
#' @param max_df Maximum number of documents.  Default `1`
#' @param highlight Whether to print the document and highlight its keywords/keyphrases. NOTE: This does not work if multiple documents are passed.
#' @param use_embeddings if `TRUE` uses embeddings
#' @param pos_pattern KeyphraseVectorizers extracts the part-of-speech tags from the documents and then applies a regex pattern to extract keyphrases that fit within that pattern. The default pattern is <J.*>*<N.*>+ which means that it extract keyphrases that have 0 or more adjectives followed by 1 or more nouns.
#' @param vocabulary `SKLearn` vocabulary
#' @param stopword_package_sources if not `NULL` c("snowball","stopword -iso", "smart", "nltk"))
#' @param assign_to_environment if `TRUE` assigns objec to environment
#' @param language Language to use.  Default `english`
#' @param iterate_individually Logical. If `TRUE`, processes documents individually. Default `FALSE`.
#' @param return_message Logical. If `TRUE`, prints processing messages. Default `TRUE`.
#' @param use_future Logical. If `TRUE`, uses parallel processing. Default `TRUE`.
#' @param decay Numeric. Decay parameter for keyphrase vectorizer. Default `NULL`.
#' @param delete_min_df Logical. If `TRUE`, deletes words below minimum document frequency. Default `NULL`.
#' @param workers Integer. Number of parallel workers. Default `1L`.
#' @param spacy_pipeline Character. Spacy pipeline to use for POS tagging. Default `"en_core_web_sm"`.
#' @param custom_pos_tagger Custom POS tagger function. Default `NULL`.
#'
#' @returns A tibble containing extracted keywords with columns: `number_document`, `number_keyword`, `keyword_keybert_*` (varies by vectorizer), and `score_keybert_*`.
#' @export
#'
#' @examples
#' \dontrun{
#' doc <- "Google is acquiring Kaggle, a platform for data science competitions."
#' keybert_keywords(docs = doc, top_n_words = 10, use_key_phrase_vectorizer = TRUE)
#' keybert_keywords(docs = doc, top_n_words = 10, use_key_phrase_vectorizer = FALSE)
#' }

keybert_keywords <-
  function(docs = NULL,
           obj = NULL,
           iterate_individually = FALSE,
           return_message = TRUE,
           model = "all-MiniLM-L6-v2",
           stopword_package_sources = NULL,
           extra_stop_words = NULL,
           exclude_stop_words = T,
           assign_to_environment = F,
           keyphrase_ngram_range = list(1L, 1L),
           use_future = TRUE,
           use_embeddings = F,
           use_key_phrase_vectorizer = T,
           top_n_words = 5L,
           use_mmr = T,
           diversity = .5,
           nr_candidates = 20L,
           seed_keywords = NULL,
           doc_embeddings = NULL,
           word_embeddings = NULL,
           candidates =  NULL,
           threshold =  NULL,
           use_yake_candidates = F,
           language = 'english',
           is_lower_case = T,
           min_df = 1,
           max_df = 1,
           pos_pattern = "<J.*>*<N.*>+",
           use_maxsum = FALSE,
           vocabulary = NULL,
           highlight = F,
           decay = NULL,
           delete_min_df = NULL,
           workers = 1L,
           spacy_pipeline = "en_core_web_sm",
           custom_pos_tagger = NULL,
           chunk_size = NULL) {
    if (length(docs) == 0) {
      stop("Enter documents")
    }
    if (length(obj) == 0) {
      obj <- keybert_model(model = model)
    } else if (!reticulate::py_has_attr(obj, "extract_keywords")) {
      obj <- obj$KeyBERT(model = model)
    }
    if (use_key_phrase_vectorizer) {
      "Using keyphrase vectorizer" |> message()
      use_embeddings <- F
      vectorizer_model <-
        keyphrase_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          exclude_stop_words = exclude_stop_words,
          extra_stop_words = extra_stop_words,
          language = language,
          pos_pattern = pos_pattern,
          workers = workers,
          decay = decay,
          delete_min_df = delete_min_df,
          spacy_pipeline = spacy_pipeline,
          custom_pos_tagger = custom_pos_tagger
        )

      slug <-
        "_keyphrase"
    }

    if (!use_key_phrase_vectorizer) {
      "Using sklearn vectorizer" |> message()
      vectorizer_model <-
        sklearn_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          ngram_range = keyphrase_ngram_range,
          vocabulary = vocabulary,
          language = language,
          exclude_stop_words = exclude_stop_words,
          extra_stop_words = extra_stop_words
        )
      slug <- "_sklearn"
    }

    if (use_yake_candidates) {
      candidates <-
        yake_keyword_extractor(docs = docs) |> pull(keyword_yake) |> unique()
    }
    if (exclude_stop_words) {
      stop_words <-
        bert_stopwords(
          language = language,
          is_lower_case = is_lower_case,
          extra_stop_words = extra_stop_words,
          stopword_package_sources = stopword_package_sources
        )
      vectorizer_model$stop_words <-
        c(stop_words) |> unique()
    }

    doc_length <- length(docs)

    if (use_embeddings) {
      out <- obj$extract_embeddings(
        docs = docs,
        candidates = candidates,
        keyphrase_ngram_range = reticulate::tuple(keyphrase_ngram_range),
        language = language,
        min_df = min_df,
        vectorizer = vectorizer_model
      )

      doc_embeddings <- out[[1]]
      word_embeddings <- out[[2]]
    }


    if (iterate_individually) {
      .extract_document_keywords_safe <-
        purrr::possibly(.extract_document_keywords, list())

      .batch_extract <- function(batch_docs) {
        obj$extract_keywords(
          docs = batch_docs,
          candidates = candidates,
          keyphrase_ngram_range = reticulate::tuple(keyphrase_ngram_range),
          stop_words = stop_words,
          top_n = as.integer(top_n_words),
          min_df = min_df,
          use_maxsum = use_maxsum,
          word_embeddings = word_embeddings,
          doc_embeddings = doc_embeddings,
          seed_keywords = seed_keywords,
          highlight = highlight,
          vectorizer = vectorizer_model,
          use_mmr = use_mmr,
          diversity = diversity,
          nr_candidates = nr_candidates
        )
      }

      if (!is.null(chunk_size) && chunk_size > 1L) {
        # Chunked batch mode: fast batching with per-chunk fallback on error
        chunks <- split(seq_along(docs), ceiling(seq_along(docs) / chunk_size))
        out <- vector("list", length(docs))
        for (chunk_idx in chunks) {
          if (return_message) {
            glue("Processing docs #{min(chunk_idx)}-#{max(chunk_idx)} of {length(docs)}") |> message()
          }
          chunk_result <- tryCatch(
            .batch_extract(docs[chunk_idx]),
            error = function(e) NULL
          )
          if (!is.null(chunk_result)) {
            for (i in seq_along(chunk_idx)) out[[chunk_idx[[i]]]] <- chunk_result[[i]]
          } else {
            if (return_message) glue("Chunk failed, retrying individually") |> message()
            for (i in seq_along(chunk_idx)) {
              out[[chunk_idx[[i]]]] <- .extract_document_keywords_safe(
                obj = obj, doc = docs[[chunk_idx[[i]]]],
                candidates = candidates,
                keyphrase_ngram_range = keyphrase_ngram_range,
                stop_words = stop_words, top_n_words = top_n_words,
                min_df = min_df, use_maxsum = use_maxsum,
                word_embeddings = word_embeddings, doc_embeddings = doc_embeddings,
                seed_keywords = seed_keywords, highlight = highlight,
                vectorizer_model = vectorizer_model, use_mmr = use_mmr,
                diversity = diversity, nr_candidates = nr_candidates,
                threshold = threshold
              )
            }
          }
        }
      } else {
        out <-
          seq_along(docs) |>
          map(function(x) {
            if (return_message) {
              glue("Extracting Keywords from Document # {x}") |> message()
            }
            .extract_document_keywords_safe(
              obj = obj,
              doc = docs[[x]],
              candidates = candidates,
              keyphrase_ngram_range = keyphrase_ngram_range,
              stop_words = stop_words,
              top_n_words = top_n_words,
              min_df = min_df,
              use_maxsum = use_maxsum,
              word_embeddings = word_embeddings,
              doc_embeddings = doc_embeddings,
              seed_keywords = seed_keywords,
              highlight = highlight,
              vectorizer_model = vectorizer_model,
              use_mmr = use_mmr,
              diversity = diversity,
              nr_candidates = nr_candidates,
              threshold = threshold
            )
          })
      }
    }


    if (!iterate_individually) {
      if (!exclude_stop_words) {
        stop_words <- NULL
      }

      out <-
        obj$extract_keywords(
          docs = docs,
          candidates = candidates,
          keyphrase_ngram_range = reticulate::tuple(keyphrase_ngram_range),
          stop_words = stop_words,
          top_n = as.integer(top_n_words),
          min_df = min_df,
          use_maxsum = use_maxsum,
          word_embeddings = word_embeddings,
          doc_embeddings = doc_embeddings,
          seed_keywords = seed_keywords,
          highlight = highlight,
          vectorizer = vectorizer_model,
          use_mmr = use_mmr,
          diversity = diversity,
          nr_candidates = nr_candidates
        )
    }

    if (assign_to_environment) {
      assign('kb_keyword_extractor', obj, envir = .GlobalEnv)
    }



    dat <-
      tbl_keybert_data(out = out,
                       use_future = use_future,
                       return_message = return_message)

    if (doc_length == 1) {
      dat <- dat |>
        rename(number_keyword = number_document) |>
        mutate(number_document = 1) |>
        select(number_document, everything())
    } else {
      dat <- dat |>
        group_by(number_document) |>
        mutate(number_keyword = 1:n(), .after = "number_document") |>
        ungroup()
    }

    if (length(slug) > 0) {
      append_cols <- dat |> select(-number_document) |> names()
      names(dat)[names(dat) %in% append_cols]  <-
        names(dat)[names(dat) %in% append_cols] |> str_c(slug)
    }


    dat

  }

#' Extract Embeddings from Documents
#'
#' @param docs Character vector of documents to process.
#' @param obj Python KeyBERT model object. If `NULL`, creates a new model.
#' @param model Character. Sentence transformer model to use. Default `"all-MiniLM-L6-v2"`.
#' @param stopword_package_sources Character vector. Sources for stopwords package. Default `NULL`.
#' @param extra_stop_words Character vector. Additional stop words to exclude. Default `NULL`.
#' @param exclude_stop_words Logical. If `TRUE`, excludes stop words. Default `TRUE`.
#' @param assign_to_environment Logical. If `TRUE`, assigns model to environment. Default `TRUE`.
#' @param keyphrase_ngram_range List of two integers. N-gram range for keyphrases. Default `list(1L, 1L)`.
#' @param use_key_phrase_vectorizer Logical. If `TRUE`, uses keyphrase vectorizer. Default `TRUE`.
#' @param candidates Character vector. Candidate keywords to use. Default `NULL`.
#' @param language Character. Language for processing. Default `"english"`.
#' @param is_lower_case Logical. If `TRUE`, converts to lowercase. Default `TRUE`.
#' @param pos_pattern Character. Regex pattern for POS tags. Default `"<J.*>*<N.*>+"`.
#' @param vocabulary Character vector. Custom vocabulary for vectorizer. Default `NULL`.
#' @param use_yake_candidates Logical. If `TRUE`, uses YAKE keyword extractor. Default `FALSE`.
#' @param min_df Integer. Minimum document frequency. Default `1L`.
#' @param max_df Integer. Maximum document frequency. Default `1L`.
#'
#' @returns A list containing two elements: document embeddings and word embeddings matrices.
#' @export
#'

keybert_embeddings <-
  function(docs = NULL,
           obj = NULL,
           model = "all-MiniLM-L6-v2",
           stopword_package_sources = NULL,
           extra_stop_words = NULL,
           exclude_stop_words = T,
           assign_to_environment = T,
           keyphrase_ngram_range = list(1L, 1L),
           use_key_phrase_vectorizer = T,
           candidates =  NULL,
           language = 'english',
           is_lower_case = T,
           pos_pattern = "<J.*>*<N.*>+",
           vocabulary = NULL,
           use_yake_candidates = F,
           min_df = 1L,
           max_df = 1L) {
    if (length(docs) == 0) {
      stop("Enter documents")
    }
    if (length(obj) == 0) {
      obj <- keybert_model(model = model)
    } else if (!reticulate::py_has_attr(obj, "extract_keywords")) {
      obj <- obj$KeyBERT(model = model)
    }
    if (use_key_phrase_vectorizer) {
      "Using keyphrase vectorizer" |> message()
      use_embeddings <- F
      vectorizer_model <-
        keyphrase_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          exclude_stop_words = exclude_stop_words,
          extra_stop_words = extra_stop_words,
          language = language,
          pos_pattern = pos_pattern
        )
    }

    if (!use_key_phrase_vectorizer) {
      "Using sklearn vectorizer" |> message()
      vectorizer_model <-
        sklearn_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          ngram_range = keyphrase_ngram_range,
          vocabulary = vocabulary,
          language = language,
          exclude_stop_words = exclude_stop_words,
          extra_stop_words = extra_stop_words,

        )
    }

    if (use_yake_candidates) {
      candidates <-
        yake_keyword_extractor(docs = docs) |> pull(keyword_yake) |> unique()
    }


    if (exclude_stop_words) {
      stop_words <-
        bert_stopwords(
          language = language,
          is_lower_case = is_lower_case,
          extra_stop_words = extra_stop_words,
          stopword_package_sources = stopword_package_sources
        )
      vectorizer_model$stop_words <-
        c(stop_words) |> unique()
    }

    if (!exclude_stop_words) {
      stop_words <- NULL
    }


    out <- obj$extract_embeddings(
      docs = docs,
      candidates = candidates,
      keyphrase_ngram_range = reticulate::tuple(keyphrase_ngram_range),
      stop_words = stop_words,
      min_df = min_df,
      vectorizer = vectorizer_model
    )

    out


  }

# keywords ----------------------------------------------------------------


.tbl_keybert_keywords <- function(data,
                                  document_column = NULL,
                                  obj = NULL,
                                  iterate_individually = FALSE,
                                  model = "all-MiniLM-L6-v2",
                                  exclude_stop_words = T,
                                  use_future = FALSE,
                                  return_message = TRUE,
                                  keyphrase_ngram_range = list(1L, 1L),
                                  use_embeddings = F,
                                  use_key_phrase_vectorizer = T,
                                  top_n_words = 5L,
                                  candidates =  NULL,
                                  use_yake_candidates = F,
                                  stop_words = 'english',
                                  language = "english",
                                  is_lower_case = T,
                                  extra_stop_words = NULL,
                                  min_df = 1L,
                                  max_df = 1L,
                                  pos_pattern = "<J.*>*<N.*>+",
                                  use_maxsum = FALSE,
                                  use_mmr = T,
                                  diversity = .5,
                                  vocabulary = NULL,
                                  nr_candidates = 20L,
                                  seed_keywords = NULL,
                                  doc_embeddings = NULL,
                                  word_embeddings = NULL,
                                  highlight = F,
                                  return_summary = F,
                                  join_to_original_data = F,
                                  decay = NULL,
                                  delete_min_df = NULL,
                                  threshold =  NULL,
                                  workers = 6L,
                                  spacy_pipeline = "en_core_web_sm",
                                  custom_pos_tagger = NULL,
                                  chunk_size = NULL,
                                  nest_data = F) {
  if (length(document_column) == 0) {
    stop("Enter Document Column")
  }

  data <-
    data |>
    mutate(number_document = 1:n())
  data <- data |>
    filter(!is.na(!!sym(document_column)))
  docs <-
    data |>
    pull(document_column)

  df_keybert <- keybert_keywords(
    docs = docs,
    obj = obj,
    model = model,
    iterate_individually = iterate_individually,
    exclude_stop_words = exclude_stop_words,
    keyphrase_ngram_range = keyphrase_ngram_range,
    use_embeddings = use_embeddings,
    use_key_phrase_vectorizer = use_key_phrase_vectorizer,
    top_n_words = top_n_words,
    candidates = candidates,
    use_yake_candidates = use_yake_candidates,
    language = language,
    is_lower_case = is_lower_case,
    extra_stop_words = extra_stop_words,
    min_df = min_df,
    max_df = max_df,
    pos_pattern = pos_pattern,
    use_maxsum = use_maxsum,
    use_mmr = use_mmr,
    diversity = diversity,
    vocabulary = vocabulary,
    nr_candidates = nr_candidates,
    seed_keywords = seed_keywords,
    doc_embeddings = doc_embeddings,
    word_embeddings = word_embeddings,
    highlight = highlight,
    use_future = use_future,
    return_message = return_message,
    decay = decay,
    delete_min_df = delete_min_df,
    spacy_pipeline = spacy_pipeline,
    custom_pos_tagger = custom_pos_tagger,
    threshold = threshold,
    chunk_size = chunk_size
  )

  if (return_summary) {
    if (use_key_phrase_vectorizer) {
      df_keybert <-    df_keybert |>
        group_by(number_document) |>
        summarise(
          score_keybert_mean_keyphrase = mean(score_keybert_keyphrase, na.rm = T),
          keywords_keybert_keyphrase = unique(keyword_keybert_keyphrase) |> str_c(collapse = " | ")
        ) |>
        ungroup() |>
        mutate(
          count_keybert_keywords_keyphrase = keywords_keybert_keyphrase |> str_count("\\|") + 1,
          has_keybert_keywords_keyphrase = TRUE
        )
    }

    if (!use_key_phrase_vectorizer) {
      df_keybert <-
        df_keybert |>
        group_by(number_document) |>
        summarise(
          score_keybert_mean_sklearn = mean(score_keybert_sklearn, na.rm = T),
          keywords_keybert_sklearn = unique(keyword_keybert_sklearn) |> str_c(collapse = " | ")
        ) |>
        ungroup() |>
        mutate(
          count_keybert_keywords_sklearn = keywords_keybert_sklearn |> str_count("\\|") + 1,
          has_keybert_keywords_sklearn = TRUE
        )
    }


    if (join_to_original_data) {
      df_keybert <-
        data |>
        left_join(df_keybert, by = c("number_document"))

      kbl_cols <-
        df_keybert |> select(matches("has_keybert_keywords")) |> names()
      df_keybert <- df_keybert |>
        mutate_at(kbl_cols, list(function(x) {
          x |> coalesce(F)
        }))
    }
    return(df_keybert)
  }

  if (nest_data) {
    df_keybert <- df_keybert |>
      group_by(number_document) |>
      nest() |>
      rename(data_keybert = data) |>
      ungroup()
  }

  df_keybert
}


#' Join Keybert Keywords to data
#'
#' @param data `tibble` of text
#' @param document_column name of `document` column
#' @param candidates Candidate keywords/keyphrases to use instead of extracting them from the document(s) NOTE: This is not used if you passed a vectorizer.
#' @param keyphrase_ngram_range Length, in words, of the extracted keywords/keyphrases. NOTE: This is not used if you passed a vectorizer.
#' @param min_df Minimum document frequency of a word across all documents if keywords for multiple documents need to be extracted. NOTE: This is not used if you passed a vectorizer.
#' @param top_n_words Number of top words to return
#' @param use_maxsum if `TRUE` Calculate Max Sum Distance for extraction of keywords We take the 2 x top_n most similar words/phrases to the document. Then, we take all top_n combinations from the 2 x top_n words and extract the combination that are the least similar to each other by cosine similarity. This is O(n^2) and therefore not advised if you use a large top_n.
#' @param use_mmr if `TRUE` Calculate Maximal Marginal Relevance (MMR) between candidate keywords and the document. and default is `TRUE`
#' @param diversity How diverse the select keywords/keyphrases are. Values between 0 and 1 with 0 being not diverse at all and 1 being most diverse. Default is `0.5`
#' @param nr_candidates The number of candidates to consider default `20L`
#' @param seed_keywords Seed keywords that may guide the extraction of keywords by steering the similarities towards the seeded keywords. Default `NULL`
#' @param doc_embeddings The embeddings of each document. Default `NULL`
#' @param word_embeddings Word embeddings to use if not `NULL`
#' @param obj Keybert Object
#' @param model Use a custom embedding model. The following backends are currently supported: * SentenceTransformers * 🤗 Transformers * Flair * Spacy * Gensim * USE (TF-Hub) You can also pass in a string that points to one of the following sentence-transformers models: * https://www.sbert.net/docs/pretrained_models.html
#' @param exclude_stop_words if `TRUE` excludes stop words
#' @param use_key_phrase_vectorizer if `TRUE` uses kephrase vectorizer
#' @param use_yake_candidates if `TRUE` uses Yake Keyword Extractor.  Default  `FALSE`
#' @param is_lower_case if  `TRUE` lower cases
#' @param extra_stop_words vector of extra stop words
#' @param max_df Maximum number of documents.  Default `1`
#' @param highlight Whether to print the document and highlight its keywords/keyphrases. NOTE: This does not work if multiple documents are passed.
#' @param use_embeddings if `TRUE` uses embeddings
#' @param pos_pattern KeyphraseVectorizers extracts the part-of-speech tags from the documents and then applies a regex pattern to extract keyphrases that fit within that pattern. The default pattern is <J.*>*<N.*>+ which means that it extract keyphrases that have 0 or more adjectives followed by 1 or more nouns.
#' @param vocabulary `SKLearn` vocabulary
#' @param language Language to use.  Default `english`
#' @param stop_words Language for stopwords.  Default is `english`
#' @param return_summary if `TRUE` returns concatanated list of matched keywords
#' @param join_to_original_data if `TRUE` joins to original data
#' @param nest_data if `TRUE` nests data
#' @param include_both_vectorizers if `TRUE` returns keybert and sklearn methods
#' @param use_future if `TRUE` uses parallel processing
#' @param return_message if `TRUE` returns a message
#' @param iterate_individually Logical. If `TRUE`, processes each document individually. Default `FALSE`.
#' @param decay Numeric. Decay factor for keyword scoring. Default `NULL`.
#' @param delete_min_df Logical. If `TRUE`, removes minimum document frequency filter. Default `NULL`.
#' @param workers Integer. Number of parallel workers for processing. Default `6L`.
#' @param spacy_pipeline Character. Spacy pipeline for POS tagging. Default `"en_core_web_sm"`.
#' @param custom_pos_tagger Custom POS tagger function. Default `NULL`.
#'
#' @returns A tibble containing keywords extracted from the documents, with columns: `number_document`, `number_keyword`, keyword and score columns (names vary by vectorizer), and optionally joined to original data if `join_to_original_data = TRUE`.
#' @export
#'

tbl_keybert_keywords <- function(data,
                                 document_column = NULL,
                                 obj = NULL,
                                 iterate_individually = FALSE,
                                 include_both_vectorizers = FALSE,
                                 model = "all-MiniLM-L6-v2",
                                 exclude_stop_words = T,
                                 use_future = FALSE,
                                 return_message = TRUE,
                                 keyphrase_ngram_range = list(1L, 3L),
                                 use_embeddings = F,
                                 use_key_phrase_vectorizer = T,
                                 top_n_words = 5L,
                                 candidates =  NULL,
                                 use_yake_candidates = F,
                                 stop_words = 'english',
                                 language = "english",
                                 is_lower_case = T,
                                 extra_stop_words = NULL,
                                 min_df = 1,
                                 max_df = 1,
                                 pos_pattern = "<J.*>*<N.*>+",
                                 use_maxsum = FALSE,
                                 use_mmr = T,
                                 diversity = .5,
                                 vocabulary = NULL,
                                 nr_candidates = 20L,
                                 threshold =  NULL,
                                 seed_keywords = NULL,
                                 doc_embeddings = NULL,
                                 word_embeddings = NULL,
                                 highlight = F,
                                 return_summary = F,
                                 join_to_original_data = F,
                                 decay = NULL,
                                 delete_min_df = NULL,
                                 workers = 6L,
                                 spacy_pipeline = "en_core_web_sm",
                                 custom_pos_tagger = NULL,
                                 chunk_size = NULL,
                                 nest_data = F) {
  if (!include_both_vectorizers) {
    data <-
      .tbl_keybert_keywords(
        data = data,
        document_column = document_column,
        obj = obj,
        model = model,
        iterate_individually = iterate_individually,
        exclude_stop_words = exclude_stop_words,
        use_future = use_future,
        return_message = return_message,
        keyphrase_ngram_range = keyphrase_ngram_range,
        use_embeddings = use_embeddings,
        use_key_phrase_vectorizer = use_key_phrase_vectorizer,
        top_n_words = top_n_words,
        candidates = candidates,
        use_yake_candidates = use_yake_candidates,
        stop_words = stop_words,
        language = language,
        is_lower_case = is_lower_case,
        extra_stop_words = extra_stop_words,
        min_df = min_df,
        max_df = max_df,
        pos_pattern = pos_pattern,
        use_maxsum = use_maxsum,
        use_mmr = use_mmr,
        diversity = diversity,
        vocabulary = vocabulary,
        nr_candidates = nr_candidates,
        seed_keywords = seed_keywords,
        doc_embeddings = doc_embeddings,
        word_embeddings = word_embeddings,
        highlight = highlight,
        return_summary = return_summary,
        join_to_original_data = join_to_original_data,
        decay = decay,
        delete_min_df = delete_min_df,
        spacy_pipeline = spacy_pipeline,
        custom_pos_tagger = custom_pos_tagger,
        chunk_size = chunk_size,
        nest_data = nest_data,
        threshold = threshold
      )

    return(data)
  }

  tbl_keyphrase <-
    .tbl_keybert_keywords(
      data = data,
      document_column = document_column,
      obj = obj,
      model = model,
      exclude_stop_words = exclude_stop_words,
      use_future = use_future,
      iterate_individually = iterate_individually,
      return_message = return_message,
      keyphrase_ngram_range = keyphrase_ngram_range,
      use_embeddings = use_embeddings,
      use_key_phrase_vectorizer = TRUE,
      top_n_words = top_n_words,
      candidates = candidates,
      use_yake_candidates = use_yake_candidates,
      stop_words = stop_words,
      language = language,
      is_lower_case = is_lower_case,
      extra_stop_words = extra_stop_words,
      min_df = min_df,
      max_df = max_df,
      pos_pattern = pos_pattern,
      use_maxsum = use_maxsum,
      use_mmr = use_mmr,
      diversity = diversity,
      vocabulary = vocabulary,
      nr_candidates = nr_candidates,
      seed_keywords = seed_keywords,
      doc_embeddings = doc_embeddings,
      word_embeddings = word_embeddings,
      highlight = highlight,
      return_summary = return_summary,
      join_to_original_data = join_to_original_data,
      chunk_size = chunk_size,
      nest_data = nest_data,
      threshold = threshold
    )

  tbl_sk <-
    .tbl_keybert_keywords(
      data = data,
      document_column = document_column,
      obj = obj,
      model = model,
      exclude_stop_words = exclude_stop_words,
      iterate_individually = iterate_individually,
      use_future = use_future,
      return_message = return_message,
      keyphrase_ngram_range = keyphrase_ngram_range,
      use_embeddings = use_embeddings,
      use_key_phrase_vectorizer = F,
      top_n_words = top_n_words,
      candidates = candidates,
      use_yake_candidates = use_yake_candidates,
      stop_words = stop_words,
      language = language,
      is_lower_case = is_lower_case,
      extra_stop_words = extra_stop_words,
      min_df = min_df,
      max_df = max_df,
      pos_pattern = pos_pattern,
      use_maxsum = use_maxsum,
      use_mmr = use_mmr,
      diversity = diversity,
      vocabulary = vocabulary,
      nr_candidates = nr_candidates,
      seed_keywords = seed_keywords,
      doc_embeddings = doc_embeddings,
      word_embeddings = word_embeddings,
      highlight = highlight,
      return_summary = return_summary,
      join_to_original_data = join_to_original_data,
      chunk_size = chunk_size,
      nest_data = nest_data,
      threshold = threshold
    )

  join_vars <-
    names(tbl_sk)[names(tbl_sk) %in% names(tbl_keyphrase)]

  data <- tbl_sk |>
    left_join(tbl_keyphrase, by = join_vars)

  log_cols <- data |> select_if(is.logical) |> names()

  data <- data |>
    mutate_at(log_cols, list(function(x) {
      x |> coalesce(FALSE)
    }))

  data

}


# summary -----------------------------------------------------------------


#' Extract Keybert Keywords from Output
#'
#' @param out Python KeyBERT output object containing extracted keywords.
#' @param use_future Logical. If `TRUE`, uses parallel processing. Default `FALSE`.
#' @param return_message Logical. If `TRUE`, prints processing messages. Default `FALSE`.
#'
#' @returns A tibble with columns: `number_document`, `keyword_keybert`, and `score_keybert`.
#' @export
#'

tbl_keybert_data <-
  function(out,
           use_future = FALSE,
           return_message = FALSE,
           slug = NULL) {
    if (return_message) {
      glue::glue("Parsing {length(out)} documents") |> message()
    }
    all_values <- lapply(out, unlist, use.names = FALSE)
    n_kw_per_doc <- lengths(all_values) %/% 2L
    all_flat <- unlist(all_values, use.names = FALSE)
    odd_idx  <- seq(1L, length(all_flat), by = 2L)
    even_idx <- seq(2L, length(all_flat), by = 2L)
    tibble(
      number_document = rep(seq_along(out), n_kw_per_doc),
      keyword_keybert = all_flat[odd_idx],
      score_keybert   = as.numeric(all_flat[even_idx])
    )
  }

#' Gather Keybert Generated Keywords
#'
#' @param data A tibble containing KeyBERT-generated keyword columns.
#' @param id_column Character. Name of ID column. If `NULL`, creates a default ID column.
#' @param only_distinct Logical. If `TRUE`, returns only distinct keywords. Default `FALSE`.
#' @param other_join_columns Character vector. Additional column names to include in output. Default `NULL`.
#'
#' @returns A tibble in long format with columns: ID column, `type_keyword` (vectorizer type), `keyword`, `is_sklearn`, and `is_keyphrase`. If `only_distinct = TRUE`, returns only the distinct keywords.
#' @export
#'

gather_keybert_keywords <-
  function(data,
           id_column = NULL,
           other_join_columns = NULL,
           only_distinct = F) {
    if (length(id_column) == 0) {
      data <- data |>
        mutate(id = 1:n())
      id_column <- "id"
    }
    keyword_cols <- data |> select(matches("^keywords")) |> names()
    data <-
      data |> select(id_column,
                     one_of(other_join_columns),
                     matches(keyword_cols)) |>
      pivot_longer(
        cols = keyword_cols,
        names_to = "type_keyword",
        values_to = "keyword",
        values_drop_na = T
      ) |>
      mutate(
        type_keyword = case_when(
          type_keyword |> str_detect("sklearn") ~ "sklearn",
          type_keyword |> str_detect("keyphrase") ~ "keyphrase",
          TRUE ~ "other"
        )
      ) |>
      separate_rows(keyword, sep = "\\|") |>
      mutate_if(is.character, str_squish)

    if (only_distinct) {
      data <-
        data |>
        distinct(keyword)

      return(data)
    }

    data <- data |>
      mutate(is_sklearn = type_keyword == "sklearn",
             is_keyphrase = type_keyword == "keyphrase")

    data
  }
