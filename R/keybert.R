


# https://github.com/MaartenGr/KeyBERT


# import ------------------------------------------------------------------



#' Import Keybert Module
#'
#' @param assign_to_environment  if `TRUE` assigns to environment
#' @param path python path
#'
#' @return
#' @export
#'
#' @examples
#' library(bertopic)
#' import_keybert()
import_keybert <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("keybert")
    ! 'keybert' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('keybert', obj, envir = .GlobalEnv)
    }
    obj
  }


# model -------------------------------------------------------------------


#' Initiate Keybert model
#'
#' Runs a keybert model
#'
#' @param keybert
#' @param model default is `all-MiniLM-L6-v2` you can pick any model from \itemize{
#' \item \href{https://www.sbert.net/docs/pretrained_models.html}{sbert}
#' }
#'
#' @return
#' @export
#'
#' @examples
#' kb <- keybert_model()
keybert_model <-
  function(keybert = NULL, model = "all-MiniLM-L6-v2") {
    if (length(keybert) == 0) {
      keybert <- import_keybert(assign_to_environment = F)
    }
    kb <- keybert$KeyBERT(model = model)
    kb
  }


#' Keybert Keywords
#'
#' @param docs The document(s) for which to extract keywords/keyphrases
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
#' @param stopword_package_sources if not `NULL` c("snowball","stopword -iso", "smart", "nltk"))
#' @param assign_to_environment if `TRUE` assigns objec to environment
#' @param language Language to use.  Default `english`
#'
#' @return `tibble` of keywords
#' @export
#'
#' @examples
#'  doc <- "Sources tell us that Google is acquiring Kaggle, a platform that hosts data science and machine learning competitions. Details about the transaction remain somewhat vague, but given that Google is hosting its Cloud Next conference in San Francisco this week, the official announcement could come as early as tomorrow. Reached by phone, Kaggle co-founder CEO Anthony Goldbloom declined to deny that the acquisition is happening. Google itself declined 'to comment on rumors'. Kaggle, which has about half a million data scientists on its platform, was founded by Goldbloom  and Ben Hamner in 2010. The service got an early start and even though it has a few competitors like DrivenData, TopCoder and HackerRank, it has managed to stay well ahead of them by focusing on its specific niche. The service is basically the de facto home for running data science and machine learning competitions. With Kaggle, Google is buying one of the largest and most active communities for data scientists - and with that, it will get increased mindshare in this community, too (though it already has plenty of that thanks to Tensorflow and other projects). Kaggle has a bit of a history with Google, too, but that's pretty recent. Earlier this month, Google and Kaggle teamed up to host a $100,000 machine learning competition around classifying YouTube videos. That competition had some deep integrations with the Google Cloud Platform, too. Our understanding is that Google will keep the service running - likely under its current name. While the acquisition is probably more about Kaggle's community than technology, Kaggle did build some interesting tools for hosting its competition and 'kernels', too. On Kaggle, kernels are basically the source code for analyzing data sets and developers can share this code on the platform (the company previously called them 'scripts'). Like similar competition-centric sites, Kaggle also runs a job board, too. It's unclear what Google will do with that part of the service. According to Crunchbase, Kaggle raised $12.5 million (though PitchBook says it's $12.75) since its   launch in 2010. Investors in Kaggle include Index Ventures, SV Angel, Max Levchin, Naval Ravikant, Google chief economist Hal Varian, Khosla Ventures and Yuri Milner."
#'
#' keybert_keywords(docs = doc, top_n_words = 10)
#' keybert_keywords(docs = doc, top_n_words = 10, return_message = T)
#' keybert_keywords(docs = doc, top_n_words = 10, pos_pattern='<N.*>') # only nouns
#' doc_sample <-  "Supervised learning is the machine learning task of learning a function that maps an input to an output based on example input-output pairs. It infers a function from labeled training data consisting of a set of training examples. In supervised learning, each example is a pair consisting of an input object (typically a vector) and a desired output value (also called the supervisory signal). A supervised learning algorithm analyzes the training data and produces an inferred function, which can be used for mapping new examples. An optimal scenario will allow for the algorithm to correctly determine the class labels for unseen instances. This requires the learning algorithm to generalize from the training data to unseen situations in a 'reasonable' way (see inductive bias)."
#'
#' keybert_keywords(docs = doc_sample, top_n_words = 10,  use_key_phrase_vectorizer = F, keyphrase_ngram_range = list(1L, 4L))
#' keybert_keywords(docs = doc_sample, top_n_words = 10,  use_key_phrase_vectorizer = F, keyphrase_ngram_range = list(1L, 4L), use_yake_candidates = T)
#' keybert_keywords(docs = doc_sample, top_n_words = 10,  use_key_phrase_vectorizer = F, keyphrase_ngram_range = list(1L, 4L), seed_keywords = 'information')
#' keybert_keywords(docs = doc_sample, top_n_words = 10,  use_key_phrase_vectorizer = T, keyphrase_ngram_range = list(1L, 4L), seed_keywords = 'information', use_embeddings = T)

keybert_keywords <-
  function(docs = NULL,
           obj = NULL,
           use_future = FALSE,
           return_message = TRUE,
           model = "all-MiniLM-L6-v2",
           stopword_package_sources = NULL,
           extra_stop_words = NULL,
           exclude_stop_words = T,
           assign_to_environment = T,
           keyphrase_ngram_range = list(1L, 1L),
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
           use_yake_candidates = F,
           language = 'english',
           is_lower_case = T,
           min_df = 1L,
           max_df = 1L,
           pos_pattern = "<J.*>*<N.*>+",
           use_maxsum = FALSE,
           vocabulary = NULL,
           highlight = F) {
    if (length(docs) == 0) {
      "Enter documents"
    }
    if (length(obj) == 0) {
      obj <- keybert_model(model = model)
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
#' @param docs
#' @param obj
#' @param model
#' @param stopword_package_sources
#' @param extra_stop_words
#' @param exclude_stop_words
#' @param assign_to_environment
#' @param keyphrase_ngram_range
#' @param use_key_phrase_vectorizer
#' @param candidates
#' @param language
#' @param is_lower_case
#' @param pos_pattern
#' @param vocabulary
#' @param use_yake_candidates
#' @param min_df
#' @param max_df
#'
#' @return
#' @export
#'
#' @examples
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
      "Enter documents"
    }
    if (length(obj) == 0) {
      obj <- keybert_model(model = model)
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
                                  nest_data = F) {
  if (length(document_column) == 0) {
    "Enter Document Column" |> message()
    return(data)
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
    return_message = return_message
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
#'
#' @return `tibble` of keywords
#'
#' @return
#' @export
#'
#' @examples
tbl_keybert_keywords <- function(data,
                                 document_column = NULL,
                                 obj = NULL,
                                 include_both_vectorizers = FALSE,
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
                                 nest_data = F) {
  if (!include_both_vectorizers) {
    data <-
      .tbl_keybert_keywords(
        data = data,
        document_column = document_column,
        obj = obj,
        model = obj,
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
        nest_data = nest_data
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
      nest_data = nest_data
    )

  tbl_sk <-
    .tbl_keybert_keywords(
      data = data,
      document_column = document_column,
      obj = obj,
      model = model,
      exclude_stop_words = exclude_stop_words,
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
      nest_data = nest_data
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
#' @param out
#' @param use_future
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
tbl_keybert_data <-
  function(out,
           use_future = FALSE,
           return_message = FALSE,
           slug = NULL) {
    total_documents <- length(out)
    if (!use_future) {
      dat <-
        1:length(out) |>
        map_dfr(function(x) {
          if (return_message) {
            glue::glue("Document {x} of {total_documents}")
          }
          values <- out[[x]] |> unlist()
          keywords <- values[c(T, F)]
          scores <-  values[c(F, T)] |> as.numeric()
          tibble(
            number_document = x,
            keyword_keybert = keywords,
            score_keybert = scores
          )
        })
    }

    if (use_future) {
      options(future.globals.maxSize = 999999 * 1024 ^ 12)
      cores <- round(parallelly::availableCores() * .8)
      future::plan(future::cluster, workers = cores)
      dat <-
        1:length(out) |>
        furrr::future_map_dfr(function(x) {
          if (return_message) {
            glue::glue("Document {x} of {total_documents}")
          }
          values <- out[[x]] |> unlist()
          keywords <- values[c(T, F)]
          scores <-  values[c(F, T)] |> as.numeric()
          tibble(
            number_document = x,
            keyword_keybert = keywords,
            score_keybert = scores
          )
        })
      closeAllConnections()
      gc()
    }

    dat

  }
