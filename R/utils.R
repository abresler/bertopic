
# stopwords ---------------------------------------------------------------


#' NLTK Stopwords
#'
#' @param language language defaults to `english`
#'
#' @return
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

#' Stopword List Generator
#'
#' @param language
#' @param is_lower_case
#' @param extra_stop_words
#'
#' @return
#' @export
#'
#' @examples
bert_stopwords <-
  function(language = "english",
           is_lower_case = T,
           extra_stop_words = NULL) {
    sw <-
      dictionary_nltk_stopwords(language = language)
    if (length(extra_stop_words) > 0) {
      extra_stop_words <- case_when(
        is_lower_case ~ str_to_lower(extra_stop_words),
        TRUE ~ extra_stop_words
      )

      sw <- c(sw, extra_stop_words) |> unique()
    }

    sw
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
    nltk <- reticulate::import("nltk")
    ! 'nltk' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('nltk', nltk, envir = .GlobalEnv)
    }
    nltk
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
    nltk_corpus <- reticulate::import("nltk.corpus")
    ! 'nltk_corpus' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('nltk_corpus', nltk_corpus, envir = .GlobalEnv)
    }
    nltk_corpus
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
    sklearn <- reticulate::import("sklearn")
    ! 'sklearn' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('sklearn', nltk_corpus, envir = .GlobalEnv)
    }
    sklearn
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
#' @param is_lower_case if `TRUE` all to lower case
#' @param max_df During fitting ignore keyphrases that have a document frequency strictly higher than the given threshold. Default `NULL`
#' @param min_df During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `NULL`
#' @param max_features  If not None, build a vocabulary that only consider the top max_features ordered by term frequency across the corpus.
#' @param binary If True, all non zero counts are set to 1. This is useful for discrete probabilistic models that model binary events rather than integer counts.
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
           strip_accents = NULL,
           exclude_stop_words = T,
           extra_stop_words = NULL,
           token_pattern = "(?u)\\b\\w\\w+\\b",
           vocabulary = NULL,
           is_lower_case = TRUE,
           max_df = 1,
           min_df = 1,
           max_features = NULL,
           binary = FALSE
  )  {
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
      all_stop <- bert_stopwords(language = language, is_lower_case = is_lower_case, extra_stop_words = extra_stop_words)
      vectorizer_model$stop_words <- all_stop

    }

    vectorizer_model
  }



# topics ------------------------------------------------------------------

#' Select Correct Python
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
select_correct_python <-
  function(path = "/usr/local/bin/python3") {
    if (length(path) == 0) {
      return(invisible())
    }
    reticulate::use_python(python = path)
  }

.bert_similar_term_topics <-
  function(topic_model,
           term = NULL,
           top_n_terms = 10L) {
    if (length(term) == 0) {
      "Enter search term" |> message()
      return(invisible())
    }
    similar_topics <-
      topic_model$find_topics(search_term = term, top_n = as.integer(top_n_terms))

    tibble(topic_bert = similar_topics[[1]], score = similar_topics[[2]] |> flatten_dbl()) |>
      mutate(term) |>
      select(term, everything())

  }

#' Simlar Term Embeddings
#'
#' @param topic_model
#' @param terms
#' @param top_n_terms
#' @param nest_data
#'
#' @return
#' @export
#'
#' @examples
bert_similar_terms_topics <-
  function(topic_model,
           terms = NULL,
           top_n_terms = 10L,
           nest_data = F,
           return_message = T) {
    data <-
      terms |>
      map_dfr(function(x) {
        if (return_message) {
          glue::glue("Finding {top_n_terms} similar term topic embeddings for {x}") |> message()
        }

        .bert_similar_term_topics(topic_model = topic_model,
                                  term = x,
                                  top_n_terms = top_n_terms)
      })


    if (nest_data) {
      data |>
        group_by(term) |>
        nest() |>
        ungroup()
    }

    data

  }

#' BERT Topic Model Info
#'
#' @param topic_model
#'
#' @return
#' @export
#'
#' @examples
bert_topic_info <-
  function(topic_model, topic_number = NULL) {
    df <-
      topic_model$get_topic_info(topic = topic_number)
    tbl_topics <-
      df |> janitor::clean_names() |> as_tibble() |>
      rename(topic_bert = topic)
    data <-
      tbl_topics |>
      mutate(name = name |> str_replace("\\_", "\\+")) |>
      separate(name,
               into = c("remove", "name_topic"),
               sep = "\\+") |>
      select(-remove)

    data
  }

#' Generate BERT Topic Labels
#'
#' @param topic_model
#' @param number_words
#' @param separator
#' @param word_length
#'
#' @return
#' @export
#'
#' @examples
bert_topic_labels <-
  function(topic_model,
           number_words = 4L,
           separator = "_",
           word_length =  NULL) {
    topic_labels <- topic_model$generate_topic_labels(
      nr_words = as.integer(number_words),
      separator = separator,
      topic_prefix = F,
      word_length = word_length
    )

    tibble(topic_labels) |>
      mutate(topic_bert = 1:n() - 2) |>
      select(topic_bert, everything())
  }




#' Topic Counts
#'
#' @param topic_model
#' @param topic_number
#'
#' @return
#' @export
#'
#' @examples
bert_topic_count <-
  function(topic_model, topic_number = NULL) {
    topic_model$get_topic_freq(topic = topic_number) |>
      janitor::clean_names() |> as_tibble() |>
      rename(topic_bert = topic)

  }

#' Topic's Representative Documents
#'
#' @param topic_model
#' @param topic_number
#'
#' @return
#' @export
#'
#' @examples
bert_representative_documents <-
  function(topic_model,
           topic_number = NULL,
           include_labels = T,
           label_words = 5L) {
    if (length(topic_number) != 0) {
      rep_docs <-
        topic_model$get_representative_docs(topic = as.integer(topic_number))
      data <-
        tibble(text = rep_docs) |>
        mutate(topic_bert = topic_number) |>
        select(topic_bert, everything())
      return(data)
    }
    rep_docs <-
      topic_model$get_representative_docs(topic = topic_number)
    data <- seq_along(rep_docs) |>
      map_dfr(function(x) {
        topic_no <- names(rep_docs[x]) |> readr::parse_number()
        tibble(text = rep_docs[[x]]) |>
          mutate(topic_bert = topic_no) |>
          select(topic_bert, everything())
      }) |>
      arrange(topic_bert)

    if (include_labels) {
      data <- data |>
        left_join(bert_topic_labels(
          topic_model = topic_model,
          number_words = as.integer(label_words)
        ),
        by = "topic_bert") |>
        select(topic_bert, topic_labels, everything())
    }


    data
  }

#' Returns keywords for the topics
#'
#' @param topic_model
#'
#' @return
#' @export
#'
#' @examples
bert_topics_keywords <-
  function(topic_model)  {
    topics <- topic_model$get_topics()
    seq_along(topics) |>
      map_dfr(function(x) {
        topic_number <- names(topics)[[x]] |> readr::parse_number()
        all_values <- topics[[x]] |> unlist()
        word <- all_values[c(T, F)]
        score <- all_values[c(F, T)] |> readr::parse_number()
        tibble(word, score) |>
          mutate(topic = topic_number,
                 length_ngram = word |> str_count("\\ ")) |>
          select(topic, everything())
      })
  }

#' Extract Berttopics from output
#'
#' @param obj
#' @param docs
#' @param id_columns
#' @param sort_by_topic
#' @param text_column
#' @param include_labels
#' @param label_words
#'
#' @return
#' @export
#'
#' @examples
extract_bert_topics <-
  function(obj,
           docs = NULL,
           id_columns = NULL,
           text_column = NULL,
           topic_model = NULL,
           include_labels = T,
           label_words = 4L,
           arrange_topics = F) {
    topics <-
      obj[[1]]

    tbl_prob <-
      obj[[2]] |> as_tibble() |> janitor::clean_names() |>
      select(v1) |>
      rename(pct_probabilty_topic_bert = v1)

    data <-
      tbl_prob |>
      mutate(topic_bert = topics)

    if (length(docs) == 0) {
      return(data)
    }

    data <- tibble(id = names(docs), text = docs)

    if (length(id_columns) > 0) {
      data <-
        data |>
        separate(id,
                 into = id_columns,
                 sep = "\\|",
                 convert = T)
    }

    if (length(text_column)) {
      data <- data |>
        rename(UQ(text_column) := text)
    }

    data <-
      tbl_prob |>
      mutate(topic_bert = topics) |>
      bind_cols(data) |>
      select(topic_bert, names(data), everything())

    if (include_labels) {
      data <-
        data |>
        left_join(
          bert_topic_labels(
          topic_model = topic_model,
          number_words = as.integer(label_words)
        ),
        by = "topic_bert") |>
        select(topic_bert, topic_labels, everything())
    }

    if (arrange_topics) {
      data <- data |>
        arrange(topic_bert, desc(pct_probabilty_topic_bert))
    }


    data
  }

# text --------------------------------------------------------------------

#' Pull Text and Name It for bertopic
#'
#' @param data
#' @param id_columns
#' @param text_column
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_text_features <-
  function(data,
           id_columns = NULL,
           text_column = NULL) {
    if (length(id_columns) == 0) {
      id <- glue("id {1:nrow(data)}") |> as.character()
      data <-
        data |>
        mutate(id) |>
        select(id, everything())
    }

    if (length(id_columns) > 0) {
      data <-
        tidyr::unite(
          data = data,
          col = "id",
          all_of(id_columns),
          sep = "|",
          remove = F
        ) |>
        select(id, everything())
    }
    ids <- data |> pull(id)
    texts <- data |> pull(text_column)
    names(texts) <- ids
    texts
  }

#' Bertopic Documents
#'
#' @param obj
#' @param docs
#' @param document_name
#'
#' @return
#' @export
#'
#' @examples
bert_topic_documents <-
  function(obj, docs, document_name = NULL) {
    data <- obj$get_document_info(docs = docs) |> as_tibble()
    data <-
      data |> setNames(c("document", "topic_bert", "topic_labels", "top_n_words", "pct_probabilty_topic_bert", "is_representative_document"))

    if (length(document_name) >0) {
      data <- data |>
        rename(UQ(document_name) := document)
    }

    data
  }


# embeddings --------------------------------------------------------------

#' Bert Embeddings
#'
#' @param embeddings
#' @param docs
#' @param id_columns
#' @param text_column
#'
#' @return
#' @export
#'
#' @examples
bert_embeddings <-
  function(embeddings,
           docs = NULL,
           id_columns = NULL,
           text_column = NULL) {
    data <- as_tibble(embeddings) |> janitor::clean_names()

    if (length(docs) == 0) {
      data <- data |>
        mutate(id = 1:n()) |>
        select(id, everything())

      return(data)
    }
    df_features <- tibble(id = names(docs), text = docs)

    if (length(id_columns) > 0) {
      df_features <-
        df_features |>
        separate(id,
                 into = id_columns,
                 sep = "\\|",
                 convert = T)
    }

    if (length(text_column)) {
      df_features <-
        df_features |>
        rename(UQ(text_column) := text)
    }
    data <-
      df_features |>
      bind_cols(data)

    data
  }


#' Extract UMAP from Object
#'
#' @param obj
#' @param data
#'
#' @return
#' @export
#'
#' @examples
extract_bert_umap <-
  function(obj, data = NULL, number_zeros =4) {
    df_umap <-
      obj$umap_model$embedding_ |> as_tibble()
    dims <- 1:ncol(df_umap) |> .pz(number_zeros = number_zeros)

    df_umap <-
      df_umap |> setNames(glue::glue("umap_{dims}"))

    pct_dbscan_prob = obj$hdbscan_model$outlier_scores_ |> as.numeric()

    df_map <- df_umap |>
      mutate(pct_dbscan_prob)

    if (length(data) == 0) {
      return(df_umap)
    }

    if (nrow(data) != nrow(df_umap))
      return(df_umap)

    data |>
      bind_cols(df_umap)

  }


# vector_stuff ------------------------------------------------------------

#' Extract Document Word Coutns
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
extract_document_word_counts <-
  function(obj, filter_zero = T) {
  X <- obj$vectorizer_model$fit_transform(bap_docs) |> as.matrix()

  X <-
    X |> as_tibble() |> setNames(obj$vectorizer_model$get_feature_names()) |> mutate(number_document = 1:n()) |>
    gather(word, count, -number_document, na.rm = T) |>
    arrange(number_document, desc(count))

  if (filter_zero) {
    X <-
      X |>
      filter(count > 0)
  }

  X

}

# vuz ---------------------------------------------------------------------

#' Write a BERT Visualization
#'
#' @param viz
#' @param base_path
#' @param viz_name
#' @param browse_url
#'
#' @return
#' @export
#'
#' @examples
write_bert_viz <-
  function(viz,
           base_path = NULL,
           viz_name = NULL,
           browse_url = T) {

    if (length(base_path) == 0) {
      viz$show()
      return(invisible())
    }
    oldwd <- getwd()
    setwd("~")

    final_path <- stringr::str_c(base_path, viz_name, sep = "/")
    .bf(paths = final_path)
    file_name <- glue:::glue("{final_path}/index.html")

    viz$write_html(file_name)

    if (browse_url) {
      file_name |> browseURL()
    }
    if (getwd() != oldwd) {
      setwd(oldwd)
    }
    return(invisible())
  }

