



# tuple -------------------------------------------------------------------

list_to_tuple <-
  function(obj) {
    reticulate::tuple(obj)
  }


# load_save ---------------------------------------------------------------

#' Load BERTopic Model
#'
#' @param model_path
#' @param obj
#' @param embedding_model
#'
#' @return
#' @export
#'
#' @examples
bert_load <-
  function(model_path = NULL, obj = NULL,
           embedding_model = NULL) {
    if (length(model_path) == 0) {
      stop("Enter Path")
    }
    if (length(obj) == 0) {
      obj <- import_bertopic()
    }

    oldwd <- getwd()

    setwd("~")

    out <-
      obj$BERTopic$load(path = model_path, embedding_model = embedding_model)

    if (oldwd != getwd()) {
      setwd(oldwd)
    }

    out
  }

#' Save BERTopic Model
#'
#' @param obj
#' @param model_path
#' @param file_name
#' @param save_embedding_model
#'
#' @return
#' @export
#'
#' @examples
bert_save <-
  function(obj, model_path = NULL, file_name = "bert_model", save_embedding_model = TRUE) {
    if (length(model_path) == 0) {
      stop("Enter Path")
    }

    oldwd <- getwd()

    setwd("~")

    model_path <- model_path |> str_remove_all("/$")

    bert_model_path <-
      glue::glue("{model_path}/{file_name}")

    obj$save(path = bert_model_path, save_embedding_model = save_embedding_model)

    if (getwd() != oldwd) {
      setwd(oldwd)
    }

    glue::glue("Saved {file_name} BERTopic Model to {model_path}") |> message()

    return(invisible())

  }



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

#' Stopword Package Sources
#'
#' @param sources
#'
#' @return
#' @export
#'
#' @examples
stopwords_sources <-
  function(sources = c("snowball",
                       "stopwords-iso",
                       "smart",
                       "nltk")) {
    1:length(sources) |>
      map(function(x) {
        out <- stopwords::stopwords(source = sources[x])
        out
      }) |>
      flatten_chr() |>
      unique()
  }

#' Stopword List Generator
#'
#' @param language
#' @param is_lower_case
#' @param extra_stop_words
#' @param stopword_package_sources
#'
#' @return
#' @export
#'
#' @examples
bert_stopwords <-
  function(language = "english",
           is_lower_case = T,
           stopword_package_sources = NULL,
           extra_stop_words = NULL) {
    sw <-
      dictionary_nltk_stopwords(language = language)
    if (length(extra_stop_words) > 0) {
      extra_stop_words <-
        case_when(is_lower_case ~ str_to_lower(extra_stop_words),
                  TRUE ~ extra_stop_words)

      sw <- c(sw, extra_stop_words) |> unique()
    }

    if (length(stopword_package_sources) > 0) {
      sw_sources <- stopwords_sources(stopword_package_sources)
      sw <- c(sw, sw_sources) |> unique()
    }

    sw
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

    tibble(topic_bert = similar_topics[[1]],
           score_c_tfidf = similar_topics[[2]] |> flatten_dbl()) |>
      mutate(term) |>
      select(term, everything()) |>
      mutate(is_outlier_bert_topic = topic_bert == -1)

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

    data <- data |>
      left_join(
        topic_model |> bert_topic_info() |> rename(count_documents_in_topic = count)
        ,
                by = c("topic_bert", "is_outlier_bert_topic"))


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
      tidyr::separate(name,
                      into = c("remove", "name_topic"),
                      sep = "\\+") |>
      select(-remove) |>
      mutate(is_outlier_bert_topic = topic_bert == -1) |>
      mutate_if(is.character, str_squish) |>
      select(-count, everything())

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
      select(topic_bert, everything()) |>
      mutate(is_outlier_bert_topic = topic_bert == -1) |>
      mutate_if(is.character, stringr::str_squish)
  }


bert_save <-
  function(obj = NULL, file_path = NULL, file_name = "bert_model") {
    if (length(file_path) == 0) {
      stop("Enter File Path")
    }

    oldwd <- getwd()
    has_slash <- file_path |> stringr::str_detect("/$")

    if (has_slash) {
      file_path <- file_path |> stringr::str_remove_all("/$")
    }
    path <- glue::glue("{file_path}/{file_name}")

    obj$save(save_embedding_model = )

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
      rename(topic_bert = topic) |>
      mutate(is_outlier_bert_topic = topic_bert == -1)

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
#' @param bert_topics
#'
#' @return
#' @export
#'
#' @examples
bert_topics_keywords <-
  function(topic_model, bert_topics = NULL)  {
    topics <- topic_model$get_topics()
    data <-
      seq_along(topics) |>
      map_dfr(function(x) {
        topic_number <- names(topics)[[x]] |> readr::parse_number()
        all_values <- topics[[x]] |> unlist()
        word <- all_values[c(T, F)]
        score <- all_values[c(F, T)] |> readr::parse_number()
        tibble(word, score_c_tfidf = score) |>
          mutate(topic_bert = topic_number,
                 length_ngram = word |> str_count("\\ ")) |>
          select(topic_bert, everything()) |>
          mutate(is_outlier_bert_topic = topic_bert == -1)
      })

    data <- data |>
      left_join(
        topic_model |> bert_topic_info() |> select(topic_bert, name_topic), by = "topic_bert"
      ) |>
      select(topic_bert, name_topic, everything())

    if (length(bert_topics) > 0) {
      data <- data |> filter(topic_bert %in% bert_topics)
    }



    data
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
        left_join(bert_topic_labels(
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

#' Topics Per Structured Class
#'
#' @param topic_model
#' @param docs
#' @param classes
#' @param global_tuning
#'
#' @return
#' @export
#'
#' @examples
bert_topic_per_class <-
  function(topic_model,
           docs = NULL,
           classes = NULL,
           global_tuning = TRUE) {
    if (length(docs) == 0) {
      stop("Enter Documents")
    }

    if (length(classes) == 0) {
      stop("Enter Classes")
    }

    dat <-
      topic_model$topics_per_class(docs = docs,
                                   classes = classes,
                                   global_tuning = global_tuning)

    dat <-
      as_tibble(dat)

    dat <-
      dat |> setNames(c("topic_bert", "top_words", "count", "class"))
    dat <-
      dat |> left_join(bert_topic_info(topic_model = topic_model) |>
                         select(topic_bert, name_topic)

                       ,
                       by = "topic_bert") |>
      select(topic_bert, name_topic, everything()) |>
      arrange(desc(class), topic_bert)

    dat
  }

#' Find Topics from Class via Tibble
#'
#' @param data
#' @param topic_model
#' @param document_name
#' @param class_name
#' @param global_tuning
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_topic_per_class <-
  function(data,
           topic_model,
           document_name = NULL,
           class_name = NULL,
           global_tuning = TRUE,
           sort_by_topic = TRUE) {
    if (length(topic_model) == 0) {
      stop("Enter Topic Model")
    }
    if (length(document_name) == 0) {
      stop("Enter Document Name Field")
    }

    if (length(class_name) == 0) {
      stop("Enter Class Name Field")
    }

    data <- data |> tidyr::unite(col = "class",
                 all_of(class_name),
                 sep = "@",
                 remove = F)

    dat <-
      bert_topic_per_class(
        topic_model = topic_model,
        docs = data[[document_name]],
        classes = data[["class"]],
        global_tuning = global_tuning
      ) |>
      tidyr::separate(
        class,
        into = class_name,
        sep = "\\@",
        convert = TRUE,
        extra = "merge",
        fill = "right"
      ) |>
      select(-count, everything())

    dat <- dat |>
      mutate_at(class_name, list(function(x){
        case_when(x == "NA", NA_character_,
                  TRUE ~ x)
      }))

    if (sort_by_topic) {
      dat <- dat |>
        arrange(name_topic, desc(count))
    }

    dat
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
#' @param obj Topic model object
#' @param docs vector of documents
#' @param document_name If not `NULL` name of the text document column
#'
#' @return
#' @export
#'
#' @examples
bert_document_info <-
  function(obj, docs, document_name = NULL) {
    data <- obj$get_document_info(docs = docs) |> as_tibble()
    data <-
      data |> setNames(
        c(
          "document",
          "topic_bert",
          "name_topic_labels",
          "top_n_words",
          "pct_probabilty_topic_bert",
          "is_representative_document"
        )
      ) |>
      left_join(obj |> bert_topic_info() |> select(-count), by = "topic_bert") |>
      select(-name_topic_labels) |>
      select(document, topic_bert, name_topic, everything())

    if (length(document_name) > 0) {
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
  function(obj,
           data = NULL,
           number_zeros = 4) {
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
#' @param docs
#' @param filter_zero
#'
#' @return
#' @export
#'
#' @examples
extract_document_word_counts <-
  function(obj, docs = NULL, filter_zero = T) {
    if (length(docs) == 0) {
      stop("Enter Docs")
    }
    X <- obj$vectorizer_model$fit_transform(docs) |> as.matrix()

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


#' Covert Arry to Tibble
#'
#' @param data a matrix or array
#' @param output_type output type for columns
#' @param number_zeros padding for zeros.  Default `3`
#'
#' @return
#' @export
#'
#' @examples
tbl_array <-
  function(data,
           output_type = NULL,
           number_zeros = 3) {
    data <- as_tibble(data)

    if (length(output_type) > 0) {
      total_cols <- 1:ncol(data)
      zeros <- .pz(total_cols, number_zeros = number_zeros)
      array_names <-
        glue::glue("{output_type}_{zeros}") |> as.character()
      data <- data |> setNames(array_names)
    }

    data
  }

# Topics Over Time --------------------------------------------------------


#' Extract Topcis Over Time
#'
#' @param obj
#' @param docs
#' @param timestamps
#' @param nr_bins
#' @param datetime_format
#' @param evolution_tuning
#' @param global_tuning
#' @param return_tibble
#'
#' @return
#' @export
#'
#' @examples
bert_topics_over_time <-
  function(obj,
           docs = NULL,
           timestamps = NULL,
           nr_bins = NULL,
           datetime_format =  NULL,
           evolution_tuning = TRUE,
           global_tuning = TRUE,
           return_tibble = TRUE
           ) {
    if (length(docs) == 0) {
      stop("Enter Documents")
    }

    if (length(timestamps) == 0)  {
      stop("Enter timestamps")
    }

    if (length(nr_bins) > 0) {
      nr_bins <- as.integer(nr_bins)
    }

    out <-  obj$topics_over_time(docs = docs,
                         timestamps = timestamps,
                         nr_bins = nr_bins,
                         datetime_format = datetime_format,
                         evolution_tuning = evolution_tuning,
                         global_tuning = global_tuning)

    if (return_tibble) {
      out <- out |> munge_bert_topics_over_time(topic_model = obj)
    }

    out
  }

#' Extract BERT Topics Over Time from a Tibble
#'
#' @param data
#' @param topic_model
#' @param document_name
#' @param time_feature
#' @param nr_bins
#' @param datetime_format
#' @param evolution_tuning
#' @param global_tuning
#' @param return_tibble
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_topics_over_time <-
  function(data,
           topic_model,
           document_name = NULL,
           time_feature = NULL,
           nr_bins = NULL,
           datetime_format =  NULL,
           evolution_tuning = TRUE,
           global_tuning = TRUE,
           return_tibble = TRUE
  ) {

    dat <-
      bert_topics_over_time(obj = topic_model,
                          docs = data[[document_name]],
                          timestamps = data[[time_feature]],
                          nr_bins = nr_bins,
                          datetime_format = datetime_format,
                          evolution_tuning = evolution_tuning,
                          global_tuning = global_tuning)

    dat
}

#' Munge Topic Over Time
#'
#' @param data
#' @param topic_model
#'
#' @return
#' @export
#'
#' @examples
munge_bert_topics_over_time <-
  function(data, topic_model = NULL) {
    data <-
      as_tibble(data) |>
      janitor::clean_names() |>
      setNames(c("topic_bert", "words", "count", "date_time")) |>
      mutate(date = as.Date(date_time))

    if (length(topic_model) > 0) {
      data <- data |>
        left_join(bert_topic_info(topic_model) |> select(-count), by = "topic_bert") |>
        select(matches("date|topic$"), topic_bert, everything())
    }

    data

  }


#' Munge Topics Per Class Output
#'
#' @param data
#' @param class_name
#' @param topic_model
#'
#' @return
#' @export
#'
#' @examples
munge_bert_topics_per_class <-
  function(data,
           class_name = NULL,
           topic_model = NULL) {
    data <-
      as_tibble(data) |>
      janitor::clean_names() |>
      setNames(c("topic_bert", "words", "count", "class"))

    if (length(class_name) > 0) {
      data <- data |>
        rename(UQ(class_name) := class)
    }

    if (length(topic_model) > 0) {
      data <- data |>
        left_join(bert_topic_info(topic_model) |> select(-count), by = "topic_bert") |>
        select(matches("date|topic$"), topic_bert, everything())
    }

    data

  }

#' Munge Document Topic Proability Distributions
#'
#' @param data
#' @param topic_model
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
munge_bert_document_approximate_distributions <-
  function(data,
           topic_model = NULL,
           return_wide = F) {
    options(scipen = 999)
    data <- data[[1]] |> as_tibble()
    topic_number <- 1:ncol(data) - 1
    data <-
      data |> setNames(str_c("topic_bert_", topic_number)) |>
      mutate(number_document = 1:n()) |>
      select(number_document, everything())

    if (length(topic_model) > 0) {
      actual_topic_names <- topic_model |> bert_topic_info() |>
        filter(topic_bert != -1) |>
        mutate(topic = glue::glue("topic_bert_{topic_bert}_{name_topic}")) |>
        pull(topic)
      names(data)[!names(data) %in% c("number_document")] <-
        actual_topic_names
    }

    if (return_wide) {
      return(data)
    }

    data <-
      data |>
      gather(label_topic_bert,
             pct_probability_topic_bert,
             -number_document) |>
      filter(pct_probability_topic_bert != 0) |>
      mutate(label_topic_bert = label_topic_bert |> str_remove_all("topic_bert_") |> str_replace("\\_", "\\|")) |>
      separate(
        label_topic_bert,
        into = c("topic_bert", "name_topic"),
        sep = "\\|",
        convert = T,
        extra = "merge",
        fill = "right",
        remove = F
      ) |>
      arrange(number_document, desc(pct_probability_topic_bert))


    data
  }


# transform ---------------------------------------------------------------


#' Transform New Documents
#'
#' @param obj bertopic objects
#' @param documents vector of documents
#' @param embeddings if not `NULL` matrix or dataframe of embeddings
#'
#' @return
#' @export
#'
#' @examples
bert_transform_documents <-
  function(obj,
           documents = NULL,
           embeddings = NULL) {
    out <- obj$transform(documents = documents, embeddings = embeddings)

    topic_bert <- out[[1]] |> unlist() |> as.numeric()
    data <-
      tibble(topic_bert, document = documents) |>
      left_join(obj |> bert_topic_info(), by = "topic_bert")

    data
  }

tbl_bert_transform_documents <-
  function(data,
           topic_model,
           document_name = NULL,
           embeddings = TRUE) {
    if (length(topic_model) == 0) {
      stop("Enter Topic Model")
    }
    if (length(document_name) == 0) {
      stop("Enter Document Name Field")
    }


    dat <-
      bert_transform_documents(obj = topic_model,
                               documents = data[[document_name]],
                               embeddings = embeddings) |>
      rename(UQ(document_name) := document)

    data <-
      data |>
      left_join(dat, by = document_name)

    data
  }
