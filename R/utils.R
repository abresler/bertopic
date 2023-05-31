







# tuple -------------------------------------------------------------------

#' List to Tuple
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
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
  function(model_path = NULL,
           obj = NULL,
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
  function(obj,
           model_path = NULL,
           file_name = "bert_model",
           save_embedding_model = TRUE) {
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

# simillar ----------------------------------------------------------------


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
      mutate(
        is_outlier_bert_topic = topic_bert == -1,
        is_c_tfidf_over_50 = if_else(score_c_tfidf >= .45, TRUE, FALSE),
        bin_c_tfidf_score = (score_c_tfidf * 100) %/% 10 * 10,
        rounded_c_tfidf_score = (score_c_tfidf * 100) |> round(digits =
                                                                 -1)
      )

  }

#' Simllar Term Embeddings
#'
#' @param topic_model Bertopic Model
#' @param terms Vector of Terms to search
#' @param top_n_terms the number of topics to return.  Default is 10
#' @param nest_data if `TRUE` Nests data
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
        by = c("topic_bert", "is_outlier_bert_topic")
      )


    if (nest_data) {
      data |>
        group_by(term) |>
        nest() |>
        ungroup()
    }

    data

  }

# info --------------------------------------------------------------------


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
                      into = c("remove", "label_bertopic"),
                      sep = "\\+") |>
      select(-remove) |>
      mutate(is_outlier_bert_topic = topic_bert == -1) |>
      mutate_if(is.character, str_squish) |>
      select(-count, everything())

    data
  }

#' Generate BERT Topic Labels
#'
#' @param obj topic model object
#' @param number_words Top n words per topic to use.  Default is 4
#' @param separator The string with which the words and topic prefix will be separated. Underscores are the default but a nice alternative is ", ".  Default `_`
#' @param word_length The maximum length of each word in the topic label. Some words might be relatively long and setting this value helps to make sure that all labels have relatively similar lengths.
#' @param topic_prefix  Whether to use the topic ID as a prefix. If set to True, the topic ID will be separated using the separator
#' @param append_number_words Append the number of words
#' @param update_topic_model_labels if `TRUE` updates new label into a custom field
#'
#' @return
#' @export
#'
#' @examples
bert_topic_labels <-
  function(obj,
           number_words = 4L,
           separator = "_",
           word_length =  NULL,
           update_topic_model_labels = FALSE,
           append_number_words = FALSE,
           topic_prefix = FALSE) {
    if (length(word_length) > 0) {
      word_length <- as.integer(word_length)
    }

    label_bertopic <- obj$generate_topic_labels(
      nr_words = as.integer(number_words),
      separator = separator,
      topic_prefix = topic_prefix,
      word_length = word_length
    )

    dat <-
      tibble(label_bertopic) |>
      mutate(topic_bert = 1:n() - 2) |>
      select(topic_bert, everything()) |>
      mutate(is_outlier_bert_topic = topic_bert == -1) |>
      mutate_if(is.character, stringr::str_squish)

    if (append_number_words) {
      new_name <-
        str_c("label_bertopic_",
              .pad_zeros(x = number_words, number_zeros = 3),
              "_words")
      names(dat)[names(dat) %in% "label_bertopic"] <-
        new_name
    }

    if (update_topic_model_labels) {
      message("Updating topic labels")
      obj$set_topic_labels(topic_labels = label_bertopic)
    }

    dat
  }



#' Topic Counts
#'
#' @param obj bertopic object
#' @param topic_number if not `NULL` the number of the topic.  If `NULL` returns all topics.
#'
#' @return
#' @export
#'
#' @examples
bert_topic_count <-
  function(obj,
           topic_number = NULL,
           include_representative_documents = TRUE,
           join_labels = TRUE,
           include_parameters = FALSE,
           parameter_filters = NULL) {
    data <-
      obj$get_topic_freq(topic = topic_number) |>
      janitor::clean_names() |> as_tibble() |>
      rename(topic_bert = topic) |>
      mutate(is_outlier_bert_topic = topic_bert == -1)

    if (include_representative_documents) {
      join_labels <- T
    }

    if (join_labels) {
      data <- data |>
        left_join(obj |> bert_topic_info() |> select(1:2), by = "topic_bert")
    }

    if (include_parameters) {
      data <- data |>
        mutate(id = 1)
      tbl_ids <-
        obj |> tbl_bert_attributes(return_wide = T, parameter_filter = parameter_filters) |>
        mutate(id = 1)
      data <- data |>
        left_join(tbl_ids, by = "id") |>
        select(one_of(names(tbl_ids)), everything()) |>
        select(-id)
    }

    if (include_representative_documents) {
      tbl_docs <- obj |> bert_representative_documents()
      tbl_docs <- tbl_docs |> group_by(topic_bert) |>
        summarise(text_representative_documents = unique(text) |> str_c(collapse = ".  "))

      data <- data |> left_join(tbl_docs, by = "topic_bert")
    }


    data

  }

#' Topic's Representative Documents
#'
#' @param obj Topic Model Object
#' @param topic_number If not `NULL` number of topic
#'
#' @return
#' @export
#'
#' @examples
bert_representative_documents <-
  function(obj,
           topic_number = NULL,
           include_labels = T,
           number_words = 5L,
           sep = "_") {
    if (length(topic_number) != 0) {
      rep_docs <-
        obj$get_representative_docs(topic = as.integer(topic_number))
      data <-
        tibble(text = rep_docs) |>
        mutate(topic_bert = topic_number) |>
        select(topic_bert, everything())
      return(data)
    }
    rep_docs <-
      obj$get_representative_docs(topic = topic_number)
    data <- seq_along(rep_docs) |>
      map_dfr(function(x) {
        topic_no <- names(rep_docs[x]) |> readr::parse_number()
        tibble(text = rep_docs[[x]]) |>
          mutate(topic_bert = topic_no) |>
          select(topic_bert, everything())
      }) |>
      arrange(topic_bert)

    if (include_labels) {
      tbl_labels <- bert_topic_labels(
        obj = obj,
        number_words = as.integer(number_words),
        separator = sep
      )
      data <-
        data |>
        left_join(tbl_labels,
                  by = "topic_bert") |>
        select(topic_bert, label_bertopic, everything())
    }


    data
  }

#' BERTopic Hieracrchy
#'
#' To create this hierarchy, BERTopic needs to be already fitted once. Then, a hierarchy is calculated on the distance matrix of the c-TF-IDF representation using scipy.cluster.hierarchy.linkage.
#'
#' @param obj Topic Model Object
#' @param docs Vector of documents
#' @param linkage_function  The linkage function to use. Default is: lambda x: sch.linkage(x, 'ward', optimal_ordering=True)
#' @param distance_function The distance function to use on the c-TF-IDF matrix. Default is: lambda x: 1 - cosine_similarity(x)
#' @param tight_layout Whether to use a tight layout (narrow width) for easier readability if you have hundreds of topics.
#' @param print_tree If `TRUE` prints tree
#'
#' @return
#' @export
#'
#' @examples
bert_topic_hierarchy <-
  function(obj,
           docs,
           linkage_function = NULL,
           distance_function = NULL,
           tight_layout = F,
           print_tree = FALSE) {
    out <-
      obj$hierarchical_topics(
        docs = docs,
        linkage_function = linkage_function,
        distance_function = distance_function
      )

    if (print_tree) {
      tree <-
        obj$get_topic_tree(hier_topics = out, tight_layout = tight_layout)

      assign('bert_tree', tree, envir = .GlobalEnv)

      tree |> cat(fill = TRUE)
    }

    out
  }

#' Print Bertopic Tree
#'
#' @param obj Bertopic Model
#' @param hierarchy  Output from `bert_topic_hierarchy`
#' @param tight_layout Whether to use a tight layout (narrow width) for easier readability if you have hundreds of topics.  Default `FALSE`
#' @param max_distance The maximum distance between two topics. This value is based on the Distance column in hier_topics.  Default `NULL`
#'
#' @return
#' @export
#'
#' @examples
print_bert_topic_tree <-
  function(obj,
           hierarchy,
           tight_layout = FALSE,
           max_distance = NULL) {
    obj$get_topic_tree(
      hier_topics  = hierarchy,
      max_distance = max_distance,
      tight_layout = tight_layout
    ) |>
      cat(fill = TRUE)
  }

#' Returns Topic Tree Text
#'
#' @param obj Bertopic Model
#' @param hierarchy  Output from `bert_topic_hierarchy`
#' @param tight_layout Whether to use a tight layout (narrow width) for easier readability if you have hundreds of topics.  Default `FALSE`
#' @param max_distance The maximum distance between two topics. This value is based on the Distance column in hier_topics.  Default `NULL`
#'
#' @return
#' @export
#'
#' @examples
bert_topic_tree_text <-
  function(obj,
           hierarchy,
           tight_layout = FALSE,
           max_distance = NULL) {
    text <-
      obj$get_topic_tree(
        hier_topics  = hierarchy,
        max_distance = max_distance,
        tight_layout = tight_layout
      )

    text
  }



#' Returns keywords for the topics
#'
#' @param obj
#' @param bert_topics
#'
#' @return
#' @export
#'
#' @examples
bert_topic_keywords <-
  function(obj, bert_topics = NULL)  {
    topics <- obj$get_topics()
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
      left_join(obj |> bert_topic_info() |> select(topic_bert, label_bertopic),
                by = "topic_bert") |>
      select(topic_bert, label_bertopic, everything())

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
           number_words = 4L,
           arrange_topics = F) {
    topics <-
      obj[[1]]

    tbl_prob <-
      obj[[2]] |> as_tibble() |> janitor::clean_names() |>
      select(v1) |>
      rename(pct_probability_topic_bert = v1)

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
      tbl_labels <- bert_topic_labels(obj = topic_model,
                                      number_words = as.integer(number_words))
      data <-
        data |>
        left_join(tbl_labels,
                  by = "topic_bert") |>
        select(topic_bert, label_bertopic, everything())
    }

    if (arrange_topics) {
      data <- data |>
        arrange(topic_bert, desc(pct_probability_topic_bert))
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
      dat |> left_join(
        bert_topic_info(topic_model = topic_model) |>
          select(topic_bert, label_bertopic)

        ,
        by = "topic_bert"
      ) |>
      select(topic_bert, label_bertopic, everything()) |>
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
      select(topic_bert,
             label_bertopic,
             matches("class_name"),
             everything()) |>
      mutate_at(class_name, list(function(x) {
        case_when(is.na(x) ~ NA_character_,
                  TRUE ~ as.character(x))
      }))

    if (sort_by_topic) {
      dat <- dat |>
        arrange(label_bertopic, desc(count))
    }

    dat
  }

#' Merge Topics
#'
#' @param obj bertopic model object
#' @param docs The documents you used when calling either fit or fit_transform
#' @param topics_to_merge Either a list of topics or a list of list of topics to merge. For example: `list(1,2,3)` will merge topics 1, 2 and 3 `list(list(1,2), list(3,4))` will merge topics 1 and 2, and separately merge topics 3 and 4.
#'
#' @return
#' @export
#'
#' @examples
bert_merge_topics <-
  function(obj, docs,
           topics_to_merge = NULL) {
    if (length(topics_to_merge) == 0) {
      "Enter a list or list of lists of topics to merge" |> message()
      return(obj)
    }
    obj$merge_topics(docs = docs, topics_to_merge = topics_to_merge)
  }

#' Reduce BERTopics
#'
#' @param obj BERTopic Topic Model
#' @param docs Vector of Documents
#' @param number_topics Number of topics to reduce to
#' @param number_words Top n words per topic to use.  Default is 4
#' @param separator The string with which the words and topic prefix will be separated. Underscores are the default but a nice alternative is ", ".  Default `_`
#' @param word_length The maximum length of each word in the topic label. Some words might be relatively long and setting this value helps to make sure that all labels have relatively similar lengths.
#' @param topic_prefix  Whether to use the topic ID as a prefix. If set to True, the topic ID will be separated using the separator
#' @param append_number_words Append the number of words
#' @param update_bert_labels If `TRUE` updates actual topic labels
#' @param update_topic_model_labels if `TRUE` updates new label into a custom field
#'
#' @return
#' @export
#'
#' @examples
bert_reduce_topics <-
  function(obj,
           docs = NULL,
           number_topics = NULL,
           update_bert_labels = TRUE,
           number_words = 1L,
           separator = "_",
           word_length = NULL,
           update_topic_model_labels = TRUE,
           append_number_words = FALSE,
           topic_prefix = FALSE) {
    if (length(number_topics) == 0) {
      "Enter number of reduced topics" |> message()
      return(obj)
    }
    if (length(docs) == 0) {
      "Enter Documents" |> message()
      return(obj)
    }
    obj$reduce_topics(docs = docs, nr_topics = as.integer(number_topics))

    if (!update_bert_labels) {
      return(obj)
    }
    message("Updating BERTopic Labels")
    obj <- bert_topic_labels(
      obj = obj,
      number_words = number_words,
      separator = separator,
      word_length = word_length,
      update_topic_model_labels = update_topic_model_labels,
      append_number_words = append_number_words,
      topic_prefix = topic_prefix
    )

    obj

  }

#' Updates the topic representation by recalculating c-TF-IDF with the new parameters as defined in this function.
#'
#' @param obj BERTopic Object
#' @param docs The documents you used when calling either fit or fit_transform
#' @param topics  A list of topics where each topic is related to a document in docs. Use this variable to change or map the topics. NOTE: Using a custom list of topic assignments may lead to errors if topic reduction techniques are used afterwards. Make sure that manually assigning topics is the last step in the pipeline.  Default `NULL`
#' @param top_n_words The number of words per topic to extract. Setting this too high can negatively impact topic embeddings as topics are typically best represented by at most 10 words.  Default `10`
#' @param n_gram_range The n-gram range for the CountVectorizer. Default `NULL`
#' @param vectorizer_model Pass in your own CountVectorizer from scikit-learn.  Default `NULL`
#' @param ctfidf_model Pass in your own c-TF-IDF model to update the representations.  Default `NULL`
#' @param representation_model  Pass in a model that fine-tunes the topic representations calculated through c-TF-IDF. Models from bertopic.representation are supported. Default `NULL`
#' @param language The main language used in your documents. The default sentence-transformers model for "english" is all-MiniLM-L6-v2. For a full overview of supported languages see bertopic.backend.languages. Select "multilingual" to load in the paraphrase-multilingual-MiniLM-L12-v2 sentence-tranformers model that supports 50+ languages. Default `english`
#' @param use_key_phrase_vectorizer if `TRUE` uses a keyphrase vectorizer
#' @param use_sklearn_vectorizer If `TRUE` uses SKLearn vectorizer
#' @param is_lower_case if `TRUE` lower case
#' @param keyphrase_ngram_range If not `NULL` range for keyphrase
#' @param exclude_stop_words if `TRUE` excludes basic stopwords
#' @param stopword_package_sources options if not `NULL` `c("snowball", "stopwords-iso", "smart", "nltk")`
#' @param extra_stop_words vector of other stopwords
#' @param min_df During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `1L`
#' @param pos_pattern Position patter for keyphrase.  Defaults to `pos_pattern = "<J.*>*<N.*>+",`
#' @param seed_topic_list A list of seed words per topic to converge around.  Default is `NULL`
#' @param max_df
#' @param vocabulary
#'
#' @return
#' @export
#'
#' @examples
bert_update_topics <-
  function(obj,
           docs = NULL,
           topics = NULL,
           top_n_words = 10,
           n_gram_range = list(1L, 1L),
           vectorizer_model = NULL,
           ctfidf_model = NULL,
           representation_model = NULL,
           language = "english",
           use_key_phrase_vectorizer = F,
           use_sklearn_vectorizer = F,
           is_lower_case = T,
           keyphrase_ngram_range = list(1L,
                                        1L),
           exclude_stop_words = T,
           stopword_package_sources = NULL,
           extra_stop_words = NULL,
           min_df = 1L,
           max_df = 1L,
           pos_pattern = "<J.*>*<N.*>+",
           seed_topic_list = NULL,
           vocabulary = NULL) {
    if (length(docs) == 0) {
      "Enter documents to fit" |> message()
      return(obj)
    }

    if (length(n_gram_range) > 0) {
      n_gram_range <- reticulate::tuple(n_gram_range)
    }

    if (use_key_phrase_vectorizer) {
      "Using keyphrase vectorizer" |> message()
      vectorizer_model <-
        keyphrase_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          exclude_stop_words = exclude_stop_words,
          language = language,
          pos_pattern = pos_pattern,
          extra_stop_words = extra_stop_words
        )
    }

    if (!use_key_phrase_vectorizer &
        length(vectorizer_model) == 0) {
      "Using sklearn vectorizer" |> message()
      use_sklearn_vectorizer <- T
      vectorizer_model <-
        sklearn_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          ngram_range = n_gram_range,
          vocabulary = vocabulary,
          language = language,
          exclude_stop_words = exclude_stop_words,
          extra_stop_words = extra_stop_words
        )
    }
    uses_vectorizer <-
      use_sklearn_vectorizer |
      use_key_phrase_vectorizer
    if (uses_vectorizer & exclude_stop_words) {
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

    obj$update_topics(
      docs = docs,
      topics = topics,
      top_n_words = as.integer(top_n_words),
      n_gram_range = n_gram_range,
      vectorizer_model = vectorizer_model,
      representation_model = representation_model,
      ctfidf_model = ctfidf_model
    )

    obj <-
      set_bert_attributes(obj = obj, representation_model = representation_model)

    obj
  }

#' Set Topic Labals
#'
#' @param obj
#' @param topic_labels
#'
#' @return
#' @export
#'
#' @examples
bert_set_topic_labels <-
  function(obj, topic_labels) {
    obj$set_topic_labels(topic_labels = topic_labels)
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
          "name_label_bertopic",
          "top_n_words_label_bertopic",
          "pct_probability_topic_bert",
          "is_bertopic_representative_document"
        )
      ) |>
      left_join(obj |> bert_topic_info() |> select(-count), by = "topic_bert") |>
      select(-name_label_bertopic) |>
      select(document, topic_bert, label_bertopic, everything())

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

    df_umap <- df_umap |>
      mutate(pct_dbscan_prob) |>
      mutate(id = row_number()) |>
      select(id, everything())

    if (length(data) == 0) {
      return(df_umap)
    }

    if (nrow(data) != nrow(df_umap))
      return(df_umap)

    data |>
      bind_cols(df_umap)

  }


# visualization_data_inputs -----------------------------------------------


#' Extract UMAP for Visualization
#'
#' @param obj
#' @param n_components
#' @param metric
#' @param random_state
#' @param min_dist
#' @param learning_rate
#' @param exclude_outlier
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_umap_label_level <-
  function(obj,
           exclude_outlier = FALSE,
           n_components = 2L,
           metric = 'cosine',
           random_state = 42L,
           min_dist = .1,
           learning_rate = 1L) {
    freq_df <- obj |>  bert_topic_count()


    umap <- import_umap()
    um <- umap$UMAP(
      n_components = n_components,
      metric = metric,
      random_state = random_state,
      min_dist = min_dist,
      learning_rate = learning_rate
    )

    if (exclude_outlier) {
      freq_df <-
        freq_df |> filter(!is_outlier_bert_topic)
      embed <-
        obj$topic_embeddings_[2:length(obj$topic_embeddings_)]
    }

    if (!exclude_outlier) {
      embed <- obj$topic_embeddings_
    }
    reduced_embeddings <-
      um$fit_transform(X = embed) |>
      tbl_array(output_type = 'umap') |>
      mutate(topic_bert = 1:n() - 2)


    freq_df <-
      freq_df |>
      left_join(reduced_embeddings,  by = "topic_bert") |>
      select(-count, everything())

    freq_df
  }

#' BERT Cosine Simiarity
#'
#' @param obj topic model object
#' @param exclude_outlier if `TRUE` excludes outlier topic
#' @param include_topic_number if `TRUE` includes topic number
#' @param return_tibble if `TRUE` returns tibble
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_label_cosine_similarity <-
  function(obj,
           exclude_outlier = FALSE,
           include_topic_number = TRUE,
           distinct_matches = FALSE,
           separate_data = T,
           return_tibble = F) {
    freq_df <- obj |>  bert_topic_count()
    sk <- import_sklearn()
    cosine_similarity <- sk$metrics$pairwise$cosine_similarity

    if (exclude_outlier) {
      freq_df <-
        freq_df |> filter(!is_outlier_bert_topic)
      embed <-
        obj$topic_embeddings_[2:length(obj$topic_embeddings_)]
    }

    if (!exclude_outlier) {
      embed <- obj$topic_embeddings_
    }

    dist <- cosine_similarity(embed)

    if (include_topic_number) {
      feature_names <- freq_df |>
        unite(name, topic_bert, label_bertopic, sep = "_") |>
        pull(name)
    }

    if (!include_topic_number) {
      feature_names <- freq_df |> pull(label_bertopic)
    }




    rownames(dist) <- feature_names
    colnames(dist) <- feature_names

    if (!return_tibble) {
      return(dist)
    }

    data <-
      as_tibble(dist) |>
      mutate(feature_01 = rownames(dist)) |>
      gather(feature_02, value, -feature_01) |>
      filter(feature_01 != feature_02) |>
      rename(cosine_similarity = value)

    if (include_topic_number & separate_data) {
      data <-
        data |>
        separate(
          feature_01,
          into = c("topic_bert_01", "label_bertopic_01"),
          sep = "\\_",
          convert = T
        ) |>
        separate(
          feature_02,
          into = c("topic_bert_02", "label_bertopic_02"),
          sep = "\\_",
          convert = T
        )



    }

    if (!include_topic_number & separate_data) {
      data <-
        data |>
        rename(label_bertopic_01 = feature_01,
               label_bertopic_02 = feature_02)
    }

    if (distinct_matches) {
      data <- data |>
        mutate(label = glue({"{label_bertopic_01}|{label_bertopic_02}"})) |>
        mutate(id = 1:n())

      tbl_labels <-
        data |>
        select(id, label) |>
        separate_rows(label, sep = "\\|", convert = T) |>
        group_by(id) |>
        summarise(label_unique = unique(label) |> sort() |> str_c(collapse = "|")) |>
        distinct(id, label_unique)

      tbl_labels <- tbl_labels |>
        mutate(row = 1:n()) |>
        group_by(label_unique) |>
        filter(row == min(row)) |>
        ungroup() |>
        select(id = row, label_unique) |>
        mutate(is_first = TRUE)


      data <- data |>
        left_join(tbl_labels, by = "id") |>
        filter(is_first) |>
        select(-c(id, label_unique, is_first))
    }

    data <- data |>
      arrange(label_bertopic_01, desc(cosine_similarity))

    data
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
  function(obj,
           docs = NULL,
           filter_zero = T) {
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
           return_tibble = TRUE) {
    if (length(docs) == 0) {
      stop("Enter Documents")
    }

    if (length(timestamps) == 0)  {
      stop("Enter timestamps")
    }

    if (length(nr_bins) > 0) {
      nr_bins <- as.integer(nr_bins)
    }

    out <-  obj$topics_over_time(
      docs = docs,
      timestamps = timestamps,
      nr_bins = nr_bins,
      datetime_format = datetime_format,
      evolution_tuning = evolution_tuning,
      global_tuning = global_tuning
    )

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
           return_tibble = TRUE) {
    dat <-
      bert_topics_over_time(
        obj = topic_model,
        docs = data[[document_name]],
        timestamps = data[[time_feature]],
        nr_bins = nr_bins,
        datetime_format = datetime_format,
        evolution_tuning = evolution_tuning,
        global_tuning = global_tuning
      )

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
# approx_distribution -----------------------------------------------------

#' Get a string representation of the current object.
#'
#' @param obj Topic Model Object
#' @param docs Vector of Documents
#' @param window Size of the moving window which indicates the number of tokens being considered.  Default `4`
#' @param stride How far the window should move at each step.  Default `1`.
#' @param min_similarity The minimum similarity of a document's tokenset with respect to the topics.  Default `.1`
#' @param batch_size The number of documents to process at a time. If None, then all documents are processed at once. NOTE: With a large number of documents, it is not advised to process all documents at once.    Default `1000`
#' @param padding  Whether to pad the beginning and ending of a document with empty tokens.  Default `FALSE`
#' @param use_embedding_model Whether to use the topic model's embedding model to calculate the similarity between tokensets and topics instead of using c-TF-IDF.  Default `FALSE`
#' @param calculate_tokens Calculate the similarity of tokens with all topics. NOTE: This is computation-wise more expensive and can require more memory. Using this over batches of documents might be preferred.  Default `FALSE`
#' @param separator  The separator used to merge tokens into tokensets.
#'
#' @return
#' @export
#'
#' @examples
bert_approximate_distribution <-
  function(obj,
           docs,
           window = 4,
           stride = 1,
           min_similarity = 0.1,
           batch_size = 1000,
           padding = FALSE,
           use_embedding_model = FALSE,
           calculate_tokens = FALSE,
           separator = ' ') {
    out <-
      obj$approximate_distribution(
        documents = docs,
        window = as.integer(window),
        stride = as.integer(stride),
        min_similarity = min_similarity,
        batch_size = as.integer(batch_size),
        padding = padding,
        use_embedding_model = use_embedding_model,
        calculate_tokens = calculate_tokens,
        separator = separator
      )


    out
  }


#' Munge Document Topic Proability Distributions
#'
#' @param out Approximate Distribution Output
#' @param obj Bertopic Object
#' @param return_wide
#'
#' @return
#' @export
#'
#' @examples
munge_bert_document_approximate_distributions <-
  function(out,
           obj = NULL,
           return_wide = F) {
    options(scipen = 999)
    data <- out[[1]] |> as_tibble()
    topic_number <- 1:ncol(data) - 1
    data <-
      data |> setNames(str_c("topic_bert_", topic_number)) |>
      mutate(number_document = 1:n()) |>
      select(number_document, everything())

    if (length(obj) > 0) {
      actual_topic_names <-
        obj |> bert_topic_info() |>
        filter(topic_bert != -1) |>
        mutate(topic = glue::glue("topic_bert_{topic_bert}_{label_bertopic}")) |>
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
        into = c("topic_bert", "label_bertopic"),
        sep = "\\|",
        convert = T,
        extra = "merge",
        fill = "right",
        remove = T
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
