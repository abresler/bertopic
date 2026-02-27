# Topic information retrieval functions

#' BERT Topic Model Info
#'
#' @param obj A BERTopic model object.
#' @param topic_number Integer. Specific topic number to retrieve, or `NULL` for all topics.
#' @param assign_to_environment Logical. If `TRUE` assigns results to global environment. Default is `TRUE`.
#' @param concatenator Character. String to concatenate multiple values. Default is ". ".
#' @param remove_list_columns Logical. If `TRUE` removes list-type columns. Default is `TRUE`.
#' @param return_summary Logical. If `TRUE` returns summary format. Default is `TRUE`.
#' @param top_n_aspect_words Integer. Number of top aspect words to include, or `NULL` for all.
#'
#' @returns A tibble containing topic information with columns: `topic_bert`, `label_bertopic`, `is_outlier_bert_topic`, and additional topic metadata.
#' @export
#'

bert_topic_info <-
  function(obj,
           topic_number = NULL,
           return_summary = TRUE,
           assign_to_environment = TRUE,
           concatenator = ".  ",
           top_n_aspect_words = NULL,
           remove_list_columns = TRUE) {
    custom_labels <-
      obj$custom_labels_

    has_custom_labels <- length(custom_labels) > 0
    df <-
      obj$get_topic_info(topic = topic_number)

    tbl_topics <-
      df |>
      janitor::clean_names() |>
      as_tibble() |>
      rename(topic_bert = topic)

    data <-
      tbl_topics |>
      mutate(name = name |> str_replace("\\_", "\\+")) |>
      tidyr::separate(name,
        into = c("remove", "label_bertopic"),
        sep = "\\+"
      ) |>
      mutate(label_bertopic = case_when(
        is.na(label_bertopic) ~ remove,
        TRUE ~ label_bertopic
      ) |> str_to_lower()) |>
      select(-remove) |>
      mutate(is_outlier_bert_topic = topic_bert == -1) |>
      mutate_if(is.character, str_squish) |>
      select(-count, everything()) |>
      select_if(~ !any(is.na(.)))

    aspect_cols <-
      data |>
      select(matches("aspect")) |>
      names()

    aspect_cols <- data |> select_if(is.list) |> names()

    if (length(aspect_cols) > 0) {
      tbl_aspect <-
        aspect_cols |>
        map(function(x) {
          .munge_aspect_column(
            data = data,
            aspect_column = x,
            assign_to_environment = assign_to_environment,
            top_n_aspect_words = top_n_aspect_words,
            return_summary = return_summary
          )
        }) |>
        discard(function(x) {
          length(x) == 0
        })

      tbl_aspect <-
        tbl_aspect |>
        reduce(full_join, by = c("topic_bert", "label_bertopic"))

      data <-
        data |>
        select(-one_of(aspect_cols)) |>
        left_join(tbl_aspect, by = c("topic_bert", "label_bertopic"))
    }

    rep_cols <- data |>
      select(matches("representation")) |>
      names()

    if (length(rep_cols) > 0) {
      tbl_rep <-
        data |> .munge_respresentation(
          assign_to_environment = assign_to_environment,
          top_n_aspect_words = top_n_aspect_words,
          return_summary = return_summary
        )

      has_actual_rep <-
        tbl_rep |>
          filter(label_bertopic != representation) |>
          nrow() > 0

      if (has_actual_rep) {
        data <- data |>
          select(-one_of(rep_cols)) |>
          left_join(tbl_rep, by = c("topic_bert", "label_bertopic"))
      }
    }

    if (data |> hasName("representative_docs")) {
      tbl_docs <-
        data |>
        select(topic_bert, label_bertopic, representative_docs) |>
        tidyr::unnest(representative_docs) |>
        group_by(topic_bert, label_bertopic) |>
        summarise(
          text_representative_documents = unique(representative_docs) |> str_squish() |> str_c(collapse = ".  ")
        ) |>
        ungroup()

      data <- data |>
        select(-representative_docs) |>
        left_join(tbl_docs, by = c("topic_bert", "label_bertopic")) |>
        select(-text_representative_documents, everything())
    }

    list_cols <- data |>
      select_if(is.list) |>
      names()

    if (data |> hasName("label_bertopic") &
      data |> hasName("representation")) {
      n_row_same <- data |>
        select(label_bertopic, representation) |>
        mutate(is_same = representation == label_bertopic) |>
        filter(is_same) |>
        nrow()

      if (nrow(data) == n_row_same) {
        data <-
          data |> select(-one_of(c(
            "representation", "data_representation"
          )))
      }
    }

    if (length(list_cols) > 0) {
      data <- data |>
        select(-one_of(list_cols), everything())

      if (remove_list_columns) {
        data <- data |> select(-one_of(list_cols))
      }
    }

    data <- data |> janitor::remove_empty(which = "cols")

    names(data) <- names(data) |>
      str_replace_all("\\__", "\\_") |>
      str_remove_all("\\_$")

    if (data |> hasName("custom_name")) {
      data <-
        data |>
        rename(label_bertopic_original = label_bertopic) |>
        select(-custom_name)
    }

    if (has_custom_labels) {
      data <- data |>
        mutate(label_bertopic = custom_labels, .after = "topic_bert")
    }


    data
  }

#' Generate BERT Topic Labels
#'
#' @param obj A BERTopic model object.
#' @param number_words Integer. Top n words per topic to use. Default is 4.
#' @param separator Character. String separating words and topic prefix. Default is `"_"`.
#' @param word_length Integer. Maximum length of each word in the label, or `NULL` for no limit.
#' @param topic_prefix Logical. If `TRUE` uses the topic ID as a prefix. Default is `FALSE`.
#' @param append_number_words Logical. If `TRUE` appends the number of words to column names. Default is `FALSE`.
#' @param update_topic_model_labels Logical. If `TRUE` updates the model with new labels. Default is `FALSE`.
#' @param aspect Character. Aspect name from which to generate labels, or `NULL` for default.
#'
#' @returns A tibble containing generated labels with columns: `topic_bert`, `label_bertopic`, and `is_outlier_bert_topic`.
#' @export
#'

bert_topic_labels <-
  function(obj,
           number_words = 4L,
           separator = "_",
           word_length = NULL,
           update_topic_model_labels = FALSE,
           append_number_words = FALSE,
           topic_prefix = FALSE,
           aspect = NULL) {
    if (length(word_length) > 0) {
      word_length <- as.integer(word_length)
    }

    label_bertopic <-
      obj$generate_topic_labels(
        nr_words = as.integer(number_words),
        separator = separator,
        topic_prefix = topic_prefix,
        word_length = word_length,
        aspect = aspect
      )

    if (length(aspect) == 0) {
      dat <-
        tibble(label_bertopic) |>
        mutate(topic_bert = 1:n() - 1) |>
        select(topic_bert, everything()) |>
        mutate(is_outlier_bert_topic = topic_bert == -1) |>
        mutate_if(is.character, stringr::str_squish)

      if (append_number_words) {
        new_name <-
          str_c(
            "label_bertopic_",
            .pad_zeros(x = number_words, number_zeros = 3),
            "_words"
          )
        names(dat)[names(dat) %in% "label_bertopic"] <-
          new_name
      }
    }

    if (length(aspect) > 0) {
      dat <-
        tibble(label_bertopic) |>
        mutate(aspect = aspect, topic_bert = 1:n() - 2) |>
        select(topic_bert, everything()) |>
        mutate(is_outlier_bert_topic = topic_bert == -1) |>
        mutate_if(is.character, stringr::str_squish)

      if (append_number_words) {
        new_name <-
          str_c(
            "label_bertopic_",
            aspect,
            "_",
            .pad_zeros(x = number_words, number_zeros = 3),
            "_words"
          )
        names(dat)[names(dat) %in% "label_bertopic"] <-
          new_name
      }
    }



    if (update_topic_model_labels) {
      message("Updating topic labels")
      obj$set_topic_labels(topic_labels = label_bertopic)
    }

    dat
  }



#' Topic Counts
#'
#' @param obj A BERTopic model object.
#' @param topic_number Integer. Specific topic number, or `NULL` to return all topics.
#' @param include_representative_documents Logical. If `TRUE` includes representative documents. Default is `FALSE`.
#' @param join_labels Logical. If `TRUE` joins topic labels. Default is `TRUE`.
#' @param only_label Logical. If `TRUE` includes only label column. Default is `TRUE`.
#' @param include_parameters Logical. If `TRUE` includes model parameters. Default is `FALSE`.
#' @param parameter_filters List of parameter filters to apply, or `NULL`.
#'
#' @returns A tibble containing topic frequency counts with columns: `topic_bert`, `frequency`, `label_bertopic`, and `is_outlier_bert_topic`.
#' @export
#'

bert_topic_count <-
  function(obj,
           topic_number = NULL,
           include_representative_documents = F,
           join_labels = TRUE,
           only_label = TRUE,
           include_parameters = FALSE,
           parameter_filters = NULL) {
    custom_labels <-
      obj$custom_labels_

    has_custom_labels <- length(custom_labels) > 0

    data <-
      obj$get_topic_freq(topic = topic_number) |>
      janitor::clean_names() |>
      as_tibble() |>
      rename(topic_bert = topic) |>
      mutate(is_outlier_bert_topic = topic_bert == -1) |>
      arrange(topic_bert)

    if (include_representative_documents) {
      join_labels <- T
    }

    if (join_labels) {
      if (!has_custom_labels) {
        if (only_label) {
          tbl_labels <- obj |>
            bert_topic_info() |>
            select(1:2)
        }


        data <-
          data |>
          left_join(tbl_labels, by = "topic_bert")
      }

      if (has_custom_labels) {
        data <- data |>
          mutate(label_bertopic = custom_labels)
      }

      if (!only_label) {
        tbl_labels <-
          obj |>
          bert_topic_info() |>
          select(-matches("docs$"))

        data <- data |>
          left_join(tbl_labels, by = "topic_bert")
      }
    }

    if (include_parameters) {
      data <- data |>
        mutate(id = 1)
      tbl_ids <-
        obj |>
        tbl_bert_attributes(return_wide = T, parameter_filter = parameter_filters) |>
        mutate(id = 1)
      data <- data |>
        left_join(tbl_ids, by = "id") |>
        select(one_of(names(tbl_ids)), everything()) |>
        select(-id)
    }

    if (include_representative_documents) {
      bert_representative_documents_safe <-
        possibly(bert_representative_documents, tibble())
      tbl_docs <- obj |> bert_representative_documents_safe()
      if (nrow(tbl_docs) > 0) {
        tbl_docs <- tbl_docs |>
          group_by(topic_bert) |>
          summarise(text_representative_documents = unique(text) |> str_c(collapse = ".  "))

        data <- data |> left_join(tbl_docs, by = "topic_bert")
      }
    }

    data <- data |> mutate_if(is.character, str_squish)

    data
  }

#' Topic's Representative Documents
#'
#' @param obj A BERTopic model object.
#' @param topic_number Integer. Specific topic number, or `NULL` to return all topics.
#' @param include_labels Logical. If `TRUE` includes topic labels. Default is `TRUE`.
#' @param number_words Integer. Number of top words to use in labels. Default is 5.
#' @param top_n_documents Integer. Maximum number of documents to return per topic, or `NULL` for all.
#' @param sep Character. Separator for label construction. Default is `"_"`.
#'
#' @returns A tibble containing representative documents with columns: `number_document_topic`, `topic_bert`, `label_bertopic`, `text`, and `is_outlier_bert_topic`.
#' @export
#'

bert_representative_documents <-
  function(obj,
           topic_number = NULL,
           include_labels = T,
           number_words = 5L,
           top_n_documents = NULL,
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
    topic_nos <- names(rep_docs) |> readr::parse_number()
    data <- tibble(
      topic_bert = rep(topic_nos, lengths(rep_docs)),
      text       = unlist(rep_docs, use.names = FALSE)
    ) |>
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
          by = "topic_bert"
        ) |>
        select(topic_bert, label_bertopic, everything())
    }

    data <- data |>
      group_by(topic_bert) |>
      mutate(number_document_topic = 1:n(), .before = "topic_bert") |>
      ungroup()

    if (length(top_n_documents) > 0) {
      data <- data |>
        filter(number_document_topic <= top_n_documents)
    }


    data
  }


# Hierarchy -------------------------------------------------------------------------


#' BERTopic Hierarchy
#'
#' To create this hierarchy, BERTopic needs to be already fitted once. Then, a hierarchy is calculated on the distance matrix of the c-TF-IDF representation using scipy.cluster.hierarchy.linkage.
#'
#' @param obj A BERTopic model object.
#' @param docs Character vector. Vector of documents, or `NULL`.
#' @param linkage_function A Python function for hierarchical clustering, or `NULL` for default (ward linkage).
#' @param distance_function A Python function for distance calculation, or `NULL` for default (1 - cosine similarity).
#' @param tight_layout Logical. If `TRUE` uses tight layout for readability with many topics. Default is `FALSE`.
#' @param print_tree Logical. If `TRUE` prints the topic tree. Default is `FALSE`.
#'
#' @returns A Python hierarchical topics object containing the topic hierarchy structure.
#' @export
#'

bert_topic_hierarchy <-
  function(obj,
           docs = NULL,
           linkage_function = NULL,
           distance_function = NULL,
           tight_layout = F,
           print_tree = FALSE) {
    if (length(docs) == 0) {
      message("No documents")
      return(invisible())
    }

    out <-
      obj$hierarchical_topics(
        docs = docs,
        linkage_function = linkage_function,
        distance_function = distance_function
      )

    if (print_tree) {
      tree <-
        obj$get_topic_tree(hier_topics = out, tight_layout = tight_layout)

      assign("bert_tree", tree, envir = .GlobalEnv)

      tree |> cat(fill = TRUE)
    }

    out
  }

#' Print Bertopic Tree
#'
#' @param obj A BERTopic model object.
#' @param hierarchy Output from `bert_topic_hierarchy`. A hierarchical topics object.
#' @param tight_layout Logical. If `TRUE` uses tight layout for readability with many topics. Default is `FALSE`.
#' @param max_distance Numeric. Maximum distance between topics, or `NULL` for no limit. Default is `NULL`.
#'
#' @returns Called for side effects; prints the topic tree to console.
#' @export
#'

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
#' @param obj A BERTopic model object.
#' @param hierarchy Output from `bert_topic_hierarchy`. A hierarchical topics object.
#' @param tight_layout Logical. If `TRUE` uses tight layout for readability with many topics. Default is `FALSE`.
#' @param max_distance Numeric. Maximum distance between topics, or `NULL` for no limit. Default is `NULL`.
#'
#' @returns Character. A string containing the formatted topic tree structure.
#' @export
#'

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
#' @param obj A BERTopic model object.
#' @param bert_topics Integer vector. Specific topic numbers, or `NULL` for all topics.
#' @param return_full_data Logical. If `TRUE` returns all forms of topic representations including aspects. Default is `FALSE`.
#'
#' @returns A tibble containing topic keywords with columns: `topic_bert`, `label_bertopic`, `word`, `score_c_tfidf`, `length_ngram`, and `is_outlier_bert_topic`.
#' @export
#'

bert_topic_keywords <-
  function(obj,
           bert_topics = NULL,
           return_full_data = FALSE) {
    topics <- obj$get_topics(full = return_full_data)

    if (!return_full_data) {
      all_values_list <- lapply(topics, unlist, use.names = FALSE)
      n_words_per_topic <- lengths(all_values_list) %/% 2L
      topic_numbers <- seq_along(topics) - 1L
      all_flat <- unlist(all_values_list, use.names = FALSE)
      odd_idx  <- seq(1L, length(all_flat), by = 2L)
      even_idx <- seq(2L, length(all_flat), by = 2L)
      word_vec  <- all_flat[odd_idx]
      score_vec <- readr::parse_number(all_flat[even_idx])
      topic_rep <- rep(topic_numbers, n_words_per_topic)
      data <- tibble(
        topic_bert            = topic_rep,
        word                  = word_vec,
        score_c_tfidf         = score_vec,
        length_ngram          = str_count(word_vec, "\\ "),
        is_outlier_bert_topic = topic_rep == -1L
      )
    }

    if (return_full_data) {
      topics_nums <- obj$get_topic_freq(topic = NULL) |> nrow()
      data <-
        1:topics_nums |>
        map_dfr(function(x) {
          topic_number <- x - 2

          aspects <- names(topics)

          dat <-
            aspects |>
            map_dfr(function(a) {
              all_values <- topics[[a]][[x]] |> unlist()
              word <- all_values[c(T, F)]
              score <- all_values[c(F, T)] |> readr::parse_number()
              tibble(aspect = a, word, score_c_tfidf = score) |>
                mutate(
                  topic_bert = topic_number,
                  length_ngram = word |> str_count("\\ ")
                ) |>
                select(topic_bert, everything()) |>
                mutate(is_outlier_bert_topic = topic_bert == -1)
            })

          dat
        })

      data <- data |> filter(word != "")
    }

    data <- data |>
      left_join(obj |> bert_topic_info() |> select(topic_bert, label_bertopic),
        by = "topic_bert"
      ) |>
      select(topic_bert, label_bertopic, everything())

    if (length(bert_topics) > 0) {
      data <- data |> filter(topic_bert %in% bert_topics)
    }



    data
  }
