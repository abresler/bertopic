# Document handling functions

#' BERTopic Documents
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents.
#' @param document_name Character. If not `NULL`, name for the text document column.
#' @param exclude_list_columns Logical. If `TRUE`, excludes list columns from output. Default `TRUE`.
#'
#' @returns A tibble with document information including topic assignments and representative documents.
#' @export
#'

bert_document_info <-
  function(obj,
           docs,
           document_name = NULL,
           exclude_list_columns = T,
           assign_to_environment = TRUE) {
    data <- obj$get_document_info(docs = docs) |> as_tibble()
    data <- .resolve_document_info_names(data = data)

    tbl_info <-
      obj |>
      bert_topic_info(
        assign_to_environment = assign_to_environment,
        remove_list_columns = TRUE
      ) |>
      select(-count)

    if (data |> hasName("representation") & tbl_info |> hasName("representation")) {
      data <- data |> select(-representation)
    }

    join_cols <- names(tbl_info)[names(tbl_info) %in% names(data)]

    data <-
      data |>
      left_join(tbl_info, by = join_cols) |>
      select(-name_label_bertopic) |>
      select(document, topic_bert, label_bertopic, everything())

    # tbl_rep <-
    #   data |> .munge_respresentation()
    #


    if (data |> hasName("representative_documents") &
      !data |> hasName("text_representative_documents")) {
      tbl_docs <-
        data |>
        select(topic_bert, label_bertopic, representative_documents) |>
        unnest() |>
        group_by(topic_bert, label_bertopic) |>
        summarise(
          text_representative_documents = unique(representative_documents) |> str_squish() |> str_c(collapse = ".  ")
        ) |>
        ungroup()

      data <-
        data |>
        select(-representative_documents) |>
        left_join(tbl_docs, by = c("topic_bert", "label_bertopic"))
    }

    if (length(document_name) > 0) {
      data <- data |>
        rename(UQ(document_name) := document)
    }

    list_cols <- data |>
      select_if(is.list) |>
      names()

    if (length(list_cols) > 0 & exclude_list_columns) {
      data <- data |>
        select(-one_of(list_cols))
    }

    if (data |> hasName("custom_name")) {
      data <- data |>
        rename(label_bertopic_custom = custom_name)
    }

    data
  }


#' Transform New Documents
#'
#' Transform new documents using an existing BERTopic model to predict topics.
#'
#' @param obj BERTopic model object.
#' @param documents Character vector of documents to transform.
#' @param embeddings If not NULL, matrix or dataframe of pre-computed embeddings.
#'
#' @returns A tibble with topic assignments and info for each document.
#' @export
#'
#' @examples
#' \dontrun{
#' new_docs <- c("New document about machine learning")
#' results <- bert_transform_documents(obj = tm, documents = new_docs)
#' }
bert_transform_documents <-
  function(obj,
           documents = NULL,
           embeddings = NULL) {
    out <- obj$transform(documents = documents, embeddings = embeddings)

    topic_bert <- out[[1]] |>
      unlist() |>
      as.numeric()
    data <-
      tibble(topic_bert, document = documents) |>
      left_join(obj |> bert_topic_info(), by = "topic_bert")

    data
  }

#' Transform Documents from Tibble
#'
#' @param data A tibble or data frame containing documents to transform.
#' @param topic_model BERTopic model object.
#' @param document_name Character. Name of the column containing documents.
#' @param embeddings Logical or matrix. Pre-computed embeddings. If `TRUE`, computes embeddings. Default `TRUE`.
#'
#' @returns A tibble containing original data with added topic assignments and related information.
#' @export
#'

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
      bert_transform_documents(
        obj = topic_model,
        documents = data[[document_name]],
        embeddings = embeddings
      ) |>
      rename(UQ(document_name) := document)

    data <-
      data |>
      left_join(dat, by = document_name)

    data
  }

#' Topics Per Structured Class
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents.
#' @param classes Character vector of class assignments, one per document.
#' @param global_tuning Logical. Fine-tune each topic representation for class by averaging its c-TF-IDF matrix with the global c-TF-IDF matrix. Default `TRUE`.
#'
#' @returns A tibble with topic assignments per class, including `topic_bert`, `label_bertopic`, `class`, and word representations.
#' @export
#'

bert_topic_per_class <-
  function(obj,
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
      obj$topics_per_class(
        docs = docs,
        classes = classes,
        global_tuning = global_tuning
      )

    dat <-
      as_tibble(dat)

    dat <-
      dat |> setNames(c("topic_bert", "top_words", "count", "class"))
    dat <-
      dat |>
      left_join(
        bert_topic_info(obj = obj) |>
          select(topic_bert, label_bertopic, matches("aspect")),
        by = "topic_bert"
      ) |>
      select(topic_bert, label_bertopic, everything()) |>
      arrange(desc(class), topic_bert)

    dat
  }

#' Find Topics from Class via Tibble
#'
#' @param data A tibble or data frame containing documents and class assignments.
#' @param topic_model BERTopic model object.
#' @param document_name Character. Name of the text feature column.
#' @param class_name Character vector. Name(s) of the class feature column(s).
#' @param global_tuning Logical. Fine-tune each topic representation for class by averaging its c-TF-IDF matrix with the global c-TF-IDF matrix. Default `TRUE`.
#'
#' @returns A tibble with topic assignments per class, including `topic_bert`, `label_bertopic`, class columns, and word representations.
#' @export
#'

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

    data <- data |> tidyr::unite(
      col = "class",
      all_of(class_name),
      sep = "@",
      remove = F
    )

    dat <-
      bert_topic_per_class(
        obj = topic_model,
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
      select(
        topic_bert,
        label_bertopic,
        matches("class_name"),
        everything()
      ) |>
      mutate_at(class_name, list(function(x) {
        case_when(
          is.na(x) ~ NA_character_,
          TRUE ~ as.character(x)
        )
      }))

    if (sort_by_topic) {
      dat <- dat |>
        arrange(label_bertopic, desc(count))
    }

    dat
  }

#' Extract BERTopic Assignments from Output
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents. If `NULL`, returns only topic probabilities.
#' @param id_columns Character vector. Column names to parse from document names/IDs.
#' @param text_column Character. Name for the text column in output. If `NULL`, defaults to `text`.
#' @param include_labels Logical. If `TRUE`, includes topic labels in output. Default `TRUE`.
#' @param number_words Integer. Number of words per topic label. Default `4L`.
#' @param arrange_topics Logical. If `TRUE`, arranges output by topic and probability. Default `FALSE`.
#'
#' @returns A tibble with columns: `topic_bert`, `pct_probability_topic_bert`, and optional `label_bertopic` and text columns.
#' @export
#'

extract_bert_topics <-
  function(obj,
           docs = NULL,
           id_columns = NULL,
           text_column = NULL,
           include_labels = T,
           number_words = 4L,
           arrange_topics = F) {
    topics <-
      obj$topics_ |> as.integer()

    tbl_prob <-
      obj$probabilities_ |>
      as_tibble() |>
      janitor::clean_names() |>
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
          convert = T
        )
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
      tbl_labels <- bert_topic_labels(
        obj = obj,
        number_words = as.integer(number_words)
      )
      data <-
        data |>
        left_join(tbl_labels,
          by = "topic_bert"
        ) |>
        select(topic_bert, label_bertopic, everything())
    }

    if (arrange_topics) {
      data <- data |>
        arrange(topic_bert, desc(pct_probability_topic_bert))
    }


    data
  }

#' Pull Text and Name It for BERTopic
#'
#' @param data A tibble or data frame containing text and ID columns.
#' @param id_columns Character vector. Column names to use for creating unique IDs.
#' @param text_column Character. Name of the column containing text.
#'
#' @returns A named character vector where names are IDs and values are text documents.
#' @export
#'

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
