# Embeddings, similarity, time series, and visualization functions

# tuple -------------------------------------------------------------------

#' List to Tuple
#'
#' @param obj List object to convert to a Python tuple.
#'
#' @returns A Python tuple object.
#' @export
#'

list_to_tuple <-
  function(obj) {
    reticulate::tuple(obj)
  }

#' Unite Features
#'
#' @param data A tibble to unite features in.
#' @param unite_columns Character vector of column names to unite.
#' @param new_column Character. Name of new united column. If `NULL`, concatenates the column names.
#' @param sep Character. Separator between united values. Default `"@"`.
#' @param remove Logical. If `TRUE`, removes original columns. Default `FALSE`.
#'
#' @returns A tibble with united columns. If `to_factor = TRUE`, also includes an `id_*` column with numeric encoding.
#' @export
#'
#' @examples
#' library(tidyverse)
#' tbl_unite_features(diamonds, unite_columns = c("cut", "color", "clarity"))
tbl_unite_features <-
  function(data,
           unite_columns = NULL,
           new_column = NULL,
           sep = "@",
           to_factor = TRUE,
           remove = F) {
    if (length(unite_columns) == 0) {
      return(data)
    }

    if (length(new_column) == 0) {
      new_column <-
        unite_columns |> str_c(collapse = "_")
    }
    data <-
      data |> tidyr::unite(
        col = UQ(new_column),
        all_of(unite_columns),
        sep = sep,
        remove = remove
      )

    if (to_factor) {
      data <- data |>
        mutate_at(new_column, as.factor)

      new_id <- glue("id_{new_column}")

      data <- data |>
        mutate(UQ(new_id) := as.numeric(!!sym(new_column)), .before = new_column)
    }

    data
  }

# similar -----------------------------------------------------------------

#' Find topics most similar to Search Terms
#'
#' @param obj BERTopic model object.
#' @param terms Character vector of search terms to find similar topics for.
#' @param top_n_terms Integer. Number of most similar topics to return per term. Default `10L`.
#' @param nest_data Logical. If `TRUE`, nests data by term. Default `FALSE`.
#' @param return_message Logical. If `TRUE`, prints processing messages. Default `TRUE`.
#'
#' @returns A tibble with columns: `term`, `topic_bert`, `similarity`, `is_outlier_bert_topic`, and topic information joined from `bert_topic_info()`.
#' @export
#'

bert_similar_terms_topics <-
  function(obj,
           terms = NULL,
           top_n_terms = 10L,
           nest_data = F,
           image = NULL,
           return_message = T) {
    data <-
      terms |>
      map_dfr(function(x) {
        if (return_message) {
          glue::glue("Finding {top_n_terms} similar term topic embeddings for {x}") |> message()
        }

        .bert_similar_term_topics(
          obj = obj,
          term = x,
          top_n_terms = top_n_terms,
          image = image
        )
      })
    tbl_count <-
      obj |>
      bert_topic_info(remove_list_columns = T) |>
      rename(count_documents_in_topic = count)
    if (nrow(tbl_count) == 0) {
      rlang::abort("bert_topic_info() returned no rows. Model may not be fitted yet.")
    }
    data <- data |>
      left_join(tbl_count,
        by = c("topic_bert", "is_outlier_bert_topic")
      ) |>
      select(term, topic_bert, names(tbl_count), everything())


    if (nest_data) {
      data |>
        group_by(term) |>
        nest() |>
        ungroup()
    }

    data
  }

# embeddings --------------------------------------------------------------

#' Bert Embeddings
#'
#' @param embeddings Numeric matrix or array of embeddings.
#' @param docs Character vector of documents corresponding to embeddings. Default `NULL`.
#' @param id_columns Character vector. Names to create from document IDs. Default `NULL`.
#' @param text_column Character. Name for the text/document column. Default `NULL`.
#'
#' @returns A tibble with embeddings converted to numeric columns, optionally with ID and text columns.
#' @export
#'

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
          convert = T
        )
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
#' @param obj BERTopic model object with UMAP embeddings.
#' @param data Optional tibble to bind UMAP coordinates to. Default `NULL`.
#'
#' @returns A tibble with UMAP coordinates (columns named `umap_00`, `umap_01`, etc.), `pct_dbscan_prob`, and optionally bound to the input data.
#' @export
#'

extract_bert_umap <-
  function(obj,
           data = NULL,
           number_zeros = 4) {
    if (length(obj$umap_model) == 0 || !reticulate::py_has_attr(obj$umap_model, "embedding_")) {
      rlang::abort("UMAP model has no `embedding_`. Fit the topic model before calling extract_bert_umap().")
    }
    df_umap <-
      obj$umap_model$embedding_ |> as_tibble()
    dims <- 1:ncol(df_umap) |> .pz(number_zeros = number_zeros)

    df_umap <-
      df_umap |> setNames(glue::glue("umap_{dims}"))

    pct_dbscan_prob <- obj$hdbscan_model$outlier_scores_ |> as.numeric()

    df_umap <- df_umap |>
      mutate(pct_dbscan_prob) |>
      mutate(id = row_number()) |>
      select(id, everything())

    if (length(data) == 0) {
      return(df_umap)
    }

    if (nrow(data) != nrow(df_umap)) {
      return(df_umap)
    }

    data |>
      bind_cols(df_umap)
  }


# visualization_data_inputs -----------------------------------------------


#' Extract UMAP for Visualization
#'
#' @param obj BERTopic model object.
#' @param n_components Integer. Number of UMAP dimensions. Default `2L`.
#' @param metric Character. Distance metric for UMAP. Default `"cosine"`.
#' @param random_state Integer. Random seed for reproducibility. Default `42L`.
#' @param min_dist Numeric. Minimum distance parameter for UMAP. Default `0.1`.
#' @param learning_rate Integer. Learning rate for UMAP. Default `1L`.
#' @param exclude_outlier Logical. If `TRUE`, excludes outlier topic. Default `FALSE`.
#'
#' @returns A tibble with topic frequencies and UMAP coordinates for visualization.
#' @export
#'

tbl_bert_umap_label_level <-
  function(obj,
           exclude_outlier = FALSE,
           n_components = 2L,
           metric = "cosine",
           random_state = 42L,
           min_dist = .1,
           learning_rate = 1L) {
    freq_df <- obj |> bert_topic_count()


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
      tbl_array(output_type = "umap") |>
      mutate(topic_bert = 1:n() - 2)


    freq_df <-
      freq_df |>
      left_join(reduced_embeddings, by = "topic_bert") |>
      select(-count, everything())

    freq_df
  }

#' BERT Cosine Similarity
#'
#' @param obj BERTopic model object.
#' @param exclude_outlier Logical. If `TRUE`, excludes outlier topic. Default `FALSE`.
#' @param include_topic_number Logical. If `TRUE`, includes topic number in feature names. Default `TRUE`.
#' @param return_tibble Logical. If `TRUE`, returns tibble; otherwise returns matrix. Default `FALSE`.
#'
#' @returns A tibble (if `return_tibble = TRUE`) or matrix with cosine similarity scores between topics, with columns: `feature_01`, `feature_02` (or separated into topic and label columns), and `cosine_similarity`.
#' @export
#'

tbl_bert_label_cosine_similarity <-
  function(obj,
           exclude_outlier = FALSE,
           include_topic_number = TRUE,
           distinct_matches = FALSE,
           separate_data = T,
           return_tibble = F) {
    freq_df <- obj |> bert_topic_count()
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
        rename(
          label_bertopic_01 = feature_01,
          label_bertopic_02 = feature_02
        )
    }

    if (distinct_matches) {
      data <- data |>
        mutate(label = glue({
          "{label_bertopic_01}|{label_bertopic_02}"
        })) |>
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

#' Extract Document Word Counts
#'
#' Extract word counts from documents using the vectorizer model.
#'
#' @param obj BERTopic model object with vectorizer_model.
#' @param docs Character vector of documents to analyze.
#' @param filter_zero Logical. If `TRUE`, filters out zero counts. Default `TRUE`.
#'
#' @returns A tibble with columns: `number_document`, `word`, and `count`.
#' @export
#'
#' @examples
#' \dontrun{
#' word_counts <- extract_document_word_counts(tm, docs)
#' }
extract_document_word_counts <-
  function(obj,
           docs = NULL,
           filter_zero = T) {
    if (length(docs) == 0) {
      stop("Enter Docs")
    }
    X <- obj$vectorizer_model$fit_transform(docs) |> as.matrix()

    X <-
      X |>
      as_tibble() |>
      setNames(obj$vectorizer_model$get_feature_names()) |>
      mutate(number_document = 1:n()) |>
      gather(word, count, -number_document, na.rm = T) |>
      arrange(number_document, desc(count))

    if (filter_zero) {
      X <-
        X |>
        filter(count > 0)
    }

    X
  }

# viz ---------------------------------------------------------------------

#' Write a BERT Visualization
#'
#' Write a BERTopic visualization to HTML file.
#'
#' @param viz BERTopic visualization object.
#' @param base_path Character. Base directory path for output. If `NULL`, shows in browser. Default `NULL`.
#' @param viz_name Character. Name for the visualization folder.
#' @param browse_url Logical. If `TRUE`, opens in browser after writing. Default `TRUE`.
#'
#' @returns Called for side effects (writes HTML file); returns invisible `NULL`.
#' @export
#'
#' @examples
#' \dontrun{
#' viz <- tm$visualize_topics()
#' write_bert_viz(viz, "~/visualizations", "topics")
#' }
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


#' Convert Array to Tibble
#'
#' Convert a matrix or array to a tibble with optional column naming.
#'
#' @param data Matrix or array to convert.
#' @param output_type Character. Prefix for column names. If `NULL`, uses default names. Default `NULL`.
#' @param number_zeros Integer. Padding for zeros in column names. Default `3`.
#'
#' @returns A tibble with the converted array data and optionally prefixed column names.
#' @export
#'
#' @examples
#' \dontrun{
#' mat <- matrix(1:6, nrow = 2)
#' tbl <- tbl_array(mat, output_type = "col")
#' }
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


#' Extract Topics Over Time
#'
#' Analyze how topics change over time using timestamps.
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents.
#' @param timestamps Vector of timestamps corresponding to documents.
#' @param nr_bins Integer. Number of time bins. Default `NULL`.
#' @param datetime_format Character. Format string for datetime. Default `NULL`.
#' @param evolution_tuning Logical. Enable evolution tuning. Default `TRUE`.
#' @param global_tuning Logical. Enable global tuning. Default `TRUE`.
#' @param return_tibble Logical. If `TRUE`, returns tibble format. Default `TRUE`.
#'
#' @returns Topics over time data, as tibble if `return_tibble = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' topics_time <- bert_topics_over_time(tm, docs, timestamps)
#' }
bert_topics_over_time <-
  function(obj,
           docs = NULL,
           timestamps = NULL,
           nr_bins = NULL,
           datetime_format = NULL,
           evolution_tuning = TRUE,
           global_tuning = TRUE,
           return_tibble = TRUE) {
    if (length(docs) == 0) {
      stop("Enter Documents")
    }

    if (length(timestamps) == 0) {
      stop("Enter timestamps")
    }

    if (length(nr_bins) > 0) {
      nr_bins <- as.integer(nr_bins)
    }

    out <- obj$topics_over_time(
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
#' Get Topics Over Time from a data frame input.
#'
#' Analyze how topics evolve over time from a data frame input.
#'
#' @param data Data frame containing documents and timestamps.
#' @param topic_model BERTopic model object.
#' @param document_name Character. Name of the column containing documents.
#' @param time_feature Character. Name of the column containing timestamps.
#' @param nr_bins Integer. Number of time bins. Default `NULL`.
#' @param datetime_format Character. Format string for datetime parsing. Default `NULL`.
#' @param evolution_tuning Logical. Enable evolution tuning. Default `TRUE`.
#' @param global_tuning Logical. Enable global tuning. Default `TRUE`.
#' @param return_tibble Logical. If `TRUE`, returns tibble format. Default `TRUE`.
#'
#' @returns A tibble with topics over time analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' topics_time <- tbl_bert_topics_over_time(data, tm, "text", "date")
#' }
tbl_bert_topics_over_time <-
  function(data,
           topic_model,
           document_name = NULL,
           time_feature = NULL,
           nr_bins = NULL,
           datetime_format = NULL,
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
#' Transform topics over time output into a tidy tibble format.
#'
#' @param data Output from bert_topics_over_time function.
#' @param topic_model BERTopic model object to add topic labels. Default `NULL`.
#'
#' @returns A tibble with cleaned topics over time data, with columns: `topic_bert`, `words`, `count`, `date_time`, `date`, and optionally topic labels from the model.
#' @export
#'
#' @examples
#' \dontrun{
#' tidy_time <- munge_bert_topics_over_time(topics_time_data, tm)
#' }
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
#' Transform topics per class output into a tidy tibble format.
#'
#' @param data Output from bert_topic_per_class function.
#' @param class_name Character. Custom name for the class column. Default `NULL`.
#' @param topic_model BERTopic model object to add topic labels. Default `NULL`.
#'
#' @returns A tibble with cleaned topics per class data, with columns: `topic_bert`, `words`, `count`, `class` (or custom named column), and optionally topic labels from the model.
#' @export
#'
#' @examples
#' \dontrun{
#' tidy_class <- munge_bert_topics_per_class(class_data, "category", tm)
#' }
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

#' Approximate Topic Distribution
#'
#' A post-hoc approximation of topic distributions across documents.
#'
#' In order to perform this approximation, each document is split into tokens according to the provided tokenizer in the CountVectorizer. Then, a sliding window is applied on each document creating subsets of the document. For example, with a window size of 3 and stride of 1, the sentence: "I love pizza" becomes ["I love", "love pizza"].
#'
#' @param obj BERTopic model object.
#' @param docs Character vector of documents.
#' @param window Integer. Size of the moving window which indicates the number of tokens being considered. Default `4`.
#' @param stride Integer. How far the window should move at each step. Default `1`.
#' @param min_similarity Numeric. The minimum similarity of a document's tokenset with respect to the topics. Default `0.1`.
#' @param batch_size Integer. The number of documents to process at a time. If `NULL`, all documents are processed at once. NOTE: With a large number of documents, it is not advised to process all documents at once. Default `1000`.
#' @param padding Logical. Whether to pad the beginning and ending of a document with empty tokens. Default `FALSE`.
#' @param use_embedding_model Logical. Whether to use the topic model's embedding model to calculate the similarity between tokensets and topics instead of using c-TF-IDF. Default `FALSE`.
#' @param calculate_tokens Logical. If `TRUE`, calculates the similarity of tokens with all topics. NOTE: This is computation-wise more expensive and can require more memory. Default `FALSE`.
#' @param separator Character. The separator used to merge tokens into tokensets. Default `" "`.
#'
#' @returns A list containing approximate topic distributions for documents.
#' @export
#'
#' @examples
#' \dontrun{
#' distributions <- bert_approximate_distribution(obj = tm, docs = documents)
#' }
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
           separator = " ") {
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


#' Munge Document Topic Probability Distributions
#'
#' Transforms approximate distribution output into a tidy tibble format.
#'
#' @param out Approximate distribution output from bert_approximate_distribution.
#' @param obj BERTopic model object. If provided, adds topic labels to output. Default `NULL`.
#' @param return_wide Logical. If `TRUE`, returns wide format; otherwise returns long format. Default `FALSE`.
#'
#' @returns A tibble with document topic probability distributions in long or wide format. Long format has columns: `number_document`, `label_topic_bert`, `pct_probability_topic_bert`. Wide format has columns for each topic.
#' @export
#'
#' @examples
#' \dontrun{
#' distributions <- bert_approximate_distribution(obj = tm, docs = docs)
#' tidy_dist <- munge_bert_document_approximate_distributions(distributions, obj = tm)
#' }
munge_bert_document_approximate_distributions <-
  function(out,
           obj = NULL,
           return_wide = F) {
    options(scipen = 999)
    data <- out[[1]] |> as_tibble()
    topic_number <- 1:ncol(data) - 1
    data <-
      data |>
      setNames(str_c("topic_bert_", topic_number)) |>
      mutate(number_document = 1:n()) |>
      select(number_document, everything())

    if (length(obj) > 0) {
      actual_topic_names <-
        obj |>
        bert_topic_info() |>
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
      gather(
        label_topic_bert,
        pct_probability_topic_bert,
        -number_document
      ) |>
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
