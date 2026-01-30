# Internal helper functions (not exported)
# These functions are used internally by other bertopic functions

# names -------------------------------------------------------------------

.dict_doc_names <-
  function() {
    tibble(
      name_bertopic = c(
        "Document",
        "Topic",
        "Name",
        "Representation",
        "main",
        "aspect1",
        "aspect2",
        "Representative_Docs",
        "Top_n_words",
        "Probability",
        "Representative_document"
      ),
      name_actual = c(
        "document",
        "topic_bert",
        "name_label_bertopic",
        "representation",
        "main",
        "aspect1",
        "aspect2",
        "representative_documents",
        "top_n_words_label_bertopic",
        "pct_probability_topic_bert",
        "is_bertopic_representative_document"
      )
    )
  }

.resolve_document_info_names <-
  function(data) {
    tbl_names <- .dict_doc_names()
    actual_names <-
      names(data) |>
      map_chr(function(x) {
        df_row <- tbl_names |> filter(name_bertopic == x)
        if (nrow(df_row) == 0) {
          return(janitor::make_clean_names(x))
        }
        df_row$name_actual
      })

    data |> setNames(actual_names)
  }

# aspects -----------------------------------------------------------------

.munge_aspect_column <-
  function(data,
           aspect_column,
           top_n_aspect_words = NULL,
           return_summary = TRUE,
           assign_to_environment = TRUE) {
    aspect_num_slug <-
      aspect_column |>
      str_extract_all("[0-9]") |>
      flatten_chr() |>
      .pad_zeros(number_zeros = 3)

    if (length(aspect_num_slug) > 0) {
      aspect_num_slug <- aspect_num_slug[[1]]
    } else {
      aspect_num_slug <- ""
    }


    other_aspect <-
      aspect_column |>
      str_remove_all("^aspect") |>
      str_remove_all("[0-9]") |>
      str_remove_all("^_")

    if (!data |> hasName(aspect_column)) {
      return(data)
    }

    if (other_aspect == "") {
      dat_slug <-
        glue("data_aspect_{aspect_num_slug}")
      aspect_slug <- aspect_slug
    }

    if (other_aspect != "") {
      dat_slug <-
        glue("data_aspect_{other_aspect}_{aspect_num_slug}")

      aspect_slug <-
        glue("{other_aspect}_{aspect_num_slug}")
    }


    tbl_aspect <-
      data |> select(topic_bert, label_bertopic, !!sym(aspect_column))

    types <- tbl_aspect[[aspect_column]] |>
      map(class) |>
      as.character()

    tbl_aspect <- tbl_aspect |>
      mutate(type = types) |>
      mutate(id = 1:n())

    types <- tbl_aspect$type |> unique()

    tbl_aspect <- types |>
      map_dfr(function(x) {
        if (x == "character") {
          dat <- tbl_aspect |>
            filter(type == x) |>
            select(-type) |>
            unnest()

          dat <- dat |> filter(!(!!sym(aspect_column)) == "")
          return(dat)
        }

        if (x == "list") {
          dat <-
            tbl_aspect |>
            filter(type == x) |>
            select(-type) |>
            unnest() |>
            unnest()

          dat <- dat |> filter(!(!!sym(aspect_column)) == "")
          return(dat)
        }
      })


    tbl_aspect <-
      tbl_aspect |>
      mutate(
        type = !!sym(aspect_column),
        type = type |> map_chr(class)
      )

    no_data <- tbl_aspect |>
      filter(!!sym(aspect_column) != "") |>
      nrow() == 0

    if (no_data) {
      return(NULL)
    }

    has_no_int <-
      tbl_aspect |>
        filter(type %in% c("integer", "numeric")) |>
        unnest() |>
        nrow() == 0

    if (has_no_int) {
      dat <-
        tbl_aspect |>
        filter(!type %in% c("integer", "numeric")) |>
        unnest() |>
        select(-type) |>
        rename(word := !!sym(aspect_column)) |>
        select(-id) |>
        filter(word != "") |>
        group_by(topic_bert) |>
        mutate(number_word = 1:n()) |>
        ungroup() |>
        select(-word, everything())
    }

    if (!has_no_int) {
      dat <-
        tbl_aspect |>
        filter(type %in% c("integer", "numeric")) |>
        unnest() |>
        select(-type) |>
        rename(value := !!sym(aspect_column)) |>
        left_join(
          tbl_aspect |> filter(!type %in% c("integer", "numeric")) |> unnest() |>
            select(-type) |> rename(word := !!sym(aspect_column)),
          by = c("topic_bert", "label_bertopic", "id")
        ) |>
        select(-id) |>
        select(-value, everything()) |>
        filter(word != "")
    }



    if (assign_to_environment) {
      tbl_slug <-
        glue("tbl_bert_aspect_{aspect_slug}")
      assign(tbl_slug, dat, envir = .GlobalEnv)
    }

    if (length(top_n_aspect_words) > 0) {
      dat <- dat |>
        group_by(topic_bert, label_bertopic) |>
        filter(number_word <= top_n_aspect_words) |>
        ungroup()
    }

    if (return_summary) {
      aspect_slug <-
        dat_slug |> str_remove_all("^data_")
      dat <- dat |>
        group_by(topic_bert, label_bertopic) |>
        summarise(UQ(aspect_slug) := word |> str_flatten_comma(last = " and ")) |>
        ungroup()
    }

    if (!return_summary) {
      dat <-
        dat |>
        group_by(topic_bert, label_bertopic) |>
        nest() |>
        rename(UQ(dat_slug) := data) |>
        ungroup()
    }



    dat
  }

.munge_respresentation <-
  function(data,
           assign_to_environment = T,
           return_summary = TRUE,
           top_n_aspect_words = NULL) {
    rep_column <-
      data |>
      select(matches("representation")) |>
      select(-matches("data_representation")) |>
      names()
    if (length(rep_column) == 0) {
      return(data)
    }
    dat <-
      data |>
      select(topic_bert, label_bertopic, !!sym(rep_column)) |>
      unnest() |>
      mutate(id = 1:n()) |>
      unnest() |>
      rename(word := !!sym(rep_column)) |>
      select(-id) |>
      filter(word != "")

    if (assign_to_environment) {
      tbl_slug <-
        glue("tbl_bert_representations")
      assign(tbl_slug, dat, envir = .GlobalEnv)
    }

    if (length(top_n_aspect_words) > 0) {
      dat <- dat |>
        group_by(topic_bert, label_bertopic) |>
        filter(number_word <= top_n_aspect_words) |>
        ungroup()
    }

    if (return_summary) {
      dat <- dat |>
        group_by(topic_bert, label_bertopic) |>
        summarise(representation := word |> str_flatten_comma(last = " and ")) |>
        ungroup()
    }

    if (!return_summary) {
      dat <-
        dat |>
        group_by(topic_bert, label_bertopic) |>
        nest() |>
        rename(data_representation = data) |>
        ungroup()
    }

    dat
  }

# similar -----------------------------------------------------------------

.bert_similar_term_topics <-
  function(obj,
           term = NULL,
           top_n_terms = 10L,
           image = NULL) {
    if (length(term) == 0) {
      "Enter search term" |> message()
      return(invisible())
    }
    similar_topics <-
      obj$find_topics(
        search_term = term,
        top_n = as.integer(top_n_terms),
        image = image
      )

    tibble(
      topic_bert = similar_topics[[1]],
      score_c_tfidf = similar_topics[[2]] |> flatten_dbl()
    ) |>
      mutate(term) |>
      select(term, everything()) |>
      mutate(
        is_outlier_bert_topic = topic_bert == -1,
        is_c_tfidf_over_50 = if_else(score_c_tfidf >= .5, TRUE, FALSE),
        bin_c_tfidf_score = (score_c_tfidf * 100) %/% 10 * 10,
        rounded_c_tfidf_score = (score_c_tfidf * 100) |> round(
          digits =
            -1
        )
      )
  }
