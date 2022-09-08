
# topics ------------------------------------------------------------------


.bert_similar_term_topics <-
  function(topic_model,term = NULL, top_n_terms = 10L){

    if (length(term) == 0) {
      "Enter search term" |> message()
      return(invisible())
    }
    similar_topics <- topic_model$find_topics(search_term = term, top_n = as.integer(top_n_terms))

    tibble(topic = similar_topics[[1]], score = similar_topics[[2]] |> flatten_dbl()) |>
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
  function(topic_model,terms = NULL, top_n_terms = 10L,
           nest_data = F, return_message = T){

    data <-
      terms |>
      map_dfr(function(x){
        if (return_message) {
          glue::glue("Finding {top_n_terms} similar term topic embeddings for {x}") |> message()
        }

        .bert_similar_term_topics(topic_model = topic_model, term = x, top_n_terms = top_n_terms)
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
  function(topic_model, topic_number = NULL){
  df <-
    topic_model$get_topic_info(topic = topic_number)
  tbl_topics <- df |> janitor::clean_names() |> as_tibble()
  tbl_topics |>
    mutate(name = name |> str_replace("\\_", "\\+")) |>
    separate(name, into = c("remove", "name_topic"), sep = "\\+") |>
    select(-remove)
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
  function(topic_model,number_words = 4L, separator = "_", word_length =  NULL) {
  topic_labels <- topic_model$generate_topic_labels(
    nr_words = as.integer(number_words),
    separator = separator,
    topic_prefix = F,
    word_length = word_length
  )

  tibble(topic_labels) |>
    mutate(topic = 1:n() - 2) |>
    select(topic, everything())
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
  function(topic_model, topic_number = NULL){
    topic_model$get_topic_freq(topic = topic_number) |>
      janitor::clean_names() |> as_tibble()

  }

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
  function(viz, base_path = NULL, viz_name = NULL, browse_url = T) {
   oldwd <- getwd()
   setwd("~")

   final_path <- stringr::str_c(base_path, viz_name, sep = "/")
   asbtools::build_folders(paths = final_path)
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
