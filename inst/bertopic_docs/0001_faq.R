#' https://maartengr.github.io/BERTopic/getting_started/quickstart/quickstart.html

library(tidyverse)
# library(bertopic)


# Quick Start -------------------------------------------------------------


sklearn <- import_sklearn()
data <- sklearn$datasets

docs <-
  data$fetch_20newsgroups(subset = 'all',
                          remove = c('headers', 'footers', 'quotes'))
docs
labels <- docs["target_names"]
number_label <- docs[["target"]]

tbl_news_labels <-
  tibble(news_label = labels) |>
  mutate(number_label = 1:n() - 1)

news_docs <- docs["data"] |> stringr::str_trim()

tbl_news_docs <-
  tibble(number_label, document = news_docs) |>
  left_join(tbl_news_labels) |>
  mutate(number_document = 1:n()) |>
  select(number_document, number_label, news_label, everything())

import_sentence_transformers()
SentenceTransformer <-
  sentence_transformers$SentenceTransformer(model_name_or_path = "all-MiniLM-L6-v2")
embeddings <-
  SentenceTransformer$encode(sentences = news_docs, show_progress_bar = T)


# Intiate BT model
topic_model <-
  bert_topic(
    calculate_probabilities = TRUE,
    exclude_stop_words = FALSE,
    vectorizer_model = NULL,
    extra_stop_words = NULL,
    use_key_phrase_vectorizer = FALSE,
    n_gram_range = list(1L, 1L),
    use_sklearn_vectorizer = F
  )

topic_model$get_params()

#' Fit the bertopic model
out <-
  topic_model$fit_transform(documents = news_docs, embeddings = embeddings)

topic_model$get_params()

# embeddings <-
#   topic_model$embedding_model$embed(documents = news_docs, verbose = T) # same as above

# saving ------------------------------------------------------------------
# setwd("~")
# topic_model$save(path = "Desktop/sample_bt_model")


# explore -----------------------------------------------------------------

tbl_info <-
  topic_model |> bert_topic_info()

#' Topic Counts
topic_model |> bert_topic_count()

topic_model |> bert_topic_labels(number_words = 3L)
topic_model |> bert_topic_labels(number_words = 10L, separator = "_")

tbl_bert_keywords <- topic_model |> bert_topics_keywords()

topic_model |> bert_topics_keywords(bert_topics = 0)

topic_model |> bert_document_info(docs = news_docs, document_name = "document")


# visualizations ----------------------------------------------------------

#' https://maartengr.github.io/BERTopic/getting_started/visualization/visualization.html#visualize-term-score-decline

# Visualize Topics

viz <- topic_model$visualize_topics(width = 800, height = 800)
viz |> write_bert_viz()
viz_bar <- topic_model$visualize_barchart()

#' Visualize Documents¶
viz_docs <-
  topic_model$visualize_documents(docs = news_docs,
                                  embeddings = embeddings,
                                  hide_annotations = T)
viz_docs |> write_bert_viz()
import_umap()
um <- umap$UMAP(
  n_neighbors = 10,
  n_components = 2,
  min_dist = 0.0,
  metric = 'cosine'
)

as_tibble(embeddings) |> modelR2::model_uwot(
  n_neighbors = 10,
  n_components = 2,
  metrics = "cosine",
  min_dists = 0.0,

) |> asbviz::hc_xy(x = "umap_001" , y = "umap_002", point_size = 1)

reduced_embeddings <- um$fit_transform(X = embeddings)
tbl_umap <-
  reduced_embeddings |> tbl_array(output_type = 'umap')

viz_docs_reduced <-
  topic_model$visualize_documents(
    docs = news_docs,
    reduced_embeddings = reduced_embeddings,
    hide_annotations = T
  )

viz_docs_reduced |> write_bert_viz()


#' Topic Hierarchy
#'

viz_hier <- topic_model$visualize_hierarchy()
viz_hier |> write_bert_viz()

hierarchical_topics  <-
  topic_model$hierarchical_topics(docs = news_docs)

hierarchical_topics |> as_tibble()

topic_model$visualize_hierarchy(hierarchical_topics = hierarchical_topics) |> write_bert_viz()

topic_model$get_topic_tree(hierarchical_topics, tight_layout = T) |> print()


# Verms
viz_bar <-
  topic_model$visualize_barchart(n_words = 15L,
                                 top_n_topics = 15L,
                                 title = "BERT")
viz_bar |> write_bert_viz()

topic_model |>
  bertopic::bert_topics_keywords() |>
  arrange(topic_bert, desc(score_c_tfidf)) |>
  group_by(topic_bert) |>
  slice(1:15) |>
  ungroup()

#' Visualize Topic Similarity

viz_sim_hm <-
  topic_model$visualize_heatmap(
    topics = NULL,
    topics = NULL,
    width = 800,
    height = 800
  )

viz_sim_hm |> write_bert_viz()
topic_model$visualize_heatmap(
  topics = NULL,
  topics = NULL,
  width = 800,
  height = 800,
  n_clusters = 4
) |> write_bert_viz()

#' Visualize Term Score Decline
#' Topics are represented by a number of words starting with the best representative word. Each word is represented by a c-TF-IDF score. The higher the score, the more representative a word to the topic is. Since the topic words are sorted by their c-TF-IDF score, the scores slowly decline with each word that is added. At some point adding words to the topic representation only marginally increases the total c-TF-IDF score and would not be beneficial for its representation.
#'
#' To visualize this effect, we can plot the c-TF-IDF scores for each topic by the term rank of each word. In other words, the position of the words (term rank), where the words with the highest c-TF-IDF score will have a rank of 1, will be put on the x-axis. Whereas the y-axis will be populated by the c-TF-IDF scores. The result is a visualization that shows you the decline of c-TF-IDF score when adding words to the topic representation. It allows you, using the elbow method, the select the best number of words in a topic.
#'
#' To visualize the c-TF-IDF score decline, run the following:

topic_model$visualize_term_rank(topics = NULL, log_scale = FALSE) |> write_bert_viz()
topic_model$visualize_term_rank(topics = list(1L, 10L, 2L), log_scale = FALSE) |> write_bert_viz()
topic_model$visualize_term_rank(topics = list(1L, 10L, 2L), log_scale = T) |> write_bert_viz()

# Visualize Topics over Time

tbl_trump_tweets <-
  "https://drive.google.com/uc?export=download&id=1xRKHaP-QwACMydlDnyFPEaFdtskJuBa6" |> arrow::read_csv_arrow()

logical_cols <-
  tbl_trump_tweets |> select(matches("^is")) |> names()
tbl_trump_tweets <-
  tbl_trump_tweets |> mutate_at(logical_cols, list(function(x) {
    if_else(x == "f", FALSE, TRUE)
  }))

clean_tweets <- function(x) {
  x %>%
    # Remove URLs
    str_remove_all(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)") %>%
    # Remove mentions e.g. "@my_account"
    str_remove_all("@[[:alnum:]_]{4,}") %>%
    # Remove hashtags
    str_remove_all("#[[:alnum:]_]+") %>%
    # Replace "&" character reference with "and"
    str_replace_all("&amp;", "and") %>%
    # Remove puntucation, using a standard character class
    str_remove_all("[[:punct:]]") %>%
    # Remove "RT: " from beginning of retweets
    str_remove_all("^RT:? ") %>%
    # Replace any newline characters with a space
    str_replace_all("\\\n", " ") %>%
    # Make everything lowercase
    str_to_lower() %>%
    # Remove any trailing whitespace around the text
    str_trim("both")
}

# tbl_trump_tweets |>
#   mutate(
#     text = str_to_lower(text) |> str_remove_all(
#       regex("[A-Za-z]{1,5}[.][A-Za-z]{2,3}/[A-Za-z0-9]+\\b", TRUE)
#     ) |>
#       str_remove_all(regex("^https", TRUE)) |>
#       str_remove_all("@[[:alnum:]_]{4,}") %>%
#       str_remove_all("^RT:? ") %>%
#       str_remove_all("^rt:? ") %>%
#       str_replace_all(regex("[^a-zA-Z]", TRUE), " ") |> str_squish()
#   ) |>
#   filter(!isRetweet, text != "")

tbl_trump_tweets <-
  tbl_trump_tweets |> mutate_at("text" , clean_tweets) |> filter(!isRetweet, text != "")

trump_tweet_times <- tbl_trump_tweets$date
tweets <- tbl_trump_tweets$text

trump_topic_model <- bert_topic(calculate_probabilities = T)
out <- trump_topic_model$fit_transform(documents = tweets)

topics_over_time <-
  trump_topic_model$topics_over_time(docs = tweets, timestamps = trump_tweet_times)

trump_topic_model$visualize_topics_over_time(topics_over_time = topics_over_time) |> write_bert_viz()
trump_topic_model$visualize_topics_over_time(topics_over_time = topics_over_time,
                                             topics = c(9, 10, 72, 83, 87, 91)) |> write_bert_viz()
trump_topic_model |> bert_topic_info()

tbl_trump_over_time <-
  munge_bert_topics_over_time(topics_over_time, topic_model = trump_topic_model)

tbl_trump_over_time <- tbl_trump_over_time |>
  mutate(year = lubridate::year(date))

tbl_trump_over_time |>
  asbtools::tbl_dates_quarter_year(date_columns = "date") |>
  filter(!is_outlier_bert_topic) |>
  asbtools::tbl_summarise(
    group_variables = c("year_quarter_date_calendar", "topic_bert", "name_topic"),
    max_variables = "date",
    amount_variables = "count"
  ) |>
  arrange(desc(year_quarter_date_calendar)) |>
  asbviz::tbl_ordered_factor(columns = "name_topic",
                             weight = "count_total",
                             reverse = F) |>
  filter(count > 5) |>
  asbviz::hc_xy(
    x = "date_max",
    y = "count_total",
    group = "name_topic",
    type = "scatter",
    transformations = "log_y",
    color_palette = "pals::parula",
    title = "Trump BERTopic Tweets by Topic by Quarter",
    theme_name = "better_unica"
  )


tbl_trump_over_time |>
  asbtools::tbl_dates_quarter_year(date_columns = "date") |>
  filter(!is_outlier_bert_topic) |>
  asbtools::tbl_summarise(
    group_variables = c("year_quarter_date_calendar", "topic_bert", "name_topic"),
    max_variables = "date",
    amount_variables = "count"
  ) |>
  arrange(desc(year_quarter_date_calendar)) |>
  asbviz::tbl_ordered_factor(columns = "name_topic",
                             weight = "count_total",
                             reverse = F) |>
  filter(count > 5) |>
  asbviz::hc_xy(
    x = "date_max",
    y = "count_total",
    selector_variables  = "name_topic",
    type = "scatter",
    transformations = "log_y",
    color_palette = "pals::parula",
    title = "Trump BERTopic Tweets by Topic by Quarter",
    theme_name = "better_unica"
  )

tbl_trump_tweets <- tbl_trump_tweets |> mutate(year = lubridate::year(date)) |> arrange(date)

topics_per_class <-
  topic_model$topics_per_class(docs = tbl_trump_tweets$text, classes =
                                 tbl_trump_tweets$year)

topic_model$visualize_topics_per_class(topics_per_class, topics = c(11:25)) |> write_bert_viz()


tbl_trump_topics_per_class <- topics_per_class |> munge_bert_topics_per_class(class_name = "year", topic_model = topic_model)


#' Visualize Probablities or Distribution


appx_dist <- topic_model$approximate_distribution(documents = tweets, min_similarity = 0, calculate_tokens = F)

munge_bert_document_approximate_distributions(data = appx_dist)

appx_dist <- topic_model$approximate_distribution(documents = tweets, min_similarity = 0, calculate_tokens = F)


# reduce_topics -----------------------------------------------------------

topic_model$reduce_topics(docs = news_docs, nr_topics = 30L)
topic_model |> bert_topic_info() |> View()
topic_model$topics_


# topic_representations ---------------------------------------------------

#' https://maartengr.github.io/BERTopic/getting_started/topicrepresentation/topicrepresentation.html#visualize-probablities-or-distribution

topic_model$update_topics(docs = news_docs, n_gram_range = list_to_tuple(list(1L, 3L)))
topic_model |> bert_topic_info()

vm <- sklearn_vectorizer(ngram_range = list(1L, 5L))
topic_model$update_topics(docs = news_docs, vectorizer_model = vm)
topic_model |> bert_topic_info()

#' custom labels
#'

new_labels <- topic_model$generate_topic_labels(
  nr_words = 5L,
  topic_prefix = FALSE,
  word_length = 200L,
  separator = ", "
)
topic_model$set_topic_labels(topic_labels = new_labels)
topic_model |> bert_topic_info()

topic_model$visualize_barchart(custom_labels = TRUE) |> write_bert_viz()
transformers <- import_transformers()
classifier <- transformers$pipeline("zero-shot-classification", model = "facebook/bart-large-mnli")
sequence_to_classify <- topic_model |> bert_topics_keywords() |> filter(topic_bert == 2) |> pull(word)
candidate_labels = list('cooking', 'dancing', 'religion')
classifier(sequence_to_classify, candidate_labels)


# find_topics -------------------------------------------------------------
out_sim <- topic_model$find_topics("china", top_n=5L)
topic_model$get_topic(topic = out_sim[[1]][1])

bert_similar_terms_topics(topic_model = topic_model, c("china", "fraud"))
