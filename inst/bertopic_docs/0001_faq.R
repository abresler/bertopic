#' https://maartengr.github.io/BERTopic/getting_started/quickstart/quickstart.html

library(tidyverse)
library(bertopic)


# Quick Start -------------------------------------------------------------

bertopic <- import_bertopic()
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



# Intiate BT model
topic_model <-
  bert_topic(
    calculate_probabilities = TRUE,
    exclude_stop_words = FALSE,
    vectorizer_model = NULL,
    extra_stop_words = NULL,
    use_key_phrase_vectorizer = FALSE,
    n_gram_range = list(1L, 1L),
    use_sklearn_vectorizer = F,
    ctfidf_model = ctfidf(bm25_weighting = TRUE, reduce_frequent_words = TRUE),
    representation_model = mmr_inspired_representation(diversity = .75)
  )

topic_model |> bert_parameters(deep = F)

out <-
  topic_model$fit_transform(documents = news_docs)



embeddings <-
  topic_model$embedding_model$embed(documents = news_docs, verbose = T) # same as above

#' Fit the bertopic model
# out <-
#   topic_model$fit_transform(documents = news_docs, embeddings = embeddings) # similar topcis wont work with custom embeddings



# saving ------------------------------------------------------------------
# setwd("~")
# topic_model$save(path = "Desktop/sample_bt_model")


# explore -----------------------------------------------------------------

tbl_info <-
  topic_model |> bert_topic_info()

#' Topic Counts
topic_model |> bert_topic_count()

topic_model |> bert_topic_labels(number_words = 3L)
topic_model |> bert_topic_labels(number_words = 3L, word_length = 1000)
topic_model |> bert_topic_labels(number_words = 10L, separator = "_")

tbl_bert_keywords <- topic_model |> bert_topics_keywords()

news_hier <-
  topic_model |> bert_topic_hierarchy(docs = tbl_news_docs$document)


print_bert_topic_tree(obj = topic_model, hierarchy = news_hier, tight_layout = TRUE, max_distance = NULL)

print_bert_topic_tree(
  obj = topic_model,
  hierarchy = news_hier,
  tight_layout = TRUE,
  max_distance = 1
)

topic_model |> bert_topic_keywords(bert_topics = 0)




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

topic_model$get_topic_tree(news_hier, tight_layout = T) |> cat(fill = T)


# Verms
viz_bar <-
  topic_model$visualize_barchart(n_words = 15L,
                                 top_n_topics = 15L,
                                 title = "BERT")
viz_bar |> write_bert_viz()

topic_model |>
  bert_topic_keywords()|>
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
  tbl_dates_quarter_year(date_columns = "date") |>
  filter(!is_outlier_bert_topic) |>
  tbl_summarise(
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
  tbl_dates_quarter_year(date_columns = "date") |>
  filter(!is_outlier_bert_topic) |>
  tbl_summarise(
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

tbl_trump_tweets <-
  tbl_trump_tweets |> mutate(year = lubridate::year(date)) |> arrange(date)

topics_per_class <-
  topic_model$topics_per_class(docs = tbl_trump_tweets$text, classes =
                                 tbl_trump_tweets$year)

topic_model$visualize_topics_per_class(topics_per_class, topics = c(11:25)) |> write_bert_viz()


tbl_trump_topics_per_class <-
  topics_per_class |> munge_bert_topics_per_class(class_name = "year", topic_model = topic_model)


#' Visualize Probablities or Distribution


appx_dist <-
  topic_model |>
  bert_approximate_distribution(
    docs = tweets,
    min_similarity = 0,
    calculate_tokens = F
  )

munge_bert_document_approximate_distributions(data = appx_dist)

appx_dist <-
  topic_model$approximate_distribution(
    documents = tweets,
    min_similarity = 0,
    calculate_tokens = F
  )


# reduce_topics -----------------------------------------------------------

topic_model$reduce_topics(docs = news_docs, nr_topics = 30L)
topic_model |> bert_reduce_topics(docs = news_docs,number_topics = 5L)
topic_model |> bert_topic_count() # Does work
topic_model$reduce_outliers()
topic_model |> bert_topic_info() |> View()
topic_model$topics_

#'
probs <- out[[2]]
new_topics = topic_model.reduce_outliers(docs, topics, probabilities = probs, strategy =
                                           "probabilities")


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
classifier <-
  transformers$pipeline("zero-shot-classification", model = "facebook/bart-large-mnli")
sequence_to_classify <-
  topic_model |> bert_topics_keywords() |> filter(topic_bert == 2) |> pull(word)
candidate_labels = list('cooking', 'dancing', 'religion')
classifier(sequence_to_classify, candidate_labels)


# find_topics -------------------------------------------------------------
out_sim <- topic_model$find_topics("china", top_n = 5L)
topic_model$get_topic(topic = out_sim[[1]][1])

bert_similar_terms_topics(topic_model = topic_model, c("china", "fraud"))

bert_similar_terms_topics(topic_model = topic_model, c("motor", "poop", "shit"))


# topic_distribution ----------------------------------------------------

#' https://maartengr.github.io/BERTopic/getting_started/distribution/distribution.html

topic_distr <-
  topic_model$approximate_distribution(
    news_docs,
    window = 4L,
    stride = 1L,
    min_similarity = .1,
    batch_size = 1000L,
    padding = FALSE,
    use_embedding_model = F,
    calculate_tokens = F,
    separator = " "
  )

tbl_news_topic_bert_wide <-
  munge_bert_document_approximate_distributions(data = topic_distr,
                                                return_wide = T,
                                                topic_model = topic_model)

tbl_news_topic_bert_wide |>
  gather(bert_topic, prob, -number_document) |>
  filter(number_document == 1) |>
  filter(prob > 0) |>
  asbviz::hc_xy(
    x = "bert_topic",
    y = "prob",
    type = "column",
    invert_chart = T
  )


topic_distr_toks <-
  topic_model$approximate_distribution(
    news_docs,
    window = 4L,
    stride = 1L,
    min_similarity = .1,
    batch_size = 1000L,
    padding = FALSE,
    use_embedding_model = F,
    calculate_tokens = T,
    separator = " "
  )
topic_distr_toks[[1]] |> str()
topic_distr_toks[[2]] |> str()
topic_distr_toks[[2]][[4]] |> str()

topic_distr_toks_08 <-
  topic_model$approximate_distribution(
    news_docs,
    window = 8L,
    stride = 1L,
    min_similarity = .1,
    batch_size = 1000L,
    padding = FALSE,
    use_embedding_model = F,
    calculate_tokens = T,
    separator = " "
  )


# topics_per_class --------------------------------------------------------

topics_per_class =
  topic_model$topics_per_class(news_docs, classes = tbl_news_docs$news_label)

tbl_bert_topic_per_class(
  data = tbl_news_docs,
  topic_model = topic_model,
  document_name = "document",
  class_name = "news_label"
)

topic_model$visualize_topics_per_class(topics_per_class = as_tibble(topics_per_class),
                                       top_n_topics = 10L) |>
  write_bert_viz()


# merge -------------------------------------------------------------------

bert_merge_topics(obj = topic_model,
                  docs = news_docs,
                  topics_to_merge = list(1L, 2L))
# Supervised Topic Modeling -----------------------------------------------
#' Add Supervised Laels
python_modules("bertopic.dimensionality")
python_modules("sklearn.linear_model")
empty_dimensionality_model <-
  bertopic_dimensionality$BaseDimensionalityReduction()

clf = sklearn_linear_model$LogisticRegression()
ctfidf_model =
  bertopic$vectorizers$ClassTfidfTransformer(reduce_frequent_words = TRUE)

topic_model_with_y = bert_topic(
  umap_model = empty_dimensionality_model,
  hdbscan_model = clf,
  ctfidf_model = ctfidf_model
)

out_with_y <-
  topic_model_with_y$fit_transform(documents = docs["data"], y = docs['target'])
topic_model_with_y |> bert_topic_info()

tbl_supervised <-
  topic_model_with_y |> bert_document_info(docs = news_docs, document_name = 'document') |>
  mutate(label = tbl_news_docs$news_label)

bert_transform_documents(obj = topic_model_with_y, documents = news_docs |> sample(5))

tbl_sample_new <-
  tbl_news_docs |> sample_n(5) |>
  tbl_bert_transform_documents(
    topic_model = topic_model_with_y,
    document_name = "document",
    embeddings = NULL
  )


# partial Supervised ------------------------------------------------------

topic_model_with_y <- bert_topic()
topic_model_with_y$fit(documents = tbl_news_docs$document, y = tbl_news_docs$number_label) # requires numeric input

topic_model_with_y |> bert_topic_info()


# dynamic_topic_modeling --------------------------------------------------

# https://maartengr.github.io/BERTopic/getting_started/topicsovertime/topicsovertime.html#use_embedding_model

trump_topic_model$topics_over_time(tweets, timestamps = tbl_trump_tweets$date, nr_bins =
                                     20L)

trump_topic_model |> bert_topics_over_time(docs = tweets,
                                           nr_bins = 20,
                                           timestamps = tbl_trump_tweets$date)

tbl_trump_tweets |> tbl_bert_topics_over_time(
  topic_model = trump_topic_model,
  document_name = "text",
  time_feature = "date",
  nr_bins = 20
)

trump_topic_model$visualize_topics_over_time(
  topics_over_time =
    trump_topic_model |> bert_topics_over_time(
      docs = tweets,
      nr_bins = 20,
      timestamps = tbl_trump_tweets$date,
      return_tibble = F
    ),
  top_n_topics = 20L
) |> write_bert_viz()


# guided_topic_modeling ---------------------------------------------------


drugs <- c("drug",
           "cancer",
           "drugs",
           "doctor")
computer <- c("windows", "drive", "dos", "file")

space <- c("space", "launch", "orbit", "lunar")
seeded_topics <- list(drugs, computer, space)


topic_model_seeded <-
  bert_topic(
    calculate_probabilities = TRUE,
    exclude_stop_words = TRUE,
    vectorizer_model = NULL,
    extra_stop_words = NULL,
    use_key_phrase_vectorizer = FALSE,
    n_gram_range = list(1L, 1L),
    use_sklearn_vectorizer = F,
    seed_topic_list = seeded_topics
  )


out <-
  topic_model_seeded$fit_transform(documents = news_docs)

topic_model_seeded |> bert_topic_info()
topic_model_seeded |> bert_document_info(docs = news_docs)


# Hierarchical Topic Modeling ---------------------------------------------


hierarchical_topics =
  topic_model$hierarchical_topics(docs = news_docs)

topic_model$visualize_hierarchy(hierarchical_topics = hierarchical_topics) |> write_bert_viz()

tree =
  topic_model$get_topic_tree(hierarchical_topics, tight_layout = T)
cat(tree, fill = T)

#' Merge Topics

topics_to_merge = list(1L, 2L)
topic_model$merge_topics(docs = news_docs, topics_to_merge = topics_to_merge)

multiple_topics <-
  list(list(1, 2),
       list(3, 4))

topic_model$merge_topics(docs = news_docs, topics_to_merge = multiple_topics)


# transformers ------------------------------------------------------------


import_sentence_transformers()
SentenceTransformer <-
  sentence_transformers$SentenceTransformer(model_name_or_path = "all-MiniLM-L6-v2")
embeddings <-
  SentenceTransformer$encode(sentences = news_docs, show_progress_bar = T)
