

library(bertopic)

library(sheldon)
library(tidyverse)
library(bertopic)
setwd("~")
setwd("~/Desktop/based_musings")

# data --------------------------------------------------------------------

#' Read Data from Notes
#'

text <- read_lines("_data/bapsim/bap_raw.txt")

# summarise ---------------------------------------------------------------

#' Summarise to unqiue quotes

df <- tibble(text) |>
  filter(text != "")

df <-
  df |>
  mutate(has_quote = text |> str_detect('\\\"') |> as.numeric()) |>
  asbtools::tbl_mutate(cumulative_sum_columns = "has_quote") |>
  rename(number_quote = has_quote_cumulative_total) |>
  select(number_quote, everything()) |>
  select(-has_quote) |>
  mutate(text = text |> str_remove_all('\\\"') |> str_squish()) |>
  group_by(number_quote) |>
  summarise(text = text |> str_c(collapse = ".  ")) |>
  ungroup() |>
  separate(text, into = c("quote", "author"), sep = "\\ - ")

df <- df |>
  mutate(author = case_when(
    is.na(author) ~ "BAP",
    author %in% c("August 2021", "Myth 20th") ~ "BAP",
    TRUE ~ author
  ))

df_text <- df


tbl_keybert <-
  df |>
  tbl_keybert_keywords(
    document_column = "quote",
    return_summary = T,
    include_both_vectorizers = TRUE,
    keyphrase_ngram_range = list(3L, 3L)
  )

bap_docs <-
  df |> tbl_bert_text_features(id_columns = c("number_quote", "author"),
                               text_column = "quote")

vocab <- NULL

rep_model <- keybert_inspired_representation(
  top_n_words = 10,
  nr_repr_docs = 5,
  nr_samples = 250,
  nr_candidate_words = 30,
  random_state = 42
)

tm_bap <-
  bert_topic(
    representation_model = rep_model,
    verbose = T,
    n_gram_range = list(1L, 3L),
    min_topic_size = 4,
    calculate_probabilities = T,
    nr_topics = "auto",
    use_key_phrase_vectorizer = T,
    use_sklearn_vectorizer = F,
    exclude_stop_words = T,
    extra_stop_words =  NULL,
    vocabulary = vocab
  )

out <-
  bert_fit_transform(obj = tm_bap, documents = bap_docs, embeddings = NULL, y = NULL)


# mmr ---------------------------------------------------------------------

bert_update_topics(
  obj = tm_bap,
  docs = bap_docs,
  topics = NULL,
  top_n_words = 3L,
  n_gram_range = list(3L, 5L),
  vectorizer_model = NULL,
  ctfidf_model = NULL,
  representation_model = mmr_inspired_representation(diversity = .5, top_n_words = 5),
  language = "english",
  use_key_phrase_vectorizer = T,
  use_sklearn_vectorizer = F,
  is_lower_case = T,
  keyphrase_ngram_range = list(1L,
                               1L),
  exclude_stop_words = T,
  stopword_package_sources = c("snowball", "stopwords-iso", "smart", "nltk"),
  extra_stop_words = NULL,
  min_df = 1L,
  max_df = 20L,
  pos_pattern = "<J.*>*<N.*>+",
  seed_topic_list = NULL,
  vocabulary = NULL
) |>
  bert_topic_count()

# keybert -----------------------------------------------------------------


bert_update_topics(
  obj = tm_bap,
  docs = bap_docs,
  topics = NULL,
  top_n_words = 3L,
  n_gram_range = list(3L, 5L),
  vectorizer_model = NULL,
  ctfidf_model = NULL,
  representation_model = keybert_inspired_representation(top_n_words = 1L, nr_repr_docs = 3),
  language = "english",
  use_key_phrase_vectorizer = T,
  use_sklearn_vectorizer = F,
  is_lower_case = T,
  keyphrase_ngram_range = list(1L,
                               1L),
  exclude_stop_words = T,
  stopword_package_sources = c("snowball", "stopwords-iso", "smart", "nltk"),
  extra_stop_words = NULL,
  min_df = 1L,
  max_df = 20L,
  pos_pattern = "<J.*>*<N.*>+",
  seed_topic_list = NULL,
  vocabulary = NULL
) |>
  .Last.valuebert_topic_count()


# pos ---------------------------------------------------------------------


bert_update_topics(
  obj = tm_bap,
  docs = bap_docs,
  topics = NULL,
  top_n_words = 3L,
  n_gram_range = list(3L, 5L),
  vectorizer_model = NULL,
  ctfidf_model = NULL,
  representation_model = part_of_speech_representation(pos_patterns = list(
    list(list('POS' = 'ADJ'), list('POS' = 'NOUN')),
    list(list('POS' = 'NOUN')), list(list('POS' = 'ADJ'))
  )),
  language = "english",
  use_key_phrase_vectorizer = T,
  use_sklearn_vectorizer = F,
  is_lower_case = T,
  keyphrase_ngram_range = list(1L,
                               1L),
  exclude_stop_words = T,
  stopword_package_sources = c("snowball", "stopwords-iso", "smart", "nltk"),
  extra_stop_words = NULL,
  min_df = 1L,
  max_df = 20L,
  pos_pattern = "<J.*>*<N.*>+",
  seed_topic_list = NULL,
  vocabulary = NULL
) |>
  bert_topic_count()
