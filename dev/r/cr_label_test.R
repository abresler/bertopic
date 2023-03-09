

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
    vocabulary = vocab,
    ctfidf_model = NULL
  )

out <-
  bert_fit_transform(obj = tm_bap, documents = bap_docs, embeddings = NULL, y = NULL)


tbl_base_topics_base_ctfidf <-
  tm_bap |> bert_topic_count(include_parameters = T)

# change_ctfidf -----------------------------------------------------------

tbl_base_topics_ctfidf_mod <-
  bert_update_topics(
  obj = tm_bap,
  docs = bap_docs,
  representation_model = rep_model,
  n_gram_range = list(1L, 3L),
  use_key_phrase_vectorizer = T,
  use_sklearn_vectorizer = F,
  exclude_stop_words = T,
  extra_stop_words =  NULL,
  vocabulary = vocab,
  ctfidf_model = ctfidf(bm25_weighting = TRUE, reduce_frequent_words = TRUE)
) |>
  bert_topic_count(include_parameters = T)

# mmr ---------------------------------------------------------------------

tbl_mmr_half_no_ctfidf <-
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
  bert_topic_count(include_parameters = T)

tbl_mmr_half_ctfidf <-
  bert_update_topics(
    obj = tm_bap,
    docs = bap_docs,
    topics = NULL,
    top_n_words = 3L,
    n_gram_range = list(3L, 5L),
    vectorizer_model = NULL,
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
    vocabulary = NULL,
    ctfidf_model = ctfidf(bm25_weighting = TRUE, reduce_frequent_words = TRUE)
  ) |>
  bert_topic_count(include_parameters = T)

#' full diversity
tbl_mmr_full_no_ctfidf <-
  bert_update_topics(
    obj = tm_bap,
    docs = bap_docs,
    topics = NULL,
    top_n_words = 3L,
    n_gram_range = list(3L, 5L),
    vectorizer_model = NULL,
    ctfidf_model = NULL,
    representation_model = mmr_inspired_representation(diversity = 1, top_n_words = 5),
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
  bert_topic_count(include_parameters = T)

tbl_mmr_full_ctfidf <-
  bert_update_topics(
    obj = tm_bap,
    docs = bap_docs,
    topics = NULL,
    top_n_words = 3L,
    n_gram_range = list(3L, 5L),
    vectorizer_model = NULL,
    representation_model = mmr_inspired_representation(diversity = 1, top_n_words = 5),
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
    vocabulary = NULL,
    ctfidf_model = ctfidf(bm25_weighting = TRUE, reduce_frequent_words = TRUE)
  ) |>
  bert_topic_count(include_parameters = T)

# keybert -----------------------------------------------------------------


tbl_kb_1_3_no_ctf_1_20_stop <-
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
  bert_topic_count()


tbl_kb_1_3_ctf_1_20_stop <-
  bert_update_topics(
    obj = tm_bap,
    docs = bap_docs,
    topics = NULL,
    top_n_words = 3L,
    n_gram_range = list(3L, 5L),
    vectorizer_model = NULL,
    ctfidf_model = ctfidf(bm25_weighting = TRUE, reduce_frequent_words = TRUE),
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
  bert_topic_count()


tbl_kb_1_3_no_ctf_1_20_no_stop <-
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
    exclude_stop_words = F,
    stopword_package_sources = NULL,
    extra_stop_words = NULL,
    min_df = 1L,
    max_df = 20L,
    pos_pattern = "<J.*>*<N.*>+",
    seed_topic_list = NULL,
    vocabulary = NULL
  ) |>
  bert_topic_count()


tbl_kb_1_3_ctf_1_20_no_stop <-
  bert_update_topics(
    obj = tm_bap,
    docs = bap_docs,
    topics = NULL,
    top_n_words = 3L,
    n_gram_range = list(3L, 5L),
    vectorizer_model = NULL,
    ctfidf_model = ctfidf(bm25_weighting = TRUE, reduce_frequent_words = TRUE),
    representation_model = keybert_inspired_representation(top_n_words = 1L, nr_repr_docs = 3),
    language = "english",
    use_key_phrase_vectorizer = T,
    use_sklearn_vectorizer = F,
    is_lower_case = T,
    keyphrase_ngram_range = list(1L,
                                 1L),
    exclude_stop_words = F,
    stopword_package_sources = NULL,
    extra_stop_words = NULL,
    min_df = 1L,
    max_df = 20L,
    pos_pattern = "<J.*>*<N.*>+",
    seed_topic_list = NULL,
    vocabulary = NULL
  ) |>
  bert_topic_count()


# pos ---------------------------------------------------------------------
pos_non_adj_noun <-
  part_of_speech_representation(top_n_words = 3L, pos_patterns = list(
    list(list('POS' = 'ADJ'), list('POS' = 'NOUN')),
    list(list('POS' = 'NOUN')), list(list('POS' = 'ADJ'))
  ))
pos_adj_noun <- part_of_speech_representation(top_n_words = 1L, pos_patterns = list(
  list(list('POS' = 'ADJ'), list('POS' = 'NOUN'))
))


tbl_pos_custom_no_ctfidf <-
  bert_update_topics(
  obj = tm_bap,
  docs = bap_docs,
  topics = NULL,
  top_n_words = 3L,
  n_gram_range = list(3L, 5L),
  vectorizer_model = NULL,
  ctfidf_model = NULL,
  representation_model = part_of_speech_representation(top_n_words = 3L, pos_patterns = list(
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

tbl_pos_custom_ctfidf <-
  bert_update_topics(
    obj = tm_bap,
    docs = bap_docs,
    topics = NULL,
    top_n_words = !L,
    n_gram_range = list(3L, 3L),
    vectorizer_model = NULL,
    ctfidf_model = ctfidf(bm25_weighting = TRUE, reduce_frequent_words = TRUE),
    representation_model = part_of_speech_representation(top_n_words = 3L, pos_patterns = list(
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





# join --------------------------------------------------------------------
base <-
  tbl_base_topics_base_ctfidf |> select(topic_bert, base_no_ctfidf = label_bertopic)
base_ctf_idf <-
  tbl_base_topics_ctfidf_mod |> select(topic_bert, base_ctfidf = label_bertopic)


mmr_half_no_ctfidf <-
  tbl_mmr_half_no_ctfidf |> select(topic_bert,
                                   mmr_no_ctfidf_half = label_bertopic)

mmr_half_ctf_idf <-
  tbl_mmr_half_ctfidf |> select(topic_bert,
                                mmr_ctf_idf_half = label_bertopic)

mmr_full_no_ctfidf <-
  tbl_mmr_full_no_ctfidf |>
  select(topic_bert,
         mmr_no_ctfidf_full = label_bertopic)

mmr_full_ctfidf <-
  tbl_mmr_full_ctfidf |>
  select(topic_bert,
         mmr_ctfidf_full = label_bertopic)

kb_1_3_no_ctf_1_20_stop <-
  tbl_kb_1_3_no_ctf_1_20_stop |>
  select(topic_bert,
         kb_1_3_no_ctf_1_20_stop = label_bertopic)

kb_1_3_ctf_1_20_no_stop <-
  tbl_kb_1_3_ctf_1_20_no_stop |>
  select(topic_bert,
         kb_1_3_ctf_1_20_no_stop = label_bertopic)

kb_1_3_no_ctf_1_20_no_stop <-
  tbl_kb_1_3_no_ctf_1_20_no_stop |>
  select(topic_bert,
         kb_1_3_no_ctf_1_20_no_stop = label_bertopic)

pos_custom_ctfidf <-
  tbl_pos_custom_ctfidf |>
  select(topic_bert,
         pos_custom_ctfidf  = label_bertopic)
pos_custom_no_ctfidf <-
  tbl_pos_custom_no_ctfidf |>
  select(topic_bert,
         pos_custom_no_ctfidf  = label_bertopic)


tbl_all_counts <-
  list(
    base,
    base_ctf_idf,
    mmr_half_no_ctfidf,
    mmr_half_ctf_idf,
    mmr_full_no_ctfidf,
    mmr_full_ctfidf,
    kb_1_3_no_ctf_1_20_stop,
    kb_1_3_ctf_1_20_stop,
    kb_1_3_no_ctf_1_20_no_stop,
    kb_1_3_ctf_1_20_no_stop,
    pos_custom_no_ctfidf,
    pos_custom_ctfidf
  ) |>
  reduce(left_join) |>
  select(topic_bert, everything())


tbl_all_counts |> gt::gt()



# trees -------------------------------------------------------------------

bap_topic_heir <-
  tm_bap |> bert_topic_hierarchy(docs = bap_docs, print_tree = T, tight_layout = T)
bert_tree |> as.character() |> jsonify::to_json() |> jsonview::json_tree_view(scroll = T, auto_unbox = T)



# treetest ----------------------------------------------------------------


# library(collapsibleTree)
#
# tbl_bap_docs <-
#   bert_document_info(obj = tm_bap, bap_docs, document_name = "quote")
#
# df <- df |>
#   left_join(
#     tbl_keybert, by = c("number_quote" = "number_document")
#   ) |>
#   left_join(
#     tbl_bap_docs
#   )
#
# z <- df |>
#   filter(topic_bert %in% c(0:2)) |>
#   select(label_bertopic,
#          keywords_keybert_keyphrase,
#          keywords_keybert_sklearn) |>
#   gather_keybert_keywords(other_join_columns = "label_bertopic")
#
# z |> collapsibleTree(c("label_bertopic", "type_keyword", "keyword"), fontSize = 8, height = 1500)
#
#
# collapsibleTree(tbl_test_count, c("is_outlier_bert_topic", "label_bertopic", "count"),
#                 collapsed = FALSE
# )
