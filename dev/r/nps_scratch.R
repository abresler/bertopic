library(bertopic)
library(sheldon)
library(tidyverse)

# extract -----------------------------------------------------------------

nps_url <- "https://dair.nps.edu/bitstream/123456789/4784/1/NPS-LM-23-006.pdf"
nps_akb <- "https://dair.nps.edu/bitstream/123456789/4550/1/SYM-AM-22-037.pdf"
df <- sheldon::extract_files(urls = nps_akb)
corp <- sheldon::qe_corpus(df, text_variable = "text")

d <- quanteda::corpus_reshape(corp, to = "sentences") |> as_tibble() |> rename(text = value) |> mutate(text =as.character(text)) |>
  mutate(id = 1:n())

docs <-
  d |> tbl_bert_text_features(id_columns = c("id"),
                              text_column = "text")
rm(corp)
gc()

# keybert -----------------------------------------------------------------

tbl_keybert_bp_spit <-
  keybert_keywords(docs = docs, top_n_words = 30L, use_key_phrase_vectorizer = T, use_maxsum = T, diversity = 1)

tbl_keybert_bp <-
  keybert_keywords(
    docs = df$text,
    top_n_words = 20L,
    use_key_phrase_vectorizer = T,
    use_maxsum = T,
    diversity = 1,
    use_mmr = F,
    extra_stop_words  = c(
      "acquisition research program department",
      "pw communications",
      "acquisition",
      "pw communications inc",
      "same approach we employed",
      "certain acquisition research program department",
      "research",
      "program",
      "department",
      "http",
      "we"
    ),
    stopword_package_sources =  c("snowball",
                                  "stopwords-iso",
                                  "smart",
                                  "nltk")
  )


tbl_keybert_sk_split <-
  keybert_keywords(
    docs = docs,
    top_n_words = 5L,
    use_key_phrase_vectorizer = F,
    use_maxsum = T,
    diversity = 1,
    stopword_package_sources =  c("snowball",
                                  "stopwords-iso",
                                  "smart",
                                  "nltk"),
    keyphrase_ngram_range = list(1L,  3L),
    use_mmr = T
  )

tbl_keybert_sk <-
  keybert_keywords(
    docs = df$text,
    top_n_words = 30L,
    use_key_phrase_vectorizer = F,
    keyphrase_ngram_range = list(1L, 3L),
    use_maxsum = T,
    diversity = 1,
    use_mmr = T,
    exclude_stop_words = T,
    stopword_package_sources =  c("snowball",
                                  "stopwords-iso",
                                  "smart",
                                  "nltk"),
    extra_stop_words = c("defense", "acquisitions", "research", "bitstream", "defense acquisition research")
  )


# bertopic ----------------------------------------------------------------



tm <-
  bert_topic(
    verbose = T,
    n_gram_range = list(1L, 3L),
    min_topic_size = 3,
    calculate_probabilities = T,
    nr_topics = "auto",
    use_key_phrase_vectorizer = T,
    exclude_stop_words = T,
    extra_stop_words  = c(
      "acquisition research program department",
      "pw communications",
      "acquisition",
      "pw communications inc",
      "same approach we employed",
      "certain acquisition research program department",
      "research",
      "program",
      "department",
      "http",
      "we"
    ),
    stopword_package_sources =  c("snowball",
                                  "stopwords-iso",
                                  "smart",
                                  "nltk")
  )

topic_model <- tm$fit_transform(documents = docs)

# tm$vectorizer_model$get_feature_names()


tbl_bert <-
  extract_bert_topics(
    obj = topic_model,
    docs = docs,
    id_columns = "id",
    text_column = "text",
    topic_model = tm,
    arrange_topics = T
  )


prob <- tm$hierarchical_topics(docs = docs)

tbl_docs <-
  bert_topic_documents(obj = tm, docs, document_name = "text")

tbl_docs |> count(topic_bert,
                  topic_labels,
                  top_n_words,
                  name = 'count_pages',
                  sort = T)

tm$visualize_barchart() |> write_bert_viz()

tm$approximate_distribution()
