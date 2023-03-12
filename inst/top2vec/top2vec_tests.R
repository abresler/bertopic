#' https://maartengr.github.io/BERTopic/getting_started/quickstart/quickstart.html

library(tidyverse)
library(bertopic)
# library(asbtools)

# Quick Start -------------------------------------------------------------

sklearn <- import_sklearn()
data <- sklearn$datasets

docs <-
  data$fetch_20newsgroups(subset = 'all',
                          remove = c('headers', 'footers', 'quotes'))
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

out <-
  top2vec_model(
    docs  = tbl_news_docs$document,
    min_count = 4L,
    document_ids = tbl_news_docs$number_document,
    workers = 2,
    ngram_vocab = TRUE,
    ngram_vocab_args = NULL,
    embedding_model = "doc2vec",
    embedding_model_path = NULL,
    embedding_batch_size = 32,
    split_documents = FALSE,
    document_chunker = "sequential",
    chunk_length = 100,
    max_num_chunks = NULL,
    chunk_overlap_ratio = 0.5,
    chunk_len_coverage_ratio = 1,
    sentencizer = NULL,
    speed = "learn",
    use_corpus_file = FALSE,
    keep_documents = TRUE,
    tokenizer = NULL,
    use_embedding_model_tokenizer = FALSE,
    umap_args = NULL,
    hdbscan_args = NULL,
    verbose = TRUE,
    obj = NULL,
    assign_to_environment = TRUE
  )

# test_top2vec_news -------------------------------------------------------






# quanteda ----------------------------------------------------------------


docs <- as.character(quanteda.corpora::data_corpus_sotu)
tbl_sotu <-
  tibble(year_president = names(quanteda.corpora::data_corpus_sotu),
         text = docs) |>
  mutate(id = 1:n())

ids <- tbl_sotu$id
out <-
  top2vec_model(
    docs = df_text$quote,
    min_count = 4L,
    document_ids = 1:length(df_text$quote),
    workers = 2
  )

out$doc_dist
out$doc_dist_reduced
out$doc_top
out$get_topics()
out$get_documents_topics(doc_ids = list(240L))
out$word_indexes |> flatten_df() |> tidyr::gather(index, word)


out$generate_topic_wordcloud(topic_num =)
out$get_topic_sizes()
out$get_documents_topics(doc_ids = list(240L), reduced = T)
