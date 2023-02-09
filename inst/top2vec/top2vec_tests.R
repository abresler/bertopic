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


out$generate_topic_wordcloud(topic_num = )
out$get_topic_sizes()
out$get_documents_topics(doc_ids = list(240L), reduced = T)
