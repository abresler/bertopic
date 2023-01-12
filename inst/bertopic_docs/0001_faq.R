#' https://maartengr.github.io/BERTopic/getting_started/quickstart/quickstart.html

library(tidyverse)
# library(bertopic)


# Quick Start -------------------------------------------------------------


sklearn <- import_sklearn()
data <- sklearn$datasets

docs <-
  data$fetch_20newsgroups(subset = 'all',
                          remove = c('headers', 'footers', 'quotes'))
news_docs <- docs["data"] |> stringr::str_trim()

import_sentence_transformers()
SentenceTransformer <-
  sentence_transformers$SentenceTransformer(model_name_or_path = "all-MiniLM-L6-v2")
embeddings <- SentenceTransformer$encode(sentences = news_docs, show_progress_bar = T)


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
out <- topic_model$fit_transform(documents = news_docs,embeddings = embeddings)

topic_model$get_params()
# embeddings <-
#   topic_model$embedding_model$embed(documents = news_docs, verbose = T) # same as above

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

# Visualize Topics

viz <- topic_model$visualize_topics(width = 800, height = 800)
viz |> write_bert_viz()
viz_bar <- topic_model$visualize_barchart()

#' Visualize Documents¶
viz_docs <- topic_model$visualize_documents(docs = news_docs, embeddings = embeddings, hide_annotations = T)
viz_docs |> write_bert_viz()
import_umap()
um <- umap$UMAP(
  n_neighbors = 10,
  n_components = 2,
  min_dist = 0.0,
  metric = 'cosine'
)

reduced_embeddings <- um$fit_transform(X = embeddings)
tbl_umap <-
  reduced_embeddings |> tbl_array(output_type = 'umap')

viz_docs_reduced <- topic_model$visualize_documents(docs = news_docs, reduced_embeddings = reduced_embeddings, hide_annotations = T)

viz_docs_reduced |> write_bert_viz()


# hierarchy ---------------------------------------------------------------


#' Topic Hierarchy
#'

viz_hier <- topic_model$visualize_hierarchy()
viz_hier |> write_bert_viz()

hierarchical_topics  <- topic_model$hierarchical_topics(docs = news_docs)

hierarchical_topics |> as_tibble()

topic_model$visualize_hierarchy(hierarchical_topics=hierarchical_topics) |> write_bert_viz()

topic_model$get_topic_tree(hierarchical_topics, tight_layout = T) |> print()



# Visualize Terms¶ --------------------------------------------------------

viz_bar <-
  topic_model$visualize_barchart(n_words = 15L, top_n_topics = 15L, title = "BERT")
viz_bar |> write_bert_viz()
topic_model |> bertopic::bert_topics_keywords() |>
  arrange(topic_bert, desc(score)) |>
  group_by(topic_bert) |>
  slice(1:15) |>
  ungroup()

# saving ------------------------------------------------------------------
# setwd("~")
# topic_model$save(path = "Desktop/sample_bt_model")
