library(dplyr)
library(tidyverse)
library(bertopic)
import_bertopic()
st <- reticulate::import("sentence_transformers")
st$SentenceTransformer()
data <- sklearn::sk_datasets()
docs <-
  data$fetch_20newsgroups(subset = 'all',
                          remove = c('headers', 'footers', 'quotes'))
docs <- docs["data"]

# fit ---------------------------------------------------------------------

tm <- bert_topic()

topic_model <- tm$fit_transform(documents = docs)


topics <- tm$fit_transform(documents = docs)


test_term <- bert_similar_terms_topics(topic_model = tm, terms = "crime", 10)


# topic_info --------------------------------------------------------------

#' Topic Info
bert_topic_info(topic_model = topic_model, topic_number = NULL)

bert_topic_count(topic_model = topic_model)


# embeddings --------------------------------------------------------------


embeddings <-
  topic_model$embedding_model$embed(documents = docs)



viz <- topic_model$visualize_documents(docs = docs, embeddings = embeddings, width = 1000, height = 1000)
viz |>
  write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "doc_viz")


# labels ------------------------------------------------------------------

bert_topic_labels(topic_model = topic_model, number_words = 3L)


# hierarchy ---------------------------------------------------------------


topic_model$get_topic_tree()

# viz ---------------------------------------------------------------------

#' barchart
bc <- topic_model$visualize_barchart(n_words = 20L, top_n_topics = 10L)

setwd("~")
bc$write_html("Desktop/test.html")


#' viz topics


viz <- topic_model$visualize_topics(width = 800,height = 800)
viz$write_html("Desktop/test.html")
browseURL("Desktop/test.html")

viz <- topic_model$visualize_term_rank(width = 1000, height = 1000)
viz |> write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "term_rank")

# embeddings --------------------------------------------------------------

tm2 <- bertopic$BERTopic(embedding_model="all-MiniLM-L6-v2")
topic_model_2 <- tm2$fit_transform(documents = docs)
viz <- tm2$visualize_term_rank()
viz$write_html("Desktop/test.html")
browseURL("Desktop/test.html")
viz$write_html()
