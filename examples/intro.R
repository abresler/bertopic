library(dplyr)
library(tidyverse)
library(bertopic)
library(asbviz)

# modules -----------------------------------------------------------------


import_bertopic()
st <- reticulate::import("sentence_transformers")
st$SentenceTransformer()


data <- sklearn::sk_datasets()

# example_01 --------------------------------------------------------------


docs <-
  data$fetch_20newsgroups(subset = 'all',
                          remove = c('headers', 'footers', 'quotes'))
docs <- docs["data"]

# fit ---------------------------------------------------------------------

tm <- bert_topic(verbose = T)


topic_model <- tm$fit_transform(documents = docs)

# Embeddings
embeddings <-
  tm$embedding_model$embed(documents = docs, verbose = T)




test_term <-
  bert_similar_terms_topics(topic_model = tm, terms = "crime", 10)


# topic_info --------------------------------------------------------------

#' Topic Info
bert_topic_info(topic_model = tm, topic_number = NULL)

bert_topic_count(topic_model = tm)


# topics ------------------------------------------------------------------

bert_similar_terms_topics(topic_model = tm,
                          terms = c("iran", "israel", "sex")) |> group_by(term) |> slice_max(score, n = 3)


tm$visualize_barchart()

viz <-
  tm$visualize_documents(
    docs = docs,
    embeddings = embeddings,
    width = 2000L,
    height = 1000L,
    sample = .2,
    hide_annotations = T

  )

viz |>
  write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "doc_viz")


# labels ------------------------------------------------------------------

bert_topic_labels(topic_model = tm, number_words = 3L)


# hierarchy ---------------------------------------------------------------


topic_tree <-
  tm$get_topic_tree(hier_topics = tm$hierarchical_topics(docs = docs))
viz <- tm$visualize_hierarchy()
viz |> write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "hierarchy")

# viz ---------------------------------------------------------------------

#' barchart
bc <- tm$visualize_barchart(n_words = 20L, top_n_topics = 10L)

setwd("~")
bc$write_html("Desktop/test.html")
browseURL("Desktop/test.html")

#' viz topics


viz <- tm$visualize_topics(width = 800, height = 800)
viz$write_html("Desktop/test.html")
browseURL("Desktop/test.html")

viz <- tm$visualize_term_rank(width = 1000, height = 1000)
viz |> write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "term_rank")
tm$visualize_hierarchy() |>
  write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "hierarchy")

tm$visualize_barchart(n_words = 25L, top_n_topics = 100L) |>
  write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "bar")

tm$visualize_heatmap() |>
  write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "heatmap")

#
tm$visualize_documents(docs = docs) |>
  write_bert_viz(base_path = "Desktop/test_viz/", viz_name = "docs")

# embeddings --------------------------------------------------------------

tm2 <- bertopic$BERTopic(embedding_model = "all-MiniLM-L6-v2")
topic_model_2 <- tm2$fit_transform(documents = docs)
viz <- tm2$visualize_term_rank()
viz$write_html("Desktop/test.html")
browseURL("Desktop/test.html")
viz$write_html()



# other -------------------------------------------------------------------
library(dplyr)
library(tidyverse)
library(bertopic)
library(asbviz)

# Step 1 - Extract embeddings
embedding_model = st$SentenceTransformer("all-MiniLM-L6-v2")
um  <- reticulate::import("umap")

hdb <-
  reticulate::import("import hdbscan")

# Step 2 - Reduce dimensionality
umap_model = um$UMAP(
  n_neighbors = 15,
  n_components = 5,
  min_dist = 0.0,
  metric = 'cosine'
)

# Step 3 - Cluster reduced embeddings
hdbscan_model = HDBSCAN(
  min_cluster_size = 15,
  metric = 'euclidean',
  cluster_selection_method = 'eom',
  prediction_data = TRUE
)

# Step 4 - Tokenize topics
vectorizer_model = CountVectorizer(stop_words = "english")

# Step 5 - Create topic representation
ctfidf_model = ClassTfidfTransformer()




