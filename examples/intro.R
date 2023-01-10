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




# bap ---------------------------------------------------------------------
setwd("~")
library(tidyverse)
tbl_bap  <-
  "Desktop/based_musings/_data/bapsim/bap_quotes.csv" |> read_csv()
bap_docs <-
  tbl_bap |> tbl_bert_text_features(id_columns = c("number_quote", "author"),
                                    text_column = "quote")
tm_bap <-
  bert_topic(
    verbose = T,
    n_gram_range = list(1L, 3L),
    min_topic_size = 3,
    calculate_probabilities = T,
    nr_topics = "auto",
    use_key_phrase_vectorizer = F,
    exclude_stop_words = T,
    extra_stop_words =  c()
  )

topic_model_bap <- tm_bap$fit_transform(documents = bap_docs)

# tm_bap$vectorizer_model$get_feature_names()


tbl_bap_bert <-
  extract_bert_topics(
    obj = topic_model_bap,
    docs = bap_docs,
    id_columns = c("number_quote", "author"),
    text_column = "quote",
    topic_model = tm_bap,
    arrange_topics = T
  )


bap_prob <- tm_bap$hierarchical_topics(docs = bap_docs)

embeddings_bap <-
  tm_bap$embedding_model$embed(documents = bap_docs, verbose = T)

t(embeddings_bap) |>
  as_tibble() |>
  asbviz::hc_xy(x = "V1", y = "V2")


tbl_umap <-
  tm_bap$umap_model$embedding_ |> as_tibble() |>
  rename_all(list(function(x) {
    glue::glue("umap_{readr::parse_number(x)}")
  })) |>
  asbviz::hc_xy(x = "umap_1", y = "umap_2")


# viz ---------------------------------------------------------------------


viz <- tm_bap$visualize_hierarchical_documents(
  docs = bap_docs,
  hierarchical_topics = bap_prob,
  embeddings =  embeddings_bap,
  hide_annotations = T,
  hide_document_hover = F
)

write_bert_viz(viz)

tm_bap$visualize_heatmap() |>
  write_bert_viz()
tm_bap$visualize_barchart(top_n_topics = 65L, n_words = 10L) |> write_bert_viz()
tm_bap$visualize_barchart(top_n_topics = 65L, n_words = 10L) |>
  write_bert_viz()

bap_embeddings <-
  bert_embeddings(
    embeddings = embeddings_bap,
    docs = bap_docs,
    id_columns = c("number_quote", "author"),
    text_column = "quote"
  )

bap_embeddings <- bap_embeddings |>
  left_join(tbl_bap_bert |> select(quote, topic_labels)) |>
  select(number_quote, topic_labels, everything())

bap_umap <-
  extract_bert_umap(tm_bap, data = tbl_bap_bert)

tbl_bap_docs <- bert_topic_documents(obj = tm_bap, bap_docs, document_name = "quote")

bap_umap |> hc_xy(
  x = "umap_0001",
  y = "umap_0002",
  group = "topic_labels",
  disable_legend = T,
  name = "quote",
  color_type = "continuous",
  color_palette = "viridis::viridis",
  disable_x = T,
  disable_y = T,
  point_size = 2.5,
  theme_name = "better_unica",
  title = "BAP UMAP via BERTOPIC"
)

bap_embeddings |>
  modelR2::model_uwot(
    remove_columns = c("number_quote", "author", "quote", "topic_labels"),
    n_neighbors = 15,
    n_components = 2,
    metrics = "cosine",
    min_dists = 0,
    set_op_mix_ratios = 1,
    local_connectivity = 1,
    bandwidths = 1,
    scales = "none",
    inits = "spca"
  ) |>
  hc_xy(
    x = "umap_001",
    y = "umap_002",
    group = "topic_labels",
    name = "quote",
    color_type = "continuous",
    color_palette = "viridis::viridis",
    disable_x = T,
    disable_y = T,
    point_size = 2.5,
    theme_name = "better_unica",
    title = "BAP UMAP via R"
  )

bap_embeddings |>
  asbviz::hc_xy(
    x = "v1",
    y = "v2",
    name = "quote",
    group = "author",
    color_type = "discrete",
    color_palette = "lisa::Jean_MichelBasquiat_1",
    disable_x = T,
    disable_y = T,
    point_size = 2.5,
    theme_name = "better_unica",
    title = "BAP Embeddings"
  )

bert_topic_info(topic_model = tm_bap) |> arrange(desc(count))
bert_topic_count(topic_model = tm_bap)

df <-
  bert_representative_documents(topic_model = tm_bap, topic_number = NULL, label_words = 3L)

bert_topic_labels(topic_model = tm_bap, number_words = 10L)

library(gt)
gt(df)

bert_representative_documents(topic_model = tm_bap, topic_number = 60L)

bert_similar_terms_topics(topic_model = tm_bap, terms = "lavendar")
bert_similar_terms_topics(topic_model = tm_bap, terms = "ethnic")
bert_similar_terms_topics(topic_model = tm_bap, terms = "gay")
tbl_bap_bert |> filter(topic_bert == 9)

tbl_keywords <- bert_topics_keywords(topic_model = tm_bap)

tbl_keywords |>
  mutate(index = 1:n()) |> asbviz::hc_xy(
    x = "index",
    y = "score",
    group = "topic",
    name = "word",
    invert_chart = T
  )

bert_topic_labels(topic_model = tm_bap, number_words = 10L)

tm_bap |> bert_representative_documents()


viz <- tm_bap$visualize_documents(
  docs =  bap_docs,
  embeddings = embeddings_bap,
  width = 2000L,
  height = 1000L,
  sample = .2,
  hide_annotations = T

)
viz$show()
viz <- tm_bap$visualize_topics()
viz$show()

viz <- tm_bap$visualize_hierarchy()
viz$show()


#' Reduce Further

tm_bap$reduce_topics(docs = bap_docs, nr_topics = 20L)
viz <- tm_bap$visualize_topics()
viz$show()
tm_bap |> bert_representative_documents() |> View()
tm_bap |> bert_topic_count()


bap_heir <- tm_bap$hierarchical_topics(docs = bap_docs)
viz <- tm_bap$visualize_heatmap(n_clusters = 4L)

viz |> write_bert_viz()
viz <- viz$add_barpolar()
viz <- viz$add_contour()
viz <- viz$add_mesh3d()
viz$show()
# scratch -----------------------------------------------------------------

tbl_word_counts <- extract_document_word_counts(tm_bap)
tbl_word_counts <- tbl_word_counts |>
  left_join(
    tbl_bap_bert |>
      distinct(number_quote, topic_labels) |>
      arrange(number_quote),
    by = c("number_document" = "number_quote")
  )

tbl_word_counts |>
  count(word, topic_labels, wt = count, name = "count") |>
  filter(count >= 2) |>
  tbl_log_odds(set = "topic_labels",
               feature  = 'word',
               count_variable = "count")

bert_topic_labels(tm_bap)

tbl_word_counts |> asbtools::tbl_summarise(group_variables = "word", amount_variables = "count") |> arrange(desc(count_total))


# good




vect <- tm_bap$vectorizer_model
vect$lowercase <- T
vect$vocabulary_ |> flatten_df() |> gather(word, index) |>
  arrange(index) |>
  View()

vect$get_feature_names()

X |> as_tibble() |> t() |> View()


# new ---------------------------------------------------------------------
str(tm_bap)

ad <- tm_bap$approximate_distribution(documents = bap_docs, calculate_tokens = TRUE)

tbl_info <- tm_bap$get_document_info(docs = bap_docs) |> janitor::clean_names() |> as_tibble()
tbl_info |> arrange(probability)

ad[[1]]

df <- tm_bap$visualize_approximate_distribution(document = bap_docs[[1]], topic_token_distribution = ad[[2]][[1]])
df$template_html$render
ad[[1]] |> as_tibble() |> mutate(number_quote = 1:n()) |>
  select(number_quote, everything()) |>
  View()

tm_bap$topic_mapper_$get_mappings()

kb <- keybert_model()

tbl_bap_keybert <-
  keybert_keywords(obj = kb, docs = bap_docs)
