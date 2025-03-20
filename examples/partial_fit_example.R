
library(bertopic)
library(tidyverse)
setwd("~")
import_bertopic()

data <- sklearn::sk_datasets()
docs_all <- data$fetch_20newsgroups(subset = 'all', remove = reticulate::tuple('headers', 'footers', 'quotes'))
docs <- docs_all["data"]
# Total length of documents
total_length <- length(docs)

# Increment value
increment <- 1000

# Calculate the number of elements based on total length and increment
num_elements <- ceiling(total_length / increment)

# Create the sequence using seq()
doc_lengths <- seq(1, length = num_elements) * increment

asbtools::python_modules("sklearn.cluster")
asbtools::python_modules("sklearn.decomposition")
asbtools::python_modules("bertopic.vectorizers")
asbtools::python_modules("sentence_transformers")

tictoc::tic()
model = sentence_transformers$SentenceTransformer("thenlper/gte-small")
embeddings = model$encode(docs, show_progress_bar = TRUE)
# embeddings = asbtools::read_rda("Desktop/test_embeddings.rda",  return_matrix = T)
# embeddings |> asbtools::write_rda(file_path = "Desktop/", file_name = "test_embeddings")
tictoc::toc()

umap_model <- sklearn_decomposition$IncrementalPCA(n_components = 5L)
cluster_model <- sklearn_cluster$MiniBatchKMeans(n_clusters = 50L, random_state = 0L)
vectorizer_model = bertopic_vectorizers$OnlineCountVectorizer(stop_words =
                                                                "english", decay = .01)

bap_rep_model_01 <-  keybert_inspired_representation(
  top_n_words = 10,
  nr_repr_docs = 5,
  nr_samples = 250,
  nr_candidate_words = 30,
  random_state = 42,
  numba_threads = 2
)

main_representation <-
  keybert_inspired_representation(top_n_words = 1L, nr_repr_docs = 3)
aspect_model1 <-
  part_of_speech_representation(model = "en_core_web_sm")

aspect_model2 <-
  list(
    keybert_inspired_representation(top_n_words = 30),
    mmr_inspired_representation(diversity = .5)
  )
spacy_pattern <-
  list(list(list('POS' = 'ADJ'), list('POS' = 'NOUN')), list(list('POS' = 'NOUN')), list(list('POS' = 'ADJ')))
aspect_model3 <-
  part_of_speech_representation(pos_patterns = spacy_pattern)

possible_topics <- list(c("gay", "china", "striver"))

aspect_zeroshot <-
  zeroshot_representation(model = "facebook/bart-large-mnli", candidate_topics = possible_topics)

aspect_keybert_best <- keybert_inspired_representation(
  top_n_words = 1L,
  nr_repr_docs = 3L,
  nr_samples = 500,
  nr_candidate_words = 10L,
  random_state = 42,
  numba_threads = 2
)

rep_model <- reticulate::dict(
  main = main_representation,
  aspect1_spacy = aspect_model1,
  aspect2_keybert_mmr = aspect_model2,
  aspect3_pos = aspect_model3,
  aspect4_bertbert_bap = bap_rep_model_01,
  aspect5_zeroshot = aspect_zeroshot,
  aspect6_keybert_best = aspect_keybert_best
)
# full --------------------------------------------------------------------

topic_model <- bertopic$BERTopic(
  umap_model = umap_model,
  hdbscan_model = cluster_model,
  vectorizer_model = vectorizer_model,
  embedding_model = "thenlper/gte-small",
  representation_model = rep_model,
  verbose = TRUE
)
tictoc::tic()
topic_model$fit_transform(docs, embeddings = as.matrix(embeddings))
tictoc::toc()

tbl_count <- topic_model |> bert_topic_count()
tbl_info <-
  topic_model |> bert_topic_info()
tbl_info <- tbl_info |>
  mutate(label_final = case_when(
    aspect_keybert_best_006 != aspect_main ~ glue::glue("{aspect_keybert_best_006} - {aspect_main}"),
    TRUE ~ aspect_keybert_best_006
  ))

labels_final <- tbl_info$label_final |> as.character()
topic_model |> bert_topic_count()
topic_model$set_topic_labels(topic_labels = c(labels_final))
topic_model |> bert_topic_count(only_label = F)
topic_model$custom_labels_
topic_model |> bert_topic_info()

# partial -----------------------------------------------------------------


topic_model_partial <- bertopic$BERTopic(
  umap_model = umap_model,
  hdbscan_model = cluster_model,
  vectorizer_model = vectorizer_model,
  embedding_model = "thenlper/gte-small",
  verbose = TRUE
)
tictoc::tic()
doc_lengths |>
  walk(function(x) {
    start <- (x - 1000) + 1
    message(start)
    docs_sample <- docs[start:x]
    topic_model_partial$partial_fit(docs_sample, embeddings = embeddings)

  })

tictoc::toc()

tbl_count_partial <- topic_model |> bert_topic_count()

