#library(bertopic)
library(tidyverse)
# https://maartengrootendorst.substack.com/p/bertopic-what-is-so-special-about
devtools::load_all(".")
import_bertopic(
  numba_threads = 4L,
  use_token_parallel = FALSE,
  use_kmp_fork = TRUE
)

ds <- reticulate::import("datasets")
dataset = ds$load_dataset("CShorten/ML-ArXiv-Papers")
all_docs <- dataset$train[["abstract"]]


docs = all_docs[1:5000]

zeroshot_topic_list <- list(
  "Clustering",
  "Topic Modeling",
  "Large Language Models"
)
zeroshot_topic_list <- NULL

# basic -------------------------------------------------------------------

topic_model_base <- bertopic$BERTopic(
  embedding_model = "thenlper/gte-small",
  min_topic_size = 15L,
  zeroshot_topic_list = zeroshot_topic_list,
  zeroshot_min_similarity = .85,
  representation_model = keybert_inspired_representation(
    nr_candidate_words = 100L
  ),
  verbose = TRUE
)

out <- topic_model_base$fit_transform(documents = docs)

tbl_info <-
  topic_model_base |> bert_topic_count()

tbl_count <-
  topic_model_base |> bert_topic_info()

topic_model_base$update_topics(docs = docs, top_n_words = 5L, )

topic_model_base |> bert_topic_info() |> View()
topic_model_base |> bert_topic_keywords() |> View()
topic_model_base |>
  bert_similar_terms_topics(terms = "Topological Data Analysis")


# pkg ---------------------------------------------------------------------

topic_model <- bert_topic(
  embedding_model = "thenlper/gte-small",
  min_topic_size = 15L,
  zeroshot_topic_list = zeroshot_topic_list,
  zeroshot_min_similarity = .85,
  representation_model = bertopic::keybert_inspired_representation(
    nr_candidate_words = 100L
  ),
  verbose = TRUE
)

out_pkg <- topic_model$fit_transform(documents = docs)


topic_model |> bert_topic_info()
topic_model |> bert_topic_keywords()
topic_model |> bert_similar_terms_topics(terms = "Topological Data Analysis")


# try_langchain -----------------------------------------------------------

prompt = "What are these documents about? Please give a single label."
lc <- reticulate::import("langchain")
load_qa_chain <- lc$chains$question_answering$load_qa_chain
langchain <- reticulate::import("langchain.chains.question_answering")
OpenAI <- lc$llms$openai$OpenAI

chain <-
  load_qa_chain(
    OpenAI(
      temperature = 0,
      openai_api_key = "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"
    ),
    chain_type = "stuff"
  )
bert_rep <- bertopic_representations()

representation_model = bert_rep$LangChain(chain, prompt = prompt)

topic_model <- bert_topic(
  embedding_model = "thenlper/gte-small",
  min_topic_size = 15L,
  zeroshot_topic_list = zeroshot_topic_list,
  zeroshot_min_similarity = .85,
  representation_model = representation_model,
  verbose = TRUE
)

out <- topic_model$fit_transform(documents = docs |> sample(1000))

# llama -------------------------------------------------------------------

# Doesnt work

# LlamaCPP <-
#   bertopic$representation$LlamaCPP
#
# llama_cpp <- reticulate::import("llama_cpp")
# Llama <- llama_cpp$Llama
# setwd("~")
# llm <- Llama(
#   model_path = "zephyr-7b-alpha.Q4_K_M.gguf",
#   n_gpu_layers = -1L,
#   n_ctx = 4096L,
#   stop = "Q:"
# )
# representation_model = LlamaCPP(llm)
#
# topic_model <- bert_topic(
#   embedding_model = "thenlper/gte-small",
#   min_topic_size = 15L,
#   zeroshot_topic_list = zeroshot_topic_list,
#   zeroshot_min_similarity = .85,
#   representation_model = representation_model,
#   verbose = TRUE
# )
#
# out <- topic_model$fit_transform(documents = docs |> sample(1000))
