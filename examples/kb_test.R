setwd("~")
library(asbtools)
library(tidyverse)
library(modelR2)
library(bertopic)

awards <-
  pq_read(
    "Desktop/nps/nps_2023/normalized_uei_awards/uei_normalized_awards.gz.parquet",
    as_data_frame = F
  )

dat_analysis <-
  awards |>
  filter(is_defense_funded,
         is_analysis_period)

dat <- dat_analysis |>
  filter(rank_2012_2021_dod_p1_p2 <= 10,
         is_analysis_entity,
         is_transition_award_sheldon) |>
  collect()

all_docs <- dat$description_award

sample_docs <-
  all_docs |> sample(150)

tictoc::tic()
tbl_01 <- keybert_keywords(
  docs = sample_docs,
  use_future = F,
  iterate_individually = F
)
tictoc::toc()

tbl_01 |> distinct(number_document)

tictoc::tic()
tbl_02 <- keybert_keywords(docs = sample_docs,
                           use_future = F,
                           use_embeddings = TRUE,
                           iterate_individually = F)
tbl_02 |> distinct(number_document)
tictoc::toc()

tictoc::tic()
tbl_03 <- keybert_keywords(docs = sample_docs,
                           iterate_individually = T)
tbl_03 |> distinct(number_document)
tictoc::toc()


df <- dat |>
  sample_n(100)

tictoc::tic()
tbl_01 <-
  tbl_keybert_keywords(
    data = df,
    document_column = "description_award",
    iterate_individually = F,
    include_both_vectorizers = TRUE,
    use_maxsum = F,
    use_mmr = F,
    return_summary = T,
  )
tictoc::toc()

tictoc::tic()
tbl_02 <-
  tbl_keybert_keywords(
    data = df,
    document_column = "description_award",
    iterate_individually = T,
    include_both_vectorizers = TRUE,
    return_summary = T,
    keyphrase_ngram_range = list(1L, 3L),
    diversity = .85
  )
tictoc::toc()
