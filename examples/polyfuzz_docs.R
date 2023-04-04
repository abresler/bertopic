devtools::load_all(".")
model <- polyfuzz_model(method = "TF-IDF")

# quickstart --------------------------------------------------------------


from_list  <- c("apple", "apples", "appl", "recal", "house", "similarity")
to_list = c("apple", "apples", "mouse")

model <- polyfuzz_model(method = "TF-IDF", verbose = T)

polyfuzz_match(obj = model, from_list = from_list, to_list = to_list, link_min_similarity = .75)
polyfuzz_match(obj = model, from_list = from_list, to_list = to_list, exclude_unmatched = T)


#' Fit / Transform¶


train_words <- c("apple", "apples", "appl", "recal", "house", "similarity")
unseen_words  <- c("apple", "apples", "mouse")
model <-
  polyfuzz_model(method = "TF-IDF", verbose = T, from_list = train_words)

polyfuzz_transform(obj = model, unseen_words)

#' Group Matches

model$group(link_min_similarity = .75)

model |> tbl_polyfuzz_matches()

#' Precision/Recall
setwd("~")


# models ------------------------------------------------------------------

#' TFIDF
tfidf <- polyfuzz_tfidf(n_gram_range = list(3L, 3L), clean_string = TRUE, min_similarity = .5, model_id = "TF-IDF", top_n = 1L, cosine_method = 'sparse')

polyfuzz_model(model_object = tfidf)

polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

#' edit distance
jf <- import_jellyfish()
edit_distance <-
  polyfuzz_edit_distance(scorer = jf$jaro_winkler_similarity)

model <- polyfuzz_model(model_object = edit_distance)

polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

model <- polyfuzz_model(method = "EditDistance")
polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

#' RapidFuzz

fuzz <- import_fuzz()

rapid_fuzz <-
  polyfuzz_rapidmatcher(n_jobs = 1, score_cutoff = .5, scorer = fuzz$WRatio, model_id = NULL)


# Embeddings --------------------------------------------------------------

flair <- import_flair(assign_to_environment = F)
embeddings <- flair_embeddings()
#' BERT
bert <- embeddings$TransformerWordEmbeddings(model = 'bert-base-multilingual-cased')

pf_embedd <-
  polyfuzz_embeddings(embedding_method = bert, min_similarity = 0, top_n = 1, cosine_method = 'sparse', model_id = "BERT")

model <- polyfuzz_model(model_object = pf_embedd)
polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

#' fasttext
fasttext <- embeddings$WordEmbeddings(embeddings = 'en-crawl')

pf_embedd_ft <-
  polyfuzz_embeddings(
    embedding_method = fasttext,
    min_similarity = 0,
    top_n = 1,
    cosine_method = 'sparse',
    model_id = "fasttext"
  )

model <- polyfuzz_model(model_object = pf_embedd_ft)
polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

#' Sentence Transformers

distance_model <- polyfuzz_sentence_embeddings(embedding_model = "all-MiniLM-L6-v2")

model <- polyfuzz_model(model_object = distance_model)
polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

st <- import_sentence_transformers()
st_model <- st$SentenceTransformer("all-MiniLM-L6-v2")
distance_model <- polyfuzz_sentence_embeddings(embedding_model = st_model)

model <- polyfuzz_model(model_object = distance_model)
polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

#' Gensim
api = reticulate::import("gensim.downloader")
embedding_model <- api$load(name = "fasttext-wiki-news-subwords-300")

distance_model <-
  polyfuzz_gensim_embeddings(embedding_model = embedding_model)
model <- polyfuzz_model(model_object = distance_model)
polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)


#' Spacy

spacy <- import_spacy()
emedding_model <- spacy$load

distance_model <-
  polyfuzz_spacy_embeddings(embedding_model = embedding_model)



# USE ---------------------------------------------------------------------

distance_model <-
  polyfuzz_use_embeddings()


model <- polyfuzz_model(model_object = distance_model)

polyfuzz_match(
  obj = model,
  from_list = from_list,
  to_list = to_list,
  link_min_similarity = .75
)

# api ---------------------------------------------------------------------

model$fit(from_list = list("string_one", "string_two"), to_list = list("string_three", "string_four"))
out <- model$fit(list("string_three", "string_four"))
model$get_matches()

