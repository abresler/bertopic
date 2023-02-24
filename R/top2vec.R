# https://github.com/ddangelov/Top2Vec


#' Import Top2Vec
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_top2vec <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("top2vec")
    ! 'top2vec' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('top2vec', obj, envir = .GlobalEnv)
    }
    obj
  }


# model -------------------------------------------------------------------



#' Top2Vec Model
#'
#' Top2Vec model \url{https://github.com/ddangelov/Top2Vec}
#'
#' @param docs Vector of Documents
#' @param min_count   Ignores all words with total frequency lower than this. For smaller corpora a smaller min_count will be necessary.  Default of `50`
#' @param ngram_vocab Add phrases to topic descriptions.  Default is `FALSE` see  \url{https://radimrehurek.com/gensim/models/phrases.html} for more information
#' @param ngram_vocab_args Pass custom arguments to gensim phrases.  Default is `NULL` also see  \url{https://radimrehurek.com/gensim/models/phrases.html} for more information
#' @param embedding_model Default is `doc2vec` and this will determine which model is used to generate the document and word embeddings. The valid string options are: doc2vec, universal-sentence-encoder, universal-sentence-encoder-large, universal-sentence-encoder-multilingual, universal-sentence-encoder-multilingual-large, distiluse-base-multilingual-cased, all-MiniLM-L6-v2, paraphrase-multilingual-MiniLM-L12-v2.
#' For large data sets and data sets with very unique vocabulary doc2vec could produce better results. This will train a doc2vec model from scratch. This method is language agnostic. However multiple languages will not be aligned.
#'
#' Using the universal sentence encoder options will be much faster since those are pre-trained and efficient models. The universal sentence encoder options are suggested for smaller data sets. They are also good options for large data sets that are in English or in languages covered by the multilingual model. It is also suggested for data sets that are multilingual.
#'
#' For more information on universal-sentence-encoder options visit: https://tfhub.dev/google/collections/universal-sentence-encoder/1
#'
#' The SBERT pre-trained sentence transformer options are distiluse-base-multilingual-cased, paraphrase-multilingual-MiniLM-L12-v2, and all-MiniLM-L6-v2.
#'
#' The distiluse-base-multilingual-cased and paraphrase-multilingual-MiniLM-L12-v2 are suggested for multilingual datasets and languages that are not covered by the multilingual universal sentence encoder. The transformer is significantly slower than the universal sentence encoder options(except for the large options).
#'
#' For more information on SBERT options visit: https://www.sbert.net/docs/pretrained_models.html
#'
#' If passing a callable embedding_model note that it will not be saved when saving a top2vec model. After loading such a saved top2vec model the set_embedding_model method will need to be called and the same embedding_model callable used during training must be passed to it.
#' @param embedding_model_path Pre-trained embedding models will be downloaded automatically by default. However they can also be uploaded from a file that is in the location of embedding_model_path.  Default `NULL`
#' @param embedding_batch_size  Batch size for documents being embedded.  Default `32`
#' @param split_documents If set to True, documents will be split into parts before embedding. After embedding the multiple document part embeddings will be averaged to create a single embedding per document. This is useful when documents are very large or when the embedding model has a token limit. Document chunking or a senticizer can be used for document splitting.  Default `FALSE`
#' @param document_chunker This will break the document into chunks. The valid string options are: `sequential` and `random`.  The sequential chunker will split the document into chunks of specified length and ratio of overlap. This is the recommended method.
#' The random chunking option will take random chunks of specified length from the document. These can overlap and should be thought of as sampling chunks with replacement from the document.
#' If a callable is passed it must take as input a list of tokens of a document and return a list of strings representing the resulting document chunks.
#' Only one of document_chunker or sentincizer should be used.
#' @param chunk_length The number of tokens per document chunk if using the document chunker string options.  Default is `100`
#' @param max_num_chunks  The maximum number of chunks generated per document if using the document chunker string options.  Default is `NULL`
#' @param chunk_overlap_ratio Only applies to the ‘sequential’ document chunker. Fraction of overlapping tokens between sequential chunks. A value of 0 will result i no overlap, where as 0.5 will overlap half of the previous chunk.  Default is `0.5`
#' @param chunk_len_coverage_ratio Only applies to the ‘random’ document chunker option.
#' Proportion of token length that will be covered by chunks. Default value of 1.0 means chunk lengths will add up to number of tokens of the document. This does not mean all tokens will be covered since chunks can be overlapping.
#' @param sentencizer A sentincizer callable can be passed. The input should be a string representing the document and the output should be a list of strings representing the document sentence chunks.
#' Only one of document_chunker or sentincizer should be used.
#' @param speed This parameter is only used when using doc2vec as embedding_model.
#' It will determine how fast the model takes to train. The fast-learn option is the fastest and will generate the lowest quality vectors. The learn option will learn better quality vectors but take a longer time to train. The deep-learn option will learn the best quality vectors but will take significant time to train. The valid string speed options are: `fast-learn`, `learn` and `deep-learn`.  Default `learn`
#' @param use_corpus_file This parameter is only used when using doc2vec as embedding_model.
#' Setting use_corpus_file to True can sometimes provide speedup for large datasets when multiple worker threads are available. Documents are still passed to the model as a list of str, the model will create a temporary corpus file for training.
#' @param document_ids  A unique value per document that will be used for referring to documents in search results. If ids are not given to the model, the index of each document in the original corpus will become the id.  Default is `NULL`
#' @param keep_documents If set to False documents will only be used for training and not saved as part of the model. This will reduce model size. When using search functions only document ids will be returned, not the actual documents. Default is `TRUE`
#' @param workers The amount of worker threads to be used in training the model. Larger amount will lead to faster training.
#' @param tokenizer Override the default tokenization method. If None then gensim.utils.simple_preprocess will be used. Tokenizer must take a document and return a list of tokens.
#' @param use_embedding_model_tokenizer  If using an embedding model other than doc2vec, use the model’s tokenizer for document embedding. If set to True the tokenizer, either default or passed callable will be used to tokenize the text to extract the vocabulary for word embedding.  Default `FALSE`
#' @param umap_args Pass custom arguments to UMAP.  Default `NULL`
#' @param hdbscan_args Pass custom arguments to HDBSCAN.  Default `TRUE`
#' @param verbose Whether to print status data during training.
#' @param obj top2vec object.  If `NULL` initiates a new top2vec model
#' @param assign_to_environment If `TRUE` assigns output to environment
#'
#' @return
#' @export
#'
#' @examples
top2vec_model <-
  function(docs = NULL,
           min_count = 50L,
           ngram_vocab = TRUE,
           ngram_vocab_args = NULL,
           embedding_model = 'doc2vec',
           embedding_model_path = NULL,
           embedding_batch_size = 32,
           split_documents = FALSE,
           document_chunker = 'sequential',
           chunk_length = 100,
           max_num_chunks = NULL,
           chunk_overlap_ratio = 0.5,
           chunk_len_coverage_ratio = 1.0,
           sentencizer = NULL,
           speed = 'learn',
           use_corpus_file = FALSE,
           document_ids = NULL,
           keep_documents = TRUE,
           workers = NULL,
           tokenizer = NULL,
           use_embedding_model_tokenizer = FALSE,
           umap_args = NULL,
           hdbscan_args = NULL,
           verbose = TRUE,
           obj = NULL,
           assign_to_environment = TRUE
           ) {
    if (length(obj) == 0) {
      obj <- import_top2vec(assign_to_environment = F)
    }
    if (length(docs) == 0) {
      stop("Enter Documents")
    }

    if (length(max_num_chunks) > 0) {
      max_num_chunks <- as.integer(max_num_chunks)
    }

    if (length(workers) > 0) {
      workers <- as.integer(workers)
    }
    out <-
      obj$Top2Vec(
      documents = docs,
      min_count = as.integer(min_count),
      ngram_vocab = ngram_vocab,
      ngram_vocab_args = ngram_vocab_args,
      embedding_model = embedding_model,
      embedding_model_path = embedding_model_path,
      embedding_batch_size = as.integer(embedding_batch_size),
      split_documents = split_documents,
      document_chunker = document_chunker,
      chunk_length = as.integer(chunk_length),
      max_num_chunks = as.integer(max_num_chunks),
      chunk_overlap_ratio = chunk_overlap_ratio,
      chunk_len_coverage_ratio = chunk_len_coverage_ratio,
      sentencizer = sentencizer,
      speed = speed,
      use_corpus_file = use_corpus_file,
      document_ids = document_ids,
      keep_documents = keep_documents,
      workers = workers,
      tokenizer = tokenizer,
      use_embedding_model_tokenizer = use_embedding_model_tokenizer,
      umap_args = umap_args,
      hdbscan_args = hdbscan_args,
      verbose = verbose
    )

    if (assign_to_environment) {
      assign('top2vec_model', out, envir = .GlobalEnv)
    }

    out
  }




# topics ------------------------------------------------------------------

#' Textop2vecec Topic Size
#'
#' @param obj
#' @param reduced
#'
#' @return
#' @export
#'
#' @examples
top2vec_topic_size <-
  function(obj, reduced = F) {
    out <- obj$get_topic_sizes(reduced = F)
    tibble(topic_top2vec = out[[2]] |> as.numeric(),
           count_documents = out[[1]] |> as.numeric())

  }

.parse_top2vec_topic <-
  function(obj) {
    obj[[1]]
    obj[[2]]
    obj[[3]] |> as.character()
    obj[[4]] |> as.numeric()
  }



#' Topic Vector Embeddings
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
top2vec_topic_embeddings <-
  function(obj){
    data <- obj$topic_vectors |> as_tibble() |>
      mutate(topic_top2vec = 1:n() - 1) |>
      select(topic_top2vec, everything())

    data
  }


#' Textop2vecec Topic Words
#'
#' @param obj textop2vecec object
#'
#' @return
#' @export
#'
#' @examples
tbl_top_2_vec_words_topics <-
  function(obj) {
    data <- as_tibble(obj$topic_words)
    topics <- 1:ncol(data) - 1

    topics <- topics |> .pz()

    data <-
      data |> setNames(glue::glue("tv2_{topics}")) |>
      mutate(number_word = 1:n()) |>
      select(number_word, everything())

    data

  }

#' Top Textop2vecec Documents
#'
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
top2vec_document_topics <-
  function(obj) {
    tibble(id_document = obj$document_ids,
           topic_top2vec = as.numeric(obj$doc_top),
           distance = as.numeric(obj$doc_dist)) |>
      mutate(number_document = 1:n()) |>
      select(number_document, everything())
  }


# search ------------------------------------------------------------------


.top2vec_similar_word <-
  function(obj, keyword = NULL, keywords_exclude = NULL, number_words = 10L, use_index = FALSE, ef = NULL) {

    if (class(keyword) != "list") {
      keyword <- list(keyword)
    }

    if (length(keywords_exclude) > 0) {
      keywords_exclude <- list(keywords_exclude)
    }

    out <- obj$similar_words(
      keywords = keyword,
      num_words = as.integer(number_words),
      use_index = use_index,
      ef = ef
    )

    words <- out[[1]] |> as.character()
    scores <- out[[2]] |> as.numeric()

    tibble(similar_words_top2vec = words,
           score_top2vec = scores) |>
      mutate(keyword_search = unlist(keyword)) |>
      select(keyword_search,everything())


  }

#' Textop2vecec Similar Words
#'
#' @param keywords
#' @param obj
#' @param keywords_exclude
#' @param number_words
#' @param use_index
#' @param ef
#'
#' @return
#' @export
#'
#' @examples
top2vec_similar_words <-
  function(keywords = NULL, obj = NULL, keywords_exclude = NULL, number_words = 10L, use_index = FALSE, ef = NULL) {

    if (length(obj) == 0) {
     stop("Enter object")
    }
    .top2vec_similar_word_safe <-
      purrr::possibly(.top2vec_similar_word, tibble())

    keywords |>
      map_dfr(function(x){
        .top2vec_similar_word_safe(obj = obj,
                          keyword = x,
                          keywords_exclude = keywords_exclude,
                          number_words = number_words,
                          use_index = use_index,
                          ef = ef)
      })

  }
