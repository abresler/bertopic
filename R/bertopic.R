# https://scikit-learn.org/0.15/index.html

# stopwords ---------------------------------------------------------------


#' bert
#'
#' @return
#' @export
#'
#' @examples
bert_base_stop_words <-
  function() {
    c(
      'between',
      'nevertheless',
      'so',
      'is',
      'me',
      'should',
      'throughout',
      'someone',
      'hereupon',
      'than',
      'cannot',
      'such',
      'amount',
      'nobody',
      'hereafter',
      'noone',
      'under',
      'or',
      'five',
      'name',
      'whereas',
      'neither',
      'everywhere',
      'somewhere',
      'top',
      'de',
      'hereby',
      'seem',
      'have',
      'before',
      'eg',
      'yourself',
      'thin',
      'across',
      'though',
      'this',
      'except',
      'very',
      'nothing',
      'become',
      'once',
      'herein',
      'among',
      'ie',
      'one',
      'nowhere',
      'via',
      'hers',
      'see',
      'thick',
      'why',
      'ourselves',
      'whether',
      'behind',
      'anything',
      'call',
      'latterly',
      'wherein',
      'either',
      'herself',
      'amoungst',
      'each',
      'whenever',
      'some',
      'alone',
      'could',
      'less',
      'per',
      'everyone',
      'bottom',
      'do',
      'thereafter',
      'also',
      'give',
      'although',
      'becomes',
      'an',
      'thru',
      'to',
      'however',
      'there',
      'which',
      'cry',
      'move',
      'along',
      'due',
      'from',
      'are',
      'upon',
      'put',
      'within',
      'beside',
      'her',
      'twenty',
      'how',
      'ever',
      'yourselves',
      'moreover',
      'ltd',
      'too',
      'after',
      'whence',
      'other',
      're',
      'others',
      'seeming',
      'many',
      'a',
      'two',
      'thereby',
      'thence',
      'again',
      'those',
      'mostly',
      'further',
      'whereafter',
      'whereupon',
      'least',
      'myself',
      'no',
      'onto',
      'besides',
      'all',
      'she',
      'several',
      'etc',
      'whole',
      'else',
      'next',
      'done',
      'serious',
      'six',
      'hence',
      'twelve',
      'ten',
      'mill',
      'through',
      'them',
      'even',
      'detail',
      'therein',
      'show',
      'itself',
      'un',
      'you',
      'your',
      'thereupon',
      'above',
      'seemed',
      'con',
      'part',
      'otherwise',
      'made',
      'he',
      'whither',
      'ours',
      'any',
      'until',
      'three',
      'anyone',
      'latter',
      'might',
      'last',
      'since',
      'same',
      'below',
      'where',
      'well',
      'of',
      'rather',
      'more',
      'up',
      'few',
      'during',
      'anywhere',
      'into',
      'system',
      'who',
      'becoming',
      'not',
      'front',
      'third',
      'elsewhere',
      'fire',
      'perhaps',
      'side',
      'both',
      'toward',
      'afterwards',
      'fill',
      'another',
      'in',
      'four',
      'anyhow',
      'against',
      'amongst',
      'themselves',
      'namely',
      'his',
      'much',
      'being',
      'take',
      'towards',
      'empty',
      'eight',
      'full',
      'found',
      'formerly',
      'and',
      'enough',
      'sometime',
      'mine',
      'be',
      'fifty',
      'but',
      'get',
      'had',
      'by',
      'been',
      'sixty',
      'then',
      'must',
      'only',
      'back',
      'couldnt',
      'cant',
      'they',
      'yours',
      'has',
      'together',
      'down',
      'please',
      'inc',
      'while',
      'every',
      'still',
      'may',
      'whoever',
      'it',
      'nor',
      'meanwhile',
      'became',
      'on',
      'first',
      'about',
      'am',
      'him',
      'keep',
      'was',
      'my',
      'forty',
      'co',
      'hasnt',
      'beforehand',
      'out',
      'sincere',
      'if',
      'often',
      'over',
      'were',
      'thus',
      'now',
      'anyway',
      'because',
      'describe',
      'here',
      'already',
      'himself',
      'around',
      'yet',
      'somehow',
      'that',
      'what',
      'therefore',
      'as',
      'never',
      'former',
      'whose',
      'will',
      'bill',
      'whatever',
      'would',
      'go',
      'none',
      'beyond',
      'off',
      'fifteen',
      'at',
      'nine',
      'can',
      'most',
      'find',
      'hundred',
      'without',
      'we',
      'their',
      'indeed',
      'almost',
      'whereby',
      'its',
      'with',
      'something',
      'everything',
      'i',
      'eleven',
      'the',
      'interest',
      'our',
      'for',
      'own',
      'whom',
      'these',
      'always',
      'wherever',
      'us',
      'seems',
      'sometimes',
      'when'
    )
  }

#' Bertopic Stopwords
#'
#' @param extra_stop_words
#'
#' @return
#' @export
#'
#' @examples
bert_stop_words <-
  function(extra_stop_words = NULL) {
    stopwords <- bert_base_stop_words()

    if (length(extra_stop_words)) {
      stopwords <- stopwords |>
        append(extra_stop_words)
    }

    stopwords <- stopwords |> unique()

    stopwords
  }

# import ------------------------------------------------------------------


#' Import Bert Topic
#'
#' \href{https://maartengr.github.io/BERTopic}{BERTopic API from python}
#'
#' @param assign_to_environment if \code{TRUE} assigns to environment
#'
#' @return python object
#' @export
#'
#' @examples
#' library(bertopic)
#' import_bertopic()
import_bertopic <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("bertopic")
    ! 'bertopic' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('bertopic', obj, envir = .GlobalEnv)
    }
    obj
  }


# stup --------------------------------------------------------------------



#' Initiate BERT Topic Model
#'
#' Functions for unsupervised clustering algorithms.
#'
#' \itemize{
#' \item \href{https://maartengr.github.io/BERTopic/api/bertopic.html}{berttopic}
#' }
#'
#' @param verbose if `TRUE` verbose output
#' @param top_n_words The number of words per topic to extract. Setting this too high can negatively impact topic embeddings as topics are typically best represented by at most 10 words.  Default to `10L`
#' @param language The main language used in your documents. The default sentence-transformers model for "english" is all-MiniLM-L6-v2. For a full overview of supported languages see bertopic.backend.languages. Select "multilingual" to load in the paraphrase-multilingual-MiniLM-L12-v2 sentence-tranformers model that supports 50+ languages. Default `english`
#' @param n_gram_range The n-gram range for the CountVectorizer. Advised to keep high values between 1 and 3. More would likely lead to memory issues. NOTE: This param will not be used if you pass in your own CountVectorizer. Default to `list(1L, 1L)`
#' @param min_topic_size The minimum size of the topic. Increasing this value will lead to a lower number of clusters/topics. NOTE: This param will not be used if you are not using HDBSCAN. Default 10
#' @param nr_topics Specifying the number of topics will reduce the initial number of topics to the value specified. This reduction can take a while as each reduction in topics (-1) activates a c-TF-IDF calculation. If this is set to None, no reduction is applied. Use `auto` to automatically reduce topics using HDBSCAN.
#' @param low_memory Sets UMAP low memory to True to make sure less memory is used. NOTE: This is only used in UMAP. For example, if you use PCA instead of UMAP this parameter will not be used. Default `FALSE`
#' @param calculate_probabilities Whether to calculate the probabilities of all topics per document instead of the probability of the assigned topic per document. This could slow down the extraction of topics if you have many documents (> 100_000). Set this only to True if you have a low amount of documents or if you do not mind more computation time. NOTE: If false you cannot use the corresponding visualization method visualize_probabilities.  Default to `FALSE`
#' @param diversity Whether to use MMR to diversify the resulting topic representations. If set to None, MMR will not be used. Accepted values lie between 0 and 1 with 0 being not at all diverse and 1 being very diverse. Default is `NULL`
#' @param seed_topic_list A list of seed words per topic to converge around.  Default is `NULL`
#' @param umap_model Pass in a UMAP model to be used instead of the default. NOTE: You can also pass in any dimensionality reduction algorithm as long as it has .fit and .transform functions.
#' @param hdbscan_model Pass in a hdbscan.HDBSCAN model to be used instead of the default NOTE: You can also pass in any clustering algorithm as long as it has .fit and .predict functions along with the .labels_ variable. Default `NULL`
#' @param vectorizer_model Pass in a custom CountVectorizer instead of the default model. Default `NULL`
#' @param ctfidf_model Pass in a custom ClassTfidfTransformer instead of the default model. Default `NULL`
#' @param exclude_stop_words if `TRUE` excludes base stop words
#' @param use_key_phrase_vectorizer if `TRUE` uses a keyphrase vectorizer
#' @param is_lower_case if `TRUE` is lowercase
#' @param embedding_model type of embedding model - either an object or `NULL` options include \itemize{
#' \item \href{https://www.sbert.net/docs/pretrained_models.html}{sbert}
#' } and it defaults to `all-MiniLM-L6-v2`
#' @param extra_stop_words
#' @param max_df During fitting ignore keyphrases that have a document frequency strictly higher than the given threshold. Default `1L`
#' @param min_df During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `1L`
#' @param pos_pattern Position patter for keyphrase.  Defaults to `pos_pattern = "<J.*>*<N.*>+",`
#' @param keyphrase_ngram_range
#' @param vocabulary
#' @param stopword_package_sources options if not `NULL` `c("snowball", "stopwords-iso", "smart", "nltk")`
#' @param use_sklearn_vectorizer
#'
#' @return python object
#' @export
#'
#' @examples
#' import_bertopic()
#' data <- sklearn::sk_datasets()
#' docs_all <- data$fetch_20newsgroups(subset = 'all', remove = c('headers', 'footers', 'quotes'))
#' docs <- docs_all["data"]
#' tm <- bert_topic()
#' topic_model <- tm$fit_transform(documents = docs)
#'
#'

bert_topic <-
  function(language = "english",
           top_n_words = 10L,
           use_key_phrase_vectorizer = F,
           use_sklearn_vectorizer = F,
           is_lower_case = T,
           n_gram_range = list(1L, 1L),
           keyphrase_ngram_range = list(1L, 1L),
           min_topic_size = 10L,
           umap_model = NULL,
           hdbscan_model = NULL,
           vectorizer_model = NULL,
           embedding_model = NULL,
           ctfidf_model = NULL,
           nr_topics = NULL,
           low_memory = F,
           exclude_stop_words = T,
           stopword_package_sources = NULL,
           extra_stop_words = NULL,
           calculate_probabilities = T,
           diversity = NULL,
           min_df = 1L,
           max_df = 1L,
           pos_pattern = "<J.*>*<N.*>+",
           seed_topic_list = NULL,
           verbose = T,
           vocabulary = NULL) {
    bertopic <- import_bertopic(assign_to_environment = F)

    if (length(n_gram_range) > 0) {
      n_gram_range <- reticulate::tuple(n_gram_range)
    }

    if (use_key_phrase_vectorizer) {
      "Using keyphrase vectorizer" |> message()
      vectorizer_model <-
        keyphrase_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          exclude_stop_words = exclude_stop_words,
          language = language,
          pos_pattern = pos_pattern,
          extra_stop_words = extra_stop_words
        )
    }

    if (!use_key_phrase_vectorizer &
        length(vectorizer_model) == 0 & use_sklearn_vectorizer) {
      "Using sklearn vectorizer" |> message()
      vectorizer_model <-
        sklearn_vectorizer(
          min_df = min_df,
          max_df =  max_df,
          ngram_range = n_gram_range,
          vocabulary = vocabulary,
          language = language,
          exclude_stop_words = exclude_stop_words,
          extra_stop_words = extra_stop_words
        )
    }
    uses_vectorizer <-
      use_sklearn_vectorizer | use_key_phrase_vectorizer
    if (uses_vectorizer & exclude_stop_words) {
      stop_words <-
        bert_stopwords(
          language = language,
          is_lower_case = is_lower_case,
          extra_stop_words = extra_stop_words,
          stopword_package_sources = stopword_package_sources
        )
      vectorizer_model$stop_words <-
        c(stop_words) |> unique()
    }


    obj <-
      bertopic$BERTopic(
        language = language,
        top_n_words = as.integer(top_n_words),
        n_gram_range = n_gram_range,
        min_topic_size = as.integer(min_topic_size),
        nr_topics = nr_topics,
        low_memory = low_memory,
        calculate_probabilities = calculate_probabilities,
        diversity = diversity,
        seed_topic_list = seed_topic_list,
        verbose = verbose,
        umap_model = umap_model,
        hdbscan_model = hdbscan_model,
        vectorizer_model = vectorizer_model,
        ctfidf_model = ctfidf_model,
        embedding_model = embedding_model
      )

    remove_sw <-
      exclude_stop_words |
      length(stopword_package_sources) > 0 |
      length(extra_stop_words) > 0


    if (remove_sw) {
      "Generating stop-words" |> message()
      stop_words <-
        bert_stopwords(
          language = language,
          is_lower_case = is_lower_case,
          extra_stop_words = extra_stop_words,
          stopword_package_sources = stopword_package_sources
        )

      obj$vectorizer_model$stop_words <-
        stop_words
    }

    # Fix ngramrange
    obj$vectorizer_model$ngram_range <- n_gram_range

    obj

  }


#' Bertopic Backend
#'
#' @return
#' @export
#'
#' @examples
bert_backend <-
  function() {
    bertopic <- import_bertopic(assign_to_environment = F)
    obj <- bertopic$backend
    obj
  }



#' Bertopic Plotting
#'
#' @return
#' @export
#'
#' @examples
bert_plotting <-
  function() {
    bertopic <- import_bertopic(assign_to_environment = F)
    obj <- bertopic$plotting
    obj
  }


# ctfidf ---------------------------------------------------------------


#' Class TFIDF Transformer
#'
#' @param bm25_weighting Uses BM25-inspired idf-weighting procedure instead of the procedure as defined in the c-TF-IDF formula. It uses the following weighting scheme: log(1+((avg_nr_samples - df + 0.5) / (df+0.5))).  Defaults to `FALSE`
#' @param reduce_frequent_words Takes the square root of the bag-of-words after normalizing the matrix. Helps to reduce the impact of words that appear too frequently. Defaults to `FALSE`
#'
#' @return
#' @export
#'
#' @examples
#' library(bertopic)
#' transformer <- class_tfidf_transformer()
#' transformer
class_tfidf_transformer <-
  function(bm25_weighting = F,
           reduce_frequent_words = F) {
    bertopic <- import_bertopic(assign_to_environment = F)
    obj <-
      bertopic$vectorizers$ClassTfidfTransformer(bm25_weighting = bm25_weighting,
                                                 reduce_frequent_words = reduce_frequent_words)
    obj
  }

#' An Online Vectorizer
#'
#' @param decay
#' @param delete_min_df
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
online_count_vectorizer <- function(decay = NULL,
                                    delete_min_df = NULL,
                                    ...) {
  bertopic <- import_bertopic(assign_to_environment = F)
  obj <- bertopic$vectorizers$OnlineCountVectorizer(decay = decay,
                                                    delete_min_df = delete_min_df, ...)
  obj
}


#' Initiate an Empty Clusterer
#'
#' @return
#' @export
#'
#' @examples
base_clusterer <-
  function() {
    bertopic <- import_bertopic(assign_to_environment = F)
    obj <- bertopic$cluster$BaseCluster
    obj
  }


#' The Base Embedder used for creating embedding models
#'
#' @param embedding_model The main embedding model to be used for extracting document and word embedding default `NULL`
#' @param word_embedding_model  The embedding model used for extracting word embeddings only. If this model is selected, then the embedding_model is purely used for creating document embeddings. Default `NULL`
#'
#' @return
#' @export
#'
#' @examples
base_embedder <-
  function(embedding_model = NULL,
           word_embedding_model = NULL) {
    bertopic <- import_bertopic(assign_to_environment = F)
    obj <-
      bertopic$backend$BaseEmbedder(embedding_model = embedding_model,
                                    word_embedding_model = word_embedding_model)

    obj

  }

#' Combine a document- and word-level embedder
#'
#' @return
#' @export
#'
#' @examples
word_doc_embedder <-
  function() {
    bertopic <- import_bertopic(assign_to_environment = F)
    obj <-
      bertopic$backend$WordDocEmbedder
    obj
  }
