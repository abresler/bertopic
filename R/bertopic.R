# https://scikit-learn.org/0.15/index.html

# stopwords ---------------------------------------------------------------


#' bert
#'
#' @return
#' @export
#'
#' @examples
bert_base_stop_words <-
  function(){
    c('between', 'nevertheless', 'so', 'is', 'me', 'should', 'throughout', 'someone', 'hereupon', 'than', 'cannot', 'such', 'amount', 'nobody', 'hereafter', 'noone', 'under', 'or', 'five', 'name', 'whereas', 'neither', 'everywhere', 'somewhere', 'top', 'de', 'hereby', 'seem', 'have', 'before', 'eg', 'yourself', 'thin', 'across', 'though', 'this', 'except', 'very', 'nothing', 'become', 'once', 'herein', 'among', 'ie', 'one', 'nowhere', 'via', 'hers', 'see', 'thick', 'why', 'ourselves', 'whether', 'behind', 'anything', 'call', 'latterly', 'wherein', 'either', 'herself', 'amoungst', 'each', 'whenever', 'some', 'alone', 'could', 'less', 'per', 'everyone', 'bottom', 'do', 'thereafter', 'also', 'give', 'although', 'becomes', 'an', 'thru', 'to', 'however', 'there', 'which', 'cry', 'move', 'along', 'due', 'from', 'are', 'upon', 'put', 'within', 'beside', 'her', 'twenty', 'how', 'ever', 'yourselves', 'moreover', 'ltd', 'too', 'after', 'whence', 'other', 're', 'others', 'seeming', 'many', 'a', 'two', 'thereby', 'thence', 'again', 'those', 'mostly', 'further', 'whereafter', 'whereupon', 'least', 'myself', 'no', 'onto', 'besides', 'all', 'she', 'several', 'etc', 'whole', 'else', 'next', 'done', 'serious', 'six', 'hence', 'twelve', 'ten', 'mill', 'through', 'them', 'even', 'detail', 'therein', 'show', 'itself', 'un', 'you', 'your', 'thereupon', 'above', 'seemed', 'con', 'part', 'otherwise', 'made', 'he', 'whither', 'ours', 'any', 'until', 'three', 'anyone', 'latter', 'might', 'last', 'since', 'same', 'below', 'where', 'well', 'of', 'rather', 'more', 'up', 'few', 'during', 'anywhere', 'into', 'system', 'who', 'becoming', 'not', 'front', 'third', 'elsewhere', 'fire', 'perhaps', 'side', 'both', 'toward', 'afterwards', 'fill', 'another', 'in', 'four', 'anyhow', 'against', 'amongst', 'themselves', 'namely', 'his', 'much', 'being', 'take', 'towards', 'empty', 'eight', 'full', 'found', 'formerly', 'and', 'enough', 'sometime', 'mine', 'be', 'fifty', 'but', 'get', 'had', 'by', 'been', 'sixty', 'then', 'must', 'only', 'back', 'couldnt', 'cant', 'they', 'yours', 'has', 'together', 'down', 'please', 'inc', 'while', 'every', 'still', 'may', 'whoever', 'it', 'nor', 'meanwhile', 'became', 'on', 'first', 'about', 'am', 'him', 'keep', 'was', 'my', 'forty', 'co', 'hasnt', 'beforehand', 'out', 'sincere', 'if', 'often', 'over', 'were', 'thus', 'now', 'anyway', 'because', 'describe', 'here', 'already', 'himself', 'around', 'yet', 'somehow', 'that', 'what', 'therefore', 'as', 'never', 'former', 'whose', 'will', 'bill', 'whatever', 'would', 'go', 'none', 'beyond', 'off', 'fifteen', 'at', 'nine', 'can', 'most', 'find', 'hundred', 'without', 'we', 'their', 'indeed', 'almost', 'whereby', 'its', 'with', 'something', 'everything', 'i', 'eleven', 'the', 'interest', 'our', 'for', 'own', 'whom', 'these', 'always', 'wherever', 'us', 'seems', 'sometimes', 'when')
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
    bertopic <- reticulate::import("bertopic")
    ! 'bertopic' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('bertopic', bertopic, envir = .GlobalEnv)
    }
    bertopic
  }


# stup --------------------------------------------------------------------



#' Initiate BERT Topic Model
#'
#' Functions for unsupervised clustering algorithms.
#'
#' \itemize{
#' \item \href{https://maartengr.github.io/BERTopic/api/bertopic.html}berttopic}
#' }
#'
#' @param verbose
#' @param top_n_words
#' @param language
#' @param n_gram_range
#' @param min_topic_size
#' @param nr_topics
#' @param low_memory
#' @param calculate_probabilities
#' @param diversity
#' @param seed_topic_list
#' @param umap_model
#' @param hdbscan_model
#' @param vectorizer_model
#' @param ctfidf_model
#' @param exclude_stop_words
#' @param use_key_phrase_vectorizer
#' @param is_lower_case
#' @param embedding_model
#' @param extra_stop_words
#' @param min_df
#' @param max_df
#' @param pos_pattern
#' @param keyphrase_ngram_range
#' @param vocabulary
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
           is_lower_case = T,
           n_gram_range = list(1L, 3L),
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

    if (!use_key_phrase_vectorizer & length(vectorizer_model) == 0) {
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
