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
#' \href{https://maartengr.github.io/BERTopic}{BERTopic API from python} and other examples \href{https://github.com/MaartenGr/BERTopic}{here}
#'
#' @param assign_to_environment if \code{TRUE} assigns to environment
#' @param path
#' @param numba_threads
#'
#' @return python object
#' @export
#'
#' @examples
#' library(bertopic)
#' import_bertopic()
import_bertopic <-
  function(assign_to_environment = T,
           path = NULL,
           numba_threads = 1) {
    select_correct_python(path = path)
    obj <- reticulate::import("bertopic")
    ! 'bertopic' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('bertopic', obj, envir = .GlobalEnv)
    }
    numba <- import_numba()
    numba$set_num_threads(as.integer(numba_threads))

    obj
  }

#' Import Numba
#'
#' @param assign_to_environment
#' @param path
#'
#' @return
#' @export
#'
#' @examples
import_numba <-
  function(assign_to_environment = T,
           path = NULL) {
    select_correct_python(path = path)
    obj <- reticulate::import("numba")
    ! 'numba' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('numba', obj, envir = .GlobalEnv)
    }
    obj
  }


# stup --------------------------------------------------------------------
#' Set BERTopic Model Attriubtes
#'
#' @param obj BERTopic Object
#' @param representation_model  If not `NULL` representation model
#'
#' @return
#' @export
#'
#' @examples
set_bert_attributes <-
  function(obj, representation_model = NULL) {
    base_params <-
      obj |> bert_parameters(use_dfc_method = T, return_attributes = T)

    attr(obj, "base_parameters") <- base_params

    hdbscan_params <-
      obj$hdbscan_model |> bert_parameters(return_attributes = T)

    attr(obj, "hdbscan_parameters") <- hdbscan_params

    ctfidf_params <-
      obj$ctfidf_model$get_params() |> flatten_df() |>  tbl_bert_parameter_features_to_attribute()

    attr(obj, "cftfidf_parameters") <- ctfidf_params

    umap_params <-
      obj$umap_model |> bert_parameters(return_tibble = T, return_attributes = T)

    attr(obj, "umap_parameters") <- umap_params

    vectorizer_params <-
      obj$vectorizer_model |> bert_parameters(
        return_tibble = T,
        use_dfc_method = T,
        return_attributes = T
      )

    attr(obj, "vectorizer_parameters") <- vectorizer_params
    if (length(representation_model) > 0) {
      representation_method <-
        attributes(representation_model)[["representation_method"]]
      attr(obj, "representation_model") <- representation_method
      representation_method_parameters <-
        attributes(representation_model)[["input_parameters"]]

      attr(obj, "representation_method_parameters") <-
        representation_method_parameters
    }
    obj
  }


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
#' @param extra_stop_words vector of other stopwords
#' @param max_df During fitting ignore keyphrases that have a document frequency strictly higher than the given threshold. Default `1L`
#' @param min_df During fitting ignore keyphrases that have a document frequency strictly lower than the given threshold. This value is also called cut-off in the literature.  Default `1L`
#' @param pos_pattern Position patter for keyphrase.  Defaults to `pos_pattern = "<J.*>*<N.*>+",`
#' \href{https://maartengr.github.io/KeyBERT/guides/countvectorizer.html#languages}{Position Pattern via KeyBERT}
#' \href{https://keyphrase-vectorizers.readthedocs.io/en/latest/api.html}{KeyPhrase POS API}
#' \href{https://github.com/TimSchopf/KeyphraseVectorizers#custom-pos-tagger}{Custom Tagger via KeyPhrase}
#' @param keyphrase_ngram_range If not `NULL` range for keyphrase
#' @param vocabulary
#' @param stopword_package_sources options if not `NULL` `c("snowball", "stopwords-iso", "smart", "nltk")`
#' @param use_sklearn_vectorizer if `TRUE` uses SKLearn vetorizer
#' @param representation_model The base representation model for fine-tuning topic representations
#' @param numba_threads
#' @param set_attributes
#' @param zeroshot_topic_list
#' @param zeroshot_min_similarity
#' @param workers
#' @param decay
#' @param delete_min_df
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
           representation_model = NULL,
           top_n_words = 10L,
           use_key_phrase_vectorizer = F,
           use_sklearn_vectorizer = F,
           is_lower_case = T,
           n_gram_range = list(1L, 1L),
           zeroshot_topic_list = NULL,
           zeroshot_min_similarity = .7,
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
           min_df = 1L,
           max_df = 1L,
           pos_pattern = "<J.*>*<N.*>+",
           seed_topic_list = NULL,
           verbose = T,
           vocabulary = NULL,
           workers = 6L,
           decay = NULL,
           delete_min_df = NULL,
           numba_threads = 1L,
           set_attributes = TRUE) {
    bertopic <- import_bertopic(assign_to_environment = F, numba_threads = numba_threads)


    if (length(zeroshot_topic_list) > 0) {
      if (class(zeroshot_topic_list) != "list") {
        zeroshot_topic_list <- as.list(zeroshot_topic_list)
      }
    }

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
          extra_stop_words = extra_stop_words,
          workers = workers,
          decay = decay,
          delete_min_df  = delete_min_df
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
      use_sklearn_vectorizer |
      use_key_phrase_vectorizer
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
        representation_model = representation_model,
        language = language,
        top_n_words = as.integer(top_n_words),
        n_gram_range = n_gram_range,
        min_topic_size = as.integer(min_topic_size),
        nr_topics = nr_topics,
        low_memory = low_memory,
        calculate_probabilities = calculate_probabilities,
        seed_topic_list = seed_topic_list,
        verbose = verbose,
        umap_model = umap_model,
        hdbscan_model = hdbscan_model,
        vectorizer_model = vectorizer_model,
        ctfidf_model = ctfidf_model,
        embedding_model = embedding_model,
        zeroshot_min_similarity = zeroshot_min_similarity,
        zeroshot_topic_list = zeroshot_topic_list
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

    if (set_attributes) {
      obj <-
        set_bert_attributes(obj = obj, representation_model = representation_model)
    }

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



# plot --------------------------------------------------------------------


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


# attributes_and_features -------------------------------------------------


#' Convert BERT Parameter Table to Attributes
#'
#' @param data
#' @param feature_separator How to concatenate features.  Defaults to `@`
#' @param text_separator How to concatenate text.  Defaults to `|`
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_parameter_features_to_attribute <-
  function(data,
           feature_separator = "@",
           text_separator = "|") {
    if (!data |> hasName("feature")) {
      data <- data |>
        tidyr::gather(feature, value)
    }

    data |>
      tidyr::unite(feature, feature, value, sep = feature_separator) |>
      pull() |>
      str_c(collapse = text_separator)
  }

#' BERTopic Parameters
#'
#' @param obj topic model object
#' @param deep if `TRUE` returns deep information
#' @param return_tibble If `TRUE` returns a `tibble`
#' @param return_attributes If `TRUE` returns concatenated character vector of the parameters
#'
#' @return
#' @export
#'
#' @examples
bert_parameters <-
  function(obj,
           deep = FALSE,
           return_tibble = FALSE,
           use_dfc_method = FALSE,
           return_attributes = FALSE) {
    out <- obj$get_params(deep = deep)

    if (return_attributes) {
      return_tibble <- TRUE
    }

    if (!return_tibble) {
      return(out)
    }
    all_classes <-
      out |> map(class)

    tbl_classes <-
      seq_along(all_classes) |>
      map_dfr(function(x) {
        parameter <- names(all_classes[x])
        class_type <- all_classes[[x]] |> str_c(collapse = " | ")
        tibble(parameter, class = class_type) |>
          mutate(
            is_excluded_feature = !class |> str_detect("list|numeric|character|factor|integer|logical")
          )
      })

    keep_params <-
      tbl_classes |>
      filter(!is_excluded_feature) |>
      pull(parameter)

    out <- out[names(out) %in% keep_params]

    if (out |> hasName("pos_patterns") | use_dfc_method) {
      out <-
        names(out) |>
        map_dfc(function(x) {
          if (x == "model") {
            return(invisible())
          }
          value <-
            out[[x]] |>
            unlist() |>
            as.character() |>
            str_c(collapse = ", ")

          tibble(UQ(x) := value)
        })
    } else {
      out <-
        out |> purrr::flatten_df()

    }

    if (return_attributes) {
      out <- tbl_bert_parameter_features_to_attribute(data = out)
    }

    out
  }

#' Return Tibble of BERT Attributes
#'
#' @param obj BERTopic Object
#' @param return_clean if `TRUE` returns clean set of parameters
#' @param return_wide if `TRUE` returns it in wide form
#' @param parameter_filter if not `NULL` filters the attrbutes
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_attributes <-
  function(obj,
           return_clean = F,
           return_wide = F,
           parameter_filter = NULL) {
    out <- attributes(obj)

    if (return_wide) {
      return_clean <- TRUE
    }

    if (length(out) == 0) {
      message("No attrbutes") |> message()
    }

    dat <- out[names(out)[!names(out) %in% "class"]] |> flatten_df()

    if (!return_clean) {
      return(dat)
    }

    if (return_clean) {
      dat <-
        names(dat) |>
        map_dfr(function(x) {
          values <-
            dat[[x]] |>
            str_split("\\|") |>
            flatten_chr()
          d <- tibble(value = values) |>
            tidyr::separate(
              value ,
              into = c("feature", "value"),
              sep = "\\@",
              convert = TRUE,
              extra = "merge",
              fill = "left"
            ) |>
            mutate(parameter = x) |>
            select(parameter, everything()) |>
            mutate_all(as.character)

          if (x == "representation_method") {
            d <- d |> mutate(feature = "model")
          }


          d
        })
    }

    dat <- dat |>
      filter(value != "NA")

    if (length(parameter_filter) > 0) {
      param_slugs <-
        parameter_filter |> str_c(collapse = "|")
      dat <- dat |>
        filter(parameter |> str_detect(param_slugs))
    }

    if (return_wide) {
      dat <-
        dat |> tidyr::unite(parameter, parameter, feature , sep = "_") |> tidyr::spread(parameter, value, convert = TRUE)
    }

    names(dat) <-
      names(dat) |>
      str_remove_all("_NA$")


    dat
  }

#' BERTopic Transform
#'
#' After having fit a model, use transform to predict new instances
#'
#' @param obj BERTopic Object
#' @param documents  A single document or a list of documents to fit on
#' @param embeddings If not `NULL` Pre-trained document embeddings. These can be used instead of the sentence-transformer model.
#'
#' @return
#' @export
#'
#' @examples
bert_transform <-
  function(obj, documents, embeddings = NULL) {
    obj <- obj$transform(documents = documents, embeddings = embeddings)

    obj
  }

#' BERTopic Fit
#'
#' Fit the models (Bert, UMAP, and, HDBSCAN) on a collection of documents and generate topics
#'
#' @param obj BERTopic Object
#' @param documents  A single document or a list of documents to fit on
#' @param embeddings If not `NULL` Pre-trained document embeddings. These can be used instead of the sentence-transformer model.
#' @param y If Not `NULL` The target class for (semi)-supervised modeling. Use -1 if no class for a specific instance is specified.
#'
#' @return
#' @export
#'
#' @examples
bert_fit <-
  function(obj,
           documents,
           embeddings = NULL,
           y = NULL,
           images = images) {
    out <-
      obj$fit(
        documents = documents,
        embeddings = embeddings,
        y = y,
        images = images
      )

    out
  }

#' BERTopic Fit and Transform
#'
#' Fit the models on a collection of documents, generate topics, and return the docs with topics.
#'
#' @param obj BERTopic Object
#' @param documents  A single document or a list of documents to fit on
#' @param embeddings If not `NULL` Pre-trained document embeddings. These can be used instead of the sentence-transformer model.
#' @param y If Not `NULL` The target class for (semi)-supervised modeling. Use -1 if no class for a specific instance is specified.#'
#' @return
#' @export
#'
#' @examples
bert_fit_transform <-
  function(obj,
           documents,
           embeddings = NULL,
           y = NULL) {
    if (length(y) > 0) {
      y <- as.numeric(y)
    }
    out <-
      obj$fit_transform(documents = documents,
                        embeddings = embeddings,
                        y = y)

    out


  }

#' BERTopic Fit and Transform from tibble.
#'
#' Fit the models on a collection of documents, generate topics, and return the docs with topics.
#'
#' @param obj BERTopic Object
#' @param data a tibble
#' @param text_field name of text field
#' @param class_fields if not `NULL` `y` or `class` fields.
#' @param embeddings If not `NULL` Pre-trained document embeddings. These can be used instead of the sentence-transformer model.
#' @param images
#'
#' @return
#' @export
#'
#' @examples
tbl_bert_fit_transform <-
  function(obj,
           data = NULL,
           text_field = NULL,
           class_fields = NULL,
           embeddings = NULL,
           images = NULL) {
    if (length(data) == 0) {
      message("Requires data")
      return(obj)
    }

    if (length(text_field) == 0) {
      message("Requires text field")
      return(obj)
    }

    data <- data |> filter(!is.na(!!sym(text_field)))

    documents <- data[[text_field]]

    if (length(class_fields) == 0) {
      y <- NULL
    }

    if (length(class_fields) > 0) {
      data <-
        data |>
        tbl_unite_features(
          unite_columns = class_fields,
          new_column = "class",
          to_factor = TRUE
        )

      y <- data[["id_class"]] |> as.numeric()
    }

    out <-
      obj$fit_transform(
        documents = documents,
        embeddings = embeddings,
        images = images,
        y = y
      )

    out

  }

# ctfidf ---------------------------------------------------------------


#' Class TFIDF Transformer
#'
#' @param bm25_weighting Uses BM25-inspired idf-weighting procedure instead of the procedure as defined in the c-TF-IDF formula. It uses the following weighting scheme: log(1+((avg_nr_samples - df + 0.5) / (df+0.5))).  Defaults to `FALSE`
#' @param reduce_frequent_words Takes the square root of the bag-of-words after normalizing the matrix. Helps to reduce the impact of words that appear too frequently. Defaults to `FALSE`
#' @param seed_words Specific words that will have their idf value increased by the value of seed_multiplier. NOTE: This will only increase the value of words that have an exact match
#' @param seed_multiplier The value with which the idf values of the words in seed_words are multiplied.  Default 2
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
           seed_words = NULL,
           seed_multiplier = 2,
           reduce_frequent_words = F) {
    bertopic <- import_bertopic(assign_to_environment = F)

    obj <-
      bertopic$vectorizers$ClassTfidfTransformer(
        bm25_weighting = bm25_weighting,
        reduce_frequent_words = reduce_frequent_words,
        seed_words = seed_words,
        seed_multiplier = as.integer(seed_multiplier)
      )
    obj
  }


# other -------------------------------------------------------------------



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

#' A Class-based TF-IDF procedure using scikit-learns TfidfTransformer as a base.
#' c-TF-IDF can best be explained as a TF-IDF formula adopted for multiple classes by joining all documents per class. Thus, each class is converted to a single document instead of set of documents. The frequency of each word x is extracted for each class c and is l1 normalized. This constitutes the term frequency.
#' Then, the term frequency is multiplied with IDF which is the logarithm of 1 plus the average number of words per class A divided by the frequency of word x across all classes.
#'
#' @param bm25_weighting  Uses BM25-inspired idf-weighting procedure instead of the procedure as defined in the c-TF-IDF formula. It uses the following weighting scheme: log(1+((avg_nr_samples - df + 0.5) / (df+0.5))). Default is `FALSE`
#' @param reduce_frequent_words Takes the square root of the bag-of-words after normalizing the matrix. Helps to reduce the impact of words that appear too frequently. Default is `FALSE`
#' @param obj bertopic object
#' @param seed_words
#' @param seed_multiplier
#'
#' @return
#' @export
#'
#' @examples
#' library(bertopic)
#' ctfidf(bm25_weighting = TRUE, reduce_frequent_words = FALSE)
ctfidf <-
  function(bm25_weighting = FALSE,
           reduce_frequent_words = FALSE,
           seed_words = NULL,
           seed_multiplier = 2,
           obj = NULL) {
    if (length(obj) == 0)   {
      obj <- import_bertopic(assign_to_environment = F)
    }

    obj$vectorizers$ClassTfidfTransformer(bm25_weighting = bm25_weighting,
                                          reduce_frequent_words = reduce_frequent_words,
                                          seed_words = seed_words,
                                          seed_multiplier = as.integer(seed_multiplier))
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


# merge -------------------------------------------------------------------


#' Merge multiple pre-trained BERTopic models into a single model
#'
#' @param obj BERTopic object
#' @param models A list of fitted BERTopic models
#' @param min_similarity The minimum similarity for when topics are merged.  Default .7
#' @param embedding_model Additionally load in an embedding model if necessary.
#'
#' @return
#' @export
#'
#' @examples
bert_merge_models <-
  function(obj,
           models = NULL,
           min_similarity = .7,
           embedding_model = NULL) {
    if (length(models) == 0) {
      "Provide models" |> message()
      return(invisible())
    }

    if (class(models) != "list") {
      models <- list(models)
    }

    obj$merge_models(
      models = models,
      min_similarity = min_similarity,
      embedding_model = embedding_model
    )
  }
