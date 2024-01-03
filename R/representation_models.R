



# utils -------------------------------------------------------------------

#' Convert Spacy Matching Pattern to Dictionary
#'
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
spacy_matching_pattern_dict_to_list <-
  function(pattern = "[{'POS': 'ADJ'},
           {'POS': 'NOUN'}]") {
    out <-
      pattern |>
      str_remove_all("^pattern = ") |>
      str_remove_all("\n") |>
      str_replace_all("\\[", "list(") |>
      str_replace_all("\\]", "\\)") |>
      str_replace_all("\\{", "list(") |>
      str_replace_all("\\}", "\\)") |>
      str_replace_all("\\:", "=")

    out <- parse(text = out) |> eval()
    out
  }


# representations ---------------------------------------------------------




#' Import Bertopic Representations
#'
#' @param obj Bertopic Module
#'
#' @return
#' @export
#'
#' @examples
bertopic_representations <-
  function(obj = NULL) {
    if (length(obj) == 0) {
      obj <- import_bertopic(assign_to_environment = F)
    }
    obj$representation
  }

#' Use a KeyBERT-like model to fine-tune the topic representations
#'
#' @param top_n_words The top n words to extract per topic.  Default `10`.
#' @param nr_repr_docs The number of representative documents to extract per cluster.  Default `5`.
#' @param nr_samples The number of candidate documents to extract per cluster.  Default `500`.
#' @param nr_candidate_words The number of candidate words per cluster.  Default `100`.
#' @param random_state The random state for randomly sampling candidate documents.  Default `42`.
#' @param obj Bertopic Module
#' @param numba_threads
#'
#' @return
#' @export
#'
#' @examples
keybert_inspired_representation <-
  function(top_n_words = 10,
           nr_repr_docs = 5,
           nr_samples = 500,
           nr_candidate_words = 10,
           random_state = 42,
           numba_threads = 1,
           obj = NULL) {
    numba <- import_numba()
    numba$set_num_threads(n = as.integer(numba_threads))
    obj <- bertopic_representations(obj = obj)
    out <-
      obj$KeyBERTInspired(
        top_n_words = as.integer(top_n_words),
        nr_repr_docs = as.integer(nr_repr_docs),
        nr_samples = as.integer(nr_samples),
        nr_candidate_words = as.integer(nr_samples),
        random_state = as.integer(random_state)
      )

    attr(out, "representation_method") <- c("keybert")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out
  }

#' A llama.cpp implementation to use as a representation model.
#'
#' @param model Either a string pointing towards a local LLM or a  `llama_cpp.Llama` object.
#' @param prompt
#' @param pipeline_kwargs
#' @param nr_docs
#' @param diversity
#' @param doc_length
#' @param tokenizer
#' @param numba_threads
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
llama_cpp_representation <-
  function(model = "zephyr-7b-alpha.Q4_K_M.gguf",
           prompt = NULL,
           pipeline_kwargs = NULL,
           nr_docs = 4,
           diversity = NULL,
           doc_length = NULL,
           tokenizer = NULL,
           numba_threads = 1,
           obj = NULL) {
    numba <- import_numba()
    numba$set_num_threads(n = as.integer(numba_threads))
    obj <- bertopic_representations(obj = obj)

    if (length(doc_length) > 0) {
      doc_length <- as.integer(doc_length)
    }

    is_model_char <-
      "character" %in% class(model)
    if (is_model_char) {
      oldwd <- getwd()
      setwd("~")

    }

    out <-
      obj$LlamaCPP(
        model = model,
        prompt = prompt,
        nr_docs = as.integer(nr_docs),
        diversity = diversity,
        doc_length = doc_length,
        tokenizer = tokenizer
      )
    if (is_model_char) {
      if (getwd() != oldwd) {
        setwd(oldwd)
      }
    }



    attr(out, "representation_method") <- c("llama_cpp")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out

  }

#' Calculate Maximal Marginal Relevance (MMR) between candidate keywords and the document.
#' MMR considers the similarity of keywords/keyphrases with the document, along with the similarity of already selected keywords and keyphrases. This results in a selection of keywords that maximize their within diversity with respect to the document.
#'
#' @param diversity How diverse the select keywords/keyphrases are. Values range between 0 and 1 with 0 being not diverse at all and 1 being most diverse.  Default `0.1`
#' @param top_n_words The number of keywords/keyhprases to return default `10`
#' @param obj Bertopic Module
#'
#' @return
#' @export
#'
#' @examples
mmr_inspired_representation <-
  function(obj = NULL,
           diversity = .1,
           top_n_words = 10) {
    obj <- bertopic_representations(obj = obj)
    out <-
      obj$MaximalMarginalRelevance(diversity = diversity, top_n_words = as.integer(top_n_words))

    attr(out, "representation_method") <- c("mmr")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out

  }

#' Cohere Representation
#'
#' @param client A cohere.Client
#' @param model Model to use within Cohere, defaults to "xlarge".
#' @param prompt Example Prompt `'I have topic that contains the following documents: [DOCUMENTS]. The topic is described by the following keywords: [KEYWORDS]. Based on the above information, can you give a short label of the topic?'`
#' @param obj BERTopic Object
#' @param delay_in_seconds
#'
#' @return
#' @export
#'
#' @examples
cohere_representation <-
  function(client = NULL,
           model = "xlarge",
           prompt = NULL,
           delay_in_seconds = NULL,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    out <-
      obj$Cohere(
        client = client,
        model = model,
        prompt = prompt,
        delay_in_seconds = delay_in_seconds
      )

    attr(out, "representation_method") <- c("cohere")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out
  }

#' OpenAI Representation Model
#'
#' See \href{https://maartengr.github.io/BERTopic/getting_started/representation/representation.html#chatgpt}{API Example} and  \href{https://maartengr.github.io/BERTopic/changelog.html#version-0141}{Change Log Example}
#'
#' @param model Open AI model NAME
#' @param prompt Set this to True if a GPT-3.5 model is used."  For example: `I have a topic that contains the following documents: [DOCUMENTS]The topic is described by the following keywords: [KEYWORDS]Based on the information above, extract a short topic label in the following format:topic: <topic label>`
#' @param obj BERTopic Object
#' @param delay_in_seconds How Long to Wait?
#' @param chat If `TRUE` enter chat mode.
#' @param open_ai_key API key
#' @param generator_kwargs Kwargs passed to `openai.Completion.create
#' @param exponential_backoff Retry requests with a random exponential backoff
#' @param nr_docs The number of documents to pass to OpenAI if a prompt.
#' @param diversity The diversity of documents to pass to OpenAI.  Accepts values between 0 and 1. A higher values results in passing more diverse documents whereas lower values passes more similar documents.
#' @param doc_length The maximum length of each document. If a document is longer, it will be truncated. If None, the entire document is passed..
#' @param tokenizer The tokenizer used to calculate to split the document into segments" used to count the length of a document.
#' If tokenizer is 'char', then the document is split up into characters which are counted to adhere to `doc_length`
#'  If tokenizer is 'whitespace', the document is split up into words separated by whitespaces. These words are counted and truncated depending on `doc_length`
#' If tokenizer is 'vectorizer', then the internal CountVectorizer is used to tokenize the document. These tokens are counted and trunctated depending on `doc_length`
#' If tokenizer is a callable, then that callable is used to tokenize the document. These tokens are counted and truncated depending on `doc_length`
#'
#' @return
#' @export
#'
#' @examples
open_ai_representation <-
  function(model = "gpt-3.5-turbo",
           open_ai_key = NULL,
           prompt = NULL,
           generator_kwargs = NULL,
           delay_in_seconds = 10,
           exponential_backoff = FALSE,
           chat = FALSE,
           nr_docs = 4L,
           diversity = NULL,
           doc_length = NULL,
           tokenizer = NULL,
           obj = NULL) {
    open_ai <- import_openai()
    client <- open_ai$OpenAI
    if (length(open_ai_key) == 0) {
      open_ai_key <- Sys.getenv("OPENAI_API_KEY")
    }
    client <- client(api_key = open_ai_key)
    obj <- bertopic_representations(obj = obj)
    out <-
      obj$OpenAI(
        client = cleint,
        model = model,
        prompt = prompt,
        generator_kwargs = generator_kwargs,
        delay_in_seconds = as.integer(delay_in_seconds),
        exponential_backoff = exponential_backoff,
        chat = chat,
        nr_docs = nr_docs,
        diversity = diversity,
        doc_length = doc_length,
        tokenizer = tokenizer
      )

    attr(out, "representation_method") <- c("openai")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out
  }


#' Extract Topic Keywords based on their Part-of-Speech
#'
#' @param model The Spacy model to use
#' @param top_n_words The top n words to extract.  Default `10`
#' @param pos_patterns Patterns for Spacy to use. See https://spacy.io/usage/rule-based-matching and https://demos.explosion.ai/matcher and https://www.kaggle.com/code/curiousprogrammer/entity-extraction-and-classification-using-spacy for more and alsohttps://spacy.io/usage/rule-based-matching
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
#' library(bertopic)
#' part_of_speech_representation(pos_patterns = list(list(list('POS' = 'ADJ'), list('POS' = 'NOUN')), list(list('POS' = 'NOUN')), list(list('POS' = 'ADJ'))))
#'
part_of_speech_representation <-
  function(model = 'en_core_web_sm',
           top_n_words = 10,
           pos_patterns = NULL,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)

    if (length(pos_patterns) == 0) {
      out <-
        obj$PartOfSpeech(model = model,
                         top_n_words = as.integer(top_n_words))
    }

    if (length(pos_patterns) > 0) {
      out <-
        obj$PartOfSpeech(
          model = model,
          top_n_words = as.integer(top_n_words),
          pos_patterns = pos_patterns
        )
    }


    attr(out, "representation_method") <- c("part_of_speech")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out
  }

#' Zero-shot Classification on topic keywords with candidate labels
#'
#' @param candidate_topics A list of labels to assign to the topics if they exceed min_prob
#' @param model A transformers pipeline that should be initialized as "zero-shot-classification". For example, pipeline("zero-shot-classification", model="facebook/bart-large-mnli")
#' @param pipeline_kwargs  Kwargs that you can pass to the transformers.pipeline when it is called. NOTE: Use {"multi_label": True} to extract multiple labels for each topic.
#' @param min_prob The minimum probability to assign a candidate label to a topic
#' @param obj Bertopic Object
#'
#' @return
#' @export
#'
#' @examples
zeroshot_representation <-
  function(candidate_topics = NULL,
           model = "facebook/bart-large-mnli",
           pipeline_kwargs = list(),
           min_prob = .8,
           obj = NULL) {
    obj <- bertopic_representations(obj = NULL)
    out <-
      obj$ZeroShotClassification(candidate_topics = candidate_topics,
                                 model = model,
                                 min_prob = min_prob)

    attr(out, "representation_method") <- c("zeroshot")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out
  }


#' Text2Text or text generation with transformers
#'
#' @param model A transformers pipeline that should be initialized as "text-generation" for gpt-like models or "text2text-generation" for T5-like models. For example, pipeline('text-generation', model='gpt2'). If a string is passed, "text-generation" will be selected by default.
#' @param prompt The prompt to be used in the model. If no prompt is given, self.default_prompt_ is used instead. NOTE: Use "[KEYWORDS]" and "[DOCUMENTS]" in the prompt to decide where the keywords and documents need to be inserted.
#' @param pipeline_kwargs Kwargs that you can pass to the transformers.pipeline when it is called.
#' @param random_state A random state to be passed to transformers.set_seed
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
transformer_representation <-
  function(model = 'gpt2',
           prompt = NULL,
           pipeline_kwargs = NULL,
           random_state = 42,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    out <-
      out <-
      obj$TextGeneration(
        model = model,
        prompt = prompt,
        pipeline_kwargs = pipeline_kwargs,
        random_state = as.integer(random_state)
      )

    attr(out, "representation_method") <- c("transformer")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters


    out
  }

#' Lang Chain Representation
#'
#' @param chain
#' @param prompt
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
langchain_representation <-
  function(chain = NULL,
           prompt = NULL,
           nr_docs = 4,
           diversity = NULL,
           doc_length = NULL,
           tokenizer = NULL,
           chain_config = NULL,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)

    if (length(doc_length) > 0) {
      doc_length <- as.integer(doc_length)
    }

    out <-
      obj$LangChain(
        chain = chain,
        prompt = prompt,
        nr_docs = as.integer(nr_docs),
        diversity = diversity,
        doc_length = doc_length,
        tokenizer = tokenizer,
        chain_config = chain_config
      )

    attr(out, "representation_method") <- c("langchain")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters
    out

  }
