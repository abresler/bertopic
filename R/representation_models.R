




# utils -------------------------------------------------------------------

#' Convert Spacy Matching Pattern to Dictionary
#'
#' @param pattern Character. A Spacy matching pattern as a string to convert to R list format.
#'
#' @returns A list representation of the Spacy matching pattern.
#' @export
#'
#' @examples
#' \dontrun{
#' pattern <- "[{'POS': 'ADJ'}, {'POS': 'NOUN'}]"
#' spacy_matching_pattern_dict_to_list(pattern = pattern)
#' }
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
#' @param obj BERTopic module object or `NULL`. If `NULL`, imports the module.
#' @param numba_threads Integer. Number of threads for Numba. Default `1`.
#' @param use_token_parallel Logical. If `TRUE`, uses token-level parallelization. Default `TRUE`.
#'
#' @returns The BERTopic representations module object.
#' @export
#'
#' @examples
#' \dontrun{
#' rep_module <- bertopic_representations()
#' }
bertopic_representations <-
  function(obj = NULL,
           numba_threads = 1,
           use_token_parallel = TRUE) {
    if (length(obj) == 0) {
      obj <-
        import_bertopic(assign_to_environment = F, numba_threads = numba_threads,
                        use_token_parallel = use_token_parallel)
    }
    obj$representation
  }

#' Use a KeyBERT-like model to fine-tune the topic representations
#'
#' @param top_n_words Integer. The top n words to extract per topic. Default `10`.
#' @param nr_repr_docs Integer. The number of representative documents to extract per cluster. Default `5`.
#' @param nr_samples Integer. The number of candidate documents to extract per cluster. Default `500`.
#' @param nr_candidate_words Integer. The number of candidate words per cluster. Default `100`.
#' @param random_state Integer. The random state for randomly sampling candidate documents. Default `42`.
#' @param numba_threads Integer. Number of threads for Numba. Default `1`.
#' @param obj BERTopic module object or `NULL`.
#'
#' @returns A Python KeyBERTInspired representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- keybert_inspired_representation(top_n_words = 10L)
#' }
keybert_inspired_representation <-
  function(top_n_words = 10,
           nr_repr_docs = 5,
           nr_samples = 500,
           nr_candidate_words = 100,
           random_state = 42,
           numba_threads = 1,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    out <-
      obj$KeyBERTInspired(
        top_n_words = as.integer(top_n_words),
        nr_repr_docs = as.integer(nr_repr_docs),
        nr_samples = as.integer(nr_samples),
        nr_candidate_words = as.integer(nr_candidate_words),
        random_state = as.integer(random_state)
      )

    attr(out, "representation_method") <- c("keybert")
    input_parameters <-
      bert_parameters(obj = out, return_attributes = TRUE)
    attr(out, "input_parameters") <- input_parameters

    out
  }

#' A llama.cpp implementation to use as a representation model
#'
#' @param model Character or Python object. Either a string pointing to a local LLM file or a `llama_cpp.Llama` object.
#' @param prompt Character or `NULL`. The prompt template for the model.
#' @param pipeline_kwargs List or `NULL`. Keyword arguments to pass to the pipeline.
#' @param nr_docs Integer. Number of documents to pass to the model. Default `4`.
#' @param diversity Numeric or `NULL`. Diversity of documents to pass to the model.
#' @param doc_length Integer or `NULL`. Maximum length of each document.
#' @param tokenizer Character or `NULL`. Tokenizer to use for document length calculation.
#' @param numba_threads Integer. Number of threads for Numba. Default `1`.
#' @param obj BERTopic module object or `NULL`.
#'
#' @returns A Python LlamaCPP representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- llama_cpp_representation(model = "zephyr-7b-alpha.Q4_K_M.gguf")
#' }
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
    # numba <- import_numba()
    # numba$set_num_threads(n = as.integer(numba_threads))
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

#' Calculate Maximal Marginal Relevance (MMR) between candidate keywords and the document
#'
#' MMR considers the similarity of keywords/keyphrases with the document, along with the similarity of already selected keywords and keyphrases. This results in a selection of keywords that maximize their within diversity with respect to the document.
#'
#' @param obj BERTopic module object or `NULL`.
#' @param diversity Numeric. Diversity level for selected keywords. Values range between 0 and 1 with 0 being not diverse and 1 being most diverse. Default `0.1`.
#' @param top_n_words Integer. The number of keywords/keyphrases to return. Default `10`.
#'
#' @returns A Python MaximalMarginalRelevance representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- mmr_inspired_representation(diversity = 0.1, top_n_words = 10L)
#' }
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
#' @param client Python Cohere client object or `NULL`.
#' @param model Character. Model to use within Cohere. Default `"xlarge"`.
#' @param prompt Character or `NULL`. Custom prompt template. Use `[DOCUMENTS]` and `[KEYWORDS]` placeholders.
#' @param delay_in_seconds Numeric or `NULL`. Delay between API calls in seconds.
#' @param obj BERTopic object or `NULL`.
#'
#' @returns A Python Cohere representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- cohere_representation(model = "xlarge")
#' }
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
#' See \href{https://maartengr.github.io/BERTopic/getting_started/representation/representation.html#chatgpt}{API Example} and \href{https://maartengr.github.io/BERTopic/changelog.html#version-0141}{Change Log Example}
#'
#' @param model Character. OpenAI model name (e.g., `"gpt-3.5-turbo"`). Default `"gpt-3.5-turbo"`.
#' @param open_ai_key Character or `NULL`. OpenAI API key. If `NULL`, uses `OPENAI_API_KEY` environment variable.
#' @param prompt Character or `NULL`. Custom prompt template. Use `[DOCUMENTS]` and `[KEYWORDS]` placeholders.
#' @param generator_kwargs List or `NULL`. Keyword arguments to pass to `openai.Completion.create`.
#' @param delay_in_seconds Integer. Delay between API calls in seconds. Default `10`.
#' @param exponential_backoff Logical. If `TRUE`, retries requests with random exponential backoff. Default `FALSE`.
#' @param chat Logical. If `TRUE`, uses chat mode. Default `FALSE`.
#' @param nr_docs Integer. Number of documents to pass to OpenAI. Default `4L`.
#' @param diversity Numeric or `NULL`. Diversity of documents (0-1). Higher values pass more diverse documents.
#' @param doc_length Integer or `NULL`. Maximum length of each document. If exceeded, document is truncated.
#' @param tokenizer Character or `NULL`. Tokenizer for document length calculation. Options: `"char"`, `"whitespace"`, `"vectorizer"`, or callable.
#' @param obj BERTopic object or `NULL`.
#'
#' @returns A Python OpenAI representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- open_ai_representation(model = "gpt-3.5-turbo", open_ai_key = "sk-...")
#' }
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
    if (length(open_ai_key) == 0 || !nzchar(open_ai_key)) {
      open_ai_key <- Sys.getenv("OPENAI_API_KEY")
    }
    if (!nzchar(open_ai_key)) {
      rlang::abort("OPENAI_API_KEY not set. Pass `open_ai_key` or set environment variable.")
    }
    client <- client(api_key = open_ai_key)
    obj <- bertopic_representations(obj = obj)
    out <-
      obj$OpenAI(
        client = client,
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
#' @param model Character. The Spacy model to use. Default `"en_core_web_sm"`.
#' @param top_n_words Integer. The top n words to extract. Default `10`.
#' @param pos_patterns List or `NULL`. Patterns for Spacy to use for matching. See Spacy documentation for pattern structure.
#' @param obj BERTopic object or `NULL`.
#'
#' @returns A Python PartOfSpeech representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' library(bertopic)
#' part_of_speech_representation(
#'   pos_patterns = list(
#'     list(list('POS' = 'ADJ'), list('POS' = 'NOUN')),
#'     list(list('POS' = 'NOUN')),
#'     list(list('POS' = 'ADJ'))
#'   )
#' )
#' }
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
#' @param candidate_topics List or `NULL`. List of labels to assign to topics if they exceed min_prob.
#' @param model Character. Transformers model name for zero-shot classification (e.g., `"facebook/bart-large-mnli"`). Default `"facebook/bart-large-mnli"`.
#' @param pipeline_kwargs List. Keyword arguments to pass to `transformers.pipeline`. Use `list(multi_label = TRUE)` for multiple labels per topic. Default `list()`.
#' @param min_prob Numeric. Minimum probability to assign a candidate label to a topic. Default `0.8`.
#' @param obj BERTopic object or `NULL`.
#'
#' @returns A Python ZeroShotClassification representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- zeroshot_representation(candidate_topics = c("topic1", "topic2"))
#' }
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
#' @param model Character. Transformers model name (e.g., `"gpt2"`). Default `"gpt2"`.
#' @param prompt Character or `NULL`. Prompt template for the model. Use `[KEYWORDS]` and `[DOCUMENTS]` placeholders.
#' @param pipeline_kwargs List or `NULL`. Keyword arguments to pass to `transformers.pipeline`.
#' @param random_state Integer. Random seed for transformers. Default `42`.
#' @param obj BERTopic object or `NULL`.
#'
#' @returns A Python TextGeneration representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- transformer_representation(model = "gpt2")
#' }
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
#' @param chain Python LangChain chain object or `NULL`.
#' @param prompt Character or `NULL`. Prompt template for the chain.
#' @param nr_docs Integer. Number of documents to pass to the chain. Default `4`.
#' @param diversity Numeric or `NULL`. Diversity of documents to pass to the chain.
#' @param doc_length Integer or `NULL`. Maximum length of each document.
#' @param tokenizer Character or `NULL`. Tokenizer for document length calculation.
#' @param chain_config List or `NULL`. Additional chain configuration parameters.
#' @param obj BERTopic object or `NULL`.
#'
#' @returns A Python LangChain representation object with representation_method and input_parameters attributes.
#' @export
#'
#' @examples
#' \dontrun{
#' repr <- langchain_representation(chain = NULL, prompt = "What are these documents about?")
#' }
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



# langchain ---------------------------------------------------------------
# Removed 2026-04-18 (RFORGE C003): anonymous unreachable function containing a
# hardcoded live OpenAI API key. Credential has been flagged for rotation.
# If LangChain integration is needed, add a properly named, exported function
# that reads the key from Sys.getenv("OPENAI_API_KEY") with nzchar() validation.
