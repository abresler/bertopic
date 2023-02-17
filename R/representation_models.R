
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
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    obj$KeyBERTInspired(top_n_words = as.integer(top_n_words),
                        nr_repr_docs = as.integer(nr_repr_docs),
                        nr_samples = as.integer(nr_samples),
                        nr_candidate_words = as.integer(nr_samples),
                        random_state = as.integer(random_state))
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
  function(diversity = .1, top_n_words = 10,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    obj$MaximalMarginalRelevance(diversity = diversity, top_n_words = as.integer(top_n_words))
  }

#' Title
#'
#' @param client
#' @param model
#' @param prompt
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
cohere_representation <-
  function(client = NULL, model = NULL, prompt = NULL,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    obj$Cohere(client = client, model = model, prompt = prompt)
  }

#' OpenAI Representation Model
#'
#' @param model
#' @param prompt
#' @param generator_kwargs
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
open_ai_representation <-
  function(model = "text-ada-0001",
           prompt = NULL ,
           generator_kwargs = NULL,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    obj$OpenAI(model = model,
               prompt = prompt ,
               generator_kwargs = generator_kwargs)
  }


#' Extract Topic Keywords based on their Part-of-Speech
#'
#' @param model The Spacy model to use
#' @param top_n_words The top n words to extract.  Default `10`
#' @param pos_patterns Patterns for Spacy to use. See https://spacy.io/usage/rule-based-matching
#' @param obj
#'
#' @return
#' @export
#'
#' @examples
part_of_speech_representation <-
  function(model = 'en_core_web_sm',
           top_n_words = 10,
           pos_patterns = NULL,
           obj = NULL) {
    obj <- bertopic_representations(obj = obj)
    obj$PartOfSpeech(model = model,
                     top_n_words = as.integer(top_n_words),
                     pos_patterns = pos_patterns)
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

    obj <- bertopic_representations(obj = obj)
    obj$ZeroShotClassification(
      candidate_topics = candidate_topics,
      model = model,
      pipeline_kwargs = pipeline_kwargs,
      min_prob = min_prob
    )
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
           random_state =42,
           obj = NULL) {

    obj <- bertopic_representations(obj = obj)
    obj$TextGeneration(
      model = model,
      prompt = prompt,
      pipeline_kwargs = pipeline_kwargs,
      random_state = as.integer(random_state)
    )
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
           obj = NULL) {
  obj <- bertopic_representations(obj = obj)
  obj$LangChain(chain = chain, prompt = prompt)

}
