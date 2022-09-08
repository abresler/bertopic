# https://scikit-learn.org/0.15/index.html

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
import_bertopic <-
  function(assign_to_environment = T) {
    bertopic <- reticulate::import("bertopic")
    !'bertopic' %>% exists() & assign_to_environment
    if (assign_to_environment) {
      assign('bertopic', bertopic, envir = .GlobalEnv)
    }
    bertopic
  }

#' Initiate BERT Topic Model
#'
#' Functions for unsupervised clustering algorithms.
#'
#' \itemize{
#' \item \href{https://maartengr.github.io/BERTopic/api/bertopic.html}berttopic}
#' }
#'
#' @return python object
#' @export
#'
#' @examples
#' import_bertopic()
#' data <- sklearn::sk_datasets()
#' docs <- data$fetch_20newsgroups(subset = 'all', remove = c('headers', 'footers', 'quotes'))
#' docs <- docs["data"]
#' tm <- bert_topic()
#' topic_model <- tm$fit_transform(documents = docs)
#'
#'

bert_topic <-
  function() {
    bertopic <- import_bertopic(assign_to_environment = F)
    obj <- bertopic$BERTopic()
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


