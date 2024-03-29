% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{bert_topic_tree_text}
\alias{bert_topic_tree_text}
\title{Returns Topic Tree Text}
\usage{
bert_topic_tree_text(obj, hierarchy, tight_layout = FALSE, max_distance = NULL)
}
\arguments{
\item{obj}{Bertopic Model}

\item{hierarchy}{Output from `bert_topic_hierarchy`}

\item{tight_layout}{Whether to use a tight layout (narrow width) for easier readability if you have hundreds of topics.  Default `FALSE`}

\item{max_distance}{The maximum distance between two topics. This value is based on the Distance column in hier_topics.  Default `NULL`}
}
\description{
Returns Topic Tree Text
}
